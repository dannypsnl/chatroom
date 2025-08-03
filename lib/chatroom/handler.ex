defmodule Chatroom.Handler do
  use GenServer

  require Logger

  @doc """
  This start_link is required by `:ranch`
  """
  def start_link(ref, socket, transport, %{online_peers: online_peers} = _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport, online_peers])
    Agent.update(online_peers, fn ps -> MapSet.put(ps, pid) end)
    {:ok, pid}
  end

  def init(ref, socket, transport, online_peers) do
    peername = peername(socket)

    Logger.info("Peer #{peername} connecting")

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}])

    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      peername: peername,
      online_peers: online_peers
    })
  end

  def push_message(pid, message) do
    GenServer.cast(pid, {:push_message, message})
  end

  def handle_cast(
        {:push_message, message},
        %{socket: socket, transport: transport} = state
      ) do
    transport.send(socket, message)

    {:noreply, state}
  end

  def handle_info(
        {:tcp, _, message},
        %{peername: peername, online_peers: online_peers} =
          state
      ) do
    Logger.info(
      "Received new message from peer #{peername}: #{inspect(message)}. Broadcast to all clients"
    )

    Agent.get(online_peers, fn ps ->
      Enum.each(ps |> MapSet.to_list(), fn pid ->
        push_message(pid, "#{peername}: #{message}")
      end)
    end)

    {:noreply, state}
  end

  def handle_info(
        {:tcp_closed, _},
        %{peername: peername, online_peers: online_peers} = state
      ) do
    Agent.update(online_peers, fn ps ->
      MapSet.delete(ps, self())
    end)

    Logger.info("Peer #{peername} disconnected")

    {:stop, :normal, state}
  end

  def handle_info(
        {:tcp_error, _, reason},
        %{peername: peername, online_peers: online_peers} = state
      ) do
    Agent.update(online_peers, fn ps ->
      MapSet.delete(ps, self())
    end)

    Logger.info("Error with peer #{peername}: #{inspect(reason)}")

    {:stop, :normal, state}
  end

  defp peername(socket) do
    {:ok, {addr, port}} = :inet.peername(socket)

    address =
      addr
      |> :inet_parse.ntoa()
      |> to_string()

    "#{address}:#{port}"
  end

  def init(_) do
    {:stop, []}
  end
end
