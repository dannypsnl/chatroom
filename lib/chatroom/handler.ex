defmodule Chatroom.Handler do
  use GenServer

  require Logger

  @doc """
  This start_link is required by `:ranch`
  """
  def start_link(ref, socket, transport, %{online_peers: online_peers} = _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport, online_peers])
    {:ok, pid}
  end

  def init(ref, socket, transport, online_peers) do
    peername = peername(socket)

    Logger.info("Peer #{peername} connecting")

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}])

    Agent.update(online_peers, fn ps -> MapSet.put(ps, socket) end)

    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      peername: peername,
      online_peers: online_peers
    })
  end

  def handle_info(
        {:tcp, _, message},
        %{transport: transport, peername: peername, online_peers: online_peers} =
          state
      ) do
    Logger.info(
      "Received new message from peer #{peername}: #{inspect(message)}. Broadcast to all clients"
    )

    Agent.get(online_peers, fn ps ->
      Enum.each(ps |> MapSet.to_list(), fn socket ->
        transport.send(socket, "#{peername}: #{message}")
      end)
    end)

    {:noreply, state}
  end

  def handle_info(
        {:tcp_closed, _},
        %{socket: socket, peername: peername, online_peers: online_peers} = state
      ) do
    Agent.update(online_peers, fn ps ->
      MapSet.delete(ps, socket)
    end)

    Logger.info("Peer #{peername} disconnected")

    {:stop, :normal, state}
  end

  def handle_info(
        {:tcp_error, _, reason},
        %{socket: socket, peername: peername, online_peers: online_peers} = state
      ) do
    Agent.update(online_peers, fn ps ->
      MapSet.delete(ps, socket)
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
