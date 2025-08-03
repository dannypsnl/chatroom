defmodule Chatroom.Server do
  use GenServer

  alias Chatroom.Handler
  require Logger

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(port: port) do
    opts = [{:port, port}]

    {:ok, online_peers} = Agent.start_link(fn -> MapSet.new() end)

    {:ok, pid} =
      :ranch.start_listener(:network, :ranch_tcp, opts, Handler, %{online_peers: online_peers})

    Logger.info("Listening for connections on port #{port}")

    {:ok, pid}
  end
end
