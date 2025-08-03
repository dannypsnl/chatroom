import Config

config :chatroom, :server, port: String.to_integer(System.get_env("PORT") || "5555")
