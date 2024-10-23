import common.{type ServerStatus, Maintenance, Offline, Online}
import dot_env as dot
import dot_env/env
import gleam/result

pub type Context {
  Context(static_directory: String, url: String)
}

pub type Environment {
  Development
  Production
}

pub type Config {
  Config(
    db_conn: String,
    secret_key_base: String,
    port: Int,
    env: Environment,
    server_status: ServerStatus,
  )
}

pub fn read_config() {
  dot.new()
  |> dot.set_path(".env")
  |> dot.load

  let assert Ok(database_path) = env.get_string("DATABASE_PATH")
  let assert Ok(secret_key_base) = env.get_string("SECRET_KEY_BASE")
  let env = case result.unwrap(env.get_string("GLEAM_ENV"), "") {
    "development" -> Development
    _ -> Production
  }
  let server_status = case result.unwrap(env.get_string("SERVER_STATUS"), "") {
    "online" -> Online
    "maintenance" -> Maintenance
    _ -> Offline
  }
  let assert Ok(port) = env.get_int("PORT")
  let db_conn = "file:" <> database_path <> "?mode=rw&cache=shared"
  Config(db_conn, secret_key_base, port, env, server_status)
}

pub fn is_dev() {
  read_config().env == Development
}

pub fn conn_path() {
  read_config().db_conn
}
