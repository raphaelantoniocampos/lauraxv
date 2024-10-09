import gleam/erlang/process
import mist
import server/config.{Context}
import server/router
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  let cnf = config.read_config()
  let secret_key_base = cnf.secret_key_base

  let url = case cnf.env {
    config.Development -> "http://localhost:1234"
    config.Production -> "https://lauraxv.fly.dev"
  }
  let ctx = Context(static_directory: static_directory(), url: url)

  let handler = router.handle_request(_, ctx)

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(cnf.port)
    |> mist.start_http

  process.sleep_forever()
}

fn static_directory() {
  let assert Ok(priv_directory) = wisp.priv_directory("server")
  priv_directory <> "/static"
}
