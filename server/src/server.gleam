import gleam/erlang/process
import mist
import server/config
import server/router
import wisp
import wisp/wisp_mist

//TODO: Ultimos testes

pub fn main() {
  wisp.configure_logger()

  let cnf = config.read_config()
  let secret_key_base = cnf.secret_key_base

<<<<<<< HEAD
<<<<<<< HEAD
=======
  let ctx = Context(static_directory: static_directory(), port: cnf.port)
=======
  let url = case cnf.env {
    config.Development -> "http://localhost:1234"
    config.Production -> "https://lauraxv.fly.dev"
  }
  let ctx = Context(static_directory: static_directory(), url: url)
>>>>>>> monorepo

  let handler = router.handle_request(_, ctx)

>>>>>>> monorepo
  let assert Ok(_) =
    wisp_mist.handler(router.handle_request, secret_key_base)
    |> mist.new
    |> mist.port(cnf.port)
    |> mist.start_http

  process.sleep_forever()
}
