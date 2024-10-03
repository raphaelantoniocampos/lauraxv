import gleam/erlang/process
import mist
import server/config
import server/router
import wisp
import wisp/wisp_mist

//TODO: Deploy
//TODO: Ultimos testes

pub fn main() {
  wisp.configure_logger()

  let cnf = config.read_config()
  let secret_key_base = cnf.secret_key_base

  let assert Ok(_) =
    wisp_mist.handler(router.handle_request, secret_key_base)
    |> mist.new
    |> mist.port(cnf.port)
    |> mist.start_http

  process.sleep_forever()
}
