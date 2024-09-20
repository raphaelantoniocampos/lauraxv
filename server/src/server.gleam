import gleam/erlang/process
import mist
import server/router
import wisp
import wisp/testing.{default_secret_key_base}
import wisp/wisp_mist

//TODO: Pegar fotos da fotos page
//TODO: Atualizar presentes
//TODO: Armazenar Confirmação de Presença
//TODO: Página de Admin (Mostrar número de confirmados e nomes)
//TODO: Ultimos testes
//TODO: Deploy

pub fn main() {
  wisp.configure_logger()

  //TODO: Criar secret key
  // let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp_mist.handler(router.handle_request, default_secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}
