import client/state.{UserChangedSlide}
import gleam/int
import lustre/attribute.{alt, attribute, class, href, id, src}
import lustre/element.{text}
import lustre/element/html.{a, button, div, h1, h2, img, p, span}
import lustre/event
import rada/date

pub fn body() {
  [
    div(
      [
        attribute("data-aos", "fade-down"),
        id("home"),
        class("slideshow-container"),
      ],
      [
        div([class("mySlides slide-fade")], [
          // Note a classe "mySlides" aqui
          div([class("numbertext")], [text("1 / 3")]),
          img([
            attribute("style", "width:100%"),
            src("/priv/static/photo1.jpeg"),
          ]),
        ]),
        div([class("mySlides slide-fade")], [
          // Outra "mySlides"
          div([class("numbertext")], [text("2 / 3")]),
          img([
            attribute("style", "width:100%"),
            src("/priv/static/photo2.jpeg"),
          ]),
        ]),
        div([class("mySlides slide-fade")], [
          // E outra
          div([class("numbertext")], [text("3 / 3")]),
          img([
            attribute("style", "width:100%"),
            src("/priv/static/photo3.jpeg"),
          ]),
        ]),
        button(
          [
            class("prev"),
            event.on_click(plus_slides(-1)),
            attribute.type_("button"),
          ],
          [text("❮")],
        ),
        a([attribute("onclick", plus_slides(1)), class("next")], [text("❯")]),
      ],
    ),
    div([class("dot-container")], [
      span([attribute("onclick", "currentSlide(1)"), class("dot")], []),
      span([attribute("onclick", "currentSlide(2)"), class("dot")], []),
      span([attribute("onclick", "currentSlide(3)"), class("dot")], []),
    ]),
    div([attribute("data-aos", "fade-up"), class("text-center mt-12")], [
      h1(
        [
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class("text-5xl text-white font-bold"),
        ],
        [text("Laura 15 Anos")],
      ),
      p([class("text-xl text-white mt-4")], [
        text("Pompéu, MG - 14 de Dezembro de 2024"),
      ]),
    ]),
    div([attribute("data-aos", "zoom-in"), class("text-center mt-6")], [
      p([class("text-3xl text-white font-bold")], [
        text("Faltam "),
        span([class("text-yellow-300"), id("countdown")], [text(countdown())]),
        text(" dias para a festa!"),
      ]),
    ]),
    div(
      [
        attribute("data-aos", "fade-right"),
        id("evento"),
        class(
          "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200",
        ),
      ],
      [
        div([class("flex items-center justify-between mb-8")], [
          img([
            class(
              "rounded-full shadow-md transform hover:scale-105 transition duration-500 w-1/3",
            ),
            alt("Laura's Birthday"),
            src("/priv/static/profile.jpeg"),
          ]),
          div([class("flex-1 ml-12")], [
            h1([class("text-5xl font-bold text-pink-600 mb-4")], [
              text("Aniversário de 15 Anos de Laura"),
            ]),
            p([class("text-gray-600 text-lg mb-6")], [
              text(
                "Lhe convido para celebrar esse dia tão especial em minha vida, meus 15 anos! Confirme sua presença até o dia 06/12 para receber seu convite individual.",
              ),
            ]),
            div([class("space-x-4")], [
              button(
                [
                  class(
                    "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                  ),
                ],
                [text("Confirmar Presença")],
              ),
              button(
                [
                  class(
                    "bg-yellow-500 hover:bg-yellow-600 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
                  ),
                ],
                [text("Lista de Presentes")],
              ),
            ]),
          ]),
        ]),
        div(
          [
            attribute("data-aos", "fade-up"),
            class("bg-gray-100 p-6 rounded-lg shadow-inner"),
          ],
          [
            h2([class("text-3xl font-semibold text-pink-700 mb-4")], [
              text("Sobre Laura"),
            ]),
            p([class("text-gray-700 text-lg")], [
              text(
                "Laura está completando 15 anos e queremos celebrar com todos que fazem parte de sua vida. A festa será cheia de alegria, música, e muita diversão. Não perca!",
              ),
            ]),
          ],
        ),
        div([attribute("data-aos", "zoom-in"), class("mt-8 text-center")], [
          a(
            [
              class("text-pink-600 hover:text-pink-800 font-semibold underline"),
              href("/event"),
            ],
            [text("Log In")],
          ),
        ]),
      ],
    ),
  ]
}

// (
//           [
//             class("prev"),
//             event.on_click(plus_slides(-1)),
//             attribute.type_("button"),
//           ],
//           [text("❮")],
//         ),

pub type Dir {
  Next
  Prev
}

pub fn change_slide_button(dir: Dir) {
  case dir {
    Next ->
      button(
        [
          class("next"),
          event.on_click(UserChangedSlide(1)),
          attribute.type_("button"),
        ],
        [text(">")],
      )
    Prev ->
      button(
        [
          class("prev"),
          event.on_click(UserChangedSlide(-1)),
          attribute.type_("button"),
        ],
        [text("<")],
      )
  }
}

fn plus_slides(current_slide_index: Int, total_slides: Int, n: Int) -> Int {
  let new_index = current_slide_index + n
  show_slide(new_index, total_slides)
}

fn current_slide(_current_slide_index: Int, total_slides: Int, n: Int) -> Int {
  show_slide(n, total_slides)
}

fn show_slide(slide_index: Int, total_slides: Int) -> Int {
  case slide_index {
    slide_index if slide_index > total_slides -> 1
    slide_index if slide_index < 1 -> total_slides
    _ -> slide_index
  }
}

fn countdown() {
  int.to_string(date.diff(
    date.Days,
    date.today(),
    date.from_calendar_date(2024, date.Dec, 14),
  ))
  // document.addEventListener('DOMContentLoaded', function () {
  //   var swiper = new Swiper('.swiper-container', {
  //     direction: 'horizontal',
  //     loop: true,
  //     pagination: {
  //       el: '.swiper-pagination',
  //       clickable: true,
  //     },
  //     navigation: {
  //       nextEl: '.swiper-button-next',
  //       prevEl: '.swiper-button-prev',
  //     },
  //   });
  // let now = birl.now()
  // let event_date = birl.Day(year: 2024, month: 12, date: 14)
  // let remaining = birl.difference(now, event_date)
  //   function updateCountdown() {
  //     const eventDate = new Date('2024-12-14T00:00:00');
  //     const now = new Date();
  //     const timeDiff = eventDate - now;
  //     const daysRemaining = Math.ceil(timeDiff / (1000 * 60 * 60 * 24));
  //     document.getElementById('countdown').textContent = daysRemaining;
  //   }
  //
  //   updateCountdown();
  //   setInterval(updateCountdown, 1000 * 60 * 60 * 24);
  // });
}
