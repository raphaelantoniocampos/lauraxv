// import {
//   Carousel,
//   initTWE,
// } from "tw-elements";
//
// initTWE({ Carousel });

import client/state.{UserChangedSlide}
import falala/fa
import gleam/int
import lustre/attribute.{alt, attribute, class, href, id, src, type_}
import lustre/carousel
import lustre/carousel/slide
import lustre/element.{text}
import lustre/element/html.{a, button, div, h1, h2, img, p, span}
import lustre/element/svg.{path, svg}
import lustre/event
import rada/date

// import tw_elements.{Carousel, initTWE}
fn slides() {
  let slide1 = slide.StaticSlide("/priv/static/photo1.jpeg")
  let slide2 = slide.StaticSlide("/priv/static/photo2.jpeg")
  let slide3 = slide.StaticSlide("/priv/static/photo3.jpeg")
  [slide1, slide2, slide3]
}

pub fn body() {
  [
    div(fa.sliders_h(), [
      img([
        alt("Foto 1"),
        class("block w-full"),
        src("/priv/static/photo1.jpeg"),
      ]),
      img([
        alt("Foto 1"),
        class("block w-full"),
        src("/priv/static/photo2.jpeg"),
      ]),
      img([
        alt("Foto 1"),
        class("block w-full"),
        src("/priv/static/photo3.jpeg"),
      ]),
    ]),
    // div(
    //   [
    //     attribute("data-twe-ride", "carousel"),
    //     attribute("data-twe-carousel-init", ""),
    //     class("relative"),
    //     id("carouselExampleCrossfade"),
    //   ],
    //   [
    //     div(
    //       [
    //         attribute("data-twe-carousel-indicators", ""),
    //         class(
    //           "absolute inset-x-0 bottom-0 z-[2] mx-[15%] mb-4 flex list-none justify-center p-0",
    //         ),
    //       ],
    //       [
    //         button(
    //           [
    //             attribute("aria-label", "Slide 1"),
    //             attribute("aria-current", "true"),
    //             class(
    //               "mx-[3px] box-content h-[3px] w-[30px] flex-initial cursor-pointer border-0 border-y-[10px] border-solid border-transparent bg-white bg-clip-padding p-0 -indent-[999px] opacity-50 transition-opacity duration-[600ms] ease-[cubic-bezier(0.25,0.1,0.25,1.0)] motion-reduce:transition-none",
    //             ),
    //             attribute("data-twe-carousel-active", ""),
    //             attribute("data-twe-slide-to", "0"),
    //             attribute("data-twe-target", "#carouselExampleCrossfade"),
    //             type_("button"),
    //           ],
    //           [],
    //         ),
    //         button(
    //           [
    //             attribute("aria-label", "Slide 2"),
    //             class(
    //               "mx-[3px] box-content h-[3px] w-[30px] flex-initial cursor-pointer border-0 border-y-[10px] border-solid border-transparent bg-white bg-clip-padding p-0 -indent-[999px] opacity-50 transition-opacity duration-[600ms] ease-[cubic-bezier(0.25,0.1,0.25,1.0)] motion-reduce:transition-none",
    //             ),
    //             attribute("data-twe-slide-to", "1"),
    //             attribute("data-twe-target", "#carouselExampleCrossfade"),
    //             type_("button"),
    //           ],
    //           [],
    //         ),
    //         button(
    //           [
    //             attribute("aria-label", "Slide 3"),
    //             class(
    //               "mx-[3px] box-content h-[3px] w-[30px] flex-initial cursor-pointer border-0 border-y-[10px] border-solid border-transparent bg-white bg-clip-padding p-0 -indent-[999px] opacity-50 transition-opacity duration-[600ms] ease-[cubic-bezier(0.25,0.1,0.25,1.0)] motion-reduce:transition-none",
    //             ),
    //             attribute("data-twe-slide-to", "2"),
    //             attribute("data-twe-target", "#carouselExampleCrossfade"),
    //             type_("button"),
    //           ],
    //           [],
    //         ),
    //       ],
    //     ),
    //     div(
    //       [
    //         class(
    //           "relative w-full overflow-hidden after:clear-both after:block after:content-['']",
    //         ),
    //       ],
    //       [
    //         div(
    //           [
    //             attribute("data-twe-carousel-active", ""),
    //             attribute("data-twe-carousel-item", ""),
    //             attribute("data-twe-carousel-fade", ""),
    //             class(
    //               "relative float-left -mr-[100%] w-full !transform-none opacity-0 transition-opacity duration-[600ms] ease-in-out motion-reduce:transition-none",
    //             ),
    //           ],
    //           [
    //             img([
    //               alt("Foto 1"),
    //               class("block w-full"),
    //               src("/priv/static/photo1.jpeg"),
    //             ]),
    //           ],
    //         ),
    //         div(
    //           [
    //             attribute("data-twe-carousel-item", ""),
    //             attribute("data-twe-carousel-fade", ""),
    //             class(
    //               "relative float-left -mr-[100%] hidden w-full !transform-none opacity-0 transition-opacity duration-[600ms] ease-in-out motion-reduce:transition-none",
    //             ),
    //           ],
    //           [
    //             img([
    //               alt("Foto 2"),
    //               class("block w-full"),
    //               src("/priv/static/photo2.jpeg"),
    //             ]),
    //           ],
    //         ),
    //         div(
    //           [
    //             attribute("data-twe-carousel-item", ""),
    //             attribute("data-twe-carousel-fade", ""),
    //             class(
    //               "relative float-left -mr-[100%] hidden w-full !transform-none opacity-0 transition-opacity duration-[600ms] ease-in-out motion-reduce:transition-none",
    //             ),
    //           ],
    //           [
    //             img([
    //               alt("Foto 3"),
    //               class("block w-full"),
    //               src("/priv/static/photo3.jpeg"),
    //             ]),
    //           ],
    //         ),
    //       ],
    //     ),
    //     button(
    //       [
    //         attribute("data-twe-slide", "prev"),
    //         attribute("data-twe-target", "#carouselExampleCrossfade"),
    //         type_("button"),
    //         class(
    //           "absolute bottom-0 left-0 top-0 z-[1] flex w-[15%] items-center justify-center border-0 bg-none p-0 text-center text-white opacity-50 transition-opacity duration-150 ease-[cubic-bezier(0.25,0.1,0.25,1.0)] hover:text-white hover:no-underline hover:opacity-90 hover:outline-none focus:text-white focus:no-underline focus:opacity-90 focus:outline-none motion-reduce:transition-none",
    //         ),
    //       ],
    //       [
    //         span([class("inline-block h-8 w-8")], [
    //           svg(
    //             [
    //               class("h-6 w-6"),
    //               attribute("stroke", "currentColor"),
    //               attribute("stroke-width", "1.5"),
    //               attribute("viewBox", "0 0 24 24"),
    //               attribute("fill", "none"),
    //               attribute("xmlns", "http://www.w3.org/2000/svg"),
    //             ],
    //             [
    //               svg.path([
    //                 attribute("d", "M15.75 19.5L8.25 12l7.5-7.5"),
    //                 attribute("stroke-linejoin", "round"),
    //                 attribute("stroke-linecap", "round"),
    //               ]),
    //             ],
    //           ),
    //         ]),
    //         span(
    //           [
    //             class(
    //               "!absolute !-m-px !h-px !w-px !overflow-hidden !whitespace-nowrap !border-0 !p-0 ![clip:rect(0,0,0,0)]",
    //             ),
    //           ],
    //           [text("Previous")],
    //         ),
    //       ],
    //     ),
    //     button(
    //       [
    //         attribute("data-twe-slide", "next"),
    //         attribute("data-twe-target", "#carouselExampleCrossfade"),
    //         type_("button"),
    //         class(
    //           "absolute bottom-0 right-0 top-0 z-[1] flex w-[15%] items-center justify-center border-0 bg-none p-0 text-center text-white opacity-50 transition-opacity duration-150 ease-[cubic-bezier(0.25,0.1,0.25,1.0)] hover:text-white hover:no-underline hover:opacity-90 hover:outline-none focus:text-white focus:no-underline focus:opacity-90 focus:outline-none motion-reduce:transition-none",
    //         ),
    //       ],
    //       [
    //         span([class("inline-block h-8 w-8")], [
    //           svg(
    //             [
    //               class("h-6 w-6"),
    //               attribute("stroke", "currentColor"),
    //               attribute("stroke-width", "1.5"),
    //               attribute("viewBox", "0 0 24 24"),
    //               attribute("fill", "none"),
    //               attribute("xmlns", "http://www.w3.org/2000/svg"),
    //             ],
    //             [
    //               path([
    //                 attribute("d", "M8.25 4.5l7.5 7.5-7.5 7.5"),
    //                 attribute("stroke-linejoin", "round"),
    //                 attribute("stroke-linecap", "round"),
    //               ]),
    //             ],
    //           ),
    //         ]),
    //         span(
    //           [
    //             class(
    //               "!absolute !-m-px !h-px !w-px !overflow-hidden !whitespace-nowrap !border-0 !p-0 ![clip:rect(0,0,0,0)]",
    //             ),
    //           ],
    //           [text("Next")],
    //         ),
    //       ],
    //     ),
    //   ],
    // ),
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
