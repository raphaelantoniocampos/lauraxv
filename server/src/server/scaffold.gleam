import lustre/attribute.{attribute, href, name, rel, src, type_}
import lustre/element
import lustre/element/html.{head, html, link, meta, script, title}

pub fn page_scaffold(content: element.Element(a)) {
  html([], [
    head([], [
      meta([attribute("charset", "UTF-8")]),
      meta([
        attribute("content", "width=device-width, initial-scale=1.0"),
        name("viewport"),
      ]),
      title([], "🌟 Laura 15 Anos"),
      meta([
        attribute("content", "Festa de 15 anos da Laura"),
        name("description"),
      ]),
      meta([attribute("content", "pt_BR"), attribute("property", "og:locale")]),
      meta([
        attribute("content", "🌟 Laura 15 Anos"),
        attribute("property", "og:site_name"),
      ]),
      meta([attribute("content", "website"), attribute("property", "og:type")]),
      meta([
        attribute("content", "🌟 Laura 15 Anos"),
        attribute("property", "og:title"),
      ]),
      meta([
        attribute("content", "Festa de 15 anos da Laura"),
        attribute("property", "og:description"),
      ]),
      meta([
        attribute("content", "https://lauraxv.fly.dev/"),
        attribute("property", "og:url"),
      ]),
      link([href("/static/client.min.css"), rel("stylesheet")]),
      script([src("/static/client.min.mjs"), attribute.type_("module")], ""),
      link([
        rel("stylesheet"),
        href("https://fonts.googleapis.com/css2?family=Pacifico&display=swap"),
      ]),
      link([
        rel("stylesheet"),
        href(
          "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css",
        ),
      ]),
    ]),
    content,
  ])
}
