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
        attribute(
          "content",
          "Um site feito para o aniversário de 15 Anos de Laura Moreira",
        ),
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
        attribute(
          "content",
          "Um site feito para o aniversário de 15 Anos de Laura Moreira",
        ),
        attribute("property", "og:description"),
      ]),
      meta([
        attribute("content", "http://localhost:1234/"),
        attribute("property", "og:url"),
      ]),
      link([href("/priv/static/client.css"), rel("stylesheet")]),
      link([
        href("https://fonts.googleapis.com/css2?family=Pacifico&display=swap"),
        rel("stylesheet"),
      ]),
      script([src("/priv/static/client.mjs"), type_("module")], ""),
    ]),
    content,
  ])
}
