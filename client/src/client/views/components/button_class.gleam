import lustre/attribute.{type Attribute, class}

pub fn button_class(min_w: String) -> Attribute(a) {
  class(
    "bg-emerald-600 hover:bg-emerald-700 min-w-"
    <> min_w
    <> " text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105",
  )
}
