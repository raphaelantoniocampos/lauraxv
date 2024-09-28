-record(gift, {
    id :: integer(),
    name :: binary(),
    pic :: binary(),
    link :: gleam@option:option(binary()),
    selected_by :: gleam@option:option(integer())
}).
