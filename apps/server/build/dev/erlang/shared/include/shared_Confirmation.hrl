-record(confirmation, {
    id :: integer(),
    user_id :: integer(),
    name :: binary(),
    invite_name :: binary(),
    phone :: binary(),
    comments :: gleam@option:option(binary()),
    people_names :: list(binary())
}).
