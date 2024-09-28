-record(create_confirmation, {
    user_id :: integer(),
    name :: binary(),
    invite_name :: binary(),
    phone :: binary(),
    comments :: gleam@option:option(binary())
}).
