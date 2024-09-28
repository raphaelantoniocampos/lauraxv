-record(list_confirmation_db_row, {
    id :: integer(),
    user_id :: integer(),
    name :: binary(),
    invite_name :: binary(),
    phone :: binary(),
    comments :: gleam@option:option(binary()),
    person_name :: gleam@option:option(binary())
}).
