-record(user, {
    id :: integer(),
    username :: binary(),
    email :: binary(),
    password :: binary(),
    is_confirmed :: boolean(),
    is_admin :: boolean()
}).
