-module(shared).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([gift/0, select_gift/0, user/0, confirmation/0, comment/0]).

-type gift() :: {gift,
        integer(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(integer())}.

-type select_gift() :: {select_gift, integer(), integer(), boolean()}.

-type user() :: {user,
        integer(),
        binary(),
        binary(),
        binary(),
        boolean(),
        boolean()}.

-type confirmation() :: {confirmation,
        integer(),
        integer(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        list(binary())}.

-type comment() :: {comment, binary(), gleam@option:option(binary())}.


