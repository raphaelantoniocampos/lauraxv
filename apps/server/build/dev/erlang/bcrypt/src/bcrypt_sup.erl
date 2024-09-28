%% @copyright 2011 Hunter Morris
%% @doc Implementation of `supervisor' behaviour.
%% @private
%% @end
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_sup).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PortChildren
        = [{bcrypt_port_sup, {bcrypt_port_sup, start_link, []}, permanent,
            16#ffffffff, supervisor, [bcrypt_port_sup]},
           {bcrypt_pool, {bcrypt_pool, start_link, []}, permanent,
            16#ffffffff, worker, [bcrypt_pool]}],
    NifChildren
        = [{bcrypt_nif_pool_sup, {bcrypt_nif_pool_sup, start_link, []}, permanent,
            16#ffffffff, supervisor, [bcrypt_nif_pool_sup]}],
    case application:get_env(bcrypt, mechanism) of
        undefined  -> {stop, no_mechanism_defined};
        {ok, nif}  -> {ok, {{one_for_all, 15, 60}, NifChildren}};
        {ok, port} -> {ok, {{one_for_all, 15, 60}, PortChildren}}
    end.
