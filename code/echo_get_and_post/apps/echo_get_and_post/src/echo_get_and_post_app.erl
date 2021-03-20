%%%-------------------------------------------------------------------
%% @doc echo_get_and_post public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_get_and_post_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] echo_get_and_post_app: " ++ S ++ "~n")).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {
      '_',
      [
        {"/", toppage_h, []}
      ]
    }
  ]),
  {ok, _} = cowboy:start_clear(
    http,
    [
      {port, 80}
    ],
    #{env => #{dispatch => Dispatch}}),
  echo_get_and_post_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%% internal functions
