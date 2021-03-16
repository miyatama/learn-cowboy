%%%-------------------------------------------------------------------
%% @doc rest public API
%% @end
%%%-------------------------------------------------------------------

-module(rest_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEBUG(S), io:fwrite("[DEBUG] rest_app: " ++ S ++ "~n")).

start(_StartType, _StartArgs) ->
  ?DEBUG("start/2"),
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
    #{
      env => #{dispatch => Dispatch}
    }),
  rest_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%% internal functions
