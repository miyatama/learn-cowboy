-module(toppage_h).

-export([init/2]).

-define(DEBUG(S), io:fwrite("[DEBUG] toppage_h: " ++ S ++ "~n")).

init(Req, Opts) ->
  ?DEBUG("init/2"),
  Result = echo(cowboy_req:method(Req), Req, Opts),
  {ok, Result, Opts}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% echo/3
echo(<<"GET">>, Req, _Opts) ->
  ?DEBUG("echo/3 - GET"),
  #{echo := Echo} = cowboy_req:match_qs([
    {echo, [], undefined}
  ], Req),
  echo(Echo, Req);
echo(<<"POST">>, Req1, _Opts) ->
  ?DEBUG("echo/3 - POST"),
  {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req1),
  Echo = proplists:get_value(<<"echo">>, PostVals),
  echo(Echo, Req2);
echo(_, Req, _Opts) ->
  ?DEBUG("echo/3"),
  cowboy_req:reply(405, Req).

% echo/2
echo(undefined, Req) ->
  ?DEBUG("echo/2 - undefined"),
  cowboy_req:reply(
    400,
    #{},
    <<"missing parameter">>,
    Req);
echo(Echo, Req) ->
  ?DEBUG("echo/2"),
  cowboy_req:reply(
    200,
    #{
      <<"content-type">> => <<"text/plain; charset=utf-8">>
    },
    Echo,
    Req).

