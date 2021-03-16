-module(toppage_h).

-export([init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  delete_resource/2,
  delete_completed/2,
  resource_exists/2,
  hello_to_html/2,
  hello_to_json/2,
  hello_to_text/2]).

-define(DEBUG(S), io:fwrite("[DEBUG] toppage_h: " ++ S ++ "~n")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] toppage_h: " ++ S ++ "~n", Args)).

init(Req, Opts) ->
  ?DEBUG("init/2"),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  ?DEBUG("allowed_methods/2"),
  {
    [
      <<"GET">>,
      <<"POST">>,
      <<"PUT">>,
      <<"DELETE">>
    ],
    Req,
    State 
  }.

content_types_accepted(Req, State) ->
  ?DEBUG("content_types_accepted/2"),
  {
    [
      {{<<"text">>, <<"html">>, '*'}, hello_to_html},
      {{<<"application">>, <<"json">>, '*'}, hello_to_json},
      {{<<"text">>, <<"plain">>, '*'}, hello_to_text}
    ],
    Req,
   State 
  }.

content_types_provided(Req, State) ->
  ?DEBUG("content_types_provided/2"),
  {
    [
      {{<<"text">>, <<"html">>, '*'}, hello_to_html},
      {{<<"application">>, <<"json">>, '*'}, hello_to_json},
      {{<<"text">>, <<"plain">>, '*'}, hello_to_text}
    ],
    Req,
   State 
  }.

delete_resource(Req, State) ->
  ?DEBUG("delete_resource/2"),
  {true, Req, State}.

delete_completed(Req, State) ->
  ?DEBUG("delete_completed/2"),
  case cowboy_req:header(<<"content-type">>, Req) of
    <<"application/json">> ->delete_hello_json(Req, State);
    <<"text/html">> -> delete_hello_html(Req, State);
    <<"text/plain">> -> delete_hello_text(Req, State)
  end.

resource_exists(Req, State) ->
  ?DEBUG("resource_exists/2"),
  {true, Req, State}.

% curl -X GET -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost
% curl -X POST -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost
% curl -X PUT -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost
% curl -X DELETE -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost
hello_to_json(Req, State) ->
  ?DEBUG("hello_to_json/2"),
  case cowboy_req:method(Req) of
    <<"GET">> -> get_hello_json(Req, State);
    <<"PUT">> -> put_hello_json(Req, State);
    <<"POST">> -> post_hello_json(Req, State)
  end.


% curl -X GET -H 'Content-Type: text/html' -H 'Accept: text/html' http://localhost
% curl -X POST -H 'Content-Type: text/html' -H 'Accept: text/html' http://localhost
% curl -X PUT -H 'Content-Type: text/html' -H 'Accept: text/html' http://localhost
% curl -X DELETE -H 'Content-Type: text/html' -H 'Accept: text/html' http://localhost
hello_to_html(Req, State) ->
  ?DEBUG("hello_to_html/2"),
  case cowboy_req:method(Req) of
    <<"GET">> -> get_hello_html(Req, State);
    <<"PUT">> -> put_hello_html(Req, State);
    <<"POST">> -> post_hello_html(Req, State)
  end.

% curl -X GET -H 'Content-Type: text/plain' -H 'Accept: text/plain' http://localhost
% curl -X POST -H 'Content-Type: text/plain' -H 'Accept: text/plain' http://localhost
% curl -X PUT -H 'Content-Type: text/plain' -H 'Accept: text/plain' http://localhost
% curl -X DELETE -H 'Content-Type: text/plain' -H 'Accept: text/plain' http://localhost
hello_to_text(Req, State) ->
  ?DEBUG("hello_to_text/2"),
  case cowboy_req:method(Req) of
    <<"GET">> -> get_hello_text(Req, State);
    <<"PUT">> -> put_hello_text(Req, State);
    <<"POST">> -> post_hello_text(Req, State)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_hello_json(Req, State) ->
  ?DEBUG("get_hello_json/2"),
  {
    <<"{ \"method\": \"get\"}">>,
    Req,
    State
  }.
  
put_hello_json(Req, State) ->
  ?DEBUG("put_hello_json/2"),
  Resp = cowboy_req:set_resp_body(
    <<"{ \"method\": \"put\" }">>,
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

post_hello_json(Req, State) ->
  ?DEBUG("post_hello_json/2"),
  Resp = cowboy_req:set_resp_body(
    <<"{ \"method\": \"post\" }">>,
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

delete_hello_json(Req, State) ->
  ?DEBUG("post_hello_json/2"),
  Resp = cowboy_req:set_resp_body(
    <<"{ \"method\": \"delete\" }">>,
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

get_hello_html(Req, State) ->
  ?DEBUG("get_hello_html/1"),
  {
    hello_html_body(<<"GET">>),
    Req,
    State
  }.

put_hello_html(Req, State) ->
  ?DEBUG("put_hello_html/1"),
  Resp = cowboy_req:set_resp_body(
    hello_html_body(<<"PUT">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

post_hello_html(Req, State) ->
  ?DEBUG("post_hello_html/1"),
  Resp = cowboy_req:set_resp_body(
    hello_html_body(<<"POST">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

delete_hello_html(Req, State) ->
  ?DEBUG("delete_hello_html/1"),
  Resp = cowboy_req:set_resp_body(
    hello_html_body(<<"DELETE">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

hello_html_body(Method) ->
  ?DEBUG("hello_html_body/1"),
  Body = <<"<html><head></head><body><h1>", Method/binary, "</h1></body></html>">>,
  Body.


get_hello_text(Req, State) ->
  ?DEBUG("get_hello_text/1"),
  {
    hello_text_body(<<"GET">>),
    Req,
    State
  }.

put_hello_text(Req, State) ->
  ?DEBUG("put_hello_text/1"),
  Resp = cowboy_req:set_resp_body(
    hello_text_body(<<"PUT">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

post_hello_text(Req, State) ->
  ?DEBUG("post_hello_text/1"),
  Resp = cowboy_req:set_resp_body(
    hello_text_body(<<"POST">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

delete_hello_text(Req, State) ->
  ?DEBUG("delete_hello_text/1"),
  Resp = cowboy_req:set_resp_body(
    hello_text_body(<<"DELETE">>),
    Req
  ),
  {ok, Resp2} = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.

hello_text_body(Method) ->
  ?DEBUG("hello_text_body/1"),
  <<"this is ",Method/binary," method">>.

