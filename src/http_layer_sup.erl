%% -*- encoding: utf-8 -*-
-module(http_layer_sup).
-behaviour(supervisor).

%%===================================================================
%% API functions
%%===================================================================
-export([start_link/0]).

%%===================================================================
%% Supervisor callbacks
%%===================================================================
-export([init/1]).

%%===================================================================
%% API functions
%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================
init([]) ->
    { ok
    , { {one_for_one, 5, 10}
      , [ { config_server
          , {config_server, start_link, []}
          , permanent, 5000, worker, [config_server]}
        , { dw_server_http
          , {dw_server_http, start_link, []}
          , permanent, 5000, worker, [dw_server_http]}
        ]
      }
    }.

%%===================================================================
%% Internal functions
%%===================================================================


% chat_room(List,Counter) ->
%     receive
%         {counter,Caller} -> Caller ! {counter,Counter}, chat_room(List,Counter);
%         {inc} -> chat_room(List,Counter+1);
%         {dec} -> chat_room(List,Counter-1);
%         {add, Message} -> chat_room([Message|List],Counter);
%         print -> io:format("~p",[List]), chat_room(List,Counter);
%         {top, Number, Caller} -> Caller ! lists:sublist(List,Number), chat_room(List,Counter);
%         {win, Page, Caller} -> Caller ! lists:sublist(List,Page*10,10), chat_room(List,Counter);
%         _ -> chat_room(List,Counter)
%     end.
