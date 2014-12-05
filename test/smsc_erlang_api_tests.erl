%% -*- encoding: utf-8 -*-
%%=============================================================================
%% @doc SMSC administration tests. In scope of issue smsmarket-242
%%
%% @copyright 2014
%%
%%=============================================================================
-module(smsc_erlang_api_tests).
-author("aivanov").

-define(application, transport_data).
-define(external_node, transport_data).

%%=============================================================================
%% Includes
%%=============================================================================
-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

%%=============================================================================
%% Test generators
%%=============================================================================
internal_api_test_() ->
    { node
    , 'test@Ivanov'
    % , fun setup_internal/0
    % , fun cleanup_internal/1
    , [ fun internal_api_get_empty_db/0
      % , fun internal_api_subscribe/0
      ]
    }.

%%=============================================================================
%% Fixtures
%%=============================================================================
% setup_internal() ->
%     % application:ensure_all_started(?application).

% cleanup_internal(_) ->
%     ok = application:stop(ranch),
%     ok = application:stop(cowboy),
%     ok = application:stop(mnesia),
%     ok = application:stop(?application).

%%=============================================================================
%% Tests
%%=============================================================================
internal_api_get_empty_db() ->
    ?LOG(debug, "~p", [node()]).

%%=============================================================================
%% Internal helpers
%%=============================================================================
% int_send_request(Resource, Headers, Method) ->
%     PortString = erlang:integer_to_list(?http_port),
%     Url = lists:flatten(["http://", ?http_host, ":", PortString, Resource]),
%     ibrowse:send_req( Url
%                     , Headers
%                     , Method
%                     ).

% int_send_request(Resource, Headers, Method, Params) ->
%     PortString = erlang:integer_to_list(?http_port),
%     Url = lists:flatten(["http://", ?http_host, ":", PortString, Resource]),
%     ibrowse:send_req( Url
%                     , Headers
%                     , Method
%                     , jiffy:encode(Params)
%                     ).
