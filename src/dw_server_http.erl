-module(dw_server_http).
-behaviour(gen_server).
-include("common.hrl").

%%===================================================================
%% API functions
%%===================================================================
-export([start_link/0]).

%%===================================================================
%% GenServer callbacks
%%===================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%===================================================================
%% Macro
%%===================================================================
-define(server, ?MODULE).

%%===================================================================
%% Records
%%===================================================================
-record(state, {}).

%%===================================================================
%% API functions
%%===================================================================
start_link() ->
  gen_server:start_link({local, ?server}, ?MODULE, [], []).

%%===================================================================
%% GenServer callbacks
%%===================================================================
init([]) ->
    {ok, WebPort} = config_server:get_component_config([web, http_port]),
    {ok, WsPort} = config_server:get_component_config([ws, http_port]),
    cowboy:start_http( {http, ws}
                     , 100
                     , [{port, WsPort}]
                     , [{env, [{dispatch, ws_dispatch_rules()}]}]
                     ),
    cowboy:start_http( {http, web}
                     , 100
                     , [{port, WebPort}]
                     , [ {env, [{dispatch, web_dispatch_rules()}]}
                       ]
                     ),
    % _Pid = spawn(fun () -> wf:reg(lobby), chat_room([],0) end),
    % wf:cache(mode,wf:config(n2o,mode,"dev")),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================
ws_dispatch_rules() ->
    MultiplexState = dw_sockjs_multiplex:init_state([ {"stream", fun dw_vs_multiplex_handler:service_stream/3, []}
                                                    ]),
    SockjsState = sockjs_handler:init_state( <<"/ws/multiplex">>
                                           , dw_sockjs_multiplex
                                           , MultiplexState
                                           , [{sockjs_url, "http://cdn.sockjs.org/sockjs-0.3.4.min.js"}]
                                           ),

    cowboy_router:compile(
        [ %% Internal administrative site for admins and moderators
          % { "admin.workofdream.:fldomain"
          % , [ { "/static/[...]"
          %     , cowboy_static
          %     , { priv_dir
          %       , Application
          %       , "static"
          %       , [ {mimetypes, cow_mimetypes, web} ]
          %       }
          %     }
          %   , {"/rest/:resource", rest_cowboy, []}
          %   , {"/rest/:resource/:id", rest_cowboy, []}
          %   , {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]}
          %   , {'_', n2o_cowboy, []}
          %   ]
          % } ,
        { %% External site for employees and HRs
            "[m].workofdream.:fldomain"
          , [ {"/ws/multiplex/[...]", sockjs_cowboy_handler, SockjsState}
            % , {"/ws/[...]", sockjs_cowboy_handler, [sockjs_handler:init_state(<<"/ws/echo">>, fun service_echo/3, state, [])]}
            ]
          }
        ]
    ).

web_dispatch_rules() ->
    {ok, Application} = application:get_application(),

    cowboy_router:compile(
        [ { %% External site for employees and HRs
            "[m].workofdream.:fldomain"
          , [ { "/static/[...]"
              , cowboy_static
              , { priv_dir
                , Application
                , "static"
                , [ {mimetypes, cow_mimetypes, web} ]
                }
              }
            , { "/widget/:widget"
              , dw_http_widget_handler
              , []
              }
            , { "/rest/[:version]/user/[:params]"
              , dw_rest_user
              , []
              }
            , {'_', dw_http_route_handler, []}
            ]
          }
        ]
    ).
