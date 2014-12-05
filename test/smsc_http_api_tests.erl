%% -*- encoding: utf-8 -*-
%%=============================================================================
%% @doc SMSC administration tests. In scope of issue smsmarket-242
%%
%% @copyright 2014
%%
%%=============================================================================
-module(smsc_http_api_tests).
-author("aivanov").

-define(application, transport_data).
-define(http_port, 8088).
-define(http_host, "127.0.0.1").

%%=============================================================================
%% Includes
%%=============================================================================
-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

%%=============================================================================
%% Test generators
%%=============================================================================
http_api_test_() ->
    { setup
    , fun setup_http/0
    , fun cleanup_http/1
    , [ fun http_api_get_404/0
      , fun http_api_put_one_with_id/0
      , fun http_api_put_one/0
      , fun http_api_post_add_one_with_id/0
      , fun http_api_post_add_one/0
      , fun http_api_post_add_bulk/0
      , fun http_api_update_one/0
      , fun http_api_update_one_with_id/0
      , fun http_api_update_bulk/0
      , fun http_api_post_add_update_bulk/0
      , fun http_api_delete_one_with_id/0
      , fun http_api_delete_one/0
      , fun http_api_delete_bulk/0
      , fun http_api_delete_by_params/0
      ]
    }.

%%=============================================================================
%% Fixtures
%%=============================================================================
setup_http() ->
    application:set_env(?application, http_port, ?http_port),
    application:ensure_all_started(?application),
    ibrowse:start().

cleanup_http(_) ->
    ibrowse:stop(),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
    stopped = mnesia:stop(),
    ok = application:stop(?application).

%%=============================================================================
%% Tests
%%=============================================================================
http_api_get_404() ->
    ?assertMatch({ok, "404", _, _}, int_send_request( "/rest/v1/smsc"
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    )),
    ?assertMatch({ok, "404", _, _}, int_send_request( "/rest/v1/smsc"
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    , #{system_id => <<"some">>}
                                                    )).

http_api_put_one_with_id() ->
    SingleResource = #{ id        => 1
                      , system_id => <<"http_api_put_one_with_id">>
                      , password  => <<"http_api_put_one_with_id">>
                      },
    {ok, Code, Headers, _} = int_send_request( "/rest/v1/smsc/0"
                                             , [{"Content-Type", "application/json"}]
                                             , put
                                             , SingleResource
                                             ),

    ?assertEqual("201", Code),
    %% Has Location header.
    Location = proplists:get_value("location", Headers),

    {ok, "200", _, Body} = int_send_request( Location
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),
    {ok, "200", _, BodyM} = int_send_request( "/rest/v1/smsc"
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),
    ?assertEqual(jiffy:decode(jiffy:encode(SingleResource), [return_maps]), jiffy:decode(Body, [return_maps])),
    ?assert(lists:member(jiffy:decode(Body, [return_maps]), jiffy:decode(BodyM, [return_maps]))).

http_api_put_one() ->
    SingleResource = #{ system_id => <<"http_api_put_one">>
                      , password  => <<"http_api_put_one">>
                      },
    {ok, Code, Headers, _} = int_send_request( "/rest/v1/smsc/2"
                                             , [{"Content-Type", "application/json"}]
                                             , put
                                             , SingleResource
                                             ),

    ?assertEqual("201", Code),
    %% Has Location header.
    Location = proplists:get_value("location", Headers),

    {ok, "200", _, Body} = int_send_request( Location
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),

    ?assertEqual(jiffy:decode(jiffy:encode(maps:put(id, 2, SingleResource)), [return_maps]), jiffy:decode(Body, [return_maps])).

http_api_post_add_one_with_id() ->
    SingleResource = #{ id        => 3
                      , system_id => <<"http_api_post_one_with_id">>
                      , password  => <<"http_api_post_one_with_id">>
                      },

    {ok, Code, Headers, _} = int_send_request( "/rest/v1/smsc"
                                             , [{"Content-Type", "application/json"}]
                                             , post
                                             , SingleResource
                                             ),

    ?assertEqual("201", Code),
    %% Has Location header.
    Location = proplists:get_value("location", Headers),

    {ok, "200", _, Body} = int_send_request( Location
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),

    ?assertEqual(jiffy:decode(jiffy:encode(SingleResource), [return_maps]), jiffy:decode(Body, [return_maps])).

http_api_post_add_one() ->
    SingleResource = #{ system_id => <<"http_api_post_one">>
                      , password  => <<"http_api_post_one">>
                      },

    {ok, Code, Headers, _} = int_send_request( "/rest/v1/smsc"
                                             , [{"Content-Type", "application/json"}]
                                             , post
                                             , SingleResource
                                             ),

    ?assertEqual("201", Code),
    %% Has Location header.
    Location = proplists:get_value("location", Headers),

    {ok, "200", _, Body} = int_send_request( Location
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),

    ?assertMatch(#{<<"id">> := 4, <<"system_id">> := <<"http_api_post_one">>, <<"password">> := <<"http_api_post_one">>}, jiffy:decode(Body, [return_maps])).

http_api_post_add_bulk() ->
    MultyResources = [ #{ system_id => <<"http_api_post_bulk">>, password  => <<"http_api_post_bulk">>}
                     || _ <- lists:seq(1,50)
                     ],

    {ok, Code, _Headers, Body} = int_send_request( "/rest/v1/smsc"
                                                 , [{"Content-Type", "application/json"}]
                                                 , post
                                                 , MultyResources
                                                 ),
    ?assertEqual("200", Code),
    Json = jiffy:decode(Body, [return_maps]),
    [ begin
        #{<<"id">> := Id} = Res,
        {ok, "200", _, BodyInt} = int_send_request( "/rest/v1/smsc/" ++ integer_to_list(Id)
                                                  , [{"Accept", "application/json"}]
                                                  , get
                                                  ),
        ?assertEqual(Res, jiffy:decode(BodyInt, [return_maps]))
      end
    || Res <- Json
    ].

http_api_update_one() ->
    {ok, "200", _, BodyM} = int_send_request( "/rest/v1/smsc"
                                           , [{"Accept", "application/json"}]
                                           , get
                                           ),

    [Single | _] = jiffy:decode(BodyM, [return_maps]),

    #{<<"id">> := Id} = Single,

    %% UPDATE SINGLE
    SingleResourceUp = Single#{ system_id => <<"updated">>},
    Location = "/rest/v1/smsc/" ++ erlang:integer_to_list(Id),
    {ok, CodeUp, _, _} = int_send_request( Location
                                         , [{"Content-Type", "application/json"}]
                                         , put
                                         , SingleResourceUp
                                         ),
    ?assertEqual("200", CodeUp),
    {ok, "200", _, BodyU} = int_send_request( Location
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),
    ?assertEqual(jiffy:decode(jiffy:encode(SingleResourceUp), [return_maps]), jiffy:decode(BodyU, [return_maps])).

http_api_update_one_with_id() ->
    {ok, "200", _, BodyM} = int_send_request( "/rest/v1/smsc"
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),
    [Single | _] = jiffy:decode(BodyM, [return_maps]),

    %% UPDATE SINGLE
    #{<<"id">> := Id} = SingleResourceUp = Single#{ system_id => <<"updated">>},
    Location = "/rest/v1/smsc/" ++ erlang:integer_to_list(Id),
    {ok, CodeUp, _, _} = int_send_request( Location
                                         , [{"Content-Type", "application/json"}]
                                         , put
                                         , SingleResourceUp
                                         ),
    ?assertEqual("200", CodeUp),
    {ok, "200", _, BodyU} = int_send_request( Location
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),
    ?assertEqual(jiffy:decode(jiffy:encode(SingleResourceUp), [return_maps]), jiffy:decode(BodyU, [return_maps])).

http_api_update_bulk() ->
    {ok, "200", _, BodyM} = int_send_request( "/rest/v1/smsc"
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),
    MultyResources = lists:map( fun(Res) ->
                                    Res#{ <<"system_id">> => <<"bulk_updated">>}
                                end
                              , jiffy:decode(BodyM, [return_maps])
                              ),
    {ok, Code, _Headers, Body} = int_send_request( "/rest/v1/smsc"
                                                 , [{"Content-Type", "application/json"}]
                                                 , post
                                                 , MultyResources
                                                 ),
    ?assertEqual("200", Code),
    Json = jiffy:decode(Body, [return_maps]),


    ?assertEqual(lists:sort(MultyResources), lists:sort(Json)),

    [ begin
        #{<<"id">> := Id} = Res,
        {ok, "200", _, BodyInt} = int_send_request( "/rest/v1/smsc/" ++ integer_to_list(Id)
                                                  , [{"Accept", "application/json"}]
                                                  , get
                                                  ),
        Got = jiffy:decode(BodyInt, [return_maps]),
        ?assertEqual(Res, Got),
        ?assert(lists:member(Got, MultyResources))
      end
    || Res <- Json
    ].

http_api_post_add_update_bulk() ->
    MultyResourcesAdd = [ #{ system_id => <<"bulk_added_updated">>}
                        || _ <- lists:seq(1,10)
                        ],

    {ok, "200", _, BodyM} = int_send_request( "/rest/v1/smsc"
                                            , [{"Accept", "application/json"}]
                                            , get
                                            , #{system_id => <<"bulk_updated">>}
                                            ),
    MultyResourcesUp = lists:map( fun(Res) ->
                                    Res#{ <<"system_id">> => <<"bulk_added_updated">>}
                                  end
                                , jiffy:decode(BodyM, [return_maps])
                                ),

    AllResources = MultyResourcesAdd ++ MultyResourcesUp,


    {ok, Code, _Headers, Body} = int_send_request( "/rest/v1/smsc"
                                                 , [{"Content-Type", "application/json"}]
                                                 , post
                                                 , AllResources
                                                 ),
    ?assertEqual("200", Code),
    Json = jiffy:decode(Body, [return_maps]),

    [ begin
        #{<<"id">> := Id} = Res,
        {ok, "200", _, BodyInt} = int_send_request( "/rest/v1/smsc/" ++ integer_to_list(Id)
                                                  , [{"Accept", "application/json"}]
                                                  , get
                                                  ),
        Got = jiffy:decode(BodyInt, [return_maps]),
        ?assertEqual(Res, Got),
        case lists:member(Got, MultyResourcesUp) of
            true ->
                ok;
            false ->
                Check = #{system_id => maps:get(<<"system_id">>, Got)},
                ?assert(lists:member(Check, MultyResourcesAdd))
        end
      end
    || Res <- Json
    ].

http_api_delete_one_with_id() ->
    SingleResource = #{ id        => 1
                      , system_id => <<"http_api_put_one_with_id">>
                      , password  => <<"http_api_put_one_with_id">>
                      },
    {ok, "200", _, _Body} = int_send_request( "/rest/v1/smsc/1"
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),

    {ok, Code, _H, BodyDel} = int_send_request( "/rest/v1/smsc"
                                              , [{"Content-Type", "application/json"}]
                                              , delete
                                              , SingleResource
                                              ),
    ?assertEqual("200", Code),

    ?assertEqual(#{<<"deleted">> => 1}, jiffy:decode(BodyDel, [return_maps])),

    {ok, "404", _, _} = int_send_request( "/rest/v1/smsc/1"
                                        , [{"Accept", "application/json"}]
                                        , get
                                        ).

http_api_delete_one() ->
    SingleResourceUri = "/rest/v1/smsc/2",

    {ok, "200", _, _Body} = int_send_request( SingleResourceUri
                                            , [{"Accept", "application/json"}]
                                            , get
                                            ),

    {ok, Code, _H, BodyDel} = int_send_request( SingleResourceUri
                                              , [{"Content-Type", "application/json"}]
                                              , delete
                                              ),
    ?assertEqual("200", Code),

    ?assertEqual(#{<<"deleted">> => 1}, jiffy:decode(BodyDel, [return_maps])),

    {ok, "404", _, _} = int_send_request( SingleResourceUri
                                        , [{"Accept", "application/json"}]
                                        , get
                                        ).

http_api_delete_bulk() ->
    Ids = lists:seq(5,54),

    MultyResources = [#{id => Id, system_id => <<"http_api_post_bulk">>, password  => <<"http_api_post_bulk">>} || Id <- Ids],

    [ begin
        ?assertMatch({ok, "200", _, _}, int_send_request( "/rest/v1/smsc/" ++ integer_to_list(Id)
                                                        , [{"Accept", "application/json"}]
                                                        , get
                                                        )
                    )
      end
    || Id <- Ids
    ],

    {ok, Code, _H, _BodyDel} = int_send_request( "/rest/v1/smsc"
                                               , [{"Content-Type", "application/json"}]
                                               , delete
                                               , MultyResources
                                               ),
    ?assertEqual("200", Code),

    % ?assertEqual(#{<<"deleted">> => 1}, jiffy:decode(BodyDel, [return_maps])),

    [ begin
        ?assertMatch({ok, "404", _, _}, int_send_request( "/rest/v1/smsc/" ++ integer_to_list(Id)
                                                        , [{"Accept", "application/json"}]
                                                        , get
                                                        )
                    )
      end
    || Id <- Ids
    ].

http_api_delete_by_params() ->
    ResourceUri = "/rest/v1/smsc",

    MultyResources = [ #{system_id => SId, password  => <<"http_api_post_bulk">>}
                     || SId <- [<<"sys1">>, <<"sys2">>, <<"sys3">>]
                      , _ <- lists:seq(1,50)
                     ],

    [ begin
        ?assertMatch({ok, "404", _, _}, int_send_request( ResourceUri
                                                        , [{"Accept", "application/json"}]
                                                        , get
                                                        , #{system_id => SId}
                                                        )
                    )
      end
    || SId <- [<<"sys1">>, <<"sys2">>, <<"sys3">>]
    ],

    ?assertMatch({ok, "200", _, _}, int_send_request( ResourceUri
                                                    , [{"Content-Type", "application/json"}]
                                                    , post
                                                    , MultyResources
                                                    )
                ),

    [ begin
        ?assertMatch({ok, "200", _, _}, int_send_request( ResourceUri
                                                        , [{"Accept", "application/json"}]
                                                        , get
                                                        , #{system_id => SId}
                                                        )
                    )
      end
    || SId <- [<<"sys1">>, <<"sys2">>, <<"sys3">>]
    ],


    {ok, Code, _H, BodyDel} = int_send_request( ResourceUri
                                              , [{"Content-Type", "application/json"}]
                                              , delete
                                              , [#{system_id => <<"sys1">>}, #{system_id => <<"sys2">>}]
                                              ),

    ?assertEqual([ #{ <<"deleted">> => 50
                    , <<"resource">> => #{<<"system_id">> => <<"sys1">>}
                    }
                 , #{ <<"deleted">> => 50
                    , <<"resource">> => #{<<"system_id">> => <<"sys2">>}
                    }
                 ], jiffy:decode(BodyDel, [return_maps])),

    ?assertEqual("200", Code),

    ?assertMatch({ok, "404", _, _}, int_send_request( ResourceUri
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    , #{system_id => <<"sys1">>}
                                                    )
                ),
    ?assertMatch({ok, "404", _, _}, int_send_request( ResourceUri
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    , #{system_id => <<"sys2">>}
                                                    )
                ),
    ?assertMatch({ok, "200", _, _}, int_send_request( ResourceUri
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    , #{system_id => <<"sys3">>}
                                                    )
                ),

    ?assertMatch({ok, "200", _, _}, int_send_request( ResourceUri
                                                    , [{"Content-Type", "application/json"}]
                                                    , delete
                                                    , [#{system_id => <<"sys3">>}]
                                                    )
                ),

    ?assertMatch({ok, "404", _, _}, int_send_request( ResourceUri
                                                    , [{"Accept", "application/json"}]
                                                    , get
                                                    , #{system_id => <<"sys3">>}
                                                    )
                ).

%%=============================================================================
%% Internal helpers
%%=============================================================================
int_send_request(Resource, Headers, Method) ->
    PortString = erlang:integer_to_list(?http_port),
    Url = lists:flatten(["http://", ?http_host, ":", PortString, Resource]),
    ibrowse:send_req( Url
                    , Headers
                    , Method
                    ).

int_send_request(Resource, Headers, Method, Params) ->
    PortString = erlang:integer_to_list(?http_port),
    Url = lists:flatten(["http://", ?http_host, ":", PortString, Resource]),
    ibrowse:send_req( Url
                    , Headers
                    , Method
                    , jiffy:encode(Params)
                    ).
