-module(dw_sockjs_multiplex).

-behaviour(sockjs_service).

-export([init_state/1]).
-export([sockjs_init/2, sockjs_handle/3, sockjs_terminate/2]).

-record(service, {callback, state, vconn}).

%% --------------------------------------------------------------------------

init_state(Services) ->
    L = [{Topic, #service{callback = Callback, state = State}} ||
            {Topic, Callback, State} <- Services],
    {orddict:from_list(L), orddict:new()}.



sockjs_init(_Conn, {_Services, _Channels} = S) ->
    {ok, S}.

sockjs_handle(Conn, Data, {Services, Channels}) ->
    [TypeBin, TopicBin, PayloadBin] = binary_split(Data, <<$,>>, 3),

    Type    = binary:bin_to_list(TypeBin),
    Topic   = binary:bin_to_list(TopicBin),

    case orddict:find(Topic, Services) of
        {ok, Service} ->
            Channels1 = action(Conn, {Type, Topic, PayloadBin}, Service, Channels),
            {ok, {Services, Channels1}};
        _Else ->
            {ok, {Services, Channels}}
    end.

sockjs_terminate(_Conn, {Services, Channels}) ->
    _ = [ {emit(closed, Channel)} ||
            {_Topic, Channel} <- orddict:to_list(Channels) ],
    {ok, {Services, orddict:new()}}.


action(Conn, {Type, Topic, Payload}, Service, Channels) ->
    case {Type, orddict:is_key(Topic, Channels)} of
        {"sub", false} ->
            Channel = Service#service{
                         vconn = sockjs_multiplex_channel:new(
                                   Conn, Topic)
                        },
            orddict:store(Topic, emit(init, Channel), Channels);
        {"uns", true} ->
            Channel = orddict:fetch(Topic, Channels),
            emit(closed, Channel),
            orddict:erase(Topic, Channels);
        {"msg", true} ->
            Channel = orddict:fetch(Topic, Channels),
            orddict:store(Topic, emit({recv, Payload}, Channel), Channels);
        _Else ->
            %% Ignore
            Channels
    end.


emit(What, Channel = #service{callback = Callback,
                              state    = State,
                              vconn    = VConn}) ->
    case Callback(VConn, What, State) of
        {ok, State1} -> Channel#service{state = State1};
        ok           -> Channel
    end.


%% --------------------------------------------------------------------------

% -spec binary_join([binary()], binary()) -> binary().
% binary_join([], _Sep) ->
%   <<>>;
% binary_join([Part], _Sep) ->
%   Part;
% binary_join(List, Sep) ->
%   lists:foldr(fun (A, B) ->
%     if
%       bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
%       true -> A
%     end
%   end, <<>>, List).

-spec binary_split(Subject :: binary(), Splitter :: binary(), Count :: pos_integer()) -> [binary()].
binary_split(Subject, Splitter, Count) ->
    binary_split(Subject, Splitter, Count-1, <<>>, []).

binary_split(Rest, _Splitter, 0, <<>>, Result) ->
    lists:reverse([Rest | Result]);

binary_split(<<>>, Splitter, Count, Buffer, Result) ->
    binary_split(<<>>, Splitter, Count-1, <<>>, [Buffer | Result]);

binary_split(<<H,T/binary>>, Splitter, Count, Buffer, Result) when <<H>> == Splitter ->
    binary_split(T, Splitter, Count-1, <<>>, [Buffer | Result]);

binary_split(<<H,T/binary>>, Splitter, Count, Buffer, Result) ->
    binary_split(T, Splitter, Count, <<Buffer/binary,H>>, Result).
