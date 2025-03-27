-module(prop_generators).
-include_lib("proper/include/proper.hrl").
-export([text_like/0, mostly_sorted/0, path/0]).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dupes() ->
    ?FORALL(KV,
            list({key(), val()}),
            begin
                M = maps:from_list(KV),
                _ = [ maps:get(K, M) || {K, _V} <- KV ],  % K がマップにない場合はクラッシュする

                % 重複している Key の数ごとに集計する
                collect(
                  {dupes, to_range(5, length(KV) - length(lists:ukeysort(1, KV)))},
                  true)
            end).


prop_collect1() ->
    ?FORALL(Bin, binary(), collect(byte_size(Bin), is_binary(Bin))).


prop_collect2() ->
    ?FORALL(Bin, binary(), collect(to_range(10, byte_size(Bin)), is_binary(Bin))).


prop_aggregate() ->
    Suits = [club, diamond, heart, spade],
    ?FORALL(Hand, vector(5, {oneof(Suits), choose(1, 13)}), aggregate(Hand, true)).


prop_escape() ->
    ?FORALL(Str, string(), aggregate(classes(Str), escape(Str))).


prop_resize() ->
    ?FORALL(Bin, resize(150, binary()), collect(to_range(10, byte_size(Bin)), is_binary(Bin))).


prop_profile1() ->
    ?FORALL(Profile,
            [{name, resize(10, string())}, {age, pos_integer()}, {bio, resize(350, string())}],
            begin
                NameLen = to_range(10, length(proplists:get_value(name, Profile))),
                BioLen = to_range(300, length(proplists:get_value(bio, Profile))),
                aggregate([{name, NameLen}, {bio, BioLen}], true)
            end).


prop_profile2() ->
    ?FORALL(Profile,
            [{name, string()},
             {age, pos_integer()},
             {bio, ?SIZED(Size, resize(Size * 35, string()))}],
            begin
                NameLen = to_range(10, length(proplists:get_value(name, Profile))),
                BioLen = to_range(300, length(proplists:get_value(bio, Profile))),
                aggregate([{name, NameLen}, {bio, BioLen}], true)
            end).


prop_queue_naive() ->
    ?FORALL(List,
            list({term(), term()}),
            begin
                Queue = queue:from_list(List),
                queue:is_queue(Queue)
            end).


prop_queue_nicer() ->
    ?FORALL(Q, queue(), queue:is_queue(Q)).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
to_range(M, N) ->
    Base = N div M,
    {Base * M, (Base + 1) * M}.


escape(_) -> true.


classes(Str) ->
    L = letters(Str),
    N = numbers(Str),
    P = punctuation(Str),
    O = length(Str) - (L + N + P),
    [{letters, to_range(5, L)},
     {numbers, to_range(5, N)},
     {punctuation, to_range(5, P)},
     {other, to_range(5, O)}].


letters(Str) ->
    length([ 1 || Char <- Str,
                  (Char >= $A andalso Char =< $Z) orelse
                  (Char >= $a andalso Char =< $z) ]).


numbers(Str) ->
    length([ 1 || Char <- Str,
                  Char >= $0 andalso Char =< $9 ]).


punctuation(Str) ->
    length([ 1 || Char <- Str,
                  lists:member(Char, ",.;:'\"-") ]).


prop_dict_gen() ->
    ?FORALL(D, dict_gen(), dict:size(D) < 5).


prop_dict_symb() ->
    ?FORALL(DSymb, dict_symb(), dict:size(eval(DSymb)) < 5).


prop_dict_autosymb() ->
    ?FORALL(D, dict_autosymb(), dict:size(D) < 5).


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
key() -> oneof([range(1, 10), integer()]).


val() -> term().


queue() -> ?LET(List, list({term(), term()}), queue:from_list(List)).


% non_empty_map(Gen) ->
%     ?SUCHTHAT(G, Gen, G =/= #{}).

% even() ->
%     ?SUCHTHAT(N, integer(), N rem 2 =:= 0).
% odd() ->
%     ?SUCHTHAT(N, integer(), N rem 2 =/= 0).

% even() -> ?LET(N, integer(), N * 2).
% odd() -> ?LET(N, integer(), N * 2 + 1).

% latin1_string() ->
%     ?SUCHTHAT(S, string(), io_lib:printable_latin1_list(S)).
% unicode_string() ->
%     ?SUCHTHAT(S, string(), io_lib:printable_unicode_list(S)).


text_like() ->
    list(frequency([{80, range($a, $z)},
                    {10, $\s},
                    {1, $\n},
                    {1, oneof([$., $-, $!, $?, $,])},
                    {1, range($0, $9)}])).


mostly_sorted() ->
    ?LET(Lists, list(frequency([{5, sorted_list()}, {1, list()}])), lists:append(Lists)).


sorted_list() ->
    ?LET(L, list(), lists:sort(L)).


path() ->
    % 最大値、現在地、accumulator（これまでの経路）、訪問済みノード、無視する方向
    % 無視する要素: 直近で試行した方向と、衝突を起こした方向
    ?SIZED(Size, path(Size, {0, 0}, [], #{{0, 0} => seen}, [])).


path(0, _Current, Acc, _Seen, _Ignore) -> Acc;  % 最大値に達した
path(_Max, _Current, Acc, _Seen, [_, _, _, _]) -> Acc;  % 4方向に試行済み
path(Max, Current, Acc, Seen, Ignore) ->
    increment_path(Max, Current, Acc, Seen, Ignore).


increment_path(Max, Current, Acc, Seen, Ignore) ->
    DirectionGen = oneof([left, right, up, down] -- Ignore),
    ?LET(Direction,
         DirectionGen,
         begin
             NewPos = move(Direction, Current),
             case Seen of
                 #{NewPos := _} ->  % 存在する
                     path(Max, Current, Acc, Seen, [Direction | Ignore]);  % 再試行
                 _ ->
                     % MEMO: Seen#{NewPos => seen} は、マップの更新を行う
                     path(Max - 1, NewPos, [Direction | Acc], Seen#{NewPos => seen}, [])
             end
         end).


move(left, {X, Y}) -> {X - 1, Y};
move(right, {X, Y}) -> {X + 1, Y};
move(up, {X, Y}) -> {X, Y + 1};
move(down, {X, Y}) -> {X, Y - 1}.


%% 通常の関数呼び出し（dict:from_list/1）
dict_gen() ->
    ?LET(X, list({integer(), integer()}), dict:from_list(X)).


%% シンボリックコール
dict_symb() ->
    ?SIZED(Size, dict_symb(Size, {call, dict, new, []})).


dict_symb(0, Dict) ->
    Dict;
dict_symb(N, Dict) ->
    dict_symb(N - 1, {call, dict, store, [integer(), integer(), Dict]}).


%% 自動シンボリックコール
dict_autosymb() ->
    ?SIZED(Size, dict_autosymb(Size, {'$call', dict, new, []})).


dict_autosymb(0, Dict) -> Dict;
dict_autosymb(N, Dict) ->
    dict_autosymb(N - 1, {'$call', dict, store, [integer(), integer(), Dict]}).
