-module(prop_exercises).
-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_keysort() ->
    ?FORALL(List,
            list({term(), term()}),
            begin
                is_key_ordered(lists:keysort(1, List))
            end).


%% 公式の解答を引用
% prop_keysort_n() ->
%     ?FORALL(List,
%             non_empty(list(non_empty(list()))),
%             begin
%                 TupleList = [ list_to_tuple(InnerList) || InnerList <- List ],

%                 % 最小の要素数を指定
%                 Pos = lists:min([ tuple_size(Tuple) || Tuple <- TupleList ]),

%                 Sorted = lists:keysort(Pos, TupleList),
%                 Keys = extract_keys(Pos, Sorted),
%                 Keys == lists:sort(Keys)
%             end).

% extract_keys(Pos, List) -> [ element(Pos, Tuple) || Tuple <- List ].


prop_set_union() ->
    ?FORALL({ListA, ListB},
            {list(number()), list(number())},
            begin
                SetA = sets:from_list(ListA),
                SetB = sets:from_list(ListB),
                ModelUnion = lists:usort(ListA ++ ListB),
                lists:sort(sets:to_list(sets:union(SetA, SetB))) =:= ModelUnion
            end).


prop_dict_merge() ->
    ?FORALL({ListA, ListB},
            {list({term(), term()}), list({term(), term()})},
            begin
                Merged = dict:merge(fun(_Key, V1, _V2) -> V1 end,
                                    dict:from_list(ListA),
                                    dict:from_list(ListB)),
                extract_keys(lists:sort(dict:to_list(Merged))) ==
                lists:usort(extract_keys(ListA ++ ListB))
            end).


prop_word_count() ->
    ?FORALL(String,
            non_empty(string()),
            begin
                exercises:word_count(String) =:= alt_word_count(String)
            end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
is_key_ordered([{A, _}, {B, _} = BTuple | T]) ->
    A =< B andalso is_key_ordered([BTuple | T]);
is_key_ordered(_) ->
    true.


extract_keys(List) -> [ K || {K, _} <- List ].


alt_word_count(String) ->
    space(String).


space([]) -> 0;
space([$\s | Str]) -> space(Str);
space(Str) -> word(Str).


word([]) -> 1;
word([$\s | Str]) -> 1 + space(Str);
word([_ | Str]) -> word(Str).
