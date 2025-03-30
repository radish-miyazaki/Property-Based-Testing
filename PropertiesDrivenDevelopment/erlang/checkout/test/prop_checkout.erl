-module(prop_checkout).
-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_no_special() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList},
            item_price_list(),
            begin
                ExpectedPrice =:= checkout:total(ItemList, PriceList, [])
            end).


prop_no_special2() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList},
            item_price_list(),
            collect(
              bucket(length(ItemList), 10),
              ExpectedPrice =:= checkout:total(ItemList, PriceList, []))).


prop_special() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList, SpecialList},
            item_price_special(),
            ExpectedPrice =:= checkout:total(ItemList, PriceList, SpecialList)).


prop_expected_result() ->
    ?FORALL({ItemList, PriceList, SpecialList},
            lax_lists(),
            collect(
              item_list_type(ItemList, PriceList),
              try checkout:total(ItemList, PriceList, SpecialList) of
                  N when is_integer(N) -> true
              catch
                  error:{unknown_item, _} -> true;
                  error:invalid_special_list -> true;
                  error:invalid_price_list -> true;
                  _:_ -> false
              end)).


prop_dupe_list_invalid() ->
    ?FORALL(
      PriceList,
      dupe_list(),
      false =:= checkout:valid_price_list(PriceList)).


prop_dupe_specials_invalid() ->
    ?FORALL(
      SpecialList,
      dupe_special_list(),
      false =:= checkout:valid_special_list(SpecialList)).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
bucket(N, Unit) ->
    (N div Unit) * Unit.


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
item_price_list() ->
    ?LET(PriceList,
         price_list(),
         ?LET({ItemList, ExpectedPrice},
              item_list(PriceList),
              {ItemList, ExpectedPrice, PriceList})).


price_list() ->
    ?LET(
      PriceList,
      non_empty(list({non_empty(string()), integer()})),
      lists:ukeysort(1, PriceList)).


item_list(PriceList) ->
    ?SIZED(Size, item_list(Size, PriceList, {[], 0})).


item_list(0, _, Acc) -> Acc;
item_list(N, PriceList, {ItemAcc, PriceAcc}) ->
    ?LET({Item, Price},
         elements(PriceList),
         item_list(N - 1, PriceList, {[Item | ItemAcc], Price + PriceAcc})).


item_price_special() ->
    ?LET(PriceList,
         price_list(),
         ?LET(SpecialList,
              special_list(PriceList),
              ?LET({{RegularItems, RegularExpected},
                    {SpecialItems, SpecialExpected}},
                   {regular_gen(PriceList, SpecialList),
                    special_gen(PriceList, SpecialList)},
                   {shuffle(RegularItems ++ SpecialItems),
                    RegularExpected + SpecialExpected,
                    PriceList,
                    SpecialList}))).


shuffle(L) ->
    Shuffled = lists:sort([ {rand:uniform(), X} || X <- L ]),
    [ X || {_, X} <- Shuffled ].


%% [{Name, Count, SpecialPrice}] の形のリストで特価を生成
%% ⚠️ 通常の価格より特価の方が高くなるケースがある
special_list(PriceList) ->
    Items = [ Name || {Name, _} <- PriceList ],
    ?LET(Specials,
         list({elements(Items), choose(3, 5), integer()}),
         lists:ukeysort(1, Specials)).  % 重複なし


regular_gen(PriceList, SpecialList) ->
    regular_gen(PriceList, SpecialList, [], 0).


regular_gen([], _, Items, Price) ->
    {Items, Price};
regular_gen([{Item, Cost} | PriceList], SpecialList, Items, Price) ->
    CountGen = case lists:keyfind(Item, 1, SpecialList) of
                   {_, Limit, _} -> choose(0, Limit - 1);  % 特価あり: 最大数を設定
                   false -> non_neg_integer()  % 特価なし: ランダムに生成
               end,
    ?LET(Count,
         CountGen,
         regular_gen(PriceList,
                     SpecialList,
                     ?LET(V, vector(Count, Item), V ++ Items),
                     Cost * Count + Price)).


special_gen(_, SpecialList) ->
    %% 商品リストは不要
    special_gen(SpecialList, [], 0).


special_gen([], Items, Price) ->
    {Items, Price};
special_gen([{Item, Count, Cost} | SpecialList], Items, Price) ->
    %% 特価対象商品の個数は、特価となる個数の倍数となるようにする
    %% Example: 特価となるのが 3 個の場合、3, 6, 9, ... 個のいずれか
    ?LET(Multiplier,
         non_neg_integer(),
         special_gen(SpecialList,
                     ?LET(V, vector(Count * Multiplier, Item), V ++ Items),
                     Cost * Multiplier + Price)).


lax_lists() ->
    KnownItems = ["A", "B", "C"],
    MaybeKnownItemGen = elements(KnownItems ++ [string()]),
    {list(MaybeKnownItemGen),
     list({MaybeKnownItemGen, integer()}),
     list({MaybeKnownItemGen, integer(), integer()})}.


item_list_type(Items, Prices) ->
    case lists:all(fun(X) -> has_price(X, Prices) end, Items) of
        true -> valid;
        false -> prices_missing
    end.


has_price(Item, ItemList) ->
    proplists:get_value(Item, ItemList) =/= undefined.


dupe_list() ->
    ?LET(Items,
         non_empty(list(string())),
         vector(length(Items) + 1, {elements(Items), integer()})).


dupe_special_list() ->
    ?LET(Items,
         non_empty(list(string())),
         vector(length(Items) + 1, {elements(Items), integer(), integer()})).
