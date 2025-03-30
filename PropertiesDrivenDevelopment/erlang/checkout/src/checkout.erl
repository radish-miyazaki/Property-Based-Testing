-module(checkout).

-export([total/3, valid_special_list/1, valid_price_list/1]).

-type item() :: string().
-type price() :: integer().
-type special() :: {item(), pos_integer(), price()}.


-spec valid_special_list([special()]) -> boolean().
valid_special_list(List) ->
    lists:all(fun({_, X, _}) -> X =/= 0 end, List) andalso
    length(List) =:= length(lists:ukeysort(1, List)).


-spec valid_price_list([{item(), price()}]) -> boolean().
valid_price_list(List) ->
    length(List) =:= length(lists:ukeysort(1, List)).


-spec total([item()], [{item(), price()}], [special()]) -> price().
total(ItemList, PriceList, Specials) ->
    valid_price_list(PriceList) orelse error(invalid_price_list),
    valid_special_list(Specials) orelse error(invalid_special_list),
    Counts = count_seen(ItemList),
    {CountsLeft, Prices} = apply_specials(Counts, Specials),
    Prices + apply_regular(CountsLeft, PriceList).


-spec count_seen([item()]) -> [{item(), pos_integer()}].
count_seen(ItemList) ->
    Count = fun(X) -> X + 1 end,
    maps:to_list(
      lists:foldl(
        fun(Item, M) -> maps:update_with(Item, Count, 1, M) end,
        maps:new(),
        ItemList)).


-spec apply_specials([{item(), pos_integer()}], [special()]) ->
          {[{item(), pos_integer()}], price()}.
apply_specials(Items, Specials) ->
    lists:mapfoldl(
      fun({Name, Count}, Price) ->
              case lists:keyfind(Name, 1, Specials) of
                  false ->  % 特価対象でない場合
                      {{Name, Count}, Price};
                  {_, Needed, Value} ->
                      {{Name, Count rem Needed},
                       Value * (Count div Needed) + Price}
              end
      end,
      0,
      Items).


-spec apply_regular([{item(), pos_integer()}], [{item(), price()}]) -> price().
apply_regular(Items, PriceList) ->
    lists:sum([ Count * cost_of_item(Name, PriceList)
                || {Name, Count} <- Items ]).


cost_of_item(Name, PriceList) ->
    case proplists:get_value(Name, PriceList) of
        undefined -> error({unknown_item, Name});
        Price -> Price
    end.
