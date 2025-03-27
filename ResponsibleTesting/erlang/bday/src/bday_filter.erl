-module(bday_filter).
-export([birthday/2]).


birthday(People, {Year, 2, 28}) ->
    case calendar:is_leap_year(Year) of
        true -> filter_dop(People, 2, 28);
        % 閏年でない場合は、2 月 28 日と 29 日の両方をフィルタリングする
        false -> filter_dop(People, 2, 28) ++ filter_dop(People, 2, 29)
    end;

birthday(People, {_, Month, Day}) ->
    filter_dop(People, Month, Day).


filter_dop(People, Month, Day) ->
    lists:filter(
      fun(#{"date_of_birth" := {_, M, D}}) -> {Month, Day} == {M, D} end,
      People).
