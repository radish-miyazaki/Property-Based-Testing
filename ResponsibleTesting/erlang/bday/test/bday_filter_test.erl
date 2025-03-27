-module(bday_filter_test).
-include_lib("eunit/include/eunit.hrl").


bday_filter_test() ->
    Years = generate_years_data(2018, 2038),
    People = generate_people_for_year(3),
    lists:foreach(fun(YearData) ->
                          Birthdays = find_birthdays_for_year(People, YearData),

                          % アサーション
                          every_birthday_once(People, Birthdays),
                          on_right_date(People, Birthdays)
                  end,
                  Years).


generate_people_for_year(N) ->
    YearSeed = generate_years_data(2016),  % 閏年なので、全ての年がカバーされている
    lists:append([ people_for_year(YearSeed) || _ <- lists:seq(1, N) ]).


people_for_year(Year) ->
    [ person_for_date(Date) || Date <- Year ].


person_for_date({_, M, D}) ->
    % @info 閏年でない年の 2 月 29 日の誕生日の従業員データも生成されるが、
    % bday_filter:birthday では M と D のみを使用するので許容する
    #{"name" => make_ref(), "date_of_birth" => {rand:uniform(100) + 1900, M, D}}.


find_birthdays_for_year(_, []) -> [];
find_birthdays_for_year(People, [Day | Year]) ->
    Found = bday_filter:birthday(People, Day),
    [{Day, Found} | find_birthdays_for_year(People, Year)].


generate_years_data(End, End) -> [];
generate_years_data(Start, End) ->
    [generate_years_data(Start) | generate_years_data(Start + 1, End)].


generate_years_data(Year) ->
    DaysInFeb = case calendar:is_leap_year(Year) of
                    true -> 29;
                    false -> 28
                end,
    month(Year, 1, 31) ++ month(Year, 2, DaysInFeb) ++ month(Year, 3, 31) ++
    month(Year, 4, 30) ++ month(Year, 5, 31) ++ month(Year, 6, 30) ++
    month(Year, 7, 31) ++ month(Year, 8, 31) ++ month(Year, 9, 30) ++
    month(Year, 10, 31) ++ month(Year, 11, 30) ++ month(Year, 12, 31).


month(Y, M, 1) -> [{Y, M, 1}];
month(Y, M, D) -> [{Y, M, D} | month(Y, M, D - 1)].


%% @doc 検索で誕生日が見つからなかった従業員と、複数回検索された従業員がいないか確認するアサーション
every_birthday_once(People, Birthdays) ->
    Found = lists:sort(lists:append([ Found || {_, Found} <- Birthdays ])),
    NotFound = People -- Found,
    FoundManyTimes = Found -- lists:usort(Found),
    ?assertEqual([], NotFound),
    ?assertEqual([], FoundManyTimes).


%% @doc 従業員の誕生日が検索に使われた月日と一致するか確認するアサーション
on_right_date(_People, Birthdays) ->
    % 閏年でない年の 2 月 29 日の誕生日の従業員データは無視する
    [ calendar:valid_date({Y, PM, PD}) andalso ?assertEqual({M, D}, {PM, PD})
      || {{Y, M, D}, Found} <- Birthdays,
         #{"date_of_birth" := {_, PM, PD}} <- Found ].
