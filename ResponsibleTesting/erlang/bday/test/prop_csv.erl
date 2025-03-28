-module(prop_csv).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([csv_source/0, field/0]).


%%%%%%%%%%%%%%%%%%
%%%    EUnit   %%%
%%%%%%%%%%%%%%%%%%
%% @doc 1 列の CSV ファイルは、RFC 4180 の末尾の CRLF のために本質的に曖昧であり、
%% このバグは予想されるものである
one_column_bug_test() ->
    ?assertEqual("\r\n\r\n", bday_csv:encode([#{"" => ""}, #{"" => ""}])),
    ?assertEqual([#{"" => ""}], bday_csv:decode("\r\n\r\n")).


rfc_record_per_line_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n")).


rfc_record_trailing_crlf_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx")).


rfc_double_quote_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"bbb\",\"ccc\"\r\n\zzz,yyy,xxx")).


rfc_crlf_escape_test() ->
    ?assertEqual([#{"aaa" => "zzz", "b\r\nbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx")).


rfc_double_quote_escape_test() ->
    ?assertEqual([#{"aaa" => "zzz", "b\"bb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"b\"\"bb\",\"ccc\"\r\nzzz,yyy,xxx")).


%% @doc RFC から引用した反例
%% マップには重複キーが存在しないので、現在の実装では動作しない
rfc_keys_unsupported_test() ->
    CSV = "field_name,field_name,field_name\r\n"
          "aaa,bbb,ccc\r\n"
          "zzz,yyy,xxx\r\n",
    [Map1, Map2] = bday_csv:decode(CSV),
    ?assertEqual(1, length(maps:keys(Map1))),
    ?assertEqual(1, length(maps:keys(Map2))),
    ?assertMatch(#{"field_name" := _}, Map1),
    ?assertMatch(#{"field_name" := _}, Map2).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_roundtrip() ->
    ?FORALL(Maps, csv_source(), Maps =:= bday_csv:decode(bday_csv:encode(Maps))).


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
field() -> oneof([unquoted_text(), quotable_text()]).


unquoted_text() ->
    list(elements(textdata())).


quotable_text() ->
    list(elements([$\r, $\n, $", $,] ++ textdata())).


textdata() ->
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ++
    ":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~".


header(Size) ->
    vector(Size, name()).


record(Size) ->
    vector(Size, field()).


name() -> field().


csv_source() ->
    ?LET(Size,
         pos_integer(),
         ?LET(Keys,
              header(Size + 1),
              list(entry(Size + 1, Keys)))).


entry(Size, Keys) ->
    ?LET(Vals, record(Size), maps:from_list(lists:zip(Keys, Vals))).
