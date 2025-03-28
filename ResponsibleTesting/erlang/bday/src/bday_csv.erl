-module(bday_csv).

-export([encode/1, decode/1]).


%% @doc 同じキーを持つマップのリストを取り、ヘッダ付きで有効な CSV である文字列に変換する
-spec encode([map()]) -> string().
encode([]) -> "";
encode(Maps) ->
    Keys = lists:join(",", [ escape(Name) || Name <- maps:keys(hd(Maps)) ]),
    Vals = [ lists:join(",", [ escape(Field) || Field <- maps:values(Map) ]) || Map <- Maps ],
    lists:flatten([Keys, "\r\n", lists:join("\r\n", Vals)]).


%% @doc 有効な CSV データダンプを表す文字列を受け取り、ヘッダーエントリをキーとするマップのリストに変換する
-spec decode(string()) -> list(map()).
decode("") -> [];
decode(CSV) ->
    {Headers, Rest} = decode_header(CSV, []),
    Rows = decode_rows(Rest),
    [ maps:from_list(lists:zip(Headers, Row)) || Row <- Rows ].


%%%%%%%%%%%%%%%
%%% private %%%
%%%%%%%%%%%%%%%


%% @private 必要であれば、エスケープされたフィールドまたは名前を返す
-spec escape(string()) -> string().
escape(Field) ->
    case escabable(Field) of
        true -> "\"" ++ do_escape(Field) ++ "\"";
        false -> Field
    end.


%% @private フィールドや名前の文字列をエスケープする必要があるかどうかを返す
-spec escabable(string()) -> boolean().
escabable(String) ->
    lists:any(fun(Char) -> lists:member(Char, [$", $,, $\r, $\n]) end, String).


%% @private CSV 中のエスケープ可能な文字（'"' のみ）を置換する
-spec do_escape(string()) -> string().
do_escape([]) -> [];
do_escape([$" | Str]) -> [$", $" | do_escape(Str)];
do_escape([Char | Rest]) -> [Char | do_escape(Rest)].


%% @private ヘッダ全体をデコードし、すべての名前を順番に返す
-spec decode_header(string(), [string()]) -> {[string()], string()}.
decode_header(String, Acc) ->
    case decode_name(String) of
        {ok, Name, Rest} -> decode_header(Rest, [Name | Acc]);
        {done, Name, Rest} -> {[Name | Acc], Rest}
    end.


%% @private 全ての行をリストにデコードする
-spec decode_rows(string()) -> [[string()]].
decode_rows(String) ->
    case decode_row(String, []) of
        {Row, ""} -> [Row];
        {Row, Rest} -> [Row | decode_rows(Rest)]
    end.


%% @private 行全体をデコードし、すべての値を順番に表示する
-spec decode_row(string(), [string()]) -> {[string()], string()}.
decode_row(String, Acc) ->
    case decode_field(String) of
        {ok, Field, Rest} -> decode_row(Rest, [Field | Acc]);
        {done, Field, Rest} -> {[Field | Acc], Rest}
    end.


%% @private 名前をデコードする
-spec decode_name(string()) -> {ok | done, string(), string()}.
decode_name([$" | Rest]) -> decode_quoted(Rest);
decode_name(String) -> decode_unquoted(String).


%% @private フィールドをデコードする
-spec decode_field(string()) -> {ok | done, string(), string()}.
decode_field([$" | Rest]) -> decode_quoted(Rest);
decode_field(String) -> decode_unquoted(String).


%% @private 引用符に囲まれた文字列をデコードする
-spec decode_quoted(string()) -> {ok | done, string(), string()}.
decode_quoted(String) -> decode_quoted(String, []).


%% @private 引用符に囲まれた文字列をデコードする
-spec decode_quoted(string(), [char()]) -> {ok | done, string(), string()}.
decode_quoted([$"], Acc) -> {done, lists:reverse(Acc), ""};
% CSV における改行は RFC4180 で CRLF として定義されている
decode_quoted([$", $\r, $\n | Rest], Acc) -> {done, lists:reverse(Acc), Rest};
decode_quoted([$", $, | Rest], Acc) -> {ok, lists:reverse(Acc), Rest};
decode_quoted([$", $" | Rest], Acc) -> decode_quoted(Rest, [$" | Acc]);
decode_quoted([Char | Rest], Acc) -> decode_quoted(Rest, [Char | Acc]).


%% @private 引用符に囲まれていない文字列をデコードする
-spec decode_unquoted(string()) -> {ok | done, string(), string()}.
decode_unquoted(String) -> decode_unquoted(String, []).


%% @private 引用符に囲まれていない文字列をデコードする
-spec decode_unquoted(string(), [char()]) -> {ok | done, string(), string()}.
decode_unquoted([], Acc) -> {done, lists:reverse(Acc), ""};
decode_unquoted([$\r, $\n | Rest], Acc) -> {done, lists:reverse(Acc), Rest};
decode_unquoted([$, | Rest], Acc) -> {ok, lists:reverse(Acc), Rest};
decode_unquoted([Char | Rest], Acc) -> decode_unquoted(Rest, [Char | Acc]).
