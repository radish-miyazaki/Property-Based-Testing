-module(exercises).
-export([word_count/1]).


word_count(String) ->
    Trimmed = string:trim(String),
    length(string:split(Trimmed, " ")).
