-module(thinking).
-export([biggest/1]).


-spec biggest([number()]) -> number().
biggest([Head | Tail]) ->
    biggest(Tail, Head).


-spec biggest([number()], number()) -> number().
biggest([], Biggest) ->
    Biggest;
biggest([Head | Tail], Biggest) when Head >= Biggest ->
    biggest(Tail, Head);
biggest([Head | Tail], Biggest) when Head < Biggest ->
    biggest(Tail, Biggest).
