-module(prop_exercises).
-include_lib("proper/include/proper.hrl").
-export([tree/0, limit_tree/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
-type tree() :: tree(term()).
-type tree(T) :: {node,
                  Value :: T,
                  Left :: tree(T) | undefined,
                  Right :: tree(T) | undefined}.


tree() ->
    tree(term()).


tree(Type) ->
    frequency([{5, {node, Type, undefined, undefined}},
               {1, {node, Type, ?LAZY(tree(Type)), undefined}},
               {1, {node, Type, undefined, ?LAZY(tree(Type))}},
               {1, {node, Type, ?LAZY(tree(Type)), ?LAZY(tree(Type))}}]).


limit_tree() ->
    ?SIZED(Size, limit_tree(Size, term())).


limit_tree(N, Type) when N =< 1 ->
    {node, Type, undefined, undefined};
limit_tree(N, Type) ->
    frequency([{1, {node, Type, ?LAZY(limit_tree(N - 1, Type)), undefined}},
               {1, {node, Type, undefined, ?LAZY(limit_tree(N - 1, Type))}},
               {5, {node, Type, ?LAZY(limit_tree(N div 2, Type)), ?LAZY(limit_tree(N div 2, Type))}}]).


stamp() -> {hour(), min(), sec()}.


hour() -> choose(0, 23).


min() -> choose(0, 59).


sec() -> choose(0, 59).

%% 午前中の時刻を返す
