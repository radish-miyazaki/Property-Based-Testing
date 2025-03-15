-module(prop_exercises).
-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_a_sample() ->
    ?FORALL(
      {Start, Count},
      {integer(), non_neg_integer()},
      begin
          List = lists:seq(Start, Start + Count),
          Count + 1 =:= length(List) andalso
          increments(List)
      end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
increments([Head | Tail]) -> increments(Head, Tail).


increments(_, []) -> true;
increments(N, [Head | Tail]) when Head == N + 1 -> increments(Head, Tail);
increments(_, _) -> false.
