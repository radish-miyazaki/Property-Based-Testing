-module(prop_foundations).
-include_lib("proper/include/proper.hrl").

boolean(_) -> true.

prop_test() ->
    ?FORALL(
        Type,
        term(),
        begin
            boolean(Type)
        end
    ).
