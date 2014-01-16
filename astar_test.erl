-module(astar_test).

-behaviour(astar_behaviour).

-export([
    get_max_width_height/0,
    g_fun/2,
    noway_fun/1,
    test/2
]).

get_max_width_height() -> {999, 999}.

g_fun(_, _) -> 0.0.

noway_fun({1,4}) -> false;
noway_fun({2,4}) -> false;
noway_fun({3,4}) -> false;
noway_fun({4,4}) -> false;
noway_fun({5,4}) -> false;
noway_fun({6,4}) -> false;
noway_fun({7,4}) -> false;
noway_fun({_,_}) -> true.

test({X,Y}, {X1,Y1}) -> astar_behaviour:astar({X,Y}, {X1,Y1}, ?MODULE).
