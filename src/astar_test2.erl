-module(astar_test2).

-behaviour(astar_behaviour_2).

-export([
    get_max_width_height/0,
    g_fun/2,
    noway_fun/1,
    test/2,
    testp/2,
    testt/0,
    testtc/0
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

test({X,Y}, {X1,Y1}) ->
    LocPid = self(),
    spawn(fun()-> 
        Ret = astar_behaviour_2:astar({X,Y}, {X1,Y1}, ?MODULE),
        LocPid ! {ret, Ret}
    end),
    receive
        {ret, Ret} -> Ret
    after 30000    -> timeout 
    end.

testp({X,Y}, {X1,Y1}) ->
    LocPid = self(),
    eprof:start(),
    Pid = spawn(fun()->
        timer:sleep(500),
        Ret = astar_behaviour_2:astar({X,Y}, {X1,Y1}, ?MODULE),
        LocPid ! {ret, Ret}
    end),
    eprof:start_profiling([Pid]),
    MR = receive
        {ret, Ret} -> Ret
    after 30000    -> timeout 
    end,
    eprof:stop_profiling(),
    %eprof:analyze(total),
    MR.

testt() -> timer:tc(astar_test2, test, [{1,1},{990,990}]).


testtc() -> 
    Sum = lists:foldl(fun(_, A)->
         {T, _} = timer:tc(astar_test2, test, [{1,1},{990,990}]),
         A + T
    end, 0, lists:seq(1, 200)),
    Sum / 200.
