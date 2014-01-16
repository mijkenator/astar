-module(astar).

-export([
    test/0,
    test/2,
    noway/1,
    get_neighbors/4,
    get_peak_withmin_f/1
]).

-record(peak, {x, y, g, h, f, step_num=0, cf=[]}).
-define(W, 10).
-define(H, 10).

test() -> astar(#peak{x=0,y=0}, #peak{x=6,y=6}).

test({X,Y}, {X1,Y1}) -> astar(#peak{x=X,y=Y}, #peak{x=X1,y=Y1}).

g(_, _) -> 0.0.

noway({1,4}) -> false;
noway({2,4}) -> false;
noway({3,4}) -> false;
noway({4,4}) -> false;
noway({5,4}) -> false;
noway({6,4}) -> false;
noway({7,4}) -> false;
noway({_, _}) -> true.

heuristic_cost_estimate(#peak{x=X0, y=Y0}, #peak{x=X, y=Y}) ->
   math:sqrt(math:pow(X-X0, 2) + math:pow(Y-Y0, 2)). 

heuristic_peak(Peak, Goal) ->
    G = g(Peak, Goal),
    H = heuristic_cost_estimate(Peak, Goal),  
    Peak#peak{g = G, h = H, f = G + H}.

astar(Start, Goal) ->
   ClosedSet = empty_set(),
   OpenSet = add_to_set(heuristic_peak(Start, Goal), empty_set()),
   PathMap = empty_set(),
   astar_finder(Goal, ClosedSet, OpenSet, PathMap).

get_neighbors(X, Y, MaxX, MaxY) ->
    lists:filter(fun noway/1, 
        [{Px, Py} || Px <- pos(X, MaxX), Py <- pos(Y, MaxY), {Px, Py}=/={X,Y}]).

pos(0, _) -> [0, 1];
pos(X, Mx) when X =:= Mx -> [X-1, X]; 
pos(X, Mx) when X > 0, X < Mx -> [X-1, X, X + 1].

astar_finder(_, _, [], _) -> 0;
astar_finder(#peak{x=X,y=Y} = Goal, ClosedSet, OpenSet, PathMap) ->
   P = get_peak_withmin_f(OpenSet),
   case P of
    #peak{x=X,  y=Y}  -> 
        {P#peak.step_num, P#peak.cf ++ [{X,Y}]}; % reconstruct_path
    #peak{x=X0, y=Y0} ->
        OpenSet1 = remove_from_set(P, OpenSet),
        ClosedSet1 = add_to_set(P, ClosedSet),
        NList = filter_not_in_set(ClosedSet1, get_neighbors(X0, Y0, ?W-1, ?H-1)),
        Fun = fun({Xu,Yu}, A) ->
            E = #peak{x=Xu, y=Yu},
            T_g_s = P#peak.g + element(4, heuristic_peak(P, E)),
            Flag = case element_in_set(E, A) of
                false -> true
                ;true -> E#peak.g > T_g_s
            end,
            if Flag =:= true -> add_to_set(heuristic_peak(E#peak{cf=P#peak.cf ++ [{X0, Y0}], step_num=P#peak.step_num + 1}, Goal), A)
               ;true         -> A
            end
        end,
        OpenSet2 = lists:foldl(Fun, OpenSet1, NList),
        astar_finder(Goal, ClosedSet1, OpenSet2, PathMap)
   end.

empty_set() -> dict:new().
element_in_set( #peak{x=X, y=Y} = _E, Set)-> dict:is_key({X,Y}, Set).
remove_from_set(#peak{x=X, y=Y} = _E, Set)-> dict:erase( {X,Y}, Set).
add_to_set(     #peak{x=X, y=Y} = E, Set) -> dict:append({X,Y}, E, Set).
filter_not_in_set(Set, PeakList) -> 
    lists:filter(fun(E)-> false =:= dict:is_key(E, Set) end, PeakList).
get_peak_withmin_f(Set) ->
    Fun = fun(_, Value, []) -> Value
            ;(_, [#peak{f=F0}] = Value, [#peak{f=F1}]) when F0 < F1 -> Value
            ;(_, [#peak{f=F0, step_num=S0}] = Value, [#peak{f=F1,step_num=S1}]) when F0 == F1, S0 < S1 -> Value
            ;(_, _V2, A) -> A
    end,
    [Peak|_] = dict:fold(Fun, [], Set),
    Peak.

