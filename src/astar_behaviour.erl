-module(astar_behaviour).

-export([
    astar/3
]).

-record(peak, {x, y, g=0.0, h=0.0, f=0.0, step_num=0, cf=[]}).

-type peak() :: #peak{x::integer(), y::integer(), g::float(), h::float(), f::float(), step_num::integer(), cf::[{integer(), integer()}]}.
-type mset() :: dict().

-callback get_max_width_height() -> tuple(Width :: integer(), Height :: integer()).
-callback g_fun(X :: integer(), Y :: integer()) -> G :: float().
-callback noway_fun(tuple(X :: integer(), Y :: integer())) -> true | false.

-spec heuristic_cost_estimate(peak(), peak()) -> float().
heuristic_cost_estimate(#peak{x=X0, y=Y0}, #peak{x=X, y=Y}) ->
    math:sqrt(math:pow(X-X0, 2) + math:pow(Y-Y0, 2)). 

-spec heuristic_peak(peak(), peak(), atom()) -> peak().
heuristic_peak(Peak, Goal, CallBackModule) ->
    G = g(Peak, Goal, CallBackModule),
    H = heuristic_cost_estimate(Peak, Goal),  
    Peak#peak{g = G, h = H, f = G + H}.

-spec g(peak(), peak(), atom()) -> float().
g(P, G, CallBackModule) -> CallBackModule:g_fun(P, G).

-spec get_neighbors(integer(), integer(), atom()) -> [{integer(), integer()}].
get_neighbors(X, Y, CallBackModule) ->
    {MaxX, MaxY} = CallBackModule:get_max_width_height(),
    lists:filter(fun CallBackModule:noway_fun/1, 
        [{Px, Py} || Px <- pos(X, MaxX), Py <- pos(Y, MaxY), {Px, Py}=/={X,Y}]).

-spec pos(integer(), integer()) -> [integer()].
pos(0, _) -> [0, 1];
pos(X, Mx) when X =:= Mx -> [X-1, X]; 
pos(X, Mx) when X > 0, X < Mx -> [X-1, X, X + 1].

-spec astar(peak()|{integer(), integer()}, peak() | {integer(), integer()}, atom())     -> {_, 'noway' | [{integer(), integer()}]}.
astar({X,Y}, {X1,Y1}, CallBackModule) -> astar(#peak{x=X,y=Y}, #peak{x=X1,y=Y1}, CallBackModule);
astar(Start, Goal, CallBackModule)    -> 
   ClosedSet = empty_set(),
   OpenSet   = add_to_set(heuristic_peak(Start, Goal, CallBackModule), empty_set()),
   astar_finder(Goal, ClosedSet, OpenSet, CallBackModule).

-spec astar_finder(peak(), mset(), mset(), atom()) -> {_, 'noway' | [{integer(), integer()}]}.
astar_finder(#peak{x=X,y=Y} = Goal, ClosedSet, OpenSet, CallBackModule) ->
   P = get_peak_withmin_f(OpenSet),
   %io:format("P -> ~p ~p ~n", [P, dict:to_list(OpenSet)]),
   case P of
    noway             -> {error, noway};
    %#peak{x=X,  y=Y}  -> {P#peak.step_num, P#peak.cf ++ [{X,Y}]};
    #peak{x=X,  y=Y}  -> {P#peak.step_num,  [{X,Y}] ++ P#peak.cf};
    #peak{x=X0, y=Y0} ->
        OpenSet1 = remove_from_set(P, OpenSet),
        ClosedSet1 = add_to_set(P, ClosedSet),
        NList = filter_not_in_set(ClosedSet1, get_neighbors(X0, Y0, CallBackModule)),
        Fun = fun({Xu,Yu}, A) ->
            E = #peak{x=Xu, y=Yu},
            T_g_s = P#peak.g + element(4, heuristic_peak(P, E, CallBackModule)),
            Flag = case element_in_set(E, A) of
                false -> true
                ;true -> E#peak.g > T_g_s
            end,
            if Flag =:= true -> add_to_set(heuristic_peak(E#peak{cf=[{X0, Y0}] ++ P#peak.cf , step_num=P#peak.step_num + 1}, Goal, CallBackModule), A)
               ;true         -> A
            end
        end,
        OpenSet2 = lists:foldl(Fun, OpenSet1, NList),
        astar_finder(Goal, ClosedSet1, OpenSet2, CallBackModule)
   end.

-spec empty_set() -> mset().
empty_set() -> dict:new().

-spec element_in_set(peak(), mset())       -> true | false.
element_in_set( #peak{x=X, y=Y} = _E, Set) -> dict:is_key({X,Y}, Set).

-spec remove_from_set(peak(), mset())      -> mset().
remove_from_set(#peak{x=X, y=Y} = _E, Set) -> dict:erase( {X,Y}, Set).

-spec add_to_set(peak(), mset())           -> mset().
add_to_set(     #peak{x=X, y=Y} = E, Set)  -> dict:append({X,Y}, E, Set).

-spec filter_not_in_set(mset(), [{integer(), integer()}]) -> [{integer(), integer()}].
filter_not_in_set(Set, PeakList) -> 
    lists:filter(fun(E)-> false =:= dict:is_key(E, Set) end, PeakList).

-spec get_peak_withmin_f(mset()) -> peak() | noway.
get_peak_withmin_f(Set) ->
    case dict:to_list(Set) of
        [] -> noway;
        [{_,[H]}|T] -> gpwfi(T, H)
    end.

gpwfi([], A) -> A;
gpwfi([{_,[#peak{f=F0} = H]} | T], #peak{f=F1}) when F0 < F1 -> gpwfi(T, H);
gpwfi([{_,[#peak{f=F0, step_num = S0} = H]} | T], #peak{f=F1, step_num=S1}) when F0 == F1, S0 < S1 -> gpwfi(T, H);
gpwfi([_|T], A) -> gpwfi(T, A).

%get_peak_withmin_f(Set) ->
%    case dict:fold(fun gpw_fun/3, [], Set) of
%        []       -> noway;
%        [Peak|_] -> Peak
%    end.
%
%gpw_fun(_, Value, []) -> Value;
%gpw_fun(_, [#peak{f=F0}] = Value, [#peak{f=F1}]) when F0 < F1 -> Value;
%gpw_fun(_, [#peak{f=F0, step_num=S0}] = Value, [#peak{f=F1,step_num=S1}]) when F0 == F1, S0 < S1 -> Value;
%gpw_fun(_, _, A) -> A.
