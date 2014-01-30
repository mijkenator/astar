-module(astar_behaviour_2).

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
   add_to_set(heuristic_peak(Start, Goal, CallBackModule), 'opened'),
   astar_finder(Goal, CallBackModule).

-spec astar_finder(peak(),  atom()) -> {_, 'noway' | [{integer(), integer()}]}.
astar_finder(#peak{x=X,y=Y} = Goal, CallBackModule) ->
   P = get_peak_withmin_f('opened'),
   case P of
    noway             -> {error, noway};
    #peak{x=X,  y=Y}  -> {P#peak.step_num,  [{X,Y}] ++ P#peak.cf};
    #peak{x=X0, y=Y0} ->
        remove_from_set(P, 'opened'),
        add_to_set(P, 'closed'),
        NList = filter_not_in_set('closed', get_neighbors(X0, Y0, CallBackModule)),
        Fun = fun({Xu,Yu}) ->
            E = #peak{x=Xu, y=Yu},
            T_g_s = P#peak.g + element(4, heuristic_peak(P, E, CallBackModule)),
            Flag = case element_in_set(E, 'opened') of
                false -> true
                ;true -> E#peak.g > T_g_s
            end,
            if Flag =:= true -> add_to_set(heuristic_peak(
                    E#peak{cf=[{X0, Y0}] ++ P#peak.cf , step_num=P#peak.step_num + 1}, Goal, CallBackModule), 'opened')
               ;true         -> ok
            end
        end,
        lists:foreach(Fun, NList), %with opened
        astar_finder(Goal, CallBackModule)
   end.

-spec element_in_set(peak(), mset())       -> true | false.
element_in_set( #peak{x=X, y=Y} = _E, Set) ->
    case erlang:get({Set, X, Y}) of
        undefined -> false
        ;_        -> true
    end.

-spec remove_from_set(peak(), mset())      -> any().
remove_from_set(#peak{x=X, y=Y} = _E, Set) -> erlang:erase({Set,X,Y}).

-spec add_to_set(peak(), mset())           -> any().
add_to_set(     #peak{x=X, y=Y} = E, Set)  -> erlang:put({Set,X,Y}, E).

-spec filter_not_in_set(mset(), [{integer(), integer()}]) -> [{integer(), integer()}].
filter_not_in_set(Set, PeakList) -> 
    lists:filter(fun({X,Y})-> false =:= element_in_set(#peak{x=X,y=Y}, Set) end, PeakList).

proc_dict_list(Set) -> [ V || {{S,_,_},V} <- erlang:get(), S =:= Set ].

-spec get_peak_withmin_f(mset()) -> peak() | noway.
get_peak_withmin_f(Set) ->
    case proc_dict_list(Set) of
        []    -> noway;
        [H|T] -> gpwfi(T, H)
    end.

gpwfi([], A) -> A;
gpwfi([#peak{f=F0} = H | T], #peak{f=F1}) when F0 < F1 -> gpwfi(T, H);
gpwfi([#peak{f=F0, step_num = S0} = H | T], #peak{f=F1, step_num=S1}) when F0 == F1, S0 < S1 -> gpwfi(T, H);
gpwfi([_|T], A) -> gpwfi(T, A).

