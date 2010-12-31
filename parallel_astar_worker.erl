-module(parallel_astar_worker).
-behavior(gen_server).

-export([start_link/3, neighbors/3]).
-export([init/1, handle_cast/2, terminate/2]).

-record(
  astar_worker_state,
  { 
    path_cost_fun, remaining_cost_estimate_fun,
    neighbor_node_fun
  }
).
       
start_link(PathCostFun, RemainingCostEstimateFun, NeighborNodeFun) ->
  InitialState = #astar_worker_state{
    path_cost_fun = PathCostFun,
    remaining_cost_estimate_fun = RemainingCostEstimateFun,
    neighbor_node_fun = NeighborNodeFun
  },

  gen_server:start_link(?MODULE, InitialState, []).

neighbors(ServerRef, Ceiling, Node) ->
  gen_server:cast(ServerRef, {self(), {neighbors, Ceiling, Node}}).

init(InitialState) ->
  {ok, InitialState}.

handle_cast({From, {neighbors, Ceiling, Node}}, State) ->
  #astar_worker_state{path_cost_fun = PathCostFun, remaining_cost_estimate_fun = RemainingCostEstimateFun, neighbor_node_fun = NeighborNodeFun} = State,
  Neighbors = NeighborNodeFun(Node, PathCostFun),
  Neighbors2 = astar_node:add_cost_estimates(Neighbors, RemainingCostEstimateFun),
  Neighbors3 = discard_over_expensive_neighbors(Neighbors2, Ceiling),
  parallel_astar_manager:found_neighbors(From, Neighbors3),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("Unrecognized cast!~n"),
  io:format("Request is ~p~nState is ~p~n", [Request, State]),
  {stop, error, State}.

terminate(Reason, State) ->
  io:format("WORKER TERMINATING!!!~n"),
  io:format("Reason is ~p~n", [Reason]).

discard_over_expensive_neighbors(Neighbors, Ceiling) ->
  OriginalNeighborCount = length(Neighbors),
  Result = lists:filter(fun(Neighbor) -> astar_node:extract_attribute(total_cost_estimate, Neighbor) < Ceiling end, Neighbors),
  ResultNeighborCount = length(Result),
  io:format("~p ~p nodes enter, ~p nodes leave!~n", [self(), OriginalNeighborCount, ResultNeighborCount]),
  Result.
