-module(parallel_astar_manager).
-behavior(gen_server).

% External API.
-export([start_link/4, search/2]).

% Internal API.
-export([found_neighbors/2]).

% gen_server callbacks.
-export([init/1, handle_cast/2, terminate/2]).

-record(
  astar_state,
  { 
    queue,                        % Priority queue of states to examine
    free_workers,                 % Workers currently idle
    busy_workers,                 % Workers currently working
    path_cost_fun,                % Calculates cost of path to given node
    remaining_cost_estimate_fun,  % Estimates cost of path from given node to a goal
    goal_predicate_fun,           % Identifies goals
    neighbor_node_fun,            % Returns neighbors of given node
    ceiling = infinity,           % Cost of best solution found so far...
    best_solution = none_found    % ...and that solution itself.
  }
).

-define(WORKER_POOL_SIZE, 5).

start_link(
  PathCostFun,
  RemainingCostEstimateFun,
  GoalPredicateFun,
  NeighborNodeFun) ->

  WorkerPool = start_workers(PathCostFun, RemainingCostEstimateFun, NeighborNodeFun),

  InitialState = #astar_state{
    free_workers = WorkerPool,
    busy_workers = sets:new(),
    path_cost_fun = PathCostFun,
    remaining_cost_estimate_fun = RemainingCostEstimateFun,
    goal_predicate_fun = GoalPredicateFun,
    neighbor_node_fun = NeighborNodeFun
  },

  gen_server:start_link(?MODULE, InitialState, []).

search(ServerRef, StartNode) ->
  gen_server:cast(ServerRef, {self(), {search, StartNode}}).

% Used by workers to report what neighbors they've found.
found_neighbors(ServerRef, Neighbors) ->
  gen_server:cast(ServerRef, {self(), {found_neighbors, Neighbors}}).

init(InitialState) ->
  {ok, InitialState}.

% TODO: Have this fail gracefully if we're already working on a search, as
% starting another one before the first is done will get us into an 
% inconsistent state.
handle_cast({From, {search, StartNode}}, State) ->
  StartingQueue = minheap:new([{0, StartNode}]),
  State2 = State#astar_state{queue = StartingQueue},
  State3 = assign_work_to_free_workers(State2),
  {noreply, State3};

handle_cast({From, {found_neighbors, Neighbors}}, State) ->
  %io:format("Worker ~p found neighbors ~p~n", [From, Neighbors]),
  State2 = mark_worker_free(From, State),
  State3 = enqueue_found_neighbors(Neighbors, State2),
  State4 = assign_work_to_free_workers(State3),
  {noreply, State4};

handle_cast(Request, State) ->
  io:format("Unrecognized cast!~n"),
  io:format("Request is ~p~nState is ~p~n", [Request, State]),
  {stop, {unrecognized_cast, Request}, State}.

terminate(Reason, State) ->
  io:format("MANAGER TERMINATING!!!~n"),
  io:format("Reason is ~p~n", [Reason]).

start_workers(PathCostFun, RemainingCostEstimateFun, NeighborNodeFun) ->
  lists:map(
    fun(_I) ->
      {ok, Pid} = parallel_astar_worker:start_link(PathCostFun, RemainingCostEstimateFun, NeighborNodeFun),
      Pid
    end, 
    lists:seq(1,?WORKER_POOL_SIZE)
  ).

% Assign expansion of each node in the priority queue to a free worker, until
% we're out of nodes or out of free workers.
assign_work_to_free_workers(State) ->
  #astar_state{queue = Queue, free_workers = FreeWorkers, busy_workers = BusyWorkers, ceiling = Ceiling, goal_predicate_fun = GoalPredicate, best_solution = BestSolution} = State,
  FinishedAssigning = (FreeWorkers =:= [] orelse minheap:empty(Queue)),
  OptimalSolutionFound = (sets:size(BusyWorkers) =:= 0 andalso minheap:empty(Queue)),

  if
    OptimalSolutionFound ->
      % TODO: Take another fun at initialization that we call with the optimal
      % solution at this point, rather than just printing it.
      io:format("********** OPTIMAL SOLUTION FOUND HOORAY HOORAY ***************~n"),
      io:format("Cost: ~p~n~n~p", [Ceiling, BestSolution]),
      State;
    FinishedAssigning ->
      State;
    true ->
      {{item, _Priority, Node}, Queue2} = minheap:extract(Queue),
      GoalFound = GoalPredicate(Node),

      % TODO: This bit is a little long, extract branches into their own
      % functions.
      if 
        GoalFound ->
          TotalCostEstimate = astar_node:extract_attribute(total_cost_estimate, Node),
          io:format("Goal state found with cost ~p~n", [TotalCostEstimate]),

          State2 = if
            TotalCostEstimate < Ceiling ->
              State#astar_state{queue = Queue2, best_solution = Node, ceiling = TotalCostEstimate};

            true ->
              State#astar_state{queue = Queue2}
          end,

          assign_work_to_free_workers(State2);

        true ->
          [FreeWorker | RemainingWorkers] = FreeWorkers,
          BusyWorkers2 = sets:add_element(FreeWorker, BusyWorkers),

          parallel_astar_worker:neighbors(FreeWorker, Ceiling, Node),

          State2 = State#astar_state{queue = Queue2, free_workers = RemainingWorkers, busy_workers = BusyWorkers2},
          assign_work_to_free_workers(State2)
      end
  end.

mark_worker_free(Worker, State) ->
  #astar_state{free_workers = FreeWorkers, busy_workers = BusyWorkers} = State,

  BusyWorkers2 = sets:del_element(Worker, BusyWorkers),
  FreeWorkers2 = [Worker | FreeWorkers],
  
  State#astar_state{free_workers = FreeWorkers2, busy_workers = BusyWorkers2}.

% Incorporate Neighbors into the priority queue.
enqueue_found_neighbors(Neighbors, State) ->
  #astar_state{queue = Queue} = State,
  Queue2 = lists:foldl(
    fun(Neighbor, ResultQueue) ->
      minheap:insert(
        astar_node:extract_attribute(total_cost_estimate, Neighbor),
        Neighbor,
        ResultQueue
      )
    end,

    Queue,
    Neighbors
  ),

  State#astar_state{queue = Queue2}.
