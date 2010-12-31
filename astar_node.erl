%% @doc Node data structure for A* search.
%% @author Phil Darnowsky <phil@darnowsky.com>
%% @copyright Phil Darnowsky 2010, released under the MIT license.
%% @version 0.0.1

-module(astar_node).
-export([new_node/0, new_node/1, extract_attribute/2, set_attribute/3, add_cost_estimates/2, add_cost_estimate/2]).

%% Copyright (c) 2010 Phil Darnowsky

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% @type node() = {node, NodeData}. One node in the A* search tree. NodeData generally contains <ul>
%% <li>the state the current node represents;</li>
%% <li>the estimated total cost of the optimal path from the start node, passing through this node, and ending at a goal node;</li>
%% <li>and the estimated "remaining" cost, that is, the cost of the optimal path from this node to a goal node.</li>
%% </ul>
%% You can also, using {@link set_attribute/3. 'set_attribute/3'}, set an arbitrary key-value pair on the node. One possible use for this is recording details about the path to this node.
%% NodeData is currently implemented with a dictionary from the {@link dict} module but you shouldn't count on that. Use only {@link set_attribute/3.} and {@link get_attribute/2.} to manipulate its contents, and {@link new_node/0.} or {@link new_node/1.} to create nodes.

%% @spec new_node() -> node()
%% @equiv new_node([])
new_node() -> new_node([]).

%% @spec new_node(Proplist::[{Key, Value}]) -> node()
%% @doc Returns a new node with each Key set to Value.
new_node(Proplist) ->
  NodeDict = dict:from_list(Proplist),
  {node, NodeDict}.

%% @spec set_attribute(Attribute::atom(), Value::any(), Node::node()) -> Node2::node()
%% @doc Sets the value stored in Node at the key Attribute to Value. Returns the updated node.
set_attribute(Attribute, Value, {node, NodeDict}) ->
  {node, dict:store(Attribute, Value, NodeDict)}.

%% @spec extract_attribute(Attribute::atom(), Node::node()) -> any()
%% @doc Returns the value stored in Node at the key Attribute. Crashes if the key is not present.
extract_attribute(Attribute, {node, NodeDict}) ->
  {ok, Result} = dict:find(Attribute, NodeDict),
  Result.

add_cost_estimates(Nodes, RemainingCostEstimateFun) ->
  lists:map(
    fun(Node) ->
        add_cost_estimate(Node, RemainingCostEstimateFun)
    end,
    Nodes
  ).

add_cost_estimate(Node, RemainingCostEstimateFun) ->
  RemainingCostEstimate = RemainingCostEstimateFun(Node),
  Node2 = set_remaining_cost_estimate(RemainingCostEstimate, Node),

  TotalCostEstimate = RemainingCostEstimate + extract_path_cost(Node2),
  set_total_cost_estimate(TotalCostEstimate, Node2).

set_remaining_cost_estimate(Estimate, Node) ->
  astar_node:set_attribute(remaining_cost_estimate, Estimate, Node).

extract_path_cost(Node) -> astar_node:extract_attribute(path_cost, Node).

set_total_cost_estimate(Estimate, Node) ->
  astar_node:set_attribute(total_cost_estimate, Estimate, Node).

