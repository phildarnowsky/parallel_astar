%% @doc Priority queue implemented as a minheap.
%% @author Phil Darnowsky <phil@darnowsky.com>
%% @copyright Phil Darnowsky 2010, released under the MIT license.
%% @version 0.0.1

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

-module(minheap).
-export([new/0, new/1, toList/1, empty/1, heapSize/1, peek/1, extract/1,
         insert/3]).

%-define(TEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @type minheap() = {minheap, MinheapData}. A priority queue implemented as a minheap; i.e., elements with a lower priority value are towards the head of the queue.

%% @spec new() -> minheap()
%% @equiv new([])

new() -> {minheap, array:new()}.

%% @spec new(Proplist::[{Priority, Element}]) -> minheap()
%% @doc Returns a new minheap populated with the elements of Proplist.

new(List) -> 
  SortedList = lists:keysort(1, List),
  {minheap, array:from_list(SortedList)}.

%% @spec toList(minheap()) -> [{Priority, Element}]
%% @doc Turn a minheap into a property list, maintaining the guarantee that elements with a lower priority value are earlier in the list. Essentially the inverse of {@link new/1.}.

toList({minheap, HeapArray}) -> array:to_list(HeapArray).

%% @spec empty(minheap()) -> boolean()
%% @doc Returns true if the minheap is empty, false otherwise.

empty({minheap, HeapArray}) -> array:sparse_size(HeapArray) =:= 0.

%% @spec heapSize(minheap()) -> integer()
%% @doc Returns the number of elements in the minheap.
heapSize({minheap, HeapArray}) -> array:sparse_size(HeapArray).

%% @spec peek(minheap()) -> nothing | ItemTuple
%%       ItemTuple = {item, Priority, Value}
%% @doc Look at the first (lowest priority value) element of the minheap without removing it.
peek(Heap={minheap, HeapArray}) ->
  case empty(Heap) of
    true  -> nothing;
    false ->
      {Priority, Value} = array:get(0, HeapArray),
      {item, Priority, Value}
  end.

%% @spec extract(OriginalHeap::minheap()) -> {nothing, OriginalHeap} | {ItemTuple, minheap()}
%%       ItemTuple = {item, Priority, Value}
%% @doc Remove the first (lowest priority value) element of the minheap and return it, along with the remaining heap.
extract(Heap={minheap, HeapArray}) ->
  TopElement = peek(Heap),

  case TopElement of
    nothing -> 
      {nothing, Heap};
    _ ->
      LastIndex = heapSize(Heap) - 1,
      LastElement = array:get(LastIndex, HeapArray),

      HeapArray2 = if 
                     (LastIndex =:= 0) ->
                       % Special case if we're removing the 0th (hence only)
                       % entry: the logic in the main case below would remove
                       % the 0th element but then put it right back in, a bug
                       % I missed the first time around.
                       array:new();
                     true ->
                       array:set(0, LastElement, array:set(LastIndex, undefined, HeapArray))
                   end,

      HeapArray3 = reheap(0, HeapArray2),
      {TopElement, {minheap, HeapArray3}}
  end.

%% @spec insert(Priority, Value, minheap()) -> minheap()
%% @doc Insert an element into the minheap.

insert(Priority, Value, Heap={minheap, HeapArray}) ->
  Size = heapSize(Heap),
  HeapArray2 = array:set(Size, {Priority, Value}, HeapArray),
  HeapArray3 = bubbleUp(Size, HeapArray2),
  {minheap, HeapArray3}.

%% End of exported functions.

bubbleUp(0, HeapArray) -> HeapArray;
bubbleUp(CurrentIndex, HeapArray) -> 
  ParentIndex = parentIndex(CurrentIndex),

  {CurrentPriority, _} = array:get(CurrentIndex, HeapArray),
  {ParentPriority, _} = array:get(ParentIndex, HeapArray),

  if
    ParentPriority =< CurrentPriority ->
      HeapArray;
    true ->
      HeapArray2 = swapArrayElements(CurrentIndex, ParentIndex, HeapArray),
      bubbleUp(ParentIndex, HeapArray2)
  end.

reheap(ParentIndex, HeapArray) ->
  RightIndex = rightIndex(ParentIndex),
  LeftIndex = leftIndex(ParentIndex),

  SmallestPriorityIndex = smallestPriorityIndex([ParentIndex, RightIndex, LeftIndex], HeapArray),

  if
    SmallestPriorityIndex =:= ParentIndex ->
      HeapArray;
    true ->
      HeapArray2 = swapArrayElements(ParentIndex, SmallestPriorityIndex, HeapArray),
      reheap(SmallestPriorityIndex, HeapArray2)
  end.

swapArrayElements(I, J, Array) ->
  Tmp = array:get(I, Array),
  Array2 = array:set(I, array:get(J, Array), Array),
  array:set(J, Tmp, Array2).

smallestPriorityIndex([InitialIndex|Indices], Array) ->
  InitialPriority = priorityAtIndex(InitialIndex, Array),

  Champion = lists:foldl(
    fun(CurrentIndex, CurrentChampion={_SmallestIndex, SmallestPriority}) ->
      CurrentPriority = priorityAtIndex(CurrentIndex, Array),
      SmallerPriority = comparePriorities(SmallestPriority, CurrentPriority),

      if
        SmallerPriority =:= SmallestPriority ->
          CurrentChampion;
        true ->
          {CurrentIndex, CurrentPriority}
      end
    end,

    {InitialIndex, InitialPriority},
    Indices
  ),
  
  {SmallestIndex, _SmallestPriority} = Champion,
  SmallestIndex.

priorityAtIndex(Index, Array) ->
  case array:get(Index, Array) of
    {Priority, _Value} -> Priority;
    undefined -> undefined
  end.

comparePriorities(A, undefined) -> A;
comparePriorities(A, B) ->
  if 
    A > B -> B;
    true -> A
  end.

parentIndex(N) -> (N - 1) div 2.
rightIndex(N) -> N * 2 + 1.
leftIndex(N) -> N * 2 + 2.

%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

assert_has_minheap_property(Heap={minheap, HeapArray}) ->
  lists:foreach(
    fun(I) -> 
        {CurrentPriority, _} = array:get(I, HeapArray),
        {ParentPriority, _} = array:get(parentIndex(I), HeapArray),
        ?assert(ParentPriority =< CurrentPriority)
    end,
    lists:seq(2, heapSize(Heap) - 1)
  ).

  
extract_option(OptionName, Default, PropList) ->
  case lists:keysearch(OptionName, 1, PropList) of
    {value, {OptionName, Value}} -> Value;
    false -> Default
  end.

new_0_returns_empty_minheap_test() ->
  ?assert(empty(new())).

new_1_returns_minheap_with_equivalent_length_test() ->
  Heap = new([{3, foo}, {2, bar}, {1, baz}]),
  ?assert(heapSize(Heap) =:= 3).

test_on_random_heaps(TestFun) -> test_on_random_heaps(TestFun, []).
test_on_random_heaps(TestFun, Options) ->
  HeapCount = extract_option(heap_count, 100, Options),
  HeapSize = extract_option(heap_size, 100, Options),
  HeapSizeFun = extract_option(heapSize_fun, null, Options),
  HeapElementFun = extract_option(heap_element_fun, fun random_heap_tuple/1, Options),

  lists:foreach(
    fun(_I) ->
      CurrentHeapSize = if 
        HeapSizeFun /= null ->
          HeapSizeFun();
        true ->
          HeapSize
      end,

      List = lists:map(
        HeapElementFun,
        lists:seq(1, CurrentHeapSize)
      ),

      Heap = new(List),
      TestFun(Heap)
    end,

    lists:seq(1, HeapCount)
  ).

random_heap_tuple(I) -> {random:uniform(500), I}.

new_minheap_has_minheap_property_test() ->
  test_on_random_heaps(fun assert_has_minheap_property/1). 

peek_on_empty_minheap_returns_nothing_test() ->
  ?assert(peek(new()) =:= nothing).

peek_on_nonempty_miheap_returns_proper_tuple_test() ->
  Heap = new([{4, quux}, {3, baz}, {2, bar}, {1, foo}]),
  ?assert(peek(Heap) =:= {item, 1, foo}).

insert_into_empty_heap_has_one_element_at_top_test() ->
  Heap = new(),
  ?assert(empty(Heap)),
  Heap2 = insert(5, foo, Heap),
  ?assert(heapSize(Heap2) =:= 1),
  ?assert(peek(Heap2) =:= {item, 5, foo}).

insert_of_new_minimum_value_bubbles_to_top_test() ->
  Heap = new([{4, quux}, {3, baz}, {2, bar}, {1, foo}]),
  NewHeap = insert(0, fnord, Heap),
  assert_has_minheap_property(NewHeap),
  ?assert(heapSize(NewHeap) =:= 5).

extract_from_empty_heap_returns_nothing_test() ->
  Heap = new(),
  ?assert(empty(Heap)),
  ?assert(extract(Heap) =:= {nothing, Heap}).

extract_from_nonempty_heap_returns_top_element_and_new_heap_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      OriginalSize = heapSize(Heap),
      {item, TopPriority, TopValue} = peek(Heap),

      {{item, TopPriority, TopValue}, Heap2} = extract(Heap),

      ?assert(heapSize(Heap2) =:= OriginalSize - 1),
      assert_has_minheap_property(Heap2)
    end
  ).

extract_from_nonempty_heap_has_all_elements_but_former_top_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      OriginalList = toList(Heap),
      {{item, TopPriority, TopValue}, Heap2} = extract(Heap),
      ExtractedList = toList(Heap2),

      ListDifference = OriginalList -- ExtractedList,
      ListDifference = [{TopPriority, TopValue}]
    end
  ).

to_list_is_inverse_of_new_test() ->
  test_on_random_heaps(
    fun(Heap) ->
      Clone = new(toList(Heap)),
      ?assert(Heap =:= Clone)
    end
  ).

extract_last_element_leaves_empty_heap_test() ->
  Heap = new([{1, foo}]),
  {_, Heap2} = extract(Heap),
  ?assert(empty(Heap2)),
  
  LongerHeap = new([{1,foo}, {2, bar}, {3, baz}]),
  {_, LongerHeap2} = extract(LongerHeap),
  {_, LongerHeap3} = extract(LongerHeap2),
  ?assert(heapSize(LongerHeap3) =:= 1),
  {_, LongerHeap4} = extract(LongerHeap3),
  ?assert(empty(LongerHeap4)).

-endif.
