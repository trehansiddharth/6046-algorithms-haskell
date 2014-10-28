6046-algorithms-haskell
=======================

All the algorithms from MIT 6.046 (Design and Analysis of Algorithms) in Haskell instead of imperative code

1-scheduling.hs
---------------

Solves the weighted scheduling problem: given a listing of events (start time, end time, and a weight to indicate utility gained from attending the event), where some overlap, find the list of events to attend that maximize total utility.

2-medianFinding.hs
------------------

Implements a function, findMedian, that finds the median of a list in O(n) time, an improvement over the naive O(n^2) and sorted-list O(n lg n) methods.

3-vanEmdeBoas.hs
----------------

Implements van Emde Boas trees, which allow for efficient sorting of integers in a known range. Supports getMinimum, getMaximum, insert, and delete.

5-unionfind.hs
--------------

Implements a disjoint-set data structure that supports union and find operations in as effecient as is possible in purely functional code, which appears to be O(lg(n)*a(n)) time, where a(n) is the inverse Ackermann function. The factor of lg(n) appears because we use a finger tree-like structure instead of an array. The technique of tying the knot is used is used.
