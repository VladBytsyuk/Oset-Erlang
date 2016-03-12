-module(check).
-compile(export_all).

check_all() ->
	list_check(),
	set_check(),
	ordset_check(),
	oset_check().
	
check_ordsets() -> 
	ordset_allcheck(),
	oset_allcheck().
	
check_full() ->
	list_check(),
	set_allcheck(),
	ordset_allcheck(),
	oset_allcheck().

list_check() -> 
	{ReadTime, List} = timer:tc(nums, read_to_list, []),  
	io:format("	Read file to list: ~ps.~n", [ReadTime / 1000000]),
	{LengthTime, Length} = timer:tc(erlang, length, [List]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	{RemoveTime, _} = timer:tc(nums, remove_from_list, [List]),
	io:format("	Remove list: ~ps.~n", [RemoveTime / 1000000]).
	
set_check() -> 
	{ReadTime, Set} = timer:tc(nums, read_to_set, []),  
	io:format("	Read file to set: ~ps.~n", [ReadTime / 1000000]),
	{LengthTime, Length} = timer:tc(sets, size, [Set]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	{IsElTime, IsEl} = timer:tc(nums, is_elem_set, [2040710597, Set]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	{RemoveTime, _} = timer:tc(nums, remove_from_set, [Set]),
	io:format("	Remove set: ~ps.~n", [RemoveTime / 1000000]).
	
ordset_check() -> 
	{ReadTime, OrdSet} = timer:tc(nums, read_to_ordset, []),  
	io:format("	Read file to ordset: ~ps.~n", [ReadTime / 1000000]),
	{LengthTime, Length} = timer:tc(ordsets, size, [OrdSet]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	{IsElTime, IsEl} = timer:tc(nums, is_elem_ordset, [2040710597, OrdSet]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	{RemoveTime, _} = timer:tc(nums, remove_from_ordset, [OrdSet]),
	io:format("	Remove ordset: ~ps.~n", [RemoveTime / 1000000]).
	
oset_check() -> 
	{ReadTime, OSet} = timer:tc(nums, read_to_oset, []),  
	io:format("	Read file to oset: ~ps.~n", [ReadTime / 1000000]),
	{LengthTime, Length} = timer:tc(oset, size, [OSet]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	{IsElTime, IsEl} = timer:tc(nums, is_elem_oset, [2040710597, OSet]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	{RemoveTime, _} = timer:tc(nums, remove_from_oset, [OSet]),
	io:format("	Remove oset: ~ps.~n", [RemoveTime / 1000000]).







set_allcheck() ->
	{ReadTime, Set} = timer:tc(nums, read_to_set, []),  
	io:format("	Read file to set: ~ps.~n", [ReadTime / 1000000]),
	
	Set2 = nums:read_to_set_2(),
	
	{LengthTime, Length} = timer:tc(sets, size, [Set]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	
	{IsElTime, IsEl} = timer:tc(nums, is_elem_set, [2040710597, Set]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	
	{ToListTime, ToList} = timer:tc(nums, to_list_set, [Set]),
	io:format("		To list: ~p", [ToList]),
	io:format(" (~ps.)~n", [ToListTime / 1000000 / 100]),
	
	ListSet = sets:to_list(Set),
	{FromListTime, FromList} = timer:tc(nums, from_list_set, [ListSet]),
	io:format("		From list: ~p", [FromList]),
	io:format(" (~ps.)~n", [FromListTime / 1000000 / 100]),
	
	{IsSetTime, IsSet} = timer:tc(nums, is_set_set, [Set]),
	io:format("		Is set: ~p", [IsSet]),
	io:format(" (~ps.)~n", [IsSetTime / 1000000 / 100]),
	
	{UnionTime, Union} = timer:tc(nums, union_set, [Set, Set2]),
	io:format("		Union: ~p", [Union]),
	io:format(" (~ps.)~n", [UnionTime / 1000000 / 100]),
	
	{IntersectionTime, Intersection} = timer:tc(nums, intersection_set, [Set, Set2]),
	io:format("		Intersection: ~p", [Intersection]),
	io:format(" (~ps.)~n", [IntersectionTime / 1000000 / 100]),
	
	{SubtractTime, Subtract} = timer:tc(nums, subtract_set, [Set, Set2]),
	io:format("		Subtract: ~p", [Subtract]),
	io:format(" (~ps.)~n", [SubtractTime / 1000000 / 100]),
	
	{IsSubsetTime, IsSubset} = timer:tc(nums, is_subset_set, [Set, Set2]),
	io:format("		Is subset: ~p", [IsSubset]),
	io:format(" (~ps.)~n", [IsSubsetTime / 1000000 / 100]),
	
	{IsDisjointTime, IsDisjoint} = timer:tc(nums, is_disjoint_set, [Set, Set2]),
	io:format("		Is disjoint: ~p", [IsDisjoint]),
	io:format(" (~ps.)~n", [IsDisjointTime / 1000000 / 100]),

	{FoldTime, FoldResult} = timer:tc(nums, fold_set, [Set]),
	io:format("		Fold: ~p", [FoldResult]),
	io:format(" (~ps.)~n", [FoldTime / 1000000 / 100]),
	
	{FilterTime, _} = timer:tc(nums, filter_set, [Set]),
	io:format("		Filter: done (~ps.)~n", [FilterTime / 1000000 / 100]),
	
	{RemoveTime, _} = timer:tc(nums, remove_from_set, [Set]),
	io:format("	Remove set: ~ps.~n", [RemoveTime / 1000000]).
		
	
ordset_allcheck() ->
	{ReadTime, OrdSet} = timer:tc(nums, read_to_ordset, []),  
	io:format("	Read file to ordset: ~ps.~n", [ReadTime / 1000000]),
	
	OrdSet2 = nums:read_to_ordset_2(),
	
	{LengthTime, Length} = timer:tc(ordsets, size, [OrdSet]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	
	{IsElTime, IsEl} = timer:tc(nums, is_elem_ordset, [2040710597, OrdSet]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	
	{ToListTime, ToList} = timer:tc(nums, to_list_ordset, [OrdSet]),
	io:format("		To list: ~p", [ToList]),
	io:format(" (~ps.)~n", [ToListTime / 1000000 / 100]),
	
	ListOrdSet = ordsets:to_list(OrdSet),
	{FromListTime, FromList} = timer:tc(nums, from_list_ordset, [ListOrdSet]),
	io:format("		From list: ~p", [FromList]),
	io:format(" (~ps.)~n", [FromListTime / 1000000 / 100]),
	
	{IsSetTime, IsSet} = timer:tc(nums, is_set_ordset, [OrdSet]),
	io:format("		Is set: ~p", [IsSet]),
	io:format(" (~ps.)~n", [IsSetTime / 1000000 / 100]),
	
	{UnionTime, Union} = timer:tc(nums, union_ordset, [OrdSet, OrdSet2]),
	io:format("		Union: ~p", [Union]),
	io:format(" (~ps.)~n", [UnionTime / 1000000 / 100]),
	
	{IntersectionTime, Intersection} = timer:tc(nums, intersection_ordset, [OrdSet, OrdSet2]),
	io:format("		Intersection: ~p", [Intersection]),
	io:format(" (~ps.)~n", [IntersectionTime / 1000000 / 100]),
	
	{SubtractTime, Subtract} = timer:tc(nums, subtract_ordset, [OrdSet, OrdSet2]),
	io:format("		Subtract: ~p", [Subtract]),
	io:format(" (~ps.)~n", [SubtractTime / 1000000 / 100]),
	
	{IsSubsetTime, IsSubset} = timer:tc(nums, is_subset_ordset, [OrdSet, OrdSet2]),
	io:format("		Is subset: ~p", [IsSubset]),
	io:format(" (~ps.)~n", [IsSubsetTime / 1000000 / 100]),
	
	{IsDisjointTime, IsDisjoint} = timer:tc(nums, is_disjoint_ordset, [OrdSet, OrdSet2]),
	io:format("		Is disjoint: ~p", [IsDisjoint]),
	io:format(" (~ps.)~n", [IsDisjointTime / 1000000 / 100]),

	{FoldTime, FoldResult} = timer:tc(nums, fold_ordset, [OrdSet]),
	io:format("		Fold: ~p", [FoldResult]),
	io:format(" (~ps.)~n", [FoldTime / 1000000 / 100]),
	
	{FilterTime, _} = timer:tc(nums, filter_ordset, [OrdSet]),
	io:format("		Filter: done (~ps.)~n", [FilterTime / 1000000 / 100]),
	
	{RemoveTime, _} = timer:tc(nums, remove_from_ordset, [OrdSet]),
	io:format("	Remove ordset: ~ps.~n", [RemoveTime / 1000000]).
	
oset_allcheck() ->
	{ReadTime, OSet} = timer:tc(nums, read_to_oset, []),  
	io:format("	Read file to oset: ~ps.~n", [ReadTime / 1000000]),
	
	OSet2 = nums:read_to_oset_2(),
	
	{LengthTime, Length} = timer:tc(oset, size, [OSet]),
	io:format("		Length: ~p (~ps.)~n", [Length, LengthTime / 1000000]),
	
	{IsElTime, IsEl} = timer:tc(nums, is_elem_oset, [2040710597, OSet]),
	io:format("		Is element: ~p", [IsEl]),
	io:format(" (~ps.)~n", [IsElTime / 1000000 / 1000]),
	
	{ToListTime, ToList} = timer:tc(nums, to_list_oset, [OSet]),
	io:format("		To list: ~p", [ToList]),
	io:format(" (~ps.)~n", [ToListTime / 1000000 / 100]),
	
	ListOset = oset:to_list(OSet),
	{FromListTime, FromList} = timer:tc(nums, from_list_oset, [ListOset]),
	io:format("		From list: ~p", [FromList]),
	io:format(" (~ps.)~n", [FromListTime / 1000000 / 100]),
	
	{IsSetTime, IsSet} = timer:tc(nums, is_set_oset, [OSet]),
	io:format("		Is set: ~p", [IsSet]),
	io:format(" (~ps.)~n", [IsSetTime / 1000000 / 100]),
	
	{UnionTime, Union} = timer:tc(nums, union_oset, [OSet, OSet2]),
	io:format("		Union: ~p", [Union]),
	io:format(" (~ps.)~n", [UnionTime / 1000000 / 100]),
	
	{IntersectionTime, Intersection} = timer:tc(nums, intersection_oset, [OSet, OSet2]),
	io:format("		Intersection: ~p", [Intersection]),
	io:format(" (~ps.)~n", [IntersectionTime / 1000000 / 100]),
	
	{SubtractTime, Subtract} = timer:tc(nums, subtract_oset, [OSet, OSet2]),
	io:format("		Subtract: ~p", [Subtract]),
	io:format(" (~ps.)~n", [SubtractTime / 1000000 / 100]),
	
	{IsSubsetTime, IsSubset} = timer:tc(nums, is_subset_oset, [OSet, OSet2]),
	io:format("		Is subset: ~p", [IsSubset]),
	io:format(" (~ps.)~n", [IsSubsetTime / 1000000 / 100]),
	
	{IsDisjointTime, IsDisjoint} = timer:tc(nums, is_disjoint_oset, [OSet, OSet2]),
	io:format("		Is disjoint: ~p", [IsDisjoint]),
	io:format(" (~ps.)~n", [IsDisjointTime / 1000000 / 100]),
	
	{FoldTime, FoldResult} = timer:tc(nums, fold_oset, [OSet]),
	io:format("		Fold: ~p", [FoldResult]),
	io:format(" (~ps.)~n", [FoldTime / 1000000 / 100]),
	
	{FilterTime, _} = timer:tc(nums, filter_oset, [OSet]),
	io:format("		Filter: done (~ps.)~n", [FilterTime / 1000000 / 100]),
	
	{RemoveTime, _} = timer:tc(nums, remove_from_oset, [OSet]),
	io:format("	Remove oset: ~ps.~n", [RemoveTime / 1000000]).
		
