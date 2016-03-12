-module(nums).
-compile(export_all).

%=================================== List ========================================================================
read_to_list() ->
	read_to_list("numbers.txt").
	
read_to_list(FileName) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_list_read(File)
      		after file:close(File)
    	end.

get_all_lines_in_list_read(File) ->
    	case io:get_line(File, []) of
        	eof  -> [];
        	Line -> [element(1, string:to_integer(Line)) | get_all_lines_in_list_read(File)]
   	end.

   	
remove_from_list(List) ->
	remove_from_list("numbers.txt", List).

remove_from_list(FileName, List) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_list_remove(File, List)
      		after file:close(File)
    	end.  	
   	
get_all_lines_in_list_remove(File, List) ->
    	case io:get_line(File, []) of
        	eof  -> List;
        	Line -> get_all_lines_in_list_remove(File, List -- [element(1, string:to_integer(Line))]) 
   	end.
   	
   	
   	
%=================================== Sets ========================================================================   	
read_to_set() ->
	read_to_set("numbers.txt").
	
read_to_set_2() ->
	read_to_set("numbers_2.txt").
	
read_to_set(FileName) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_set_read(File)
      		after file:close(File)
    	end.

get_all_lines_in_set_read(File) ->
    	case io:get_line(File, []) of
        	eof  -> sets:new();
        	Line -> sets:add_element(element(1, string:to_integer(Line)), get_all_lines_in_set_read(File)) 
   	end.
   	
   	
is_elem_set(Elem, Set) ->
	is_elem_set_N(Elem, Set, 1000).
	
is_elem_set_N(_, _, 0) ->
	done;
	
is_elem_set_N(Elem, Set, N) ->
	sets:is_element(Elem, Set),
	is_elem_set_N(Elem, Set, N - 1).   	
   	
   	
   	
to_list_set(Set) ->
	to_list_set_N(Set, 100).
	
to_list_set_N(_, 0) ->
	done;
	
to_list_set_N(Set, N) ->
	sets:to_list(Set),
	to_list_set_N(Set, N - 1).   	
   	
   	

from_list_set(List) ->
	from_list_set_N(List, 100).
	
from_list_set_N(_, 0) ->
	done;
	
from_list_set_N(List, N) ->
	sets:from_list(List),
	from_list_set_N(List, N - 1).   	
   	
 

is_set_set(Set) ->
	is_set_set_N(Set, 100).
	
is_set_set_N(_, 0) ->
	done;
	
is_set_set_N(Set, N) ->
	sets:is_set(Set),
	is_set_set_N(Set, N - 1).   	
   
   	
union_set(SetA, SetB) ->
	union_set_N(SetA, SetB, 100).
	
union_set_N(_, _, 0) ->
	done;
	
union_set_N(SetA, SetB, N) ->
	sets:union(SetA, SetB),
	union_set_N(SetA, SetB, N - 1).   


intersection_set(SetA, SetB) ->
	intersection_set_N(SetA, SetB, 100).
	
intersection_set_N(_, _, 0) ->
	done;
	
intersection_set_N(SetA, SetB, N) ->
	sets:intersection(SetA, SetB),
	intersection_set_N(SetA, SetB, N - 1).  
   	

subtract_set(SetA, SetB) ->
	subtract_set_N(SetA, SetB, 100).
	
subtract_set_N(_, _, 0) ->
	done;
	
subtract_set_N(SetA, SetB, N) ->
	sets:subtract(SetA, SetB),
	subtract_set_N(SetA, SetB, N - 1).     	
   	
   	
is_subset_set(SetA, SetB) ->
	is_subset_set_N(SetA, SetB, 100).
	
is_subset_set_N(_, _, 0) ->
	done;
	
is_subset_set_N(SetA, SetB, N) ->
	sets:is_subset(SetA, SetB),
	is_subset_set_N(SetA, SetB, N - 1).  
	   	
	   	
is_disjoint_set(SetA, SetB) ->
	is_disjoint_set_N(SetA, SetB, 100).
	
is_disjoint_set_N(_, _, 0) ->
	done;
	
is_disjoint_set_N(SetA, SetB, N) ->
	sets:is_disjoint(SetA, SetB),
	is_disjoint_set_N(SetA, SetB, N - 1). 	   	
	   	
   	
fold_set(Set) ->
	fold_set_N(Set, 100).
	
fold_set_N(_, 0) ->
	done;
	
fold_set_N(Set, N) ->
	sets:fold(fun(X, Y) -> X + Y end, 0, Set),
	fold_set_N(Set, N - 1).
	

filter_set(Set) ->
	filter_set_N(Set, 100).
	
filter_set_N(_, 0) ->
	done;
	
filter_set_N(Set, N) ->
	sets:filter(fun(X) -> X rem 2 == 0 end, Set),
	filter_set_N(Set, N - 1).	   	
   	
   	
    	
   	
   	
   	
   	
remove_from_set(Set) ->
	remove_from_set("numbers.txt", Set).

remove_from_set(FileName, Set) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_set_remove(File, Set)
      		after file:close(File)
    	end.  	
   	
get_all_lines_in_set_remove(File, Set) ->
    	case io:get_line(File, []) of
        	eof  -> Set;
        	Line -> get_all_lines_in_set_remove(File, sets:del_element(element(1, string:to_integer(Line)), Set)) 
   	end.


   	
%=================================== Ordsets =====================================================================   	
read_to_ordset() ->
	read_to_ordset("numbers.txt").
	
read_to_ordset_2() ->
	read_to_ordset("numbers_2.txt").
	
read_to_ordset(FileName) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_ordset_read(File)
      		after file:close(File)
    	end.

get_all_lines_in_ordset_read(File) ->
    	case io:get_line(File, []) of
        	eof  -> ordsets:new();
        	Line -> ordsets:add_element(element(1, string:to_integer(Line)), get_all_lines_in_ordset_read(File)) 
   	end.
   	
   	
is_elem_ordset(Elem, Set) ->
	is_elem_ordset_N(Elem, Set, 1000).
	
is_elem_ordset_N(_, _, 0) ->
	done;
	
is_elem_ordset_N(Elem, Set, N) ->
	ordsets:is_element(Elem, Set),
	is_elem_ordset_N(Elem, Set, N - 1).   	   	
   	
   	
to_list_ordset(OrdSet) ->
	to_list_ordset_N(OrdSet, 100).
	
to_list_ordset_N(_, 0) ->
	done;
	
to_list_ordset_N(OrdSet, N) ->
	ordsets:to_list(OrdSet),
	to_list_ordset_N(OrdSet, N - 1).   	
   	
   	

from_list_ordset(List) ->
	from_list_ordset_N(List, 100).
	
from_list_ordset_N(_, 0) ->
	done;
	
from_list_ordset_N(List, N) ->
	ordsets:from_list(List),
	from_list_ordset_N(List, N - 1).   	
   	
 

is_set_ordset(OrdSet) ->
	is_set_ordset_N(OrdSet, 100).
	
is_set_ordset_N(_, 0) ->
	done;
	
is_set_ordset_N(OrdSet, N) ->
	ordsets:is_set(OrdSet),
	is_set_ordset_N(OrdSet, N - 1).   	
   
   	
union_ordset(OrdSetA, OrdSetB) ->
	union_ordset_N(OrdSetA, OrdSetB, 100).
	
union_ordset_N(_, _, 0) ->
	done;
	
union_ordset_N(OrdSetA, OrdSetB, N) ->
	ordsets:union(OrdSetA, OrdSetB),
	union_ordset_N(OrdSetA, OrdSetB, N - 1).   


intersection_ordset(OrdSetA, OrdSetB) ->
	intersection_ordset_N(OrdSetA, OrdSetB, 100).
	
intersection_ordset_N(_, _, 0) ->
	done;
	
intersection_ordset_N(OrdSetA, OrdSetB, N) ->
	ordsets:intersection(OrdSetA, OrdSetB),
	intersection_ordset_N(OrdSetA, OrdSetB, N - 1).  
   	

subtract_ordset(OrdSetA, OrdSetB) ->
	subtract_ordset_N(OrdSetA, OrdSetB, 100).
	
subtract_ordset_N(_, _, 0) ->
	done;
	
subtract_ordset_N(OrdSetA, OrdSetB, N) ->
	ordsets:subtract(OrdSetA, OrdSetB),
	subtract_ordset_N(OrdSetA, OrdSetB, N - 1).     	
   	
   	
is_subset_ordset(OrdSetA, OrdSetB) ->
	is_subset_ordset_N(OrdSetA, OrdSetB, 100).
	
is_subset_ordset_N(_, _, 0) ->
	done;
	
is_subset_ordset_N(OrdSetA, OrdSetB, N) ->
	ordsets:is_subset(OrdSetA, OrdSetB),
	is_subset_ordset_N(OrdSetA, OrdSetB, N - 1).  
	   	
	   	
is_disjoint_ordset(OrdSetA, OrdSetB) ->
	is_disjoint_ordset_N(OrdSetA, OrdSetB, 100).
	
is_disjoint_ordset_N(_, _, 0) ->
	done;
	
is_disjoint_ordset_N(OrdSetA, OrdSetB, N) ->
	ordsets:is_disjoint(OrdSetA, OrdSetB),
	is_disjoint_ordset_N(OrdSetA, OrdSetB, N - 1). 	   	
	   	
   	
fold_ordset(OrdSet) ->
	fold_ordset_N(OrdSet, 100).
	
fold_ordset_N(_, 0) ->
	done;
	
fold_ordset_N(OrdSet, N) ->
	ordsets:fold(fun(X, Y) -> X + Y end, 0, OrdSet),
	fold_ordset_N(OrdSet, N - 1).
	

filter_ordset(OrdSet) ->
	filter_ordset_N(OrdSet, 100).
	
filter_ordset_N(_, 0) ->
	done;
	
filter_ordset_N(OrdSet, N) ->
	ordsets:filter(fun(X) -> X rem 2 == 0 end, OrdSet),
	filter_ordset_N(OrdSet, N - 1).	   	
   	
   	
   	
remove_from_ordset(OrdSet) ->
	remove_from_ordset("numbers.txt", OrdSet).

remove_from_ordset(FileName, OrdSet) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_ordset_remove(File, OrdSet)
      		after file:close(File)
    	end.  	
   	
get_all_lines_in_ordset_remove(File, OrdSet) ->
    	case io:get_line(File, []) of
        	eof  -> OrdSet;
        	Line -> get_all_lines_in_ordset_remove(File, ordsets:del_element(element(1, string:to_integer(Line)), OrdSet)) 
   	end.  	
   	

   	
   	
   	
   	
   	   	
%=================================== Osets =====================================================================   	
read_to_oset() ->
	read_to_oset("numbers.txt").
	
read_to_oset_2() ->
	read_to_oset("numbers_2.txt").
	
read_to_oset(FileName) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_oset_read(File)
      		after file:close(File)
    	end.

get_all_lines_in_oset_read(File) ->
    	case io:get_line(File, []) of
        	eof  -> oset:new();
        	Line -> oset:add_element(element(1, string:to_integer(Line)), get_all_lines_in_oset_read(File)) 
   	end.
   	
   	
is_elem_oset(Elem, OSet) ->
	is_elem_oset_N(Elem, OSet, 1000).
	
is_elem_oset_N(_, _, 0) ->
	done;
	
is_elem_oset_N(Elem, OSet, N) ->
	oset:is_element(Elem, OSet),
	is_elem_oset_N(Elem, OSet, N - 1).   	
   	
   	
   	
to_list_oset(OSet) ->
	to_list_oset_N(OSet, 100).
	
to_list_oset_N(_, 0) ->
	done;
	
to_list_oset_N(OSet, N) ->
	oset:to_list(OSet),
	to_list_oset_N(OSet, N - 1).   	
   	
   	

from_list_oset(List) ->
	from_list_oset_N(List, 100).
	
from_list_oset_N(_, 0) ->
	done;
	
from_list_oset_N(List, N) ->
	oset:from_list(List),
	from_list_oset_N(List, N - 1).   	
   	
 

is_set_oset(OSet) ->
	is_set_oset_N(OSet, 100).
	
is_set_oset_N(_, 0) ->
	done;
	
is_set_oset_N(OSet, N) ->
	oset:is_set(OSet),
	is_set_oset_N(OSet, N - 1).     	
 
 
  	
union_oset(OSetA, OSetB) ->
	union_oset_N(OSetA, OSetB, 100).
	
union_oset_N(_, _, 0) ->
	done;
	
union_oset_N(OSetA, OSetB, N) ->
	oset:union(OSetA, OSetB),
	union_oset_N(OSetA, OSetB, N - 1).   


intersection_oset(OSetA, OSetB) ->
	intersection_oset_N(OSetA, OSetB, 100).
	
intersection_oset_N(_, _, 0) ->
	done;
	
intersection_oset_N(OSetA, OSetB, N) ->
	oset:intersection(OSetA, OSetB),
	intersection_oset_N(OSetA, OSetB, N - 1).  
   	

subtract_oset(OSetA, OSetB) ->
	subtract_oset_N(OSetA, OSetB, 100).
	
subtract_oset_N(_, _, 0) ->
	done;
	
subtract_oset_N(OSetA, OSetB, N) ->
	oset:subtract(OSetA, OSetB),
	subtract_oset_N(OSetA, OSetB, N - 1).     	
   	
   	
is_subset_oset(OSetA, OSetB) ->
	is_subset_oset_N(OSetA, OSetB, 100).
	
is_subset_oset_N(_, _, 0) ->
	done;
	
is_subset_oset_N(OSetA, OSetB, N) ->
	oset:is_subset(OSetA, OSetB),
	is_subset_oset_N(OSetA, OSetB, N - 1).  
	   	
	   	
is_disjoint_oset(OSetA, OSetB) ->
	is_disjoint_oset_N(OSetA, OSetB, 100).
	
is_disjoint_oset_N(_, _, 0) ->
	done;
	
is_disjoint_oset_N(OSetA, OSetB, N) ->
	oset:is_disjoint(OSetA, OSetB),
	is_disjoint_oset_N(OSetA, OSetB, N - 1). 	   	
   
   
   
     	
   	
fold_oset(OSet) ->
	fold_oset_N(OSet, 100).
	
fold_oset_N(_, 0) ->
	done;
	
fold_oset_N(OSet, N) ->
	oset:fold(fun(X, Y) -> X + Y end, 0, OSet),
	fold_oset_N(OSet, N - 1).
	

filter_oset(OSet) ->
	filter_oset_N(OSet, 100).
	
filter_oset_N(_, 0) ->
	done;
	
filter_oset_N(OSet, N) ->
	oset:filter(fun(X) -> X rem 2 == 0 end, OSet),
	filter_oset_N(OSet, N - 1).	    	
   	
   	
remove_from_oset(OSet) ->
	remove_from_oset("numbers.txt", OSet).

remove_from_oset(FileName, OSet) ->
	{ok, File} = file:open(FileName, [read]),
    	try get_all_lines_in_oset_remove(File, OSet)
      		after file:close(File)
    	end.  	
   	
get_all_lines_in_oset_remove(File, OSet) ->
    	case io:get_line(File, []) of
        	eof  -> OSet;
        	Line -> get_all_lines_in_oset_remove(File, oset:del_element(element(1, string:to_integer(Line)), OSet)) 
   	end.  	
