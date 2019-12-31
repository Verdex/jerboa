

exception NthIndexOutOfRangeError of int * int 

let l_nth l i = 
    if i >= List.length l then
        raise (NthIndexOutOfRangeError(i, (List.length l)))
    ;
    List.nth l i
