

exception NthIndexOutOfRangeError of int * int 

let l_nth l i = 
    if i >= List.length l then
        raise (NthIndexOutOfRangeError(i, (List.length l)))
    ;
    List.nth l i

let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
    let rec h la lb r = 
        match (la, lb) with
        | ([], _) -> r
        | (_, []) -> r
        | (a::ar, b::br) -> h ar br ((a,b) :: r)

    in List.rev (h l1 l2 [])

