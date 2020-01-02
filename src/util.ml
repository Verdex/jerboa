

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

let sub (list : 'a list) (index : int) : 'a list = 
    let rec h ll i =
        match (ll, i) with
        | ([], _) -> []
        | (l, 0) -> l
        | (_::r, n) -> h r (n - 1)

    in h list index

