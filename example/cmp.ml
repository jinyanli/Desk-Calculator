
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
 
    let rec cmp list1 list2 = match (list1, list2) with
     | [], []    -> 0
     | list1, [] -> 1
     | [], list2 -> 2
     | car1::cdr1, car2::cdr2 ->
       if (List.length list1) = (List.length list2)
       then let list1'= (reverse list1) and list2' = (reverse list2) in
            if (List.hd list1') > (List.hd list2')
            then 1
            else if (List.hd list1') < (List.hd list2')
            then 2
            else cmp (List.tl list1') (List.tl list2')
       else if (List.length list1) > (List.length list2) 
       then 1 
       else 2 ;;

(*get the value of the list*)
    let rec listval l pos= match l with
      | []->0
      | h::t-> h * pos+ (listval t pos*10)

#trace cmp     ;;
