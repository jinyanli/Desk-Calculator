(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(*
Jinyan Li jli134
cmps112 Fall 2015 asg2
*)
open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

let trimzeros list =
    let rec trimzeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
             let cdr' = trimzeros' cdr
             in  match car, cdr' with
                 | 0, [] -> []
                 | car, cdr' -> car::cdr'
    in trimzeros' list

(* "asdasd" -> ['a'; 's'; 'd'; 's'; 'a'; 'd'] 
*turn a string into a list of char (helper function)
*)
    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

(* "_asd" -> (Neg,[52;77;49]) 
*turn a string into a Bigint
*)
    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

(* (Bigint (Pos,[4;3;2;1])) -> string="4321") 
*turn a Bigint into a string 
*)
    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

(*compare value of two lists*) 

    let rec cmp' list1 list2 = match (list1, list2) with
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
        else cmp' (reverse (List.tl list1')) (reverse (List.tl list2'))
       else if (List.length list1) > (List.length list2) 
            then 1 
            else 2

 (*compare value of two lists*) 
   (*let cmp list1 list2 =
           let l1=(trimzeros list1) and l2 = (trimzeros list2)
            in if l1=[] && (List.length l2) > 0
               then cmp' [0] l2
               else if (List.length l1) > 0 && l2=[]
                    then cmp' l1 [0]
                    else if l1=[] && l2=[]
                         then cmp' [0][ ]
                         else cmp' l1 l2*)
   let cmp list1 list2 =
           cmp' (trimzeros list1) (trimzeros list2)




(*get the value of the list*)
    let rec listval l pos= match l with
      | []->0
      | h::t-> h * pos+ (listval t pos*10)
 
(*compare value of two lists (doesn't work well)*)   
    let cmp2 list1 list2 = 
       if (List.length list1) < 19 && (List.length list2 < 19)
        then if (listval list1 1) > (listval list2 1)
             then 1
             else if (listval list1 1) < (listval list2 1)
             then 2
             else 0
        else if List.length list1 > List.length list2 
        then 1 
        else if List.length list1 < List.length list2
        then 2
        else cmp list1 list2

 

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  trimzeros (sum mod radix :: add' cdr1 cdr2 (sum / radix))

 
   
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0   -> list1
        | [], list2, 0   -> raise (Failure "sub':empty list1")
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> raise (Failure "sub':empty list2")
        | car1::cdr1, car2::cdr2, carry ->    
       let subtract = if (car1 - carry) < car2
                      then  car1- carry + 10 - car2
                      else car1 - carry - car2
          in  trimzeros (subtract  :: sub' cdr1  cdr2 
                        (if (car1 - carry) < car2
                         then  1
                         else 0))
    
           

      let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if (neg1 = Pos) && (neg2 = Pos)
        then Bigint (Pos, add' value1 value2 0)
        else if (neg1 = Neg) && (neg2 = Neg) 
             then Bigint (Neg, add' value1 value2 0)
             else if (neg1 = Neg) && (neg2 = Pos) 
                  then if (cmp value1 value2) = 1
                       then Bigint (Neg, sub' value1 value2 0)
                       else if (cmp value1 value2) = 2
                            then Bigint (Pos, sub' value2 value1 0)
                            else zero                       
        else if (neg1 = Pos) && (neg2 = Neg) 
             then if (cmp value1 value2) = 1
                  then Bigint (Pos, sub' value1 value2 0)
                  else if (cmp value1 value2) = 2
                       then Bigint (Neg, sub' value2 value1 0)
                       else zero
        else zero
  
     let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if (neg1 = Neg) && (neg2 = Neg)
        then if (cmp value1 value2) = 1
                  then Bigint (Neg, sub' value1 value2 0)
                  else if (cmp value1 value2) = 2
                       then Bigint (Pos, sub' value2 value1 0)
                       else zero
        else if (neg1 = Pos) && (neg2 = Pos)
             then if (cmp value1 value2) = 1
                  then Bigint (Pos, sub' value1 value2 0)
                  else if (cmp value1 value2) = 2
                       then Bigint (Neg, sub' value2 value1 0)
                       else zero
        else if (neg1 = Pos) && (neg2 = Neg)
             then  Bigint (Pos, add' value1 value2 0)
             else if (neg1 = Neg) && (neg2 = Pos)        
             then  Bigint (Neg, add' value1 value2 0)
             else zero

   let double number =add' number number 0

  let rec mul' multiplier powerof2 multiplicand' =
    if (cmp powerof2  multiplier) = 1
    then multiplier, [0]
    else let remainder, product =
          mul' multiplier (double powerof2) (double multiplicand')
      in  if  (cmp remainder powerof2) =2
           then remainder, product
          else sub' remainder  powerof2 0, add' product multiplicand' 0

   let mul (Bigint (neg1,multiplier)) (Bigint (neg2,multiplicand)) =
    if neg1 = neg2
       then let _, product = mul' multiplier [1] multiplicand in 
              Bigint(Pos, product)
       else let _, product = mul' multiplier [1] multiplicand in 
              Bigint(Neg, product)
       

    let rec divrem' dividend powerof2 divisor' =
    if (cmp divisor'  dividend) = 1
    then [0], dividend
    else let quotient, remainder =
             divrem' dividend (double powerof2) (double divisor')
         in  if (cmp remainder  divisor') = 2
             then quotient, remainder
             else add' quotient powerof2 0, sub' remainder  divisor' 0

    let divrem dividend divisor' = divrem' dividend [1] divisor'

    let div (Bigint (neg1,dividend)) (Bigint (neg2,divisor)) =
    let quotient, _ = divrem dividend divisor
    in if neg1 = neg2
       then Bigint(Pos, quotient)
       else Bigint(Neg, quotient)

    let rem (Bigint (neg1,dividend)) (Bigint (neg2,divisor)) =
    let _, remainder = divrem dividend divisor
    in if neg1 = neg2
       then Bigint(Pos, remainder)
       else Bigint(Neg, remainder)

    (*check the number is even or not*)
    let even number = 
        let _, remainder = divrem number [2] in  
           if trimzeros remainder = []
             then true
             else false       

 

    let rec power' base expt result = match expt with
    | [0]                   -> result
    | expt when even expt ->let _, base' = mul' base [1] base
                              and  expt', _ = divrem expt [2] in
                              power' base' expt' result
    | expt                ->let _, result' = mul' base [1] result in
                            power' base (sub' expt [1] 0)  result'

    let power (Bigint (neg1,base)) (Bigint (neg2,expt)) =
      if neg2 = Neg then zero
                else if even expt
                     then Bigint (Pos,power' base expt [1])
                     else Bigint (neg1,power' base expt [1])

    
    let pow=power
end

