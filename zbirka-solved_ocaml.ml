/*SOLVED BY :EKATERINA BOCHVAROSKA

2.1 

let rec sum  n = 
let square  = fun x -> x*x  in
match n with 
| 1 -> 1 
|_ -> square n + sum (n-1);;
sum 5;;

2.2
let rec  pow  n= 
match n with 
| 1 -> 1.
|_ -> 1./. 2.** float_of_int n +. (pow (n-1)) 
;;
pow 3;;


2.3
let rec fib n = 
match n with 
|1 -> 1 
|0 -> 1 
|_ -> fib(n-1) + fib (n-2) ;;
fib 12;;


2.10
let rec sestej l = 
match l with 
|[] -> 0
|hd :: tl -> (List.hd l) + sestej(List.tl l);;
sestej [1;2;3;4;5];;


2.11
let rec unija (l1:int list) (l2:int list) = 
match l1 with 
|[] -> l2 
|hd1:: tl1 -> match l2 with 
        |[] -> l1 
        |hd2::tl2 when (hd1<> hd2) -> hd1 :: unija tl1 l2
        |_::_ -> unija  l1 (List.tl l2) ;;
unija [1;2;4;7] [2;4;7;9];;

2.12
let rec zdruzi (l1: int list) (l2: int list )=
match l1 with 
|[] -> l2 
|hd1::tl1 -> match l2 with 
    | [] -> l1 
    | hd2:: tl2 ->hd1:: zdruzi tl1 l2 ;;
zdruzi [2;3;4] [5;7;9;10;13];;

2.13  

let rec zdruzi (l1,l2) =
match (l1,l2) with
|([],[])-> []
|([],l2) -> l2
|(l1,[]) -> l1 
|(hd1::[], hd2::tl2) -> hd1::hd2::tl2
|(hd1::tl1,hd2::[]) -> hd1::hd2::tl1 
|(hd1::tl1,hd2::hd3::tl2) -> hd1::hd2::hd3::zdruzi(tl1,tl2);;

2.14
let rec vecjeod (l: int list) (x: int) =
match l with 
|[] -> []
|hd::tl when (hd>x) -> hd::vecjeod tl x 
|_::tl -> vecjeod tl x ;;
vecjeod [2;5;26;87;2;6] 5;;

2.15
let rec seznamnm a b  = 
if a>b then []
else [a] @ seznamnm (a + 1) b;;
seznamnm 5 11;;

2.16
let rec palindrom (list: int list)=
let rec reverse list =
match list with 
|[]->[]
|hd::tl -> reverse tl @[hd] in 
if (list <> reverse list ) then false 
else true ;;
palindrom [1;2;3;2;1];;
palindrom [1;2;3];;

2.17
let  vsotaSodeLihe list =

let rec oddSum list =
match list with 
|[] -> 0 
|hd::tl -> if( hd mod 2 <> 0) then hd + oddSum tl  else oddSum tl in
let rec evenSum list =+
match list with 
|[] -> 0 
|hd::tl -> if( hd mod 2 == 0) then hd+ evenSum tl  else evenSum tl in
(oddSum list,evenSum list);;
vsotaSodeLihe [1;1;1;2;4];;

2.18 vidi ja pak (sekogas matchvi par ko ke se dve listi)
let rec podseznam l1 l2 = 
match (l1,l2) with 
|([],_ )-> true 
|(hd1::tl1,hd2::tl2) when List.length l1 <= List.length l2 -> if (hd1=hd2) then podseznam tl1 tl2 else false 
|_-> false;;  
  
podseznam [1;2] [3;4;1;2];;
podseznam [1;2] [1;2;3];;
podseznam [1;2] [4;2];;

2.19
let rec cnta list =
match list with
|[]->[]
|'a':: 'a'::'a'::tl -> 3 :: cnta (List.tl list)
|'a'::'a'::tl -> 2 ::cnta tl 
|'a'::tl -> 1:: cnta tl 
|_ ::tl -> 0 ::cnta tl;;
cnta ['a';'a';'a';'a';'a';'b';'a';'a'];;

2.20 3 uslov od zmenaj vidi go pak!
let rec zamenaj list = 
match list with 
|[] -> ([],false)
|[x] -> ([x],false) 
|x :: y :: rest -> if x > y then (y :: x :: rest , true) 
                    else 
                    let (sorted,flag) = zamenaj (y:: rest) in 
                    (x:: sorted, flag)
                 
let rec sortiraj list = 
let (sorted,flag) = zamenaj list in 
if flag = true then sortiraj sorted 
else  sorted (x::sorted, flag)
  in
  let (sorted, flag) = zamenaj x in
  if flag then sortiraj sorted else sorted
;;

sortiraj [7;2;4;0];;

2.21
let ace list = 
let pattern = ['a';'c';'e'] in 
let rec helper_function list pattern = 
match list,pattern with 
|[],_ -> true
|_,[] -> false 
|hd :: tl , p::rest -> if hd = p then helper_function tl rest 
else helper_function  tl pattern 
in helper_function list pattern;;


2.22
let rec convert list = 
match list with 
|[] -> []
|1:: 1:: 1::tl -> 3:: convert tl
|1:: 1:: tl -> 2:: convert tl
|1::tl -> 1 ::convert tl
|0::tl -> 0 :: convert tl
|_ -> convert (List.tl list);;
convert [1;1;0;1;1;1;1;1;0;1;0];;

2.23
let rec count n m =
if n>(m-1)then []else n::count(n+1)m;;
let rec cikli m n =
if n=0 then []else count 0 m @ cikli m (n-1);;
cikli 3 4 ;;
                    

2.24 NOT PERFECT COUDNT MAKE THE COUNTER 
type dnk = A | C | T | G 
let longest_subseq (dna_list: dnk list)  =
let rec aux  pos counter dna_list=
match dna_list with 
|[] -> (pos,counter)
|hd1::hd2::tl when (hd1==hd2) ->aux (pos+1) (counter+2) tl
|_:: tl -> aux (pos) (counter) tl in
  match dna_list with
  |[] -> ( 0, 0)  (* Handle an empty list case *)
  |hd :: tl -> aux 1 0 dna_list;;
longest_subseq [A;C;A;A;A;A;A;G;T] ;;


2.25 one occurence 
type dnk = A | C | T | G

let prestej (list : dnk list) =
  let rec count_elements list prev_elem count acc =
    match list with
    | [] -> List.rev((prev_elem, count) :: acc)
    | hd :: tl ->
      if hd = prev_elem then count_elements tl prev_elem (count + 1) acc
      else count_elements tl hd 1 ((prev_elem, count) :: acc)
  in
  match list with
  | [] -> []
  | hd :: tl -> count_elements tl hd 1 [];;
  prestej [ A; C;A ;A ;G; C; C ];; 



let longest_subseq list =
  let rec count_elements pos list prev_elem count =
    match list with
    | [] -> (pos , count)
    | hd :: tl ->
      if hd = prev_elem then count_elements (pos) tl prev_elem (count + 1)
      else (pos+1, count)
  in
  match list with
  | [] -> (0, 0)
  | hd :: tl -> count_elements 0 tl hd 1
;;

longest_subseq [1; 2;2;2; 2; 3; 4; 5; 1; 2];;



2.26
let rec zamenjaj (lista : int list) =
match lista with
|[]->[]
|x::[] -> [x]
|x::y::tl when ( x>=y) -> y::x::tl
|x::tl -> zamenjaj tl ;;
zamenjaj [2;3;1;4;5];;

////////////////////////////////
let rec zamenjaj (lista : int list) =
  match lista with
  | [] -> []
  | x::y::tl when x >= y -> y :: zamenjaj (x :: tl)
  | x::tl -> x :: zamenjaj tl  
let sort_zamenjaj lista =
  let modified_list = zamenjaj lista in
  List.sort compare modified_list;;
sort_zamenjaj [2;3;1;4;5];;


2.27
let rec razsiri (rpairs:(int*int) list) (numbers:int list):int list=
match rpairs with 
|[]->[]
|(a,b)::tl -> let x =b in 
let rec brojki numbers=
match numbers with 
|[]->[]
|h::tl when(x=h) ->  x::(brojki tl)
|_::tl -> brojki tl in 
brojki numbers  @ razsiri (List.tl rpairs) numbers
;;
razsiri [(1, 5); (2, 8); (3, 5); (4, 10)] [5; 8];;


2.28
let rec zdruzi (a: int list) (b : int list) =
match a with 
|[] -> b 
| hd_a::tl_a -> match b with 
|[] -> a 
|hd_b :: tl_b -> let c = a @ b in 
let result= List.sort compare c in
let rec remove =
function
  | []  -> []
  | x::[] -> x::[]
  | x::y::tl ->
     if x=y then remove (y::tl)
     else x::remove (y::tl)
     in remove result ;;
zdruzi [1;2;3] [5;3;2];;

2.29

let rec fib3 y =
  match y with 
  | 1 | 2 | 3 -> 1 
  | n -> fib3 (n-1) + fib3 (n-2) + fib3 (n-3) 

let fib3list x =
  let rec aux x acc =
    if x = 0 then
      acc
    else
      aux (x-1) (fib3 x :: acc)
  in
  aux x [];;

2.30

let sumEvenOdd x =
let even =  ref 0 in 
let odd = ref 0 in 
let rec pomosna list =
match list with 
|[] -> ()
| hd :: tl -> if( hd mod 2 = 0 ) 
            then even := !even + hd 
              else odd := !odd + hd 
              ;pomosna tl
              
in 
pomosna x ;
(!even,!odd);;
sumEvenOdd [2;3;4;1;0];;

2.31 raboti za 1 stop not multiple
let rec povezava (start,target) (list: (string *string)list) =
match list with 
|[] -> false
|(a,b) ::tl -> if ( (start = a && target = b) || (start = b) || (target =a)) then true 
else povezava (start,target) tl ;;

let flights = [("Ljubljana", "Vienna"); ("Vienna", "Munich"); ("Munich", "Berlin")];;

povezava ("Ljubljana", "Vienna") flights;;
(* Expected output: true *)

povezava ("Vienna", "Berlin") flights;;
(* Expected output: true *)

povezava ("Ljubljana", "Berlin") flights;;
(* Expected output: true *)

povezava ("Vienna", "Ljubljana") flights;;
(* Expected output: false *)

povezava ("Berlin", "Ljubljana") flights;;
(* Expected output: false *)

2.32 
type plist = (int * string) list 
let rec vapply  (list: plist) (f : ( string -> string)):plist=
match list with 
| [] -> []
| (a,b)::tl ->let x = (f b ) in (a,x) :: vapply tl f ;;
let plist = [(1, "hello"); (2, "world"); (3, "foo")];;

let uppercase = String.uppercase;;

let result = vapply plist uppercase;;
(* Output: [(1, "HELLO"); (2, "WORLD"); (3, "FOO")] *)

let add_suffix s = s ^ "!";;
let result2 = vapply plist add_suffix;;
(* Output: [(1, "hello!"); (2, "world!"); (3, "foo!")] *)

2.33
let rec merge l1 l2 f =
  match l1 with
  | [] -> l2
  | h1 :: t1 ->
    match l2 with
    | [] -> l1
    | h2 :: t2 ->
      let result = f (h1, h2) in
      result :: merge t1 t2 f
;;

let l1 = [1; 2; 3];;
let l2 = [4; 5; 6];;

let sum_merge (a,b) = a + b;;  

merge l1 l2 sum_merge;;

2.34

let digits list =
let counter = ref 0 in
let rec dig1 list =
  match list with
  | [] -> []
  | h :: t ->
    let result = (!counter + 1, h) in
    counter := !counter + 1;
    result :: dig1 t in dig1 list;;

digits [10; 20; 30; 40];;


2.35 wrong synatx error 
let rec meet (list1:('a * 'b) list) (list2:('a * 'b) list) =
  match list1, list2 with
  | [], [] -> []
  | hd::tl, [] -> []
  | [], hd::tl -> []
  | hd1::tl1, hd2::tl2 ->
  let (k1, v1) = hd1 and (k2, v2) = hd2 in
  if k1 = k2 then (k1, (v1, v2))::(meet list1 tl2)
  else if k1 < k2 then meet tl1 list2
  else meet tl1 list2;;
meet [(1 ,2) ;(2 ,3) ;(4 ,5);(4 ,9)] [(2 ,4);(4 ,6)];;


 2.36 prasaj zosto go stava (4,5) parot 
let rec diff (l1: ('a * 'b) list) (l2: ('a * 'b) list) =
  match l1 with
  | [] -> []
  | hd1::tl1 ->
    let (k1, v1) = hd1 in
    if List.exists (fun (k2, _) -> k1 = k2) l2 then
      diff tl1 l2
    else
      (k1, v1) :: diff tl1 l2
;;


2.37

let rec podlista list (z,k) =
match list with 
|[] -> []
|hd::tl ->if (z>=k || k >= List.length list) then []
else (List.nth list z):: podlista list (z+1,k) ;;
podlista [1;2;3;4;5;6] (2,5);;

2.38
let rec filter f (list: ('k*'v)list) : ('k list)=
match list with 
|[] -> []
|hd :: tl -> let (a,b) = hd in
if (f b) then a :: (filter f tl )
else (filter f tl );;
filter ( function x -> x =0) [(1 ,0) ;(2 ,1) ;(3 ,0)
;(4 ,1)];;

2.39
let editPair (x,y) f=
if (f (x,y)) then (x,y)
else (y,x) 
let greater (a,b) =
if a>b then true 
else false 
;;
editPair (2,3) greater;;

2.40
let rec izberi l f =
match l with 
|[] -> []
|hd::tl -> if (f hd) then hd:: (izberi tl f)
else izberi tl f ;;
let f a = if a >2 then true else false ;;
izberi [1;2;3;5] f ;;

2.41  nee e skroz tocna 
let rec skrci (f : int * int -> int) seznam =
  match seznam with
  | [] -> failwith "List must have at least two elements"
  | [x] -> x
  | hd1 :: hd2 :: tl -> skrci f ((f (hd1, hd2)) :: tl)
;;
let add_numbers (a, b) = a + b

let result = skrci add_numbers [1; 2; 3; 4; 5];;

2.42
let rec foldx alist b functions =
  match alist, functions with
  | [], _ -> b
  | _, [] -> b
  | x :: xs, f :: fs ->
    let result = f x (foldx xs b fs) in
    result;;
let f1 a b = a + b;;
let f2 a b = a * b;;

let functions = [f1; f1; f1; f1;f1];;

foldx [1; 2; 3; 4; 5] 0  functions;;

2.43
type izraz =
  | Nil
  | Stevilo of int * izraz
  | Oper of char * izraz

let rec evaluate (e: izraz) =
  match e with
  | Nil -> 0
  | Stevilo (x, y) ->
    let number = x in
    match y with
    | Nil -> number
    | Oper (m, n) ->
      if m = '+' then number + evaluate n
      else if m = '-' then number - evaluate n
      else  evaluate n ;;
evaluate (Stevilo(1,Oper('+' ,Stevilo (5,Oper('-',Stevilo(3,Nil))))));;

2.44 type double = {
  value : int;
  mutable next : double option;
  mutable prev : double option;
}

(* Create nodes *)
let rec node1 = { value = 1; next = Some node3; prev = None }
and node3 = { value = 3; next = Some node5; prev = Some node1 }
and node5 = { value = 5; next = None; prev = Some node3 }
;;

(* Example usage *)
let list_head = node1;;

let insert (list : double) (x: int) =
  let new_node = { value = x; next = None; prev = None } in
  let rec append current_list =
    match current_list.next with
    | None ->
        new_node.prev <- Some current_list;
        current_list.next <- Some new_node
    | Some next_node ->
        if next_node.value >= x then 
        (
          new_node.prev <- Some current_list;
          new_node.next <- Some next_node;
          current_list.next <- Some new_node;
          next_node.prev <- Some new_node
        )
        else
          append next_node
  in  append list;
list
;;

(* Example usage *)
insert list_head 4;
 

2.45

A)
type bdrevo = List of int 
            |Drevo of bdrevo *int *bdrevo 

let rec prestej (tree: bdrevo) =
match tree with 
|List v -> v 
|Drevo (l,v,d) ->  v+ prestej l + prestej  d ;;
let example_tree =
  Drevo (
    Drevo (Drevo (List 0, 3, List 0), 5, Drevo (List 0, 6, List 0)),
    7,
    Drevo (Drevo (List 0, 8, List 0), 10, Drevo (List 0, 11, List 0))
  );;
  prestej example_tree;;
 
pod B) ne e tocna 
type bdrevo = List of int | Drevo of bdrevo * int * bdrevo

let rec prestej (tree: bdrevo) =
  match tree with 
  | List v -> v 
  | Drevo (l,v,d) -> v + prestej l + prestej d

let modify_tree (result: int) (tree: bdrevo) =
  let rec aux tree =
    match tree with 
    | List v -> List result
    | Drevo (l,v,d) -> Drevo (aux l, result, aux d)
  in
  aux tree

let tree = Drevo (Drevo (Drevo (List 0, 3, List 0), 5, Drevo (List 0, 6, List 0)), 7, Drevo (Drevo (List 0, 8, List 0), 10, Drevo (List 0, 11, List 0)))

let result = prestej tree
let modified_tree = modify_tree result tree

2.46 

type drvo = {
elm: int;
sum: int;
levo : drvo option ;
desno : drvo option}

let rec sum (tree : drvo) = 
match tree with 
|{ elm  ; sum= x ;levo =None ; desno = None}-> x 
|{ elm ; sum = y ;levo =Some (left_subtree) ; desno= None} -> y+ sum left_subtree
|{ elm ; sum= z;levo = None; desno= Some (right_subtree)} -> z+ sum right_subtree
|{elm  ;sum= m; levo= Some (subtree1); desno= Some (subtree2)} -> m+ sum subtree1 + sum subtree2;;


let rec print (tree: drvo) = match tree with 
|{ elm  ; sum= x ;levo =None ; desno = None}-> if x <10 then print_int elm 
|{ elm ; sum = y ;levo =Some (left_subtree) ; desno= None} -> 
if y <10 then print_int elm else print left_subtree
|{ elm ; sum= z;levo = None; desno= Some (right_subtree)} ->
if z <10 then print_int elm else print right_subtree 
|{elm  ;sum= m; levo= Some (subtree1); desno= Some (subtree2)} ->if m <10 then print_int elm 
else (print subtree1; print subtree2 )

2.47
let rec checkOrder (tree : drvo) = 
match tree with 
|{ elm =e ; sum= x ;levo =None ; desno = None}-> 
true 
|{ elm = e ; sum = y ;levo =Some (left_subtree) ; desno= None} -> 
if e > left_subtree.elm then true && checkOrder left_subtree  else false 

|{ elm =e ; sum= z;levo = None; desno= Some (right_subtree)} ->
if e < right_subtree.elm then true && checkOrder right_subtree  else false 

|{elm =e ;sum= m; levo= Some (subtree1); desno= Some (subtree2)} -> 
if e < subtree1.elm && e > subtree2.elm 
then true && checkOrder subtree1 && checkOrder subtree2 
else false ;;

 
2.48 
i) 
type operation = PLUS | TIMES ;;
type element = Val of int | Op of operation ;;
type expr = element list;;

let expression = [Val 1;  ];;

let rec check (expression: expr) : bool =
  match expression with
  | [] -> false
  | Op _ :: _ -> false  
  | Val _ :: [] ->false
      | Val _::Op _::Val _::[]-> true
      | Val _ :: Val _ ->  false
      | Val _::Op _ :: tl2 -> check tl2

;;

ii)
let calc (expression:expr) =
let rec apply acc  (expression: expr) =

match expression with 
    |[] -> acc 
    |Val n :: tl when (n>0) ->  apply n (List.tl expression)
    |Op x :: tl -> match x with 
                |PLUS -> acc + apply 0 (List.tl expression)
                |TIMES -> acc * apply 1 (List.tl expression)
in apply 0 expression ;;
calc [Val 1;Op  PLUS;Val 2 ;Op TIMES ; Val 3;Op TIMES; Val 4];;
check expression;;

2.49
type itree = Nil | Node of itree* int *itree ;;
let rec sum (tree : itree) =
match tree with
|Nil -> 0
|Node (l,v,d) ->  sum l + sum d + v

let rec  sumsub (tree :itree) =
match tree with
|Nil -> Nil 
|Node ( l,v,d) ->
let suma = sum  l + sum   d+ v   in
Node ((sumsub  l) ,suma  ,(sumsub d) );;
let tree = Node ( Node ( Nil ,3 , Nil ) ,5 , Node ( Nil ,2 , Node (
Nil ,1 , Nil ) )) ;;
sumsub tree;;

2.50
type bdrevo = Leaf of int |Node of bdrevo * int * bdrevo ;;
let rec bapply (tree : bdrevo) (f : ( int -> int)) =
match tree with 
|Leaf v ->  Leaf  (f v) 
| Node (l,v,d ) -> Node (bapply l f, ( f v ), bapply d f);;
let example = Node ( Node ( Leaf 0 ,3 , Leaf 0 ) ,5 , Node (Leaf 0 ,2 , Node (
Leaf 0,1 ,Leaf 0 ) )) ;;
let f x =  x+2 in 
bapply example f ;;

2.51  VIDI JA PAK ! 
type operation = PLUS | TIMES ;;
type element = Val of int | Op of operation ;;
type expr = element list ;;

let calc (izraz : expr) =
  let rec apply (izraz : expr) =
    match izraz with
    | [] -> failwith "Invalid expression"
    | [Val v] -> v
    | Val v1 :: Op o :: Val v2 :: tl ->
        let result =
          match o with
          | PLUS -> v1 + v2
          | TIMES -> v1 * v2
        in
        apply (Val result :: tl)
    | _ -> failwith "Invalid expression"
  in
  apply izraz;;


2.52
type bindrevo = List of int | Drevo of bindrevo * bindrevo ;;

let rec izpis (tree : bindrevo) (x : int) : unit =
  match tree with 
  | List v ->if (v > x) then print_int  v

              else izpis tree x
  | Drevo (l, d) -> izpis l x; izpis d x 

;;
let example = Drevo ( Drevo (List 5,List 10 ),Drevo (Drevo (List 10,List 10 ),List 6)) ;;
izpis example;;

2.53
type bool_exp =
|Val of bool
|Not of bool_exp 
|And of  bool_exp * bool_exp 
|Or of bool_exp * bool_exp 
let rec eval (expr: bool_exp) =
match expr with 
|Val x -> x
|Not y -> not(eval  y)
|And (a,b) -> if ( eval a = true && eval b = true ) then true else false 
|Or (c,d) -> if (eval c = false && eval d = false ) then false else true ;;
(* Boolean expression: (true && false) || (not true) *)
eval (Or (And (Val true, Val false ) ,Not (Val true)));;

2.54
type text = Eot | Line of line * text
and line = Eol | Word of string * line

let rec search (text : text) (line : line) : bool =
  match text with
  | Eot -> false
  | Line (x, y) ->
    match x with
    | Eol -> false
    | Word (s, m) -> m = line ||
     search y line ;;
    let example_text =
  Line (Word ("Hello", Word ("world!", Eol)), Line (Word ("This", Word ("is", Word ("an", Word ("example", Eol)))), Eot));;

let example_line = Word ("is", Word ("an", Word ("example", Eol)));;

search example_text example_line;;

2.55
type int_tree = { mutable key : int ;
mutable trees : int_tree list }
let rec tree_filter (tree : int_tree) x =
if tree.key > x then tree.key :: List.concat(List.map (fun t -> tree_filter t x ) tree.trees)
else List.concat(List.map (fun t -> tree_filter t x ) tree.trees);;
let example_tree ={key=10;trees=[{key=5;trees=[{key=12;trees=[{key=8;trees=[]}]};]};]};;
tree_filter example_tree 9;;


2.56
type 'a list = Nil|Value of 'a * 'a list;;
let p x y = if x > y then -1 else if x=y then 0 else 1

let rec dodaj (list:'a list) (p:('a->'a->int))  elem=
match list with 
|Nil -> Value (elem,Nil)
|Value (x,rest) -> if p x elem = -1  then Value (elem, dodaj rest  p x)
else if primeraj x elem =0  then dodaj rest p x
else Value (x,dodaj rest p elem);;

let example_list = Value (3, Value (7, Value (10, Nil)))

let new_list = dodaj example_list p 2

2.57

type 'a element = {
  mutable vrednost : 'a;
  mutable naslednji : 'a seznam
}
and 'a seznam = Prazen | Element of 'a element

let rec podvoji (seznam : 'a seznam) : 'a seznam =
  match seznam with
  | Prazen -> Prazen
  | Element(e) ->
      let nov_element = { vrednost = e.vrednost; naslednji = podvoji e.naslednji } in
      Element({ vrednost = e.vrednost; naslednji = Element(nov_element) });;



let lista =Element({ vrednost = 1; naslednji = Element({ vrednost = 2; naslednji = Prazen }) })
in podvoji lista;;





2.58
i)
let rec concatenate_seznam seznam1 seznam2=match seznam1 with
|Prazen->seznam2|Element el->el.naslednji<-concatenate_seznam el.naslednji seznam2;
seznam1;;
let lista =Element({ vrednost = 1; naslednji = Element({ vrednost = 2; naslednji = Prazen }) });;
let lista2 =Element({ vrednost = 3; naslednji = Element({ vrednost = 4; naslednji = Prazen }) });;
concatenate_seznam lista lista2;;

ii)
type 'a element = {
 mutable vrednost : 'a ;
 mutable naslednji : 'a seznam
 }
 and 'a seznam = Prazen | Element of 'a element
;;
let rec copy s1 s2 =
match s1 with|Prazen->s2
|Element(e)->let new_list={vrednost=e.vrednost;naslednji=copy e.naslednji s2}in
Element(new_list);;

2.59 prasaj ja kiki dali e vaka ???


let rec dolzinevej tree =
  match tree with
  | Nic -> () 
  | Ena (x, grm) -> print_endline "1"; dolzinevej grm;
  | Dva (grm1, y, grm2) -> dolzinevej grm1;print_endline "2" ;dolzinevej grm2;;
let tree = Ena (1, Dva (Ena (2, Nic), 3, Nic));;

dolzinevej tree;;

2.60 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

2.61
type 'a tree = Empty |Node of 'a * 'a tree list ;;
let rec count_nodes (tree : 'a tree) : int =
match tree with
|Empty -> 0 
|Node (_,lista) -> let subtree = List.map  count_nodes  lista in
List.fold_left (+) 1 subtree;;
let example_tree = Node (1, [Node (2, [Empty; Node (3, [Empty]) ; Node (3, [Empty]);Node (3, [Empty])]); Empty]);;
count_nodes example_tree;;

2.62 

i)

type 'a bindrevo = List of 'a | Drevo of 'a * 'a bindrevo * 'a bindrevo ;;

let rec izpis (drvo : 'a bindrevo ) (f:('a -> bool) ) =
    match drvo with
    | List x -> if f x then print_newline x
    | Drevo (e, l, d) -> if f e then print_newline e else izpis l f; izpis d f

ii)
let rec obrni (tree: 'a bindrevo) =
match tree with 
|List x -> List x 
|Drevo (y,l,d) -> Drevo (y,obrni d, obrni l);;

obrni (Drevo (1, List 2, List 3));;

2.63
type 'a tree = Empty | Node of 'a * 'a tree list ;;
let rec filter (tree: 'a tree) (f :('a -> bool)) =
match tree with 
|Empty->Empty 
|Node(x,lista)->
let subList=List.map(fun subtree->filter subtree f)lista in 
let subTree=List.filter(fun t->t<>Empty)subList in
if f x || subTree<>[] then Node(x, subTree) else Empty;; 
let example = Node (4,[Node (1,[Node(3,[Empty])])]);;
filter example (fun x -> x mod 2 = 0);;

2.64

type operacija = PLUS | MINUS;;
type 'a izraz = Value of 'a | Expr of 'a izraz * operacija * 'a izraz;;


type operacija = PLUS | MINUS
type 'a izraz = Value of 'a | Expr of 'a izraz * operacija * 'a izraz

let rec plus (a: 'a) (b: 'a) : 'a =
  a + b;;

let rec minus (a: 'a) (b: 'a) : 'a =
  a - b;;

let rec izracun (expr: 'a izraz) : 'a =
  match expr with
  | Value e -> e
  | Expr (x, op, y) ->
    let left_value = izracun x in
    let right_value = izracun y in
    match op with
    | PLUS -> plus left_value right_value
    | MINUS -> minus left_value right_value
;;

2.65  
let obrni = fun niza ->
let lista = Array.to_list niza in 
let lista2 = List.rev lista in 
let reversed_array = Array.of_list lista2 in reversed_array;;

obrni [|1;2;3|];;

2.66
type 'a minvrsta = 'a array ;;
let kreiraj n v = Array.create n v ;;
let a : int minvrsta = kreiraj 10 0;;
let dodaj x a =
let rec pomosna x a index = 
if a.(index) =0 || a.(index) >= x then 
a.(index) <- x 
else 
pomosna x a (index -1 ) in 
pomosna x a (Array.length a -1);
a;;
dodaj 2 a;;
dodaj 3 a;;
dodaj 1 a;;
dodaj 4 a;;
dodaj 5 a;;
dodaj 1 a;;

2.67
type ('a ,'b) drvo =
|Prazno 
|Vozliscea of 'a * ('a,'b) drvo list 
|Vozlisceb of 'b * ('a,'b) drvo list ;;
let rec split (tree : ('a,'b) drvo ) =
match tree with 
|Prazno -> [],[]
|Vozliscea (a,list_a)->
let subtreeList_A = List.map split list_a in 
let ax,bx = List.split subtreeList_A in 
(a :: List.concat ax, List.concat bx) 
|Vozlisceb (b,list_b) ->
let subtreeList_B = List.map split list_b in 
let ax,bx = List.split subtreeList_B in 
(List.concat ax, b::List.concat bx);;

let tree_example : (int, string) drvo =
  Vozliscea (1, [Vozlisceb ("a", []);
                 Vozliscea (2, [Vozlisceb ("b", []); Prazno]);
                 Vozliscea (3, [])]);;

split tree_example;;

2.68 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

2.69
type 'a element = 
{
mutable value: 'a ;
mutable next : 'a list;
} 
and 'a list = Empty| Element of 'a element ;;
let rec length (mylist: 'a list) : int =
match mylist with 
|Empty -> 0
|Element (e)-> if e.value > 0 then  1 + length e.next 
else length e.next ;;

2.70 (JA DODAOV FUNKCIJATA VNATRE SAMO DA NE E BEZ , INACE NE MORAT)

type 'a slika = 'a array array ;;
let rec  zdruzi (s1 : 'a slika ) (s2: 'a slika ) (f :('a -> 'a -> 'a)) = 
let f x y = if x = y then y
else x in
let rows1 = Array.length s1 in
let columns1 = Array.length s1.(0) in 
let s3 = Array.make_matrix rows1 columns1 0 in
for i = 0 to rows1 -1 do
for j = 0 to columns1 -1 do 
s3.(i).(j) <-f s1.(i).(j) s2.(i).(j) 
done;
done;
s3
;;

2.71
type 'a struc = 
|Elm of 'a 
|Pair of 'a struc * 'a struc 
|Triple of 'a struc * 'a struc * 'a struc ;;

let rec smap (f: ('a -> 'b ))  (struc : 'a struc ) =
match struc with 
|Elm (e) -> Elm (f e )
|Pair (a,b) -> Pair ( smap f a, smap f b)
|Triple (x,y,z) -> Triple (smap f x, smap f y, smap f z);;
smap ( function x -> x +1) ( Triple ( Pair ( Elm 1, Elm 2) , Elm 3 , Elm 4)) ;;

2.72
type 'a ftab = ('a -> 'a) array
let rec tapply  l  (t: 'a ftab)  =
match l with 
|[] -> []
|(i,a) :: tl -> (t.(i) a) :: tapply tl t;;


let functions : int ftab = [|((+) 1); (( * ) 2); ((-) 3)|];;
let input_list = [(0, 2); (1, 3); (2, 4)];;
tapply input_list functions ;;


2.73
type ( 'b, 'a) dictionary = ( int * 'a) list
let rec read d   (key: int) : 'a =
match d with 
|[] -> failwith "Key not found" 
|(a,b) :: tl -> if a = key then b  
                else read tl key;;

2.74 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

2.75
type ('a ,'b) dictionary = ('a * 'b) list ;;
let equal x y = 
if x = y then true 
else false 

let rec duplicate (d :('a,'b) dictionary)   =
match d with 
|[] -> []
|(_, _)::[] -> d
|(k1,v1) :: (k2,v2):: tl -> if equal v1 v2 then  duplicate ((k1,v1) ::tl ) 
else (k1, v1) :: duplicate ((k2, v2) :: tl);;
duplicate [(1, "apple"); (2, "banana"); (3, "banana"); (4, "orange"); (5, "kiwi"); (6, "kiwi")];;

2.76
type 'a rnode ={
mutable cont : 'a ;
mutable next : 'a rlist }
and 'a rlist = Nil | Elm of 'a rnode ;;
let rec filter (list : 'a rlist) f  =
match list with 
|Nil -> Nil 
|Elm (e) -> if (f e.cont  ) then Elm ({cont = e.cont ;next = filter e.next f })
else filter e.next f ;;

2.77
type ('a ,'b) tree =
Nil 
|Nodea of 'a * ('a,'b) tree list 
|Nodeb of 'b * ('a,'b) tree list ;;
let rec rascepi  (drvo : ('a ,'b) tree) =
match drvo with
|Nil -> [],[]
|Nodea (a,lista_A) ->
 
let subtreeA = List.map rascepi  lista_A in 
let sa,sb = List.split subtreeA in 
(a :: List.concat sa , List.concat sb )

|Nodeb (b,lista_B) ->

let subtreeB = List.map rascepi  lista_B in 
let sa,sb = List.split subtreeB in 
(List.concat sa ,b:: List.concat sb )


2.78 type 'a tree = { mutable key:'a ; mutable trees:'a tree list } 
let rec tree_apply (tree:'a tree) (f:('a->'b)):'b tree =
match tree with
|{key;trees=[]}->let new_value=f key in {key=new_value;trees=[]}
|{key;trees}->let new_value=f key in{key=new_value;trees=List.map(fun t->tree_apply t f)tree.trees}
let example_tree =
  { key = 10;
    trees =
      [
        { key = 5; trees = [] };
        { key = 12; trees = [] };
        { key = 8; trees = [] }
      ]
  };;
let increment x = x + 1 in
tree_apply example_tree increment ;;



2.79 
type ('a ,'b) key_val = ('a * 'b) array ;;
let array_filter (arr: ('a ,'b) key_val)  (f : ('a -> bool))=
let rows = Array.length arr in  
let arr2 : ('a ,'b) key_val = (Array.copy  arr) in
for i = 0 to rows -1 do 
if (f  (fst arr.(i)) = false ) 
then arr2.(i) <- arr.(i) 
done;
arr2;;

better sollution :
type ('a ,'b) key_val = ('a * 'b) array ;;

let array_fillter (arr :('a ,'b) key_val) (f:('a -> bool)) :('a,'b) key_val=
let result = ref [||] in 
for i =0 to Array.length arr -1 do 
if( f (snd arr.(i)) = true ) then 
result:= Array.append !result [|arr.(i)|] 
done;
!result ;;

2.80
type geo_objekt = Tocka | Premica | Krog | Trikotnik ;;
let gl : geo_objekt list= [Tocka; Tocka; Premica; Krog; Krog];;

let rec prestej (gl: geo_objekt list) (elm : geo_objekt) =
  match gl with 
  | [] -> 0
  | hd :: tl -> if hd = elm then begin  
                  match elm with 
                  | Tocka -> 1 + prestej tl elm
                  | Premica -> 1 + prestej tl elm
                  | Krog -> 1 + prestej tl elm 
                  | Trikotnik -> 1 + prestej tl elm
                end
                else prestej tl elm;;
prestej [Tocka; Tocka; Premica; Krog; Krog] Premica;;

2.81 hw

2.82
type oper = PLUS | MINUS ;;
type elm = LP | RP | Vr of int | Op of oper ;;
type formula = Nil | Form of elm * formula ;;

let rec izpis (f : formula) = 
  match f with 
  | Nil ->  print_newline ()
  | Form (elm, form) -> 
    let element = elm in 
    begin 
      match element with 
      | LP -> print_string "("
      | RP -> print_string ")"
      | Vr x -> print_int x
      | Op op -> 
        match op with 
        | PLUS -> print_string "+"
        | MINUS -> print_string "-"
    end ;f;
izpis form;;
 let s = Form ( Vr (1) , Form ( Op ( PLUS ) , Form ( Vr (5) ,
Form ( Op ( MINUS ) , Form ( Vr (3) , Nil ))) )) ;;
izpis s;;

2.83
type oper = PLUS | MINUS ;;
type elm = | Vr of int | Op of oper ;;
type formula = Nil | Form of elm * formula ;;
let  izracunaj (f : formula) = 
let rec iz acc f  =
    match f with 
    |Nil -> acc
    |Form (elm,form) -> 
match elm with 
|Vr x -> iz  x form
|Op op -> begin match op with 
|PLUS -> acc+ iz acc form
|MINUS ->acc- iz acc form  end
in iz 0 f ;;
let s = Form ( Vr (1) , Form ( Op ( PLUS ) , Form ( Vr (5) ,
Form ( Op ( MINUS ) , Form ( Vr (3) , Nil ))) )) ;;
izracunaj s;;

2.84

type tip = Kraljica|Kral |Fant |Punca ;;
type colour = Srce | Kara | Pik | Kriz ;;
type simplKart = Nil | Cards of ( tip * colour) list;;
let seznam : simplKart = (Cards ([(Punca,Srce);(Kral,Kriz);(Fant,Pik)]));;

2.85
type 'a element  = E of 'a |L of  'a element list ;;
let a = L [E 1; E 2 ; L[E 3; E 4]];;
let rec print (m: 'a element )  =match m with 
|E x -> print_int x; print_newline ()
|L list ->  List.iter print list ;;
print a ;;


**************************************************************************
ELEMENTI IMPERATIVNIH JEZIKOV 
**************************************************************************

3.1
let m = "banana je lepa" ;;
let x = String.length m ;;
let d = String.get m (x-1) ;;

let rec reverse( m: string ) = 
match m with 
|"" -> ""
|x ->let last = String.length m -1 in 
String.make 1 ( String.get m last ) ^ reverse (String.sub m  0 last);; 
reverse m;;

3.2


let is_subsequence sub main =
  let sub_length = String.length sub in
  let main_length = String.length main in
  let sub_index = ref 0 in
  let main_index = ref 0 in
  let result = ref true in
  
  while !sub_index < sub_length && !main_index < main_length do
    if sub.[!sub_index] = main.[!main_index] then
      sub_index := !sub_index + 1;
    main_index := !main_index + 1;
  done;
  
  !sub_index = sub_length
;;

let result1 = is_subsequence "MATI" "MATEMATIKA";;   (* true *)
let result2 = is_subsequence "SENO" "SOSEDNOST";;     (* true *)
let result3 = is_subsequence "SENO" "S E NO";;        (* true *)
let result4 = is_subsequence "MT" "MATI";;         (* true *)
let result5 = is_subsequence "MATI" "M";;             (* false *)




3.3 let podnizi str =
  let len = String.length str in
  for i = 0 to len do
    for j = i to len do
      let sub = String.sub str i (j - i) in
      print_string ("\"" ^ sub ^ "\"; ");
    done;
  done;
  print_newline ()
;;

podnizi "miza";;


3.4

3.5
let number_of_ones arr = 
let rows = Array.length arr in 
let counter = ref 0 in 
for i = 0 to rows -1 do 
    if (arr.(i) = 1 ) then counter := !counter +1  
    else ()
done;
!counter;;

number_of_ones [|1; 0 ; 1 ; 1|];;

3.6
let product arr1 arr2 = 
let final = Array.make 5 0 in 
for i =0 to 4 do 
final.(i) <- arr1.(i) * arr2.(i) 
done;
final;;
let a = [|1;2;3;2;1|];;
let b = [|2;3;1;2;3|];;
product a b ;;

3.7
type pixel ={r : int ; g : int ; b : int ; mutable x: int ; mutable y : int }
type screen = pixel array array 
let move  (screen : screen) =
let rows = Array.length screen in 
let cols = Array.length screen.(0) in 
for i =0 to rows - 1 do 
for j = 0  to cols -1 do 

screen.(i).(j).x <- screen.(i).(j).x + 1 ;
if (screen.(i).(j).x = cols -1 )
then 
screen.(i).(j).x <- 0
done;
done;
screen;;

3.8 ???


3.9
a)
type slika = { x: int ; y : int ; p: int array array
};;

let matches (s1 : slika ) (s2 : slika ) = 
let flag = ref true in 
let pair = ref ( -1, -1) in
for i = 0 to s1.x -s2.x do 
    for j = 0 to s1.y -s2.y do 
        for m = 0 to s2.x -1 do 
            for n = 0 to s2.y-1 do 
            if ( s1.p.(i+m).(j+n) <> s2.p.(m).(n)  ) then  
            flag := false 
        done;
    done;
    if !flag then pair :=(i,j)
    done;
done;
!pair;;

let slika = { x = 3 ; y = 3 ; p= [|[|1; 2; 3|];[|4; 5 ; 6|];[|7; 8; 9|]|] };;
let slika2 = { x = 2; y = 2 ; p= [|[|1; 2|];[|4; 5 |]|] };;
matches slika slika2 ;;

b) instead of making a reference where I store the pair i 
can make a reference of a an empty list 
and then just add the matched pair into the list 
and return the list of matched pairs at the end of the function

type slika = { x: int ; y : int ; p: int array array
};;

let matches (s1 : slika ) (s2 : slika ) = 
let list_holder = ref [] in 
let pair = ref ( -1, -1) in
for i = 0 to s1.x -s2.x do 
    for j = 0 to s1.y -s2.y do 
        for m = 0 to s2.x-1 do 
            for n = 0 to s2.y-1 do 
            if ( s1.p.(i+m).(j+n) <> s2.p.(m).(n)  ) then  
            begin 
		pair := (i+m,j+n);
		list_holder := !pair :: !list_holder
	    end 

        done;
    done; 
  done;
done;
List.rev !list_holder;;

let slika = { x = 3 ; y = 3 ; p= [|[|1; 2; 3|];[|4; 5 ; 6|];[|7; 8; 9|]|] };;
let slika2 = { x = 2; y = 2 ; p= [|[|1; 2|];[|4; 5 |]|] };;
matches slika slika2 ;;

c)
i would introduce a new variable let tolerance = 1 in for example
and then in the if statement instead of this operator <>
i would subtract the values of the matrices and compare with that value 
if they are bigger then a match is not found 
else a match is found 

3.10
type slika = int array array 
let xor (s1: slika)  (s2 : slika ) =
let rows = Array.length s1 in 
let cols = Array.length s1.(0) in 
let (s3 : slika )= Array.make_matrix rows cols 0 in
for i =0 to rows -1 do 
for j =0 to cols -1 do 

if (s1.(i).(j) == s2.(i).(j)) 
then 
s3.(i).(j) <- 0
else 
s3.(i).(j) <- 1
done;
done;
s3;;

3.11

type text = char array array
let search (s1:text ) str =
 let rows = Array.length s1 in
  let cols = Array.length s1.(0) in
  let result = ref [] in
  let len = String.length str in 
  for i =0 to rows - 1 do 
  for j =0 to cols -1 do
  if j + len <= cols then 
  begin
  let substring = String.init  len (fun k -> text.(i).(k+j)) in
  if substring = str then
  result := (i,j) :: !result 
  end
  done;
  done;
  List.rev !result;;
  
  let text = [|
  [|'a'; 'b'; 'c'; 'd'|];
  [|'e'; 'f'; 'g'; 'h'|];
  [|'i'; 'j'; 'k'; 'l'|];
|];;

let pattern = "ab";;

search text pattern;;

PRASAJ GO NA PROFESOROT  ! ZA OVIE DVE 


3.12

type tekst = char array array ;;
let vsearch (tekst : tekst ) str = 
let rows = Array.length tekst in 
let cols = Array.length tekst.(0) in 
let len = String.length str in 
let result = ref [] in 

for i = 0 to rows -1 do 
for j = 0 to cols -1 do 
if i + len <= rows then 
begin 
let substring = String.init len (fun k -> tekst.(i+k).(j)) in
if str = substring then 
result := (i,j) :: !result
end 
done;
done;
!result;;

let text = [|
  [|'a'; 'b'; 'c'; 'd'|];
  [|'e'; 'f'; 'g'; 'h'|];
  [|'i'; 'j'; 'k'; 'l'|];
|];;

let pattern = "cgk";;

vsearch text pattern;;

3.13
A) BRUTE SOLUTION WHICH RUNS IN O(N^2) NOT SO EFFICIENT 
type slika = int array array

let zasukaj90 (img: slika) : slika =
  let rows = Array.length img in
  let cols = Array.length img.(0) in
  let rotated = Array.make_matrix cols rows 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      rotated.(j).(rows - 1 - i) <- img.(i).(j)
    done;
  done;
  rotated;;
  let image = [|
  [|1; 2; 3; 4|];
  [|5; 6; 7; 8|];
  [|9; 10; 11; 12|];
  [|13; 14; 15; 16|];
|];;

zasukaj90 image;;

OPTIMAL SOLUTION WHICH RUNS IN O(N) OR O(1) IN CONSTANT TIME
but this sollution works correctly for nxn matrix  
type slika = int array array

let rotate img =
  let l = ref 0 in
  let r = ref (Array.length img.(0) - 1) in
  while !l < !r do
    for i = 0 to !r - !l do
      let top = !l in
      let bottom = !r in
      (* Save the top-left value *)
      let topLeft = img.(top).(!l) in

      (* Move the bottom-left into the top-left *)
      img.(top).(!l) <- img.(bottom).(!l);

      (* Move the bottom-right into the bottom-left *)
      img.(bottom).(!l) <- img.(bottom).(!r);

      (* Move the top-right into the bottom-right *)
      img.(bottom).(!r) <- img.(top).(!r);

      (* Move the top-left into the top-right *)
      img.(top).(!r) <- topLeft
    done;

    (* Increment the left index and decrement the right index *)
    incr l;
    decr r
  done

B)type slika = int array array 
let rotate  (img : slika ) = 
let rows = Array.length img in
  let cols = Array.length img.(0) in
for i =0 to rows -1 do 
for j =0 to cols -1  do 
img.(j).(rows -1 -i) <- img.(i).(j)

done;
done;
img 

let rotate90xn img n = 
let rotated = ref img in 
for x = 1 to n do 
rotated := rotate !rotated 
done;
!rotated;;

3.14 type pika = int * int 
type slika = pika array array

let pika (x, y) =
  if x = 0 && y = 0 then
    false
  else
    true

let poisci slika pika =
  let width = Array.length slika in
  let height = Array.length slika.(0) in
  let radius = 5 in
  let center_not_found = (-1, -1) in

  let center = ref center_not_found in
  let found = ref false in

  (* Search for the circle center *)
  for x = radius to width - radius - 1 do
    for y = radius to height - radius - 1 do
      let circle_valid = ref true in
      for dx = -radius to radius do
        for dy = -radius to radius do
          let distance_squared = dx * dx + dy * dy in
          if distance_squared <= radius * radius then
            if not (pika slika.(x + dx).(y + dy)) then
              circle_valid := false
        done;
      done;

      if !circle_valid then begin
        center := (x, y);
        found := true;
      end
    done;
  done;

  !center
;;

let create_test_slika () =
  let width = 20 in
  let height = 20 in
  let slika = Array.make_matrix width height (0, 0) in
  (* Assume the circle is at (50, 50) with a radius of 5 *)
  for x = 2 to 13 do
    for y = 2 to 13 do
      slika.(x).(y) <- (x, y)
    done;
  done;
  slika

let test_slika = create_test_slika ()
let center = poisci test_slika pika
;;


3.15
type tekst = char array array 
let replace_v (tekst : tekst) str1 str2 =
let rows = Array.length tekst in
let cols = Array.length tekst.(0) in 
let len = String.length str1 in 

for i =0 to rows -1 do
for j =0 to cols -1 do 
if i + len <= rows then
let string1 = String.init len (fun k -> tekst.(i+k).(j)) in
if (string1 = str1) then 
for k =0 to len -1 do 
tekst.(i+k).(j) <- String.get str2 k 
done

done;
done;
tekst ;;
let text : tekst = [|
  [|'a'; 'b'; 'c'; 'd'|];
  [|'e'; 'f'; 'g'; 'h'|];
  [|'i'; 'j'; 'k'; 'l'|];
|];;

let search_string = "ei";;
let replacement_string = "XX";;

replace_v text search_string replacement_string;;

3.16

type ('k, 'v) pair_array = ('k * 'v) array

let equal (x: 'k) (y: 'k) =
  x = y

let merge (v1: 'v) (v2: 'v) =
  v1 + v2

let stick (arr1: ('k, 'v) pair_array) (arr2: ('k, 'v) pair_array) =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let result = ref [||] in
  let i = ref 0 in  (* Index for arr1 *)
  let k = ref 0 in  (* Index for arr2 *)

  while !i < len1 && !k < len2 do
    let key1 = fst arr1.(!i) in
    let key2 = fst arr2.(!k) in

    if equal key1 key2 then
      let value1 = snd arr1.(!i) in
      let value2 = snd arr2.(!k) in
      let merged_value = merge value1 value2 in
      result := Array.append !result [|(key1, merged_value)|];
      i := !i + 1;
      k := !k + 1;
    else if key1 < key2 then begin
      result := Array.append !result [|arr1.(!i)|];
      i := !i + 1
      end
    else
    begin
      result := Array.append !result [|arr2.(!k)|];
      k := !k + 1
      end
  done;

  (* Append remaining elements from arr1 and arr2 if any *)
  while !i < len1 do
    result := Array.append !result [|arr1.(!i)|];
    i := !i + 1;
  done;

  while !k < len2 do
    result := Array.append !result [|arr2.(!k)|];
    k := !k + 1;
  done;

  !result

let arr1: (int, int) pair_array = [|(1, 10); (5, 20); (6, 30)|]
let arr2: (int, int) pair_array = [|(1, 5); (4, 15); (6, 25)|]
let merged_array = stick arr1 arr2


EXAM TASK WITH BLUE CIRCLE 
type pixel = Blue | Black | Red
type screen = pixel array array
let find_circle (screen: screen) =
  let rows = Array.length screen in
  let cols = Array.length screen.(0) in
  let result = ref ((0, 0), 0) in
  let maxx = ref (cols - 1) in
  let minx = ref 0 in
  let maxy = ref (rows - 1) in
  let miny = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if screen.(i).(j) = Blue then (
        minx := min !minx j;
        maxx := max !maxx j;
        miny := min !miny i;
        maxy := max !maxy i;
      )
    done;
  done;
  result := ((((!minx + !maxx) / 2), ((!miny + !maxy) / 2)), (!maxx - !minx));
  !result;;

3.17

let mix (arr : 'a array ) (code: ( int * int) array) = 
let len = Array.length arr in 
let c_len = Array.length code in 
for i =0 to len -1 do 
for j =0 to c_len - 1 do 
let first = ref (fst code.(j)) in  (* the first index *)
let second = ref (snd code.(j)) in (* the second index *)
arr.(!first) <- arr.(!second) ;

done;
done;
arr;;
let arr : int array = [|1; 2; 3; 4; 5|];;
let code : (int * int) array = [|(1, 3); (2, 4)|];;

mix arr code;;

3.18 
class ['a] array (ini:'a) =
object 
val mutable arr = Array.make 10 ini
method  size : int = Array.length arr 
method set pos elm =arr.(pos) <- elm
method get pos =arr.(pos)
end;;
class ['a] arrayM (ini: 'a list) =
  object (self)
    inherit ['a list]array ini  as super (* Initial size 100, you can adjust as needed *)

method get (x:int) = super#get x 
method set (m : int) n = super#set m n 
method del index elem =
if index < 0 || index >= Array.length arr then
        invalid_arg "Index out of bounds"
else begin 
let current_list = arr.(index) in
let updated_list = List.filter (fun x -> x <> elem) current_list in
      arr.(index) <- updated_list
      end

  end

3.19
maybe the operator should be && AND since I dont know the slovene traslation
let podniz matrix arr =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let len = Array.length arr in
  let flag = ref false in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      for k = 0 to len - 1 do
        if i + len <= rows && j + len <= cols then
          if matrix.(i + k).(j + k) = arr.(k) || matrix.(i + k).(cols - k - i - 1) = arr.(k) then
            flag := true
          else
            flag := false
      done
    done
  done;
  !flag
;;

let matrix = [|
  [|1; 2; 3; 4|];
  [|5; 6; 7; 8|];
  [|9; 2; 3; 4|];
  [|5; 6; 7; 8|]
|];;

let arr1 = [|2; 3; 6; 3|];;
let arr2 = [|4; 7; 2; 5|];;

podniz matrix arr1;;
podniz matrix arr2;;

3.20 hint napraj lista i posle vrati ja reversed pa vo array napraja zs inace ne mojt !

let encode arr =
  let len = Array.length arr in
  let counter = ref 1 in
  let arr2 = ref [] in
  for i = 0 to len - 1 do
    if i < len - 1 && arr.(i) = arr.(i + 1) then
      incr counter
    else begin
      arr2 := (arr.(i), !counter) :: !arr2;
      counter := 1
    end
  done;
  !arr2 |> List.rev |> Array.of_list
;;

let result = encode [|1; 1; 3; 4; 4; 5|];;

3.21 
class ['a] matrix  (ini:'a) (m_init : int ) (n_init : int ) =
object 
val mutable matrix  = Array.make_matrix  m_init n_init ini
val mutable m = m_init 
val mutable n = n_init 

method set (pos1,pos2) elm =
matrix.(pos1).(pos2) <- elm

method get (pos1,pos2 )=
matrix.(pos1).(pos2)
end;;

class int_matrix (ini : int) m1 n1 =
object(self)
inherit [int] matrix ini  m1 n1 as super 

method equals  (k1,k2) (v1,v2) =
super#get (k1,k2)= super#get(v1,v2)

end;; 

3.22 potential!
type text = string list array

let find_replace (txt: text) (target: string) (replacement: string) : text =
  let updated_txt = Array.map (fun line ->
    List.map (fun word -> if word = target then replacement else word) line
  ) txt in
  updated_txt;;
let my_text : text = [|
  ["Hello"; "world"; "world"];
  ["This"; "is"; "a"; "sample"];
  ["text"; "with"; "world"]
|];;

find_replace my_text "a" "planet"

 

3.23


type 'v ppolje = (string * 'v) array

let stik v1 v2 =

let v=  Array.append v1 v2 in
let len = Array.length v in 

let temp = ref (fst v.(0), snd v.(0)) in

for i = 0 to len -1 do
for j = i+1 to len-2 do
if  fst v.(j) > fst v.(i) then 
begin
temp:= v.(i);
v.(i) <-  v.(j);
v.(j) <- !temp;
end
done;
done;
v;;

stik [|("ab" ,10) ,("de" ,9) |] [| ("bc" ,8) ,("cd",12) |];;

3.24
type student = string * int * int * string list

let izpis ((ip, l, o, h) : student) =
  let l =
    match l with
    | 1 -> "prvi"
    | 2 -> "drugi"
    | 3 -> "tretji"
  in
  let o =
    match o with
    | 6 -> "zadosno"
    | 7 -> "dobro"
    | 8 | 9 -> "prav dobro"
    | 10 -> "odlicno"
  in
  let hobbies = String.concat "," h in
  Printf.printf
    "Student %s obiskuje %s letnik. Njegova povecna ocena je %s. Hobija studenta so: %s.\n"
    ip l o hobbies;;
izpis (" Tone Novak ",1 ,8 ,[" kolesarjenje "]) ;;

3.25
type 'a atree = { mutable key: 'a ;
mutable trees : 'a atree list };;

let rec tree_filter (tree : 'a atree)  (f: ( 'a -> bool )) = 
if f tree.key then
tree.key :: List.concat(List.map (fun t -> tree_filter t f) tree.trees)
else List.concat(List.map (fun t -> tree_filter t f) tree.trees);;

let tree=   { key = 10;
    trees =
      [
        { key = 5; trees = [] };
        { key = 12; trees = [] };
        { key = 8; trees = [] }
      ]
  };;
tree_filter tree (fun x -> x mod 2 = 0);;

3.26module Stack = struct
  type 'a t = 'a list

   let isempty s =
   if !s =[] then true 
   else false 
  let create () = ref []

  let push x s = s := x :: !s

  let pop s =
    match !s with
    | [] -> failwith "empty stack"
    | x::l -> s := l; x

  let rec iter f s =
    match !s with
    | [] -> ()
    | x::l -> (f x); iter f (ref l)
end

let is_valid_parentheses_sequence input =
  let stack = Stack.create () in
  let is_valid = ref true in
  let check_parentheses char =
    match char with
    | '(' -> Stack.push char stack
    | ')' ->
        
        if Stack.isempty stack then is_valid:=false 
         else begin
            let top = Stack.pop stack in
            if top <> '(' then
              is_valid := false
           
        end
    | _ -> ()
  in
  String.iter check_parentheses input;
  !is_valid
;;

is_valid_parentheses_sequence "()";;
is_valid_parentheses_sequence "() ()";;
is_valid_parentheses_sequence "(() ) ()";;
is_valid_parentheses_sequence " () )(";;
is_valid_parentheses_sequence  "() () (() ))";;



3.27
class ['a] stack =
object 
    val mutable lst = ([] : 'a list)
    
    method size : 'a = List.length lst
    
    method push (a : 'a)  = 
    lst <- a :: lst
    
    method pop  =
    match lst with 
    |[] -> failwith "empty list"
    |e:: rest -> lst <- rest ; e 
    
end;;

class calculator = 
object(self) 

inherit ['a] stack as super 

method sum =
super#push (super#pop + super#pop) 

method sub =
super#push (super#pop - super#pop) 

method div =
super#push (super#pop / super#pop)

super#push (super#pop * super#pop) 
end;;

OBJECTS AND CLASSES 


EXMAPLE :

class int_stack =
object
val mutable l = ([] : int list)
method push x = 
l <- x:: l 
method pop = match l with 
|[] -> failwith "Empty" 
|a::a'-> l <-a' ; a
method clear = l <-[]
method length = List.length l
method see =l;
end;;
let is = new int_stack;;
is#push 1;;
is#push 2;;
is#push 3;;
is#push 4;;
is#see;;
is#pop;;
is#length;;


4.1
class integers (initX: int) (initY : int)=
object (self1)
val mutable x = initX
val mutable y = initY 
method getX = x 
method getY = y 

method setX x1= x<- x1 
method setY y1 = y<- y1 

method addition = x+ y 
method substraction = x- y 
method division  = x / y 
method multiplication = x * y 
end;;

class positive_integers (x_pos : int) (y_pos : int) =
object(self2)
inherit integers  (abs x_pos) (abs y_pos) as super (*ne od integers *)

method setX x_pos = 
let new_x = if x_pos < 0 then  (abs x_pos)* 100 
else x_pos in 
super#setX new_x

end;;
let m = new positive_integers (-4) 3;;
m#setX (-4);;
m#multiplication;;
m#addition;;

4.2
class niza =
object(self)
val mutable  arr = ([|1; 2; 3; 4|]: int array)
method get = arr
method set arr1 =
arr<-arr1
method sort arr1= 
let combined_array = Array.append arr arr1 in
Array.sort compare  (combined_array);
self#set combined_array

end;;
let m = new niza ;;
m#get;;

m#sort [|3;6;8;1|];;
m#get;;

4.3
class matrix x y =
object(self)
val mutable matrix : int array array = Array.make_matrix x y 0 
method get i j=
matrix.(i).(j)

method set i j value =
matrix.(i).(j) <- value 

method transpose =
let transposed = Array.make_matrix y x 0 in
for i =0 to x -1 do
for j =0 to y-1 do
transposed.(j).(i) <- matrix.(i).(j)

done;
done;
matrix<- transposed;


  method inverted =
    let rows = Array.length matrix in 
    let cols = Array.length matrix.(0) in
    let temp = ref 0 in 
    for i = 0 to (rows / 2 )-1 do
      for j = 0 to (cols -1) do
        temp := matrix.(i).(j);
        matrix.(i).(j) <- matrix.(rows - 1 - i).(cols - 1 - j);
        matrix.(rows - 1 - i).(cols - 1 - j) <- !temp
      done
    done;
    matrix

method main() =
let matricA = new matrix 5 6 in
self#set 0 0 7 ;
self#inverted ;
end;;
let myMatrix = new matrix 4 4;;
myMatrix# set 0 0 1;;
myMatrix# set 0 1 2;;
myMatrix# set 0 2 3;;
myMatrix# set 0 3 4;;

myMatrix# set 1 0 5;;
myMatrix# set 1 1 6;;
myMatrix# set 1 2 7;;
myMatrix# set 1 3 8;;

myMatrix# set 2 0 9;;
myMatrix# set 2 1 10;;
myMatrix# set 2 2 11;;
myMatrix# set 2 3 12;;

myMatrix# set 3 0 13;;
myMatrix# set 3 1 14;;
myMatrix# set 3 2 15;;
myMatrix# set 3 3 16;;

myMatrix#inverted;;

4.5
class stack =
object 
val mutable arr = ([||] : int array)
method push x =
let l = x :: (Array.to_list arr ) in 
arr <- (Array.of_list l )

method pop =
let l=(Array.to_list arr ) in 
match l with 
|[] -> failwith "empty"
|hd:: tl -> arr<-(Array.of_list tl) ; hd 

method clear = arr <- [||]
method length = Array.length arr 
method see = arr 
end;;

let s= new stack ;;
s# push 1;;
s# push 12;;
s# push 13;;
s# push 41;;
s# pop;;

class queue =
object 
inherit stack as super 

method pop =
let l=(Array.to_list arr ) in 
let m = (List.rev l) in
match m with 
|[] -> failwith "empty"
|hd:: tl -> arr<-(Array.of_list tl) ; hd 
end;;

let q = new queue ;;
q# push 1 ;;
q# push 4 ;;
q# push 7 ;;
q# push 6 ;;
q# push 8 ;;
q# pop;;



4.6 
class point (x_init : int) (y_init : int)=
object(point)
val mutable x = x_init
val mutable y = y_init 

method getX= x
method getY = y 

method move dx dy =
x <- x + dx ;
y <- y + dy

end;;

class line (x1_line: int) (y1_line) x2_line y2_line =
object(line)
inherit point x1_line y1_line as parent1 
inherit point x2_line y2_line as parent2


method move_to dx dy =
parent1#move dx dy ;
parent2#move dx dy 

end;;

class circle x_init y_init (radius_init : int)=
  object(self)
    inherit point x_init y_init as super

    val mutable radius = radius_init
    method get_radius = radius
    method set_radius r = radius <- r
    method move dx dy =
      super#move dx dy

    
  end

4.7

class basicRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
  object (basic)
    val mutable name = nameR
    val mutable x = start_x
    val mutable y = start_y
    val mutable dx = offsetdx
    val mutable dy = offsetdy

    initializer
      Printf.printf ">> Hi i am a Basic Robot and my name is: %s\n" (basic#to_string())

    method getName = name
    method setName new_name = name <- new_name

    method getX = x
    method getY = y
    method getdx = dx
    method getdy = dy

    method moveto dx dy =
      let new_x = x + dx in
      let new_y = y + dy in
      x <- if new_x > 100 || new_x < 1 then x - dx else new_x;
      y <- if new_y > 100 || new_y < 1 then y - dy else new_y

    method move = basic#moveto dx dy

    method to_string () =
      "[" ^ name ^ "]" ^ " STARTING POSITION:(" ^ string_of_int start_x ^ "," ^ string_of_int start_y ^ ")" ^
      "\n" ^ "ENDING POSITION:(" ^ string_of_int (basic#getX) ^ "," ^ string_of_int (basic#getY) ^ ")"
  end


let robot1 = new basicRobot "Bob" (0,4) 10 7;;
robot1#setName "Greg";;
robot1#getX;;
robot1#move ;;
print_endline (robot1#to_string ());;
robot1#moveto 11 11;;
print_endline (robot1#to_string ());;
class sinkingRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
object(sinking)

inherit basicRobot nameR  (start_x, start_y )offsetdx offsetdy as super1 

 initializer
      Printf.printf ">> Hi i am a Sinking Robot and my name is: %s\n" (sinking#to_string())
    
end;;
let robot2 = new sinkingRobot "Julie" (1,1) 4 10;;
robot2#move;;


class cikcakRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
object(cikcak) 

inherit basicRobot nameR  (start_x, start_y )offsetdx offsetdy as super2 

 initializer
      Printf.printf ">> Hi i am a CikCak Robot and my name is: %s\n" (cikcak#to_string())
val cikcak = 2 

method moveto dx dy = 
let levo= (super2#getX -cikcak,  super#getY) in 
let desno = (super2#getX + cikcak,  super#getY) in
      


end;;





**********************************

class basicRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
  object (basic)
    val mutable name = nameR
    val mutable x = start_x
    val mutable y = start_y
    val mutable dx = offsetdx
    val mutable dy = offsetdy

    initializer
      Printf.printf ">> Hi i am a Basic Robot and my name is: %s\n" (basic#to_string())

    method getName = name
    method setName new_name = name <- new_name

    method getX = x
    method getY = y
    method getdx = dx
    method getdy = dy

    method moveto dx dy =
      let new_x = x + dx in
      let new_y = y + dy in
      x <- if new_x > 100 || new_x < 1 then x - dx else new_x;
      y <- if new_y > 100 || new_y < 1 then y - dy else new_y

    method move = basic#moveto dx dy

    method to_string () =
      "[" ^ name ^ "]" ^ " STARTING POSITION:(" ^ string_of_int start_x ^ "," ^ string_of_int start_y ^ ")" ^
      "\n" ^ "ENDING POSITION:(" ^ string_of_int (basic#getX) ^ "," ^ string_of_int (basic#getY) ^ ")"
  end


let robot1 = new basicRobot "Bob" (0,4) 10 7;;
robot1#setName "Greg";;
robot1#getX;;
robot1#move ;;
print_endline (robot1#to_string ());;
robot1#moveto 11 11;;
print_endline (robot1#to_string ());;
class sinkingRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
object(sinking)

inherit basicRobot nameR  (start_x, start_y )offsetdx offsetdy as super1 

 initializer
      Printf.printf ">> Hi i am a Sinking Robot and my name is: %s\n" (sinking#to_string())
 


end;;
let robot2 = new sinkingRobot "Julie" (1,1) 4 10;;
robot2#move;;


class cikcakRobot (nameR: string) ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =
object(cikcak) 

inherit basicRobot nameR  (start_x, start_y )offsetdx offsetdy as super2 

 initializer
      Printf.printf ">> Hi i am a CikCak Robot and my name is: %s\n" (cikcak#to_string())
val mutable zigzag_direction = 1 
method move = 
 let next_x = super2#getX + 2 * zigzag_direction in
      let next_y = super2#getY + super2#getdy in
      if next_x > 100 || next_x < 1 then
        zigzag_direction <- -zigzag_direction;
      super2#moveto (2 * zigzag_direction) super2#getdy

end;;
let zigzag_robot = new cikcakRobot"Ziggy" (10, 5) 2 1;;

(* Print the initial position *)
print_endline (zigzag_robot#to_string ());

(* Move and print the new position *)
zigzag_robot#move;
print_endline (zigzag_robot#to_string ());

(* Move and print the new position *)
zigzag_robot#move;
print_endline (zigzag_robot#to_string ());

(* Move and print the new position *)
zigzag_robot#move;
print_endline (zigzag_robot#to_string ());
4.8 
(*
• name and surname,
• address,
• phone number,
• entered year,
• enrolled program,
• existing education and
• Average rating.
*)

class person (pName : string) (pSur: string) (add : string)(num : int)=
object(person)
val mutable name= pName 
val mutable surname = pSur 
val mutable number = num
val mutable address = add 
method getName = name 
method getSurname = surname 
method getNum = number 
method getAdd= address 
method setName new_name = name <- new_name 
method setSurname new_surname = surname <-new_surname

method setNum new_number= number <- new_number
method setAdress new_adress= address <- new_adress
 initializer
      number <- 0;
      address<-"Koper"
      
      
method to_string ()="{"^name^"}" ^"{"^surname^"}"^"{"^add^"}"^"{"^(string_of_int num)^"}"
end;;

class student pName pSur add num nYear (nProg) averageGrade=
object(student)
inherit person pName pSur add num as super1

val year =( nYear : int)
val mutable program = nProg 
val mutable averageGrade = 0.0
method getYear = year
method getProgram = program 
method getAverageGrade = averageGrade
method setProgram new_program = program <-new_program
method setAverageGrade new_grade = averageGrade <- new_grade 
initializer 
program <- "Computer Science"
method to_string ()= super1# to_string() ^ "["^(string_of_int year)^"]"^"["^program^"]"^"["^(string_of_float averageGrade)^"]"

end;;

class graduatedStudent  pName pSur add num nYear (nProg: string) averageGrade (sch: string)=
object(graduate)
inherit student pName pSur add num nYear (nProg: string) averageGrade as super2 
val mutable school= sch
method getSchool =school
method setSchool new_school= school<- new_school 
method to_string () = super2#to_string() ^ "*"^(school)^"*"
end;;

class asistent pName pSur add num nYear (nProg: string) (averageGrade: float) (sch: string) =
object(asistent)
inherit graduatedStudent pName pSur add num nYear nProg averageGrade sch  as super3 
method to_string () = super3# to_string() 
end;;
let person1 = new person "John" "Doe" "123 Main St" 12345;;
let student1 = new student "Alice" "Smith" "456 Elm St" 987 2022 "Computer Science" 3.8;;
let graduate1 = new graduatedStudent "Bob" "Johnson" "789 Oak St" 2468 2020 "Physics" 4.5 "University of XYZ";;
let asst1 = new asistent "Emily" "Davis" "321 Pine St" 135 2018 "Mathematics" 4.0 "University of ABC";;

print_endline (person1#getName);;        
print_endline (student1#getProgram);;   
print_endline (graduate1#getSchool);;            

graduate1#setSchool "University of PQR";;
asst1#setNum 246;;

print_endline (person1#to_string ());;    
print_endline (student1#to_string ());;   
print_endline (graduate1#to_string ());;  


4.9 ERROR! 
let temperatura (floor : int) (room_num : int) (matrix : float array array) =
let element = ref 0.0 in 
for i =0 to Array.length matrix -1 do 
for j =0 to Array.length matrix.(0) - 1 do 

if (i = floor && room_num = j ) then 
element := matrix.(i).(j)
done;
done;
!element ;;

class house (n: int )( m : int)=
object(house)
val mutable floors = n 
val mutable rooms = m 
val mutable house = Array.make_matrix n m 0.0

method get_floor = floors 
method get_rooms = rooms 
method get_house = house 

method set_floors new_floors = floors <- new_floors 
method set_rooms new_rooms = rooms <- new_rooms 


method odcitajTemperaturo = 
let temp = temperatura floors rooms house in 
house.(floors).(rooms) <- temp ;
temp;

method prosekTemperatura=
let avge = ref 0.0 in 
for i =0 to Array.length house -1 do 
for j =0 to Array.length house.(0) -1 do 
avge:= house.(i).(j) +. !avge 
done;
done;
let result = !avge /. ( float_of_int m *. float_of_int n) in 
result
end;;

class floor (floor_num: int ) (room_num: int) ( temp: float) =
object(floor)
inherit house floor_num  room_num  as super 

method odcitajTemperaturo = 
let temp = temperatura floor#get_floor floor#get_rooms floor#get_house in 
floor#get_house.(floor#get_floor).(floor#get_rooms) <- temp ;
temp;
end;;

class room (floor_num: int) (room_num:int)( temp: float) =
object(room)
inherit floor (floor_num: int )(room_num:int) ( temp: float) as super

(*method for setting the room temperature *)
method odcitajTemperaturo  =
let temp = temperatura floor_num room_num room#get_house  in
room#get_house.(room#get_floor ).(room#get_rooms) <- temp ;
temp;

end;;

let my_house = new house 4 3;;
my_house#get_house.(2).(3) <- 25.5;;
let temp1 = my_house#odcitajTemperaturo;;



4.10
class virtual geo =
object
  method virtual predstavi : string 
  method virtual draw : unit -> unit
end;;

class point x1 y1 =
object(point)
inherit geo 
val mutable x= x1 
val mutable y = y1 

method get_x = x 
method get_y = y 
method move (a,b) = 
x<-a ;
y<-b

method draw ()= print_endline point#predstavi  
 

method predstavi =
let s ="Tocka "^"(" ^ (string_of_int (point#get_x) ) ^ "," ^ (string_of_int (point#get_y)) ^ ")"  in
 s   

end;;


class line x1 y1 x2 y2 =
object(line)
inherit geo
inherit point x1 x2 as super1 
inherit point x2 y2 as super2 
method predstavi =
let s ="Line "^"(" ^ (string_of_int (super1#get_x) ) ^ "," ^ (string_of_int (super1#get_y)) ^ ")"^"(" ^ (string_of_int (super2#get_x) ) ^ "," ^ (string_of_int (super2#get_y)) ^ ")"  in
 s   
method draw ()= print_endline line#predstavi 

end;;
class circle x1 x2 r =
object(circle) 
inherit geo 
inherit point x1 x2 as super1 
val mutable radius = r

method get_radius = r 

method predstavi =
let s ="Circle "^"(" ^ (string_of_int (super1#get_x) ) ^ "," ^ (string_of_int (super1#get_y)) ^ ")"^"the radius is :"^(string_of_int circle#get_radius )  in
 s   

method draw ()= print_endline circle#predstavi 
end;;

let point = new point 3 3;;
point#draw();;

let line = new line 2 3 4 4 ;;
line#predstavi ;;
line#draw();;

let circle = new circle 5 5 5;;
circle #draw();;

4.11
class virtual ['a] tip_list = 
object(self) 

method virtual length : 'a list -> int 
method virtual cons: 'a -> 'a list -> 'a list 
method virtual head : 'a list -> 'a 
method virtual append: 'a list -> 'a list -> 'a list 
method virtual member : 'a -> 'a list -> bool 
method virtual search : ('a -> bool) ->  'a list -> 'a 
method virtual fillter : ('a -> bool) -> 'a list -> 'a list 
method virtual find_all : ('a -> bool) -> 'a list -> 'a list 


end;;

class lista ( l : 'a list)= 
object(lista)
inherit ['a] tip_list 

val mutable lst = ([] : 'a list)
method get_list = lst 
method length l = List.length l 
method cons x l = x :: l 
method head l = List.hd l 
method member x l = List.mem x l 
method search f l =List.find f l
method fillter f l = List.filter f l 
method find_all f l = List.find_all f l 

end;;

4.12 
class virtual ['a] array =
object(self)
  method virtual fix : int -> 'a -> unit 
  method virtual read :  int -> 'a
end;;
class realInt (size : int) =
object
  inherit [float] array
  val mutable arr = Array.make size 0.0
  val mutable s = size
  method get_size = s
  method set_size new_size = s <- new_size
  method fix  index elem =
    arr.(index)<-elem
  method read index = arr.(index)
end;;


4.13
class virtual ['a] tocka (x : int) (y : int) (color : 'a) =
object
  val x_coord = x
  val y_coord = y
  val point_color = color

  method get_x = x_coord
  method get_y = y_coord
  method get_color = point_color

  method virtual enakost : 'a tocka -> bool
end;;

class itocka x1 y1 c =
object (self)
  inherit [int] tocka x1 y1 c

  method enakost (other : int tocka) =
    self#get_x = other#get_x && self#get_y = other#get_y && self#get_color = other#get_color
end;;

class virtual ['a] slika =
object 
val mutable l : 'a tocka list =[]

method get_list = l
method add_pixel  (x : 'a tocka) = l <- x:: l

method remove_pixel (y: 'a tocka) =
List.filter ( fun m -> m#enakost y ) l

end;;

class islika =
object(slika)
inherit [int] slika

end;;
let slika = new islika ;;
slika#get_list;;

slika #add_pixel ( new itocka  2 2 20);;
slika #add_pixel ( new itocka 4 4 4);;
slika #add_pixel ( new itocka 7 17 77);;
slika #add_pixel ( new itocka 9 9 9);;
let lista = slika #get_list;;


4.14
class virtual robot ((start_x, start_y): int * int) (offsetdx: int) (offsetdy: int) =

object 

val mutable x = start_x
val mutable y = start_y
val mutable dx = offsetdx
val mutable dy = offsetdy

method getX = x
method getY = y
method getdx = dx
method getdy = dy

method setY new_y = y <- new_y
method setX new_x = x <- new_x
method virtual move : unit 
end;;

class robot1 ((a,b) : int * int) (move_x : int) (move_y : int) =
object(self) 
inherit robot (a,b) move_x move_y

method move =
let new_x = self#getX + self#getdx in 
let new_y = self#getY + self#getdy in 
if new_x > 100 || new_x < 1 then self#setX (self#getX - self#getdx) else self#setX new_x;
if new_y > 100 || new_y < 1 then self#setY (self#getY - self#getdy) else  self#setY new_y;
 
end;;

class robot2 ((c,d): int* int) (move_x: int) (move_y: int)=
object(self2)
inherit robot (c,d) move_x move_y 
method move =
  let random = if Random.bool () then 1 else -1 in
  let new_x = self2#getX + random * self2#getdx in
  self2#setX new_x


end;;

class robot3 ((e,f): int * int) (move_x : int) (move_y: int)= 
object(self3)
inherit robot1 (e,f) move_x move_y as super
method move =
let new_x = super#getX + super#getdx * 2 in 
let new_y = super#getY + super#getdy *2 in 
if new_x > 100 || new_x < 1 then super#setX (super#getX - super#getdx *2) else super#setX new_x;
if new_y > 100 || new_y < 1 then super#setY (super#getY - super#getdy *2 ) else  super#setY new_y;

end;;

4.15
class virtual robot (ime: string) =
object 
val mutable direction = 1 
val mutable name = ime 
method getName = name 
method setName new_name = name <- new_name 
method virtual move : unit 

end;;

class robotX (name_x_robot : string) ((start_x,start_y): int * int) (offset_x : int) (offset_y : int) =
  object(self)
    inherit robot name_x_robot
    val mutable x = start_x
    val mutable dx = offset_x

    method setX new_x = x <- new_x
    method getX = x

    method get_dx = dx
    method move = 

      let new_x = self#getX + self#get_dx in 
      if (new_x = 10 || new_x = -10) then
        direction <- -direction;
      self#setX (self#getX + direction * self#get_dx)
   
  end;;

class robotY (name_y_robot : string) ((start_x,start_y): int * int) ( offset_x : int) (offset_y : int) =
object(self)
inherit robot name_y_robot 
val x = start_x 
method getX = x 

val mutable y = start_y 
val mutable dy = offset_y
method getY = y
method get_dy = dy
method setY new_y = y <- new_y 

method move = 

let new_y = self#getY + self#get_dy in 
if (new_y = 10 || new_y = -10) then 
direction <- -direction ;
self#setY ( self#getY + direction * self#get_dy)

end;;
4.16

class  zbirka =
object 
val mutable lista = ([] : int list )


method dodaj_zacetek  x  = 
lista <- x :: lista 


method brisi_zacetek  =
match lista with 
|[] -> failwith "empty"
| hd :: tl  -> lista <- tl ; hd 


method  dodaj_konec x = 
 lista <- lista @ [x]

method brisi_konec =
let reversed_list = List.rev lista in
match reversed_list with 
|[] -> failwith "empty"
|hd::tl -> lista<- tl ; hd 


end;;
let zbirka = new zbirka ;;
zbirka#dodaj_konec 4;;
zbirka#brisi_zacetek;;
zbirka#dodaj_zacetek 10 ;;
zbirka#dodaj_konec 11;;
zbirka#brisi_konec;;

class queue = 
object(self) 
inherit zbirka as super 


method enqueue = 
super#dodaj_konec 
method dequeue =
super#brisi_zacetek

end;;

class stack =
object(self) 
inherit zbirka as parent 

method push =
parent#dodaj_konec 

method pop =
parent#brisi_konec 

end;;
let my_queue = new queue ;;
my_queue#enqueue 1 ;;
my_queue#enqueue 2 ;;
my_queue#dequeue ;;      (* This should remove the first element 1 from the list *)
my_queue#dequeue ;;      (* This should remove the second element 2 from the list *)      (* This should raise an exception since the list is empty *)
let my_stack = new stack ;;
my_stack#push 1 ;;
my_stack#push 2 ;;
my_stack#pop ;;          (* This should remove the last element 2 from the list *)
my_stack#pop ;;          (* This should remove the first element 1 from the list *)
my_stack#pop ;;          (* This should raise an exception since the list is empty *)

4.17



method filter value =
 let rec filter_elements acc components =
 match components with 
 |[] -> acc 
 |comp :: rest -> if comp#get_komponente = [] && comp#get_komponente >= value then filter_elements (comp:: acc) rest
                 else filter_elements acc (comp#get_komponente @ rest)
in filter_elements [] komponente


4.18
class naprava ( im : string ) ( tp : string ) (wg :int) ( ln :
naprava list ) =
 object
 val ime = im
 val tip = tp
 val weight = wg 
 val mutable komponente = ln
 
 
 method get_ime = ime 
 method get_tip = tip
 method get_wg = weight
 method get_komponente = komponente 
 
 method dodaj_komponente x=
  komponente <- x :: komponente 
 
 method listi = 
 let rec traverse acc components = 
 match components with 
 |[] ->acc
 |comp :: rest -> match comp#get_komponente with 
 |[] -> traverse (comp :: acc) rest 
 |nested -> traverse acc (nested @ rest) in traverse [] komponente
 
 end ;;
(* Create instances for each component *)
let comp1 = new naprava "Component 1" "Type A" 10 [];;
let comp2 = new naprava "Component 2" "Type B" 15 [];;
let comp3 = new naprava "Component 3" "Type C" 20 [];;

(* Build the tree structure *)
comp1#dodaj_komponente comp2;
comp1#dodaj_komponente comp3;



(* Print the leaf devices *)
List.iter (fun comp -> print_endline ("Leaf device: " ^ comp#get_ime))  comp1#listi;;
4.19
class virtual geo (x_init: int ) (y_init: int)=
object 
val mutable x = x_init 
method getX = x
method setX new_x = x<- new_x 

val mutable y = y_init 
method getY = y
method setY new_y = y<- new_y 


method virtual move : int * int ->  unit 
method virtual print : unit

end;;
(*class point *)
class point  (x1: int ) (y1: int)=
object(self)

inherit geo x1 y1 

method move (a, b) = self#setX a; self#setY b
method print  =
print_string "Geo: Tocka"
end;;

(*class circle *)
class circle (x_c : int) (y_c : int) (r_init : int) =
object(self)
inherit point x_c y_c as super 
val mutable r = r_init 
method getR = r
method setR new_radius = r <- new_radius 
method print = 
super#print;
print_string "Geo: Circle"
end;;

(*class square *)
class square (x1 : int) (y1: int) ( x2: int ) (y2: int) (x3: int) (y3: int) (x4: int) (y4: int) =
object 
inherit point x1 y1 as point1 
inherit point x2 y2 as point2 
inherit point x3 y3 as point3 
inherit point x4 y4 as point4 

method print = 
point1#print;
point2#print;
point3#print;
point4#print;
print_string
"Geo: Square"
end;;

4.20
class matrika  (m_init : int )(n_init : int)=
object(self)
val mutable matrix = Array.make_matrix m_init n_init  0.0 
val mutable m = m_init 
val mutable n = n_init 

method getM = m 
method getN = n
method getMatrix = matrix 

(*get : int -> int -> float*)
method get (x: int) (y: int) =
let result = ref 0.0 in 
for i = 0 to m -1 do 
for j = 0 to n-1 do 
    if(i = x && j = y ) then result := matrix.(i).(j)
done;
done;
!result

(* set : int -> int -> float -> unit*)
method set (a: int ) (b : int) (ele : float) =
for i =0 to m -1 do 
for j= 0 to n-1 do 
if(a = i && j = b) then 
matrix.(i).(j) <- ele
done;
done;

(*mul : matrika -> unit*)

method mul (matrix2 : matrika) =

if n<> matrix2#getM then failwith "matrices cannot be multiplied "
else 
let result_matrix = Array.make_matrix m matrix2#getN 0.0 in
for i =0 to m -1 do 
for j=0 to matrix2#getN-1 do 
let sum = ref 0.0 in 
for k =0 to n -1 do 
sum := !sum +. matrix.(i).(k) *. matrix2#get k j 
done;
result_matrix.(i).(j) <- !sum
done;
done;
matrix <- result_matrix ;
n <- matrix2#getN
end;;

class kmatrika (n1 : int) ( n2: int) =
object
inherit matrika n1 n2 
end;;

4.21
class virtual ['a] rpc = 
object 
val mutable stack= ([] : 'a list)

method get_stack = stack 
method virtual push : 'a -> unit 
method virtual pop : 'a 
method virtual add : unit
method virtual sub : unit
method virtual mul : unit
method virtual div : unit
end;;
class int_rpc =
object(self) 
inherit [int] rpc 

method push (x: int) =
stack <- x :: stack 

method pop =
match stack with 
|[] -> failwith "empty stack"
|hd :: tl -> stack <- tl ; hd 

method add =
let result = self#pop + self#pop in 
self#push result 

method sub =
let result = self#pop + self#pop in self#push result

method mul=
let result = self#pop * self#pop in 
self#push result
method div =
let result = self#pop / self#pop in 
self#push result
end;;

let calculator = new int_rpc ;;
calculator# push 5 ;;
calculator#push 3;;
calculator#add;;
calculator#pop

4.22
class queue  (size_queue : int) =
object(self)
val size : int = size_queue 
val mutable queue = Array.make size_queue 0 
val mutable front : int = 0
val mutable rear : int = 0 
val mutable count : int = 0 
method private is_empty =
if (count = 0) then true 
else false 

method private is_full =
if( count = size ) then true else false 

method enqueue (element : int) =
if self#is_full then
     failwith "Queue is full";
    queue.(rear) <- element;
    rear <- (rear + 1) mod size;
    count <- count + 1
method dequeue =
if self#is_empty then failwith " the stack is empty"
else
let element_to_remove = queue.(front) in 
front <-( front+1) mod size ;
count<- count-1 ;
element_to_remove 
initializer
        self#enqueue 0
end;;
let q = new queue 11;;
q#enqueue 1;;
q#enqueue 2;;
q#enqueue 3;;
q#dequeue;;
q#dequeue;;
q#dequeue;;


4.23
class ['a] queue  (size_q : int) =
object(self)
val mutable lst = ([] : 'a list)
method get_lst = lst

method push (lst2 : 'a list)=
lst <- lst @ lst2

method pop (k : int) =
      let rec remove n acc =
        if n = k then acc
        else
          match acc with
          | [] -> []
          | _ :: rest -> remove (n + 1) rest
      in
      lst <- remove 0 lst
end;;
class  fqueue (size_q : int) =
object(sel2)
inherit [float] queue size_q as parent 

end;;

4.24

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

4.25
class  stack = 
object
    val mutable seznam =([]: int list )
    method empty =
    []= seznam
    method pop =
    let r = List.hd seznam in 
    seznam <- List.tl seznam ; r
    
    method push elm  = 
    seznam <- elm :: seznam
    method reverse = seznam <- List.rev seznam
end;;
class queue =
object
val mutable q1 = new stack 
val mutable q2 = new stack 

method enqueue (x: int) =
q1#push x 

method dequeue =
if (q2#empty = true ) then 
begin 
if not q1#empty then
q1#reverse ;
q2 <- q1 ;
q2#pop ;
end 
else 
q2#pop
end;;

4.26

class ['a] stack =
object 
    val mutable lst = ([] : 'a list)
    
    method size : 'a = List.length lst
    
    method push (a : 'a)  = 
    lst <- a :: lst
    
    method pop  =
    match lst with 
    |[] -> failwith "empty list"
    |e:: rest -> lst <- rest ; e 
    
end;;

class calculator = 
object(self) 

inherit ['a] stack as super 

method sum =
super#push (super#pop + super#pop) 

method sub =
super#push (super#pop - super#pop) 

method div =
super#push (super#pop / super#pop)

super#push (super#pop * super#pop) 
end;;

4.27
type 'a btree = None | Some of 'a btree * 'a  * 'a btree
class ['a] tree =
object(self)
val mutable tree : 'a btree = None

method insert x =
let rec insert_aux x (tree : 'a btree) =
match tree with 
|None -> Some (None,x,None)
|Some (l,y,r) -> match l with 
|None -> Some (Some(None,x,None),y,r)
|Some _ -> insert_aux x l 
in
tree <- insert_aux x tree
end;;

4.28 type 'a element = {
mutable vrednost: 'a ; mutable naslednji:'a seznam}
and 'a seznam = Prazen | Element of 'a element;; 

let rec zdruzi (s1: 'a seznam) (s2 : 'b seznam)=
match s1,s2 with 
|Prazen,Prazen -> Prazen
|_,Prazen|Prazen,_ -> Prazen
|Element (a),Element (b) -> let new_list = {vrednost = (a.vrednost,b.vrednost) ;
naslednji = zdruzi a.naslednji b.naslednji} in Element(new_list);;

zdruzi (Element { vrednost = 1; naslednji = Prazen }) (Element { vrednost = "a"; naslednji = Prazen });;



4.29

let rec rapp f n =
if n =0 then  fun x-> x 
else fun x-> rapp f (n-1) (f x);;

4.30 

type 'a btree = |List of 'a |Drvo of 'a btree *'a*'a btree;;
let rec mimi(tree1 : 'a btree) (tree2 : 'a btree) =
match tree1,tree2 with 
|List a1,List a2 -> true 
|Drvo (l1,x1,d1),List x2 -> false
|List x1,Drvo (l2,x2,d2)-> false 
|Drvo (l1,x1,d1),Drvo (l2,x2,d2) -> mimi l1 l2 && mimi d1 d2;;

***************************************************************************
				MODULES
***************************************************************************
5.1
module Pairs = struct 
type par = int * int


let sestej (a : par ) (b : par ) : par= 
let first_a = (fst a) in 
let second_a = snd a in 
let first_b = fst b in 
let second_b = snd b in 
(first_a + first_b , second_a + second_b)

let odstej (a : par ) (b : par ) : par= 
let first_a = (fst a) in 
let second_a = snd a in 
let first_b = fst b in 
let second_b = snd b in 
(first_a - first_b , second_a - second_b)


let mnozi (a : par ) (b : par ) : par= 
let first_a = (fst a) in 
let second_a = snd a in 
let first_b = fst b in 
let second_b = snd b in 
(first_a * first_b , second_a * second_b)

end;;

Pairs.sestej (2,3) (1,2);;
Pairs.odstej (2,3) (1,2) ;;
Pairs.mnozi (2,3) (1,2) ;;

5.2
module Queue = struct 
type 'a t = ('a list * 'a list ) ref 
exception Empty 
exception Full
let create () = ref([],[])

let enqueue x queue =
let front,back = !queue in
let max_length =5 in 
if List.length front >= max_length then raise Full 
else 
(x :: front , back)

let rec dequeue queue = 
match !queue with 
|([],[]) -> raise Empty 
|(front,x::back) -> queue:= (front,back);x 
|(front,[]) -> queue:= ([],List.rev front);
                dequeue queue 

end;;

5.3
module type ARANZMA = sig
  type t

  val create : unit -> t
  val correct_destination : t -> string -> unit
  val correct_tip : t -> string -> unit
  val correct_days : t -> int -> unit
  val correct_price : t -> int -> unit
  val view_arrangement : t -> unit 
end;;

module Aranzma : ARANZMA = struct
  type t = (string * string * int * int) ref

  let create () = ref ("", "", 0, 0)

  let correct_destination arrangement new_destination =
    let destination, tip, days, price = !arrangement in
    if destination <> new_destination then
      arrangement := (new_destination, tip, days, price)

  let correct_tip arrangement new_tip =
    let destination, tip, days, price = !arrangement in
    if tip <> new_tip then
      arrangement := (destination, new_tip, days, price)

  let correct_days arrangement new_days =
    let destination, tip, days, price = !arrangement in
    if days <> new_days then
      arrangement := (destination, tip, new_days, price)

  let correct_price arrangement new_price =
    let destination, tip, days, price = !arrangement in
    if price <> new_price then
      arrangement := (destination, tip, days, new_price)
      
  let view_arrangement arrangement =
    let destination, tip, days, price = !arrangement in
    Printf.printf "Destination: %s\nType: %s\nDuration: %d\nPrice: %d\n"
      destination tip days price

end;;

module type AGENT = sig
  val create_arrangement : unit -> Aranzma.t
  val check : Aranzma.t -> unit
  val rename_destination : Aranzma.t -> string -> unit
  val set_accommodation_type : Aranzma.t -> string -> unit
  val update_duration : Aranzma.t -> int -> unit
  val update_price : Aranzma.t -> int -> unit
end;;

module Agent : AGENT = struct
  let create_arrangement () = Aranzma.create ()

  
  let rename_destination arrangement new_destination =
    Aranzma.correct_destination arrangement new_destination

  let set_accommodation_type arrangement new_tip =
    Aranzma.correct_tip arrangement new_tip

  let update_duration arrangement new_days =
    Aranzma.correct_days arrangement new_days

  let update_price arrangement new_price =
    Aranzma.correct_price arrangement new_price

  let check arrangement = 
  Aranzma.view_arrangement arrangement
end;;

module type STRANKA = sig 
val view : Aranzma.t -> unit 
end;;
module Stranka :STRANKA =struct 
let view  arrangement = 
  Aranzma.view_arrangement arrangement
end;;


5.6  **produces some error with the insert function ! 


module type SEQUENCE = sig 
type 'a t 
val create : ('a -> 'a -> int) ->'a t 
val add :'a t -> 'a -> unit 
val remove : 'a t -> 'a -> unit 
val min : 'a t -> 'a 
val max : 'a t -> 'a 
end;;
module Sequence : SEQUENCE = struct 

type 'a t = {
compare_function : 'a -> 'a -> int;
mutable elements : 'a list; }
exception Empty
let create funct = { compare_function = funct; elements = [] }
let add sequence element =
    sequence.elements <- element :: sequence.elements;
    sequence.elements <- List.sort (sequence.compare_function) sequence.elements
let remove sequence element = 
sequence.elements <- List.filter (fun el -> sequence.compare_function el  element <> 0 ) sequence.elements 
let min sequence =
match sequence.elements with 
|[] -> raise Empty
|hd :: tl -> List.fold_left (fun el1 el2 ->if sequence.compare_function el1 el2 = -1 then el1 else el2) hd sequence.elements 
let max sequence =
match sequence.elements with 
|[] -> raise Empty 
|hd :: tl -> List.fold_left (fun el1 el2 -> if sequence.compare_function el1 el2 = 1 then el1 else el2) hd sequence.elements
end;;

5.6 
module type CALCULATOR = sig 
type 'a t 
val create : unit ->'a t 
val insert :'a t -> int-> 'a t
val plus :'a t-> int 
val minus :'a t -> int 
val mnozi :'a t -> int 
val deli :'a t -> int 
end;;

module Polish :CALCULATOR  = struct 
type 'a t = int list
exception Empty
let create() = ([] )

let insert lst (x : int) =
[x] @ lst 


let plus lst =
let reversed = List.rev lst in
match reversed with 
|[] -> raise Empty
|_::[] -> failwith "not enough operands"
| el1 :: el2 :: tl ->  el1 + el2 

let minus lst =
let reversed = List.rev lst in
match reversed with 
|[] -> raise Empty
|_::[] -> failwith "not enough operands"
| el1 :: el2 :: tl ->  el1 - el2 


let mnozi lst =
let reversed = List.rev lst in
match reversed with 
|[] -> raise Empty
|_::[] -> failwith "not enough operands"
| el1 :: el2 :: tl ->  el1 * el2 
 

let deli lst =
let reversed = List.rev lst in
match reversed with 
|[] -> raise Empty
|_::[] -> failwith "not enough operands"
| el1 :: el2 :: tl ->  el1 / el2 
 
end;;

5.7
module type SLIKA = sig 
type 'a slika  
val create : int -> int -> 'a -> 'a slika
val set : 'a slika -> int -> int -> 'a -> unit
val mirror_x : 'a slika -> 'a slika 
val mirror_y : 'a slika -> 'a slika
end;;

module Slika = struct 
type 'a slika = { mutable x : int;
mutable y : int ;
mutable p : 'a array array }

let create x_init y_init ele =
{ x= x_init ; y= y_init ; p= Array.make_matrix x_init y_init ele }

let set (matrix : 'a slika) ( i: int) (j: int) (e : 'a)=
matrix.p.(i).(j) <- e

let mirror_x (matrix : 'a slika)= 
for i = 0 to (matrix.x-1)/2 do 
let temp = ref matrix.p.(i) in
matrix.p.(i) <- matrix.p.(matrix.x-1-i) ;
matrix.p.(matrix.x-1-i) <- !temp;
done;
matrix.p

let mirror_y (matrix : 'a slika)= 
for i = 0 to matrix.x-1 do 
for j =0 to (matrix.y -1)/2 do
let temp = ref matrix.p.(i).(j) in
matrix.p.(i).(j) <- matrix.p.(i).(matrix.y-1-j) ;
matrix.p.(i).(matrix.y-1-j) <- !temp;
done;
done;
matrix.p

end;;

example : let image = Slika.create 4 3 0 in

  (* Set pixel values *)
  Slika.set image 0 0 1;
  Slika.set image 1 1 2;
  Slika.set image 2 2 3;

  (* Print the original image *)
  Array.iter (fun row -> Array.iter (fun pixel -> print_int pixel; print_string " ") row; print_newline ()) image.p;

  (* Mirror the image horizontally and print *)
  let mirrored_x = Slika.mirror_y image in
  print_string "Mirrored Image (Horizontal):\n";
  Array.iter (fun row -> Array.iter (fun pixel -> print_int pixel; print_string " ") row; print_newline ()) mirrored_x.p;

 5.8
module type BARVA = sig 
type t 
val equal : t -> t -> bool 
end;;

module type SLIKA = sig 
type 'a slika 
val create : int -> int -> 'a -> 'a slika
val matching : 'a slika -> 'a slika -> (int * int) -> bool 
end;;

module Slika  (Barva : BARVA )= struct 
type slika = ( int * int * Barva.t) array array 
let create x y barva = Array.make_matrix x y barva 

 let matching slika1 slika2 (x, y) =
    let rows = Array.length slika2 in 
    let cols = Array.length slika2.(0) in 
    let flag = ref true in
    for i = 0 to rows - 1 do 
      for j = 0 to cols - 1 do 
        if not (i = x && j = y) then begin
          let (_, _, slika_color) = slika1.(i).(j) in 
          let (_, _, slika2_color) = slika2.(i).(j) in 
          if not (Barva.equal slika_color slika2_color) then 
            flag := false 
        end
      done;
    done;
    !flag
end;;

module Barva = struct 
type t = int

let equal (color1 : t) ( color2 : t)= 
if color1=color2 then true 
else false


end;;


5.9 e ista so 5.6 

5.10
module type Stack = sig 
  type 'a t 
  exception Empty 
  val create : unit -> 'a t 
  val push : 'a -> 'a t -> unit 
  val pop : 'a t -> 'a 
  val reverse : 'a t -> 'a t (*I added this method because i didn't know how to do it otherwise *)
end;;

module Queue (Stack : Stack) : sig 
  type 'a t 
  exception Empty 
  val create : unit -> 'a t 
  val enqueue : 'a -> 'a t -> 'a t
  val dequeue : 'a t -> 'a 
end = struct 
  type 'a t = {  front : 'a Stack.t ; back :'a Stack.t }
  exception Empty 

  let create () = { front = Stack.create(); back = Stack.create() }

  let enqueue x queue = 
    Stack.push x queue.back;
    queue 

  let dequeue queue =
    match Stack.pop queue.front with
    | x -> x
    | exception Stack.Empty -> 
        match Stack.pop (Stack.reverse queue.back) with 
        | x -> x 
        | exception Stack.Empty -> raise Empty
     
end;;

5.11
module type Array = sig 
  type 'a e
  val create : int -> 'a -> 'a e 
  val append : 'a e -> 'a e -> 'a e
  val empty : 'a e
  val delete : 'a -> 'a e -> 'a e 
end;;

module MyArray : Array = struct 
  type 'a e = {elements : 'a array}
  let create (len: int) (init :'a ) =  {elements = Array.make len init }
  let append (arr1: 'a e ) (arr2: 'a e) = {elements= Array.append arr1.elements arr2.elements } 
  let empty = {elements = [||]}
  
   let delete (x:'a) (arr:'a e) =
    let l = Array.length arr.elements in
    let result = ref [||] in  
    let d = ref 0 in 
    for i = 0 to l - 1 do 
      if (arr.elements.(i) <> x && !d < l) then (
        result := Array.append !result [|arr.elements.(i)|];
        incr d
      )
    done;
{elements= !result}
end;;

GOOD LUCK!
