signature BIGNAT =
sig
 eqtype bignat
 exception overflow
 exception underflow
 val zero : bignat
 val normalize : bignat -> bignat
 val fromString : string -> bignat
 val toString : bignat -> string
 val toInt : bignat -> int 
 val fromInt : int ->  bignat
 val ++ : bignat * bignat -> bignat 
 val succ : bignat -> bignat        
 val min : bignat * bignat -> bignat
 val max : bignat * bignat -> bignat
 val ** : bignat * bignat -> bignat
 val compare : bignat * bignat -> order
 val << : bignat * bignat -> bool
 val <<= : bignat * bignat -> bool
 val >> : bignat * bignat -> bool
 val >>= : bignat * bignat -> bool
 val == : bignat * bignat -> bool
 val len : bignat -> int
 val lenCompare : bignat * bignat -> order
 val lenLt : bignat * bignat -> bool
 val lenLeq : bignat * bignat -> bool
 val lenGt : bignat * bignat -> bool
 val lenGeq : bignat * bignat -> bool
 val lenEq : bignat * bignat -> bool
 val -- : bignat * bignat -> bignat
 val pred : bignat -> bignat
 exception division_by_zero
 exception emptyList
 val %% : bignat * bignat -> bignat * bignat
 val quo : bignat * bignat -> bignat
 val rem : bignat * bignat -> bignat   
end

infix ++
infix --
infix **
infix %%
infix <<
infix <<=
infix >>
infix >>=
infix ==
structure Bignat:BIGNAT  =
  struct
    type bignat	= int list	(* least significant first *)
    exception underflow
    exception division_by_zero
    exception emptyList
    exception overflow
    val zero =[]
    val base = 10000		(* must be <=sqrt(Int.maxInt) *)
    
    fun norm ([],x)=rev x
      | norm (x :bignat,x1)= if not(hd(x)=0) then norm([],x) else norm(tl(x),x1)   
    fun normalize x = norm(rev x,[])

    fun fromInt 0 = []
      | fromInt n = 
	if n < 0 
	then raise underflow
      	else n mod base :: fromInt(n div base)

    fun toInt [] = 0
      | toInt (d::ds) = d + base * toInt ds	(* may raise Overflow *)

    fun pad s = 
	if String.size s = 4 
	then s 
	else pad("0" ^ s)
	     
    fun toString ds = 
	case List.rev ds of  
	  []   => "0"
	| d::dr => String.concat (Int.toString d :: List.map (pad o Int.toString) dr)

    infixr 5 ::: (* cons that discards leading zeros' *)
    fun 0:::[] = []
      | n:::ns = n::ns
      
    fun add (ar, [], 0) = ar
      | add ([], br, 0) = br
      | add ([], [], c) = [c]
      | add (ar, [], c) = add (ar, [0], c)
      | add ([], br, c) = add ([0], br, c)
      | add (a::ar, b::br, c) = 
	let val (d, carr) = 
		if a+b+c < base
		then (a+b+c, 0)
		else (a+b+c-base, 1)
	in
	  d :: add (ar, br, carr)
	end
    fun adj (d,xs) = if xs=[] andalso d=0 then [] else d::xs
    fun subtract(ar,    [],    0)  = ar
      | subtract([],    _ ,    _)  = raise underflow
      | subtract(ar,    [],    c)  = subtract(ar,[c],0)
      | subtract(a::ar, b::br, c)  = 
        let
          val s      = a-b-c
          val (d,carr) = if s>=0 then (s, 0) else (s+10000, 1)
        in
          adj(d, subtract(ar,br,carr))
        end  
        
    fun x ++ y = add (x,y,0)                              
    fun x -- y = subtract(x,y,0)
    fun succ x = x ++ [1]
    fun pred x = if x=[] then raise emptyList else 
    	         x -- [1]
    fun mul (x, 0) = []
      | mul ([], b) = []
      | mul (a::ar, b) = fromInt(a*b) ++ (0::mul(ar,b))
    
(*we shoulsd not use normalize in x**y or x++y or other similar operation because 
  it decrease the efficiency of program drastically i.e. when we have a fun
  which use these function repeatedly and in every repeatition ormalize is called 
  unnecessarily which creates havoc like in fact progrm we use multiplication
  repeatedly and calculation of fact 10000 give result within 5 sec without use 
  of normalize but when we use it computer fails i.e. it donot give answer at all
  even after 5 mins so when a user wants write output then he sould give correct
  input himself as machine is not responsible for input part*)
    

    fun [] ** y = []
      | x  ** [] = []
      | x  ** (b::br) = mul(x,b) ++ (0::(x ** br))

    fun x <<= y = (subtract(y,x,0) ; true) handle underflow => false
    fun (x : bignat) << (y : bignat) = x <> y andalso x <<= y
    fun x >> y = if x <<= y then false else true
    fun x >>= y = if x << y then false else true
    fun (x:bignat) == y = not (x <> y)
    fun min(x,y) = if x << y then x else y
    fun max(x,y) = if x >> y then x else y
    fun compare(x,y) = if x << y then LESS else if x<>y then EQUAL else GREATER	
    
    fun divmod (x, []) = raise division_by_zero
      | divmod (x, y) = 
	if x << y then ([],x) else
	let fun dm(x,y,c) = 
		if x << y then (x,c)
		else dm(x--y,y,Int.+(c,1))
	    val (d,m)  = divmod(x,0::y)
	    val (m',c) = dm(m,y,0)
	in
	  (c:::d, m')
	end
     
    fun length ([] :bignat) = 0
    |   length (hd::tl :bignat) = Int.+(1,length(tl)) 

    fun len x = if (length x) > 1000000000 then raise overflow else length x 

    fun lenLeq(x,y) =  len(x)<=len(y)  
    fun lenLt(x,y)  =  len(x)<len(y)
    fun lenEq(x,y)  =  len(x)=len(y)
    fun lenGeq(x,y) =  len(x)>=len(y)
    fun lenGt(x,y)	=  len(x) > len(y)
    fun lenCompare(x:bignat,y:bignat) = if lenLt(x,y) then LESS else if lenEq(x,y) then EQUAL else GREATER

    fun quo(x,y) = #1 (divmod(x,y))
    
    fun rem(x,y) = #2 (divmod(x,y))
    
    fun x %% y = divmod(x,y)
    
    fun fromString s = 
    let
      val a = size s;
      fun str(s,start,l)= if start=a then normalize l else
        let
          val SOME sbstr=Int.fromString (substring(s,start,4))
        in
          str(s,Int.+(start,4),sbstr::l)
        end 
    in if a=0 then [] else
      if Int.mod(a,4)=0 then 
      str(s,0,[])
      else
      str(s,Int.mod(a,4),[valOf(Int.fromString (substring(s,0,Int.mod(a,4))))])
    end
  end;





signature BIGINT = 
sig
 type bigint
 val bigzero: bigint
 val normalize : bigint -> bigint
 val tobigint: int -> bigint
 val fromString : string -> bigint (*option*)
 val toString : bigint -> string
 val toint : bigint -> int option
 val ~~ : bigint -> bigint
 val abs : bigint -> bigint
 val ++ : bigint * bigint -> bigint
 val succ : bigint -> bigint
 val min : bigint * bigint -> bigint
 val max : bigint * bigint -> bigint
 val sign : bigint -> string
 val sameSign : bigint * bigint -> bool
 val ** : bigint * bigint -> bigint
 val compare : bigint * bigint -> order
 val << : bigint * bigint -> bool
 val <<= : bigint * bigint -> bool
 val >> : bigint * bigint -> bool
 val >>= : bigint * bigint -> bool
 val == : bigint * bigint -> bool
 val len : bigint -> int
 val lenCompare : bigint * bigint -> order
 val lenLt : bigint * bigint -> bool
 val lenLeq : bigint * bigint -> bool
 val lenGt : bigint * bigint -> bool
 val lenGeq : bigint * bigint -> bool
 val lenEq : bigint * bigint -> bool
 val -- : bigint * bigint -> bigint
 val pred : bigint -> bigint
 exception division_by_zero
 val %% : bigint * bigint -> bigint * bigint
 val div : bigint * bigint -> bigint
 val mod : bigint * bigint -> bigint
 val quo : bigint * bigint -> bigint
 val rem : bigint * bigint -> bigint
end

functor bint (bn:BIGNAT) :BIGINT =
struct
     type bigint = bool * bn.bignat
     exception division_by_zero

     val bigzero :bigint = (true,bn.zero)
     val base = 10000
      
     fun normalize (x:bigint) = (#1 x, bn.normalize(#2 x))
     fun tobigint n = if n<0 then (false,bn.fromInt(~n)) else
     	               (true,bn.fromInt n)
     fun fromString s : bigint = 
     	let val a = bn.fromString ( substring(s,1, (size s)-1) )
     		val a1 = bn.fromString ( substring(s,0, (size s)-1) )
        in 
     		if String.sub(s,0)=(#"-") andalso not(a=bn.zero) then (false , a) else 
     		(true , a1)
        end
     fun toString (x:bigint) = if (#1 x) then bn.toString(#2 x) else 
     	                  "-"^(bn.toString(#2 x))
      
     fun toint (x,y) = (if x then SOME (bn.toInt(y)) else SOME (~(bn.toInt(y)))) handle overflow => NONE
     fun ~~ x = if (#1 x) andalso not((#2 x)=bn.zero) then (false ,#2 x) else 
     	        if #1 x  then x else (true , #2 x) 
     fun abs x = if #1 x then x else (true,#2 x)  
     fun (x:bigint) ++ (y:bigint) = if #1 x andalso #1 y then (true,bn.++((#2 x),(#2 y))) else
     	    if #1 x andalso not (#1 y) then (if bn.<<((#2 x),(#2 y)) then (false,bn.--((#2 y),(#2 x))) else (true,bn.--((#2 x),(#2 y)))) else
     	    if not (#1 x) andalso #1 y then (if bn.<<((#2 y),(#2 x)) then (false,bn.--((#2 x),(#2 y))) else (true,bn.--((#2 x),(#2 y)))) else 
              (false,bn.++((#2 x),(#2 y)))
     fun x -- y = x ++ (~~ y)
     fun succ (x:bigint) = if #1 x then (#1 x,bn.succ(#2 x)) else (if bn.==(#2 x,bn.fromString "") then (not(#1 x),bn.fromString "1") else
                  if bn.==(#2 x,bn.fromString "1") then (not(#1 x),bn.fromString "") else (#1 x,bn.--(#2 x,bn.fromString "1")))
     fun pred (x:bigint) = x -- (fromString "1")
     fun max (x:bigint,y:bigint) = if #1 x then (if #1 y then (if bn.<<(#2 x,#2 y) then y  else x) else x ) else (if #1 y then y else (if bn.<<(#2 x,#2 y) then x else y ))  
     fun min (x:bigint,y:bigint) = if #1 x then (if #1 y then (if bn.<<(#2 x,#2 y) then x  else y) else y ) else (if #1 y then x else (if bn.<<(#2 x,#2 y) then y else x )) 
     fun sign (x:bigint) = if #1 x then "positive" else "negitive"
     fun sameSign (x:bigint,y:bigint)= if #1 x = #1 y then true else false
     
     fun (x:bigint)<<=(y:bigint) =if #1 x = #1 y then (if #1 x =true then bn.<<=(#2 x,#2 y) else not(bn.<<=(#2 x,#2 y))) else (if #1 x =true then false else true)
     fun (x:bigint)<<(y:bigint) = not(bn.==(#2 x,#2 y))  andalso x <<= y
     fun (x:bigint)>>=(y:bigint) = not(x << y)
     fun (x:bigint)>>(y:bigint) = not(x <<= y)
     fun (x:bigint)==(y:bigint) = not(x<<y) andalso x<<=y

     fun len (x:bigint) = bn.len(#2 x) 
     fun lenCompare (x:bigint,y:bigint)=if len x < len y then LESS else (if len x = len y then EQUAL else GREATER)
     fun lenLt (x:bigint,y:bigint) = len x < len y 
     fun lenLeq (x:bigint,y:bigint) = len x <= len y
     fun lenGt (x:bigint,y:bigint) = len x > len y
     fun lenGeq (x:bigint,y:bigint) = len x >= len y 
     fun lenEq (x:bigint,y:bigint) =  len x = len y

     fun (x:bigint)**(y:bigint)= 
     	let val a = bn.**(#2 x,#2 y)
     	in 
     		if #1 x = #1 y then (true , a) else (false,a)
        end

     fun compare (x:bigint,y:bigint) = if x << y then LESS else (if x == y then EQUAL else GREATER)
     
     fun (x:bigint)%%(y:bigint) =
      let
       	val name = bn.%%(#2 x,#2 y)
      in if #2 y = bn.zero then raise division_by_zero else 
       	if #1 x = #1 x then ((true,#1 name),(true,#2 name)) else 
       		let val b = (#2 name)  
       		in
       		  if b = bn.zero then ((false,#1 name),(true, b))    else
       		  ((false,bn.++(#1 name,bn.fromString "1")),(true,bn.--(bn.fromString "10000" , b)))
            end
      end  
     
     fun (x:bigint) div (y:bigint) = #1 (x%%y)
     fun (x:bigint) mod (y:bigint) = #2 (x%%y)
     fun quo(x:bigint,y:bigint) = #1 (x%%y)
     fun rem(x:bigint,y:bigint) = #2 (x%%y)

end


structure BigInt = bint(Bignat)

(*open Bignat 
fun fact [1] = [1]
  | fact x = x**fact(x--[1])*)
