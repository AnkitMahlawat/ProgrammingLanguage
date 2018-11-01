signature  FILEIO =
sig
    val getclist : string -> TextIO.StreamIO.elem list
    val getclistNN : string -> char list
    val slurp : string -> TextIO.StreamIO.vector
    val readAll : string -> TextIO.StreamIO.vector
    val bite : string -> string list
    val lick : string -> string list 
    val readLines : string -> string list
    val write : string * TextIO.vector -> unit
    val writeLine : string * string -> unit
    val writeLines : string * string list -> unit
    val append : string * TextIO.vector -> unit
    val appendLine : string * string -> unit
    val appendLines : string * string list -> unit
end (* sig FILEIO *)

structure FileIO: FILEIO =
struct

(*=============== reading a text file ====================== *)

(* Partly  from the SML Basis LIbrary -- Gansner and Reppy *)

(* get a list of characters from a file *)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end

(* get list of chars except newline characters *)
fun getclistNoNewline  (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loopNoNewline (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#"\n", f') => loopNoNewline (clist, f')
	      | SOME (c, f') => loopNoNewline (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loopNoNewline ([], f))
    end

val getclistNN = getclistNoNewline

(* Get the contents of an entire text file as a string *)
fun slurp (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	val (s, _) = TextIO.StreamIO.inputAll f
    in  TextIO.StreamIO.closeIn; s
    end

val readAll = slurp;

(* Reading a chunk of characters at a time and outputting a list of strings *)
fun bite (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case TextIO.StreamIO.input f
	     of ("", f')    => (TextIO.StreamIO.closeIn f'; accum)
	      | (chunk, f') => loop (chunk::accum, f')
            (* esac *)
    in  rev(loop ([], f))
    end



(* Reading by lines list of strings 
fun lick (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop (chunk::accum, f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
            (* esac *)
    in  rev(loop ([], f))
    end


This function which is functional style yields the following error
	
/home/sak/sml/programs/fileio/fileIO.sml:74.6-76.52 Error: case object and rules don't agree [tycon mismatch]
  rule domain: (string * ?.TextIO.instream) option
  object: string * ?.TextIO.instream
  in expression:
    (case (TextIO.StreamIO.inputLine f)
      of SOME (chunk,f') => loop (<exp> :: <exp>,f')
       | NONE => (TextIO.StreamIO.closeIn f; accum))

So we do the imperative style
*)

fun lick (filename:string) =
    let val f = TextIO.openIn filename
        fun loop (accum: string list) =
            case (TextIO.inputLine f) of 
		NONE => accum
              | SOME line => loop (line::accum)
            (* esac *)
        val lines =   rev(loop [])
    in TextIO.closeIn f; lines
    end




(* The perl function chomp *)
fun chomp1 s = 
    let val charlist = rev (explode s)
	fun nibble [] = []
	  | nibble (#"\n"::l) = l
	  | nibble l = l
    in  implode (rev (nibble charlist))
    end

fun chomp (L: string list)  = map chomp1 L;

val readLines = chomp o lick

(*======================== writing a text file ========================= *)

(* writing a single string into a text file without adding a new line*)
fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

(* writing a single string terminated by newline into a text file *)
fun writeLine (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

(* the perl function join *)
fun join (glue, []) = ""
  | join (glue, [s]) = s
  | join (glue, (h::t)) = h^glue^(join (glue, t))

(* writing a string list into a file *)
fun writeLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  writeLine (filename: string, ss)
    end

(* appending a line to a file without a newline character *)
fun append (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end
(* same as append but adds a newline character *)
fun appendLine  (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

fun appendLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  appendLine (filename: string, ss)
    end
end (* struct Fileio *)

     
val a1 = FileIO.slurp "mdtab.md"

fun repstar [] = []
  | repstar (x::[]) = if x = #"\\" then [] else [x]	
  | repstar (x::y) = if x = #"\\"  then
  	                if hd(y) = #"*" then #"$"::repstar(tl(y)) else x::repstar(y)
  	                 else x::repstar(y)
val a = explode a1
val b = repstar a

fun italic ([],x) =
    let
     	val a = "</em>"
     	val b = explode a
     in
     	if x = 0 then [] else b
    end  
  | italic ((x::[]),it) =
    let
     	val a = "</em>"
     	val b = explode a
     in
     	if x = #"*" then if it = 1 then b else [] else x::italic([],it)
    end  
  | italic ((x::y),it) = 	 		 
    let
     	val a = "</em>"
     	val b = explode a
        val c = "<em>"
        val d = explode c
        fun chstr ([],rt) = ([],rt)
          | chstr ((x::y),rt) = if x = #"*" then chstr(y,x::rt) else (x::y,rt)	
        val e = chstr ((x::y),[])
     in
     	if x = #"*" then
     	 if not(hd(y) = #"*") then 
     		if it = 0 then d@italic(y,1) 
     		else b@italic(y,0) 
     	 else (#2 e)@italic((#1 e),it)
        else x::italic(y,it)
    end

val c = italic (b,0)     

fun bold ([],bl) = 
     let
     	val a = "</strong>"
        val b = explode a
     in
     	if bl = 1 then b else []
    end
  | bold ((x::[]),bl) = 
     let
     	val a = "</strong>"
        val b = explode a
     in
     	if bl = 1 then x::b else []
    end
  | bold ((x::(y::[])),bl) =
     let
     	val a = "</strong>"
        val b = explode a
     in
     	if x = #"*" andalso y = #"*" then if bl = 1 then b else [] else x::(y::[])
    end
  | bold ((x::y),bl) = 
     let
     	val a = "</strong>"
        val b = explode a
        val c = "<strong>"
        val d = explode c
        fun chstr ([],rt) = ([],rt)
          | chstr ((x::y),rt) = if x = #"*" then chstr(y,x::rt) else (x::y,rt)	
        val e = chstr ((x::y),[])
     in
     	if x = #"*" andalso hd(y) = #"*" then
     	 if not(hd(tl(y)) = #"*") then 
     	 	if bl = 0 then d@bold(tl(y),1) 
     	 	else b@bold(tl(y),0) 
     	 else (#2 e)@bold((#1 e),bl)
     	else x::bold(y,bl) 	
    end

val d = bold (c,0)

fun revstar [] = []
  | revstar (x::y) = if x = #"$" then #"*"::revstar(y) else x::revstar(y)

val e = revstar d  	

fun undrlne [] = []
  | undrlne (x::[]) = if x = #"\\" then [] else [x]	
  | undrlne (x::y) = if x = #"\\"  then
  	                if hd(y) = #"_" then #"$"::undrlne(tl(y)) else x::undrlne(y)
  	                 else x::undrlne(y)


val f = undrlne e

fun underline ([],x) =
    let
     	val a = "</u>"
     	val b = explode a
     in
     	if x = 0 then [] else b
    end  
  | underline ((x::[]),it) =
    let
     	val a = "</u>"
     	val b = explode a
     in
     	if x = #"_" then if it = 1 then b else [] else x::underline([],it)
    end  
  | underline ((x::y),it) = 	 		 
    let
     	val a = "</u>"
     	val b = explode a
        val c = "<u>"
        val d = explode c
        fun chstr ([],rt) = ([],rt)
          | chstr ((x::y),rt) = if x = #"_" then chstr(y,x::rt) else (x::y,rt)	
        val e = chstr ((x::y),[])
     in
     	if x = #"_" then
     	 if not(hd(y) = #"_") then 
     		if it = 0 then d@underline(y,1) 
     		else b@underline(y,0) 
     	 else (#2 e)@underline((#1 e),it)
        else x::underline(y,it)
    end


val g = underline (f,0)

fun revlne [] = []
  | revlne (x::y) = if x = #"$" then #"_"::revlne(y) else x::revlne(y)

val h = revlne g

fun hrizntal [] = []
  | hrizntal (x::[]) = if x = #"\\" then [] else [x]	
  | hrizntal (x::y) = if x = #"\\"  then
  	                if hd(y) = #"-" then #"$"::hrizntal(tl(y)) else x::hrizntal(y)
  	                else x::hrizntal(y)

val i = hrizntal h

fun hrline [] = []
  | hrline (x::[]) = [x]	
  | hrline (x::(y::[])) = x::[y] 
  | hrline (x::(y::(z::[]))) = 
    let
    	val a = "<hr>"
    	val b = explode a
    in
    	if x = #"-" andalso y = #"-" andalso z = #"-" then b else x::(y::[z])
    end
  | hrline (x::y) = 
    let
    	val a = "<hr>"
    	val b = explode a
    	fun hypen [] = []
    	  | hypen (x::y) = if x = #"-" then hypen(y) else x::y
        val c = hypen (tl(tl(y)))
    in
    	if x = #"-" andalso hd(y) = #"-" andalso hd(tl(y)) = #"-" then b@hrline(c) else x::hrline(y)
    end

val j = hrline i

fun revhr [] = []
  | revhr (x::y) = if x = #"$" then #"-"::revhr(y) else x::revhr(y)

val k1 = revhr j

fun bsbq [] = []
  | bsbq (x::[]) = if x = #"\\" then [] else [x]	
  | bsbq (x::y) = if x = #"\\"  then
  	                if hd(y) = #">" then #"$"::bsbq(tl(y)) else x::bsbq(y)
  	                else x::bsbq(y)

val k = bsbq k1  	                	

fun blockquotes ([],bl) =
    let
    	val a = "\n</blockquote>"
    	val a1 = "</p>"
        val b = explode a
    	fun bq x =  if x = 0 then [] else b@bq(x-1)
    	val c = (explode a1)@(bq bl)
    in
    	if bl = 0 then [] else c
    end
  | blockquotes ((x::[]),bl) =  
    let
    	val a = "\n</blockquote>"
    	val a1 = "</p>"
        val b = explode a
    	fun bq x =  if x = 0 then [] else b@bq(x-1)
    	val c = (explode a1)@(bq bl)
    in
    	if bl = 0 then [x] else x::c
    end
  | blockquotes ((x::y),bl) = 
    let
       	val a = "\n</blockquote>"
    	val a1 = "</p>"
    	val a2 = "<blockquote>\n"
    	val a3 = "<p>"
    	val b = explode a
    	val b1 = explode a2
    	fun bqs x = if x = 0 then (explode a3) else b1@bqs(x-1)
        fun check ([],s) = ([],s)
          | check ((x::y),s) = if x = #">"  then check(y,s+1) else (x::y,s)
        fun checksp [] = []
          | checksp (x::y) = if x = #" "  then checksp(y) else x::y  	
    	fun bq x =  if x = 0 then [] else b@bq(x-1)
        fun argse x =if x = 0 then [] else 
        	         if x>0 then bqs x else (explode a1)@(bq (~x))
        val c = argse ((#2 (check ((checksp y),0)))-bl)
        val d = "</p>\n<p>"
        val e = (#1 (check ((checksp y),0)))
        fun abs x = if x<0 then ~x else x
        val f = ((#2 (check ((checksp y),0)))-bl)
        val g = if(not(null(tl (checksp e)))) then check ((checksp (tl(checksp e))),0) else ([],0)
       in
       	if x = #"\n" then
       	  if not(null(checksp y)) andalso hd(checksp y) = #"\n" then if bl = 0 then x::blockquotes(y,bl) else ((explode a1)@(bq bl))@blockquotes((checksp y),0)
       	  else if not(null(checksp y)) andalso hd(checksp y) = #">" andalso not(null (checksp e)) andalso hd(checksp e) = #"\n" andalso (#2 g) = bl andalso f = 0 andalso not(bl = 0)  then (explode d)@blockquotes(tl(e),bl)  
       	  else if not(null(checksp y)) andalso hd(checksp y) = #">" then if bl = 0 then (bqs (abs f))@blockquotes(e,(abs f)) else c@blockquotes(e,f+bl) 
       	else x::blockquotes(y,bl)
       	else x::blockquotes(y,bl)
       end   

val l = blockquotes(k,0)       

fun revbq [] = []
  | revbq (x::y) = if x = #"$" then #">"::revbq(y) else x::revbq(y)

val l1 = revbq l

fun bslink [] = []
  | bslink (x::[]) = if x = #"\\" then [] else [x]	
  | bslink (x::y) =
    let
     	val a = explode "{$1}"
     	val b = explode "{$2}"
     	val c = explode "{$3}"
     	val d = explode "{$4}"
     in
     	if x = #"\\"  then
  	    if hd(y) = #"(" then a@bslink(tl(y)) else
  	    if hd(y) = #")" then b@bslink(tl(y)) else
  	    if hd(y) = #"[" then c@bslink(tl(y)) else
  	    if hd(y) = #"]" then d@bslink(tl(y)) else x::bslink(y)
  	    else x::bslink(y)
     end            

val l2 = bslink l1


(*fun iden ([],z,c,s,m) = []  
  | iden ((a::b),z,c,s,m) =
    let
    	fun checksp [] = []
          | checksp (a::b) = if a = #" "  then checksp(b) else a::b
        fun xyz [] = []
          | xyz (a::b) = if a = #" "  then a::xyz(b) else []  	
    in
        if s = 0 then if a = #"[" then iden(b,z,c,1,m) else a::iden(b,z,c,0,m) else
    	if s = 1 then if a = #"]" then iden(b,z,c,2,m) else if a = #"\n" then (explode ("["^(implode (rev z))))@iden(b,[],[],0,[]) else iden(b,(a::z),c,s,m) else
        if s = 2 then if a = #"(" then iden(b,z,c,3,m) else if a = #" " andalso not(null(checksp b)) andalso hd(checksp b) = #"(" then iden(tl(b),z,c,3,xyz(b)) else (explode ("["^(implode (rev z))^"]"))@iden(b,[],[],0,m) else
        if a = #")" then (explode ("<a href=\""^(implode(rev c))^"\">"^(implode(rev z))^"</a>"))@iden(b,[],[],0,[]) else if a = #" " orelse a = #"\n" orelse a = #"\t" then (explode ("["^(implode (rev z))^"]"^(implode m)^"("^(implode (rev c))))@iden(b,[],[],0,[]) else iden(b,z,(a::c),3,m)
    end *)

(*fun links ([],s) = []
  | links ((x::y),s) = if s = 0 then if x = #"[" then links(y,1) else  links(y,0)  else 
  	                   if x = #")" then [] else x::links(y,1)   	   
    

fun check ([],s) = []
  | check ((x::y),s) = if s = 0 then if x = "]" then ([],links(y,1)) else expression else expression*)

(*val l3 = iden (l2,[],[],0,[])*)

fun revbslink [] = []
  | revbslink (x::[]) = [x]
  | revbslink (x::(y::[])) = x::[y]
  | revbslink (x::(y::(z::[]))) = x::(y::[z])
  | revbslink (x::y) = if x = #"{" andalso hd(y) = #"$" andalso hd(tl(y)) = #"1" andalso hd(tl(tl(y))) = #"}" then #"("::revbslink(tl(tl(tl(y)))) else 
  	                   if x = #"{" andalso hd(y) = #"$" andalso hd(tl(y)) = #"2" andalso hd(tl(tl(y))) = #"}" then #")"::revbslink(tl(tl(tl(y)))) else
                       if x = #"{" andalso hd(y) = #"$" andalso hd(tl(y)) = #"3" andalso hd(tl(tl(y))) = #"}" then #"["::revbslink(tl(tl(tl(y)))) else
                       if x = #"{" andalso hd(y) = #"$" andalso hd(tl(y)) = #"4" andalso hd(tl(tl(y))) = #"}" then #"]"::revbslink(tl(tl(tl(y)))) else
                       x::revbslink(y)	

val l4 = revbslink l2 




fun slhead [] = []
  | slhead (x::[]) = if x = #"\\" then [] else [x]	
  | slhead (x::y) = if x = #"\\"  then
  	                if hd(y) = #"#" then #"$"::slhead(tl(y)) else x::slhead(y)
  	                else x::slhead(y)

val l5 = slhead l4

fun heading ([],s,z) = [] 
  | heading ((x::y),s,z) = 
      let
        val aa = "A"	
      in
       if s = 0 then if not(null(y)) andalso hd(y) = #" " andalso x = #"\n" then x::heading(y,1,z) else if not(null(y)) andalso hd(y) = #"#" andalso x = #"\n"  then x::heading(y,2,z) else x::heading(y,0,z) else
       if s = 1 then if not(null(y)) andalso hd(y) = #"#" andalso x = #" " then x::heading(y,2,z) else if not(null(y)) andalso hd(y) = #" " andalso x = #" " then x::heading(y,1,z) else x::heading(y,0,z) else 
       if s = 2 then if x = #"#" then heading(y,2,z+1) else (explode ("<h"^Int.toString(z)^">"))@[x]@heading(y,3,z) else 
       if x = #"\n" then (explode ("</h"^(Int.toString(z))^">"))@heading((x::y),0,0) else x::heading(y,3,z)
      end

val l6 = heading (l5,0,0)

fun revhead [] = []
  | revhead (x::y) = if x = #"$" then #"-"::revhead(y) else x::revhead(y)

val l7 = revhr l6
  


(*fun heading ([],filename,hding) = (FileIO.appendLine(filename,hding);[]) 
  | heading ((x::y),filename,hding) = 
      let
      	fun nwlnrm [] = []
      	  | nwlnrm (x::y) = if x = "\n" then nwlnrm(y) else x::y	
      val a1 = nwlnrm y 
      fun tf x = null(x)
      val tfnwln = tf a1
      fun sze x = if not(tfnwln) then String.size(hd(x)) else 0
      in
      	if not(tfnwln) then
      	if x = "\n" andalso (hd(a1) = "#" orelse hd(a1) = "##" orelse hd(a1) = "###" orelse hd(a1) = "####" orelse hd(a1) = "#####" orelse hd(a1) = "######") then (FileIO.append(filename,hding^"\n<h"^(Int.toString(sze a1))^"><p>");heading(tl(a1),filename,"<h"^(Int.toString(sze a1))^"><p>")) else
        if x = "\n" then (FileIO.appendLine(filename,hding);(a1)) else
  	    (FileIO.append(filename,x^" ");heading(y,filename,hding))
      else if x = "\n" then (FileIO.appendLine(filename,hding);[]) else (FileIO.append(filename,x^" ");heading(a1,filename,hding))
      end*)
val m = FileIO.write("mdtabs.html",(implode l7))
                                       
fun tokns filename = 
	let
		val ss = FileIO.slurp (filename)
		fun isspace x =if x = #" " orelse x = #"\t" then true else false
		val token = String.tokens isspace ss
        fun lntokn [] = []
          | lntokn (hd::tl) = 
          let
           	val head = String.explode hd
            fun spln [] = []
              | spln (hd::tl) =
                let
                	val a = spln(tl)
                	val aa = (#" ")::a
                    val ab =  (#"\n")::aa
                    val ac = (#" ")::ab
                in
                	if hd = #"\n" then ac else hd::a
                end
            val nwhd  = String.implode (spln head)
            val xhd = String.tokens isspace nwhd
           in
           	xhd@lntokn(tl)
           end
           val finltkn = lntokn token 
	in
		finltkn
	end


val l8 = tokns "mdtabs.html"

fun olists ([],s) = if s = 2 then ["</p>\n</li>\n</ol>"] else []
  | olists ((x::y),s) = 
    let
     fun check [] = true
       | check (x::[]) = if x = #"." then true else false
       | check (x::y) = if x = #"1" orelse x = #"2" orelse x = #"3" orelse x = #"4" orelse x = #"5" orelse x = #"6" orelse x = #"7" orelse x = #"8" orelse x = #"9" orelse x = #"0" then check(y) else false	
     fun rmnwln ([],s) = ([],s)
       | rmnwln ((x::y),s) = if x = "\n" then rmnwln(y,s+1) else (x::y,s)
     fun fnwn s = if s = 0 then [] else "\n"::fnwn(s-1)	
     val a = "</p>\n</li>\n</ol>"
     val b = "<ol>\n<li>\n<p>"
     val c = "</p></li>\n<li><p>"
     val d = "</p>\n<p>"
     in
      if s = 0 then if x = "\n" then (fnwn (#2 (rmnwln (y,1))))@olists(#1 (rmnwln (y,1)),1) else x::olists(y,0) 
      else if s = 1 then if check (explode x) then b::olists(y,2) else x::olists(y,0) 
      else if x = "\n" andalso not(null(y)) andalso hd(y) = "\n" andalso not(null(tl(y))) andalso check(explode (hd(tl(y)))) then c::olists(tl(tl(y)),2) 
      else if x = "\n" andalso not(null(y)) andalso check (explode (hd(y))) then c::olists(tl(y),2)
      else if x = "\n" andalso not(null(y)) andalso hd(y) = "\n" andalso not(null(tl(y))) andalso hd(tl(y)) = "\n" then a::(x::olists(y,0)) 
      else if x = "\n" andalso not(null(y)) andalso hd(y) = "\n" andalso not(null(tl(y))) andalso not(check(explode (hd(tl(y))))) then d::olists(y,2) 
      else x::olists(y,2)
     end 

val l9 = olists(l8,0)


fun ulists ([],s) = if s = 2 then ["</p>\n</li>\n</ul>"] else []
  | ulists ((x::y),s) = 
    let
     fun rmnwln ([],s) = ([],s)
       | rmnwln ((x::y),s) = if x = "\n" then rmnwln(y,s+1) else (x::y,s)
     fun fnwn s = if s = 0 then [] else "\n"::fnwn(s-1)	
     val a = "</p>\n</li>\n</ul>"
     val b = "<ul>\n<li>\n<p>"
     val c = "</p></li>\n<li><p>"
     val d = "</p>\n<p>"
     in
      if s = 0 then if x = "\n" then (fnwn (#2 (rmnwln (y,1))))@ulists(#1 (rmnwln (y,1)),1) else x::ulists(y,0) 
      else if s = 1 then if x = "-" orelse x = "+" then b::ulists(y,2) else x::ulists(y,0) 
      else if x = "\n" andalso not(null(y)) andalso (hd(y) = "-" orelse hd(y)="+") then c::ulists(tl(y),2)
      else if x = "\n" andalso not(null(y)) andalso hd(y) = "\n" then a::(x::ulists(y,0)) 
      else if x = c then a::(x::ulists(y,0))	
      else if x = "\n" andalso not(null(y)) andalso not((hd(y) = "-" orelse hd(y)="+")) then d::ulists(y,2) 
      else x::ulists(y,2)
     end

val l10 = ulists(l9,0)     

fun join (glue, []) = ""
  | join (glue, [s]) = s
  | join (glue, (h::t)) = h^glue^(join (glue, t))

val l11 = join(" ", l10)  

val m = FileIO.write("mdtabs.html",l11)

(*fun ulists ([],filename,lsts) = (FileIO.appendLine(filename,lsts);[])
  | ulists ((hd::[]),filename,lsts) = (FileIO.append(filename,hd^" ");FileIO.appendLine(filename,lsts);[])
  | ulists ((x::y),filename,lsts) = 
    let
     	val hdtl = hd(y)
     in
     	if (x ="\n" andalso ( hdtl="+" orelse hdtl="-")) then
        (FileIO.append(filename,"</p></li>\n<li><p>");ulists(tl(y),filename,lsts)) else if x = "\n" andalso hdtl = "\n" then
        (FileIO.appendLine(filename,lsts);	x::y)  else
        (FileIO.append(filename,x^" ");ulists(y,filename,lsts))
     end *)


exception error_table_closing_bracket_not_found

local
fun tabledata ([],filename,tend) = raise error_table_closing_bracket_not_found
  | tabledata ((x::y),filename,tend) = 
	let
		val eplx = explode x
		fun isspace x =if x = #" " orelse x = #"\t" then true else false
	    fun slsh [] = []
	      | slsh (x::y) = 	
                let
                	val a = slsh(y)
                	val aa = (#" ")::a
                    val ab =  (#"|")::aa
                    val ac = (#" ")::ab
                in
                	if x = #"|" then ac else x::a
                end
	    val nwhd  = String.implode (slsh eplx)
        val xhd = String.tokens isspace nwhd
        fun check [] = []
          | check (x::[] ) = [x]
          | check (x::y) = if x = "|" andalso hd(y) = "|" then x::(("")::check(y)) else x::check(y)	
        val new = check xhd
        fun chks [] = ("")::[]
          | chks (x::[]) = if x = "|" then ""::[x] else [x]	
          | chks (x::y) = if x = "|" then ""::(x::y) else x::y
        val fnl  = chks new  	
        fun tdset [] = ()
          | tdset (x::[]) = if not(x = "|") then FileIO.append(filename,"<td>"^x^"</td></tr>\n") else FileIO.append(filename,"<td></td></tr>\n") 
	      | tdset (x::y) = if not(x = "|") then (FileIO.append(filename,"<td>"^x^"</td>");tdset(y)) else tdset(y)
	in     
		if x = ">>" then (FileIO.appendLine(filename,tend);y) 
		else if x = "\n" orelse x = "\t" then (tabledata(y,filename,tend)) else ( FileIO.append(filename,"<tr>");tdset(fnl); tabledata(y,filename,tend))
	end
in
fun table ([],filename,tend) = raise error_table_closing_bracket_not_found
  | table ((x::[]),filename,tend) = if x = ">>" then (FileIO.appendLine(filename,tend); []) else raise error_table_closing_bracket_not_found
  | table ((x::y),filename,tend) = 
          let
           		val td = tabledata ((x::y),filename,tend)
           	in
           		td
           	end 	
end






































(*fun link [] = []
  | link (x::[]) = [x]
  | link (x::y) = 
    let
    		fun iden ([],z,c,s) = ([],z,c,s)  
    		  | iden (a::b,z,c,s) =
    		    let
    		     	fun checksp [] = []
                      | checksp (a::b) = if a = #" "  then checksp(b) else a::b
                    val a1 = iden(b,z,c,1)
                    val a2 = iden(b,z,c,2)
                    val a3 = iden(b,z,c,3)
                    val a4 = if not(null(checksp b)) then iden(tl(checksp b),z,c,3)
                             else ([],z,c,s)  		
    		     in
    		     	
                    print("s: " ^ (Int.toString(s))^" "^Char.toString(a)^ "\n");    		     	if s = 1 then if a = #"]" then (#1 a2,z,#3 a2,#4 a2) else (#1 a1,a::(#2 a1),#3 a1,#4 a1) else
    		     	if s = 2 then if a = #"(" then (#1 a3,#2 a3,#3 a3,#4 a3) else if a = #" " andalso not(null(checksp b)) andalso hd(checksp b) = #"(" then (#1 a4,#2 a4,#3 a4,#4 a4) else (b,z,c,2) else
    		        if a = #")" then (b,z,c,4) else (#1 a3,#2 a3,a::(#3 a3),#4 a3)
    		     	
    		     end 
    		val a = iden (y,[],[],1)
    		val b = (implode (#2 a),implode (#3 a))
    		val c = "<a href=\""^(#2 b)^"\">"^(#1 b)^"</a>"
    		val d = explode c 
    	in
    		print("x: "^Char.toString(x)^ "\n");
    		if x = #"[" then if (#4 a) = 4 then d@link(#1 a) else x::link(y) else x::link(y)
    	end*)	
