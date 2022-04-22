
use "fileIO.sml";
use "stdIO.sml";
val maxMemSize=256
val mem = Array.array (maxMemSize, 0);(*Memory array initialised to all 0 values*)
val x = FileIO.readLines(Stdio.readln());(*Quadruples read as list of strings*)

fun remCharR (c, s) =(*function to remove occurence of char c in string s*)
    let fun rem [] = []
          | rem (d::cs) =
              if c = d
              then rem cs
              else d::rem cs
    in implode (rem (explode s)) end
    
fun iscomma(x)=(*returns true if x is a comma char*)
    if Char.compare(x,#",")=EQUAL then true
    else false;

fun quadtoarray(x)=(*converts quadruple string without ( and ) , to array of integers*)
    let val y=String.tokens iscomma x
    in y
end;

fun mapping0 [] = [](*Takes list of quadruples in string form as parameter and returns list of list of int*)
         |   mapping0(x::xs) = 
         let val y= remCharR (#")", x)
             val z=remCharR(#"(",y)
             val z2=quadtoarray(z)
         in z2::mapping0(xs)
    end;


fun stringtoint(x)=(*converts string to its int value*)
	let val y=valOf(Int.fromString(x))
	in y
end;


  

fun mapping2 [] = [](*applies stringtoint to every element of list*)
         |   mapping2(x::xs) = 
         let val y= stringtoint(x) 
         in y::mapping2(xs)
    end;
    
fun mapping3 [] = [](*applies mapping 2 to every element of list*)
         |   mapping3(x::xs) = 
         let val y= mapping2(x) 
         in y::mapping3(xs)
    end;    



val code=mapping3(mapping0(x));(*code array stores the bdim file as a list of list of int*)

fun andd(x,y)=(*logical and applied to int*)
    if ((x=0)orelse(y=0)) then 0
    else 1;
    

fun orr(x,y)=(*logical or applied to int*)
    if ((x=0)andalso(y=0)) then 0
    else 1;

fun nott(x)=(*logical not applied to int*)
    if(x=0) then 1
    else 0;


exception Outofbounds;
fun interpret(t)=(*actual interpret*)

    let val x=List.nth(code,t) (*code[t] stored in var x*)
        val i=List.nth(x,1)    (*code[t][1] stored in var i*)
        val j=List.nth(x,2)
        val k=List.nth(x,3)
    in
    if ((t>=List.length(code))orelse(t<0)) then (print("code array index out of bounds");raise Outofbounds) (*this is an error if t is greater than no. of lines of .bdim *)
    else
    (*now the cases of the first number of the quadruple starts. This is implementing the table of opcode. This function uses tail recursion*)
    case List.nth(x,0) of
    0=> print("program halted"^"\n")
    | 1=> (print("input: ");Array.update(mem,k,stringtoint(Stdio.readln()));interpret(t+1))
    | 2=>( Array.update(mem,k,Array.sub(mem, i));interpret(t+1))
    | 3=> (Array.update(mem,k,nott(Array.sub(mem, i)));interpret(t+1))
    | 4=>(Array.update(mem,k,orr(Array.sub(mem, i),Array.sub(mem, j)));interpret(t+1))
    | 5=>(Array.update(mem,k,andd(Array.sub(mem, i),Array.sub(mem, j)));interpret(t+1))
    | 6=>(Array.update(mem,k,Array.sub(mem, i)+Array.sub(mem, j));interpret(t+1))
    | 7=>(Array.update(mem,k,Array.sub(mem, i)-Array.sub(mem, j));interpret(t+1))
    | 8=>(Array.update(mem,k,Array.sub(mem, i)*Array.sub(mem, j));interpret(t+1))
    | 9=>(Array.update(mem,k,(Array.sub(mem, i))div(Array.sub(mem, j)));interpret(t+1))
    | 10=>(Array.update(mem,k,Array.sub(mem, i) mod Array.sub(mem, j));interpret(t+1))
    | 11=> if (Array.sub(mem, i)=Array.sub(mem, j)) then (Array.update(mem,k,1);interpret(t+1))
            else (Array.update(mem,k,0);interpret(t+1))
    | 12=> if (Array.sub(mem, i)>Array.sub(mem, j)) then (Array.update(mem,k,1);interpret(t+1))
            else (Array.update(mem,k,0);interpret(t+1))
    | 13=> if  (Array.sub(mem, i)=1) then interpret(k)
            else interpret(t+1)
    | 14=> interpret(k)
    | 15=> (print("output: "^Int.toString (Array.sub(mem, i))^"\n");interpret(t+1))
    | 16=> (Array.update(mem,k,i);interpret(t+1))
    
    end;




interpret(0);(*The interpreter executes a function interpret which evaluates the code starting from the instruction
code[0] as per the explanations given in the instruction set.*)


    
    


    
    




