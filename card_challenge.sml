(* Card Challenge *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw
exception IllegalMove


fun card_color(c)=
    case c of(Spades, r) => Black
	    | (Clubs, r) => Black
	    | (Hearts, r) => Red
	    | (Diamonds, r) => Red  
			 

				   
fun card_value(c)=
    case c of(s, Num n) => n
	    | (s, Ace) => 11
	    | (s, r) => 10
    
	     
