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

fun remove_card(cs, c, e)=
    case (cs, c, e) of
	([],c, e) => raise e 
      | (x::tl, c, e) => if x = c then tl else x::remove_card(tl, c, e)
							
								
				      

fun all_same_color(cs)=
    case cs of
	[] => true
	   | (x::[]) => true
	   | (x::y::tl) => if card_color(x) = card_color(y) then all_same_color(x::tl) else false

fun sum_cards(cs)=
    let fun sum_cards_aux(acc, cs)=
	case cs of
	    [] => acc 
	  | (x::tl) => sum_cards_aux(card_value(x)+acc, tl)
    in
	sum_cards_aux(0, cs)
    end
	

fun score(held_cards, goal)=
    let val sum = sum_cards(held_cards)
	val areSameColor = all_same_color(held_cards)
	val preliminary_score = if sum>goal then 3*(sum-goal)
				else (goal-sum)
    in
	if areSameColor then preliminary_score div 2 else  preliminary_score

    end
	
						  

	
											

    
	     
