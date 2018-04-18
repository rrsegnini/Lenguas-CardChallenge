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
      (* | (x::tl, c, e) => if x = c then tl else x::remove_card(tl, c, e)*)		   
      | (x::tl, c, e) => if (card_color(x),card_value(x)) = (card_color(c),card_value(c)) then tl else x::remove_card(tl, c, e)
							
								
				      

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
	


	
											
fun officiate(card_list,move_list,goal:int)=
    let val held_cards = []
	val actual_score = 0
	fun main(card_list,move_list,held_cards)=
	    case move_list of
		[] => score(held_cards,goal)
	       |(move::moves_tail)  => ( case move of
				      Draw =>(case card_list of
						  [] =>  score(held_cards,goal)
						| (card::cards_tail) => if sum_cards(card::held_cards)>goal
									then score(card::held_cards,goal)
									else main(cards_tail,moves_tail,card::held_cards))						 
			             |Discard c  => main(card_list,moves_tail,remove_card(held_cards,c,IllegalMove)))
    in
	main(card_list,move_list,held_cards)

    end



	

fun card_value_1(c)=
    case c of(s, Num n) => n
	    | (s, Ace) => 1
	    | (s, r) => 10
			    
			    

			    
fun sum_cards_challenge(cs)=
    let fun sum_cards_aux(acc11,acc1, cs)=
	case cs of
	    [] => if acc11 > acc1 then acc11 else acc1
	  | (x::tl) => sum_cards_aux(card_value(x)+acc11, card_value_1(x)+acc1, tl)
    in
	sum_cards_aux(0, 0, cs)
    end
	
	
fun score_challenge(held_cards, goal)=
    let val sum = sum_cards_challenge(held_cards)
	val areSameColor = all_same_color(held_cards)
	val preliminary_score = if sum>goal then 3*(sum-goal)
				else (goal-sum)
    in
	if areSameColor then preliminary_score div 2 else  preliminary_score

    end
	


	
											
fun officiate_challenge(card_list,move_list,goal:int)=
    let val held_cards = []
	val actual_score = 0
	fun main(card_list,move_list,held_cards)=
	    case move_list of
		[] => score_challenge(held_cards,goal)
	       |(move::moves_tail)  => ( case move of
				      Draw =>(case card_list of
						  [] =>  score_challenge(held_cards,goal)
						| (card::cards_tail) => if sum_cards_challenge(card::held_cards)>goal
									then score_challenge(card::held_cards,goal)
									else main(cards_tail,moves_tail,card::held_cards))						 
			             |Discard c  => main(card_list,moves_tail,remove_card(held_cards,c,IllegalMove)))
    in
	main(card_list,move_list,held_cards)

    end	



fun check_discard_draw(card_list,goal,card)=
    let val saved_cards = []
	fun cdd_aux(card_list,goal,card,saved_cards)=
	    case card_list of
		[] => false
	      | (discard_card::card_tail) => if score(discard_card::card_tail,goal)=0
				      then true
				      else cdd_aux(card_tail,goal,card,saved_cards)
    in
	cdd_aux(card_list,goal,card,saved_cards)
    end

	
				     
	
fun careful_player(card_list,goal:int)=
    let val held_cards = []
	val move_list = []
	val actual_score = 0
	fun main(card_list,move_list,held_cards)=
	    case card_list of
		[] => move_list   
	      | (card::[]) => if (goal-sum_cards(held_cards))>10
			      then main([],Draw::move_list,card::held_cards)
			      else main([],move_list,held_cards)
	      | (card::card2::card_tail) => if score(held_cards, goal)=0
					    then move_list
					    else
						if (goal-sum_cards(held_cards))>10
						then main(card2::card_tail,Draw::move_list,card::held_cards)
						else
						    if check_discard_draw(held_cards,goal,card)
						    then main(card_tail,Discard card::Draw::move_list,card2::held_cards)
						    else main(card2::card_tail,Discard card::move_list,held_cards)
    in
	main(card_list,move_list,held_cards)
    end
	
					    

					    
