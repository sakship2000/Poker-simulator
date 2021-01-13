//AUTHOR: Sakshi Padhiar

//creates hand one using cards at even indices in the array
fn make_hand_one(array: [u32;10]) -> [u32;5]{
    let hand_one: [u32;5] = [array[0],array[2],array[4],array[6],array[8]];
    hand_one
}

//creates hand two using cards at odd indices in the array
fn make_hand_two(array: [u32;10]) -> [u32;5]{
    let hand_two: [u32;5] = [array[1],array[3],array[5],array[7],array[9]];
    hand_two
}

//filters out the values in a given hand, leaving just the suits
fn isolate_suits(hand: [String;5]) -> [std::string::String; 5] {
    let a: char = hand[0].to_string().chars().last().unwrap();
    let b: char = hand[1].to_string().chars().last().unwrap();
    let c: char = hand[2].to_string().chars().last().unwrap();
    let d: char = hand[3].to_string().chars().last().unwrap();
    let e: char = hand[4].to_string().chars().last().unwrap();

    let suits: [String;5] = [a.to_string(),b.to_string(),c.to_string(),d.to_string(),e.to_string()];
    suits
}

//filters out the suits in a given hand, leaving just the values
fn isolate_values(hand: [String;5]) -> [u32; 5] {
    let a: &str = &hand[0].to_string()[..hand[0].len()-1];
    let b: &str = &hand[1].to_string()[..hand[1].len()-1];
    let c: &str = &hand[2].to_string()[..hand[2].len()-1];
    let d: &str = &hand[3].to_string()[..hand[3].len()-1];
    let e: &str = &hand[4].to_string()[..hand[4].len()-1];
    let values: [u32;5] = [a.parse::<u32>().unwrap(),b.parse::<u32>().unwrap(),
        c.parse::<u32>().unwrap(),d.parse::<u32>().unwrap(),e.parse::<u32>().unwrap()];
    values    
}

//sorts a given array of numbers from smallest to largest
fn sort_ascending(mut array: [u32;5]) -> [u32; 5] { //selection sort
    for i in 0..array.len() {
      let mut small = i;
      for j in (i + 1)..array.len() {
        if array[j] < array[small] {
          small = j;
        }
      }
      array.swap(small, i)
    }
    array
}

//converts a given hand of integers into their associated card values
fn string_form(hand: [u32; 5]) -> [String; 5] {
    let mut string_hand: [String; 5] = [" ".to_string()," ".to_string(), " ".to_string(), " ".to_string(), " ".to_string()];
    for i in 0..5 {
        if (hand[i] > 0) && (hand[i] <= 13) {
            let mut rank: u32 = hand[i] % 13;
            if rank == 0 {rank = 13;}
            string_hand[i] = format!("{}C", rank);
        } 
        else if (hand[i] > 13) && (hand[i] <= 26) {
            let mut rank: u32 = hand[i] % 13;
            if rank == 0 {rank = 13;}
            string_hand[i] = format!("{}D", rank);
        } 
        else if (hand[i] > 26) && (hand[i] <= 39) {
            let mut rank: u32 = hand[i] % 13;
            if rank == 0 {rank = 13;}
            string_hand[i] = format!("{}H", rank);
        } 
        else if (hand[i] > 39) && (hand[i] <= 52) {
            let mut rank: u32 = hand[i] % 13;
            if rank == 0 {rank = 13;}
            string_hand[i] = format!("{}S", rank);
        }
        else {
            println!("ERROR: invalid integer");
        }
    }
    string_hand
}

//checks if the given hand has a rank of flush 
fn flush(hand: [String;5]) -> bool {
    let suits: [String;5] = isolate_suits(hand);
    if suits[0] == suits[1] && suits[1] == suits[2] && suits[2] == suits[3] && suits[3] == suits[4] && suits[4] == suits[0] {
        true
    }
    else{
        false
    }
}

//checks if the given hand has a rank of straight 
fn straight(hand: [String;5]) -> bool {
    let values: [u32;5] = sort_ascending(isolate_values(hand));
    if (values[0] == 1) && (values[1] == 2 && values[2] == 3 && values[3] == 4 && values[4] == 5){
        true
    }
    else if (values[0] == 1) && ((values[1] == 10) && (values[2] == 11) && (values[3] == 12) && (values[4] == 13)) {
        true
    }
    else if values[1] == values[0] + 1 && values[2] == values[1] + 1 && values[3] == values[2] + 1 && values[4] == values[3] + 1{
        true
    }
    else{
        false
    }
}

//checks if the given hand has a rank of straight flush
fn straight_flush(hand: [String;5]) -> bool {
    let hand_clone: [String;5] = hand.clone();
    if (straight(hand) == true) && (flush(hand_clone) == true){
        true
    }
    else{
        false
    }
}

//checks if the given hand has a rank of royal flush
fn royal_flush(hand: [String;5]) -> bool {
    let hand_clone1: [String;5] = hand.clone();
    let hand_clone2: [String;5] = hand.clone();
    let values: [u32;5] = sort_ascending(isolate_values(hand));
    if (values[0] == 1 && values[1] == 10) && (flush(hand_clone1) == true && straight(hand_clone2)){
        true
    }
    else{
        false
    }
}

//checks if the given hand has a rank of four of a kind
fn four_of_a_kind(hand: [String; 5]) -> bool {
    let sorted_hand: [u32; 5] = sort_ascending(isolate_values(hand));
    (((sorted_hand[0]) == (sorted_hand[1])) &&
    (((sorted_hand[1]) == (sorted_hand[2])) &&
    ((sorted_hand[2]) == (sorted_hand[3])))) ||

    (((sorted_hand[1]) == (sorted_hand[2])) && 
    (((sorted_hand[2]) == (sorted_hand[3])) &&
    ((sorted_hand[3]) == (sorted_hand[4]))))
}

//checks if the given hand has a rank of full house
fn full_house(hand: [String; 5]) -> bool {
    let sorted_hand: [u32; 5] = sort_ascending(isolate_values(hand));
    (((sorted_hand[0]) == (sorted_hand[1])) && 
    (((sorted_hand[1]) == (sorted_hand[2])) &&
    ((sorted_hand[3]) == (sorted_hand[4])))) ||

    (((sorted_hand[0]) == (sorted_hand[1])) && 
    (((sorted_hand[2]) == (sorted_hand[3])) &&
    ((sorted_hand[3]) == (sorted_hand[4]))))
}

//checks if the given hand has a rank of three of a kind
fn three_of_a_kind(hand: [String; 5]) -> bool {
    let hand_clone1: [String; 5] = hand.clone();
    let hand_clone2: [String; 5] = hand.clone();
    let sorted_hand: [u32; 5] = sort_ascending(isolate_values(hand));
    if four_of_a_kind(hand_clone1) || full_house(hand_clone2) {
        false
    }
    else {
        (((sorted_hand[0]) == (sorted_hand[1])) &&
        ((sorted_hand[1]) == (sorted_hand[2]))) ||
    
        ((((sorted_hand[1]) == (sorted_hand[2])) &&
        ((sorted_hand[2]) == (sorted_hand[3]))) ||

        (((sorted_hand[2]) == (sorted_hand[3])) &&
        ((sorted_hand[3]) == (sorted_hand[4]))))
    }
}

//checks if the given hand has a rank of two pairs
fn two_pairs(hand: [String; 5]) -> bool {
    let hand_clone1: [String; 5] = hand.clone();
    let hand_clone2: [String; 5] = hand.clone();
    let hand_clone3: [String; 5] = hand.clone();
    let sorted_hand: [u32; 5] = sort_ascending(isolate_values(hand));
    if four_of_a_kind(hand_clone1) || (full_house(hand_clone2) || three_of_a_kind(hand_clone3)) {
        false
    }
    else {
        (((sorted_hand[0]) == (sorted_hand[1])) &&
        ((sorted_hand[2]) == (sorted_hand[3]))) ||
        
        ((((sorted_hand[0]) == (sorted_hand[1])) &&
        ((sorted_hand[3]) == (sorted_hand[4]))) ||
        
        (((sorted_hand[1]) == (sorted_hand[2])) &&
        ((sorted_hand[3]) == (sorted_hand[4]))))
    }
}

//checks if the given hand has a rank of one pair
fn one_pair(hand: [String; 5]) -> bool {
    let hand_clone1: [String; 5] = hand.clone();
    let hand_clone2: [String; 5] = hand.clone();
    let hand_clone3: [String; 5] = hand.clone();
    let hand_clone4: [String; 5] = hand.clone();
    let sorted_hand: [u32; 5] = sort_ascending(isolate_values(hand));
    if (four_of_a_kind(hand_clone1) || (full_house(hand_clone2) || three_of_a_kind(hand_clone3))) || two_pairs(hand_clone4) {
        false
    }
    else {
        ((sorted_hand[0]) == (sorted_hand[1])) ||
        (((sorted_hand[1]) == (sorted_hand[2])) ||
        (((sorted_hand[2]) == (sorted_hand[3])) ||
        ((sorted_hand[3]) == (sorted_hand[4]))))
    }
}

//assigns an integer value to a given hand based on the rank it belongs to
fn rank(hand: [String;5]) -> u32 {
    let hand_clone1: [String;5] = hand.clone();
    let hand_clone2: [String;5] = hand.clone();
    let hand_clone3: [String;5] = hand.clone();
    let hand_clone4: [String;5] = hand.clone();
    let hand_clone5: [String;5] = hand.clone();
    let hand_clone6: [String;5] = hand.clone();
    let hand_clone7: [String;5] = hand.clone();
    let hand_clone8: [String;5] = hand.clone();
    if royal_flush(hand){1} 
    else if straight_flush(hand_clone1){2}
    else if four_of_a_kind(hand_clone2){3}
    else if full_house(hand_clone3){4}
    else if flush(hand_clone4){5}
    else if straight(hand_clone5){6}
    else if three_of_a_kind(hand_clone6){7}
    else if two_pairs(hand_clone7){8}
    else if one_pair(hand_clone8){9}
    else{10}
}

//breaks a tie between the two given hands of rank flush, royalFlush or straightFlush based on their suits 
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn break_flush_tie_suit (hand1: [String;5], hand2: [String;5]) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();
    
    let suits1: [String;5] = isolate_suits(hand1);
    let suits2: [String;5] = isolate_suits(hand2);

    if suits1[0] == "C" {hand2_clone1}
    else if suits2[0] == "C" {hand1_clone1}
    else if suits1[0] == "S" {hand1_clone2}
    else if suits2[0] == "S" {hand2_clone2}
    else if suits1[0] == "D" && suits2[0] == "H" {hand2_clone3}
    else {hand1_clone3} //else if suits1[0] == "H" && suits2[0] == "D" 
}

//breaks a tie between the two given hands based on their suits at a given card
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn break_suit_tie (hand1: [String;5], hand2: [String;5], card: usize) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();

    let suits1: [String;5] = isolate_suits(hand1);
    let suits2: [String;5] = isolate_suits(hand2);

    if suits1[card] == "C" {hand2_clone1}
    else if suits2[card] == "C" {hand1_clone1}
    else if suits1[card] == "S" {hand1_clone2}
    else if suits2[card] == "S" {hand2_clone2}
    else if suits1[card] == "D" && suits2[card] == "H" {hand2_clone3}
    else {hand1_clone3} //else if suits1[0] == "H" && suits2[0] == "D" 
}

//breaks a tie between the two given hands of rank onePair based on their suits 
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn break_pair_tie_suit (hand1: [String;5], hand2: [String;5]) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand1_clone4: [String;5] = hand1.clone();
    let hand1_clone5: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();
    let hand2_clone4: [String;5] = hand2.clone();

    let suits1: [String;5] = isolate_suits(hand1);
    let values1: [u32;5] = isolate_values(hand1_clone1);

    if values1[0] == values1[1]{
        if suits1[0] == "S" || suits1[1] == "S" {hand1_clone2}
        else {hand2_clone1}
    }
    else if values1[1] == values1[2]{
        if suits1[1] == "S" || suits1[2] == "S" {hand1_clone3}
        else {hand2_clone2}
    }
    else if values1[2] == values1[3]{
        if suits1[2] == "S" || suits1[3] == "S" {hand1_clone4}
        else {hand2_clone3}
    }
    else{
        if suits1[3] == "S" || suits1[4] == "S" {hand1_clone5}
        else {hand2_clone4}
    }
}

//breaks a tie between the two given hands of rank twoPair based on their suits 
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn break_two_pair_tie_suit (hand1: [String;5], hand2: [String;5]) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand1_clone4: [String;5] = hand1.clone();
    let hand1_clone5: [String;5] = hand1.clone();
    let hand1_clone6: [String;5] = hand1.clone();
    let hand1_clone7: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();
    let hand2_clone4: [String;5] = hand2.clone();
    let hand2_clone5: [String;5] = hand2.clone();
    let hand2_clone6: [String;5] = hand2.clone();

    let suits1: [String;5] = isolate_suits(hand1);
    let suits2: [String;5] = isolate_suits(hand2);
    let values1: [u32;5] = isolate_values(hand1_clone1);

    if (values1[0] == values1[1]) && (values1[2] == values1[3]){
        if (suits1[0] == "S" || suits1[1] == "S") && (suits1[2] == "S" || suits1[3] == "S") {hand1_clone2}
        else if  (suits2[0] == "S" || suits2[1] == "S") && (suits2[2] == "S" || suits2[3] == "S") {hand2_clone1}
        else {break_suit_tie(hand1_clone3,hand2_clone2,4)}
    }
    else if (values1[0] == values1[1]) && (values1[3] == values1[4]){
        if (suits1[0] == "S" || suits1[1] == "S") && (suits1[3] == "S" || suits1[4] == "S") {hand1_clone4}
        else if (suits2[0] == "S" || suits2[1] == "S") && (suits2[3] == "S" || suits2[4] == "S") {hand2_clone3}
        else {break_suit_tie(hand1_clone5,hand2_clone4,2)}
    }
    else{
        if (suits1[1] == "S" || suits1[2] == "S") && (suits1[3] == "S" || suits1[4] == "S") {hand1_clone6}
        else if (suits2[1] == "S" || suits2[2] == "S") && (suits2[3] == "S" || suits2[4] == "S") {hand2_clone5}
        else {break_suit_tie(hand1_clone7,hand2_clone6,0)}
    }
}

//breaks a tie between the two given hands of rank straight based on their suits 
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn break_straight_tie_suit (hand1: [String;5], hand2: [String;5]) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();

    let values1: [u32;5] = isolate_values(hand1);
    let values2: [u32;5] = isolate_values(hand2);
    
    if (values1[0] == 1) && (values2[0] == 1){
        break_suit_tie(hand1_clone1,hand2_clone1,0)
    }
    else{
        break_suit_tie(hand1_clone2,hand2_clone2,4)
    }
}

//breaks a tie between two given hands of any rank based on their suits
//precedence of suits being Spades > Hearts > Diamonds > Clubs
fn highest_suit_rank (hand1: [String;5], hand2: [String;5]) -> [String;5]{
    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand1_clone4: [String;5] = hand1.clone();
    let hand1_clone5: [String;5] = hand1.clone();
    let hand1_clone6: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();
    let hand2_clone4: [String;5] = hand2.clone();
    let hand2_clone5: [String;5] = hand2.clone();
    let hand2_clone6: [String;5] = hand2.clone();

    if rank(hand1) == 6 && rank(hand2) == 6 {break_straight_tie_suit(hand1_clone1, hand2_clone1)}
    else if rank(hand1_clone2) == 8 && rank(hand2_clone2) == 8 {break_pair_tie_suit(hand1_clone3, hand2_clone3)}
    else if rank(hand1_clone4) == 9 && rank(hand2_clone4) == 9 {break_two_pair_tie_suit(hand1_clone5, hand2_clone5)}
    else {break_flush_tie_suit(hand1_clone6, hand2_clone6)}
}

//checks if given hand contains an ace, if yes it adds 14 to the end (helper function for highestCard)
//(helper function for final output of royalFlush)
fn ace_to_14(hand: [u32; 5]) -> [u32; 5] {
    let mut hand_clone: [u32; 5] = hand.clone();
    if hand_clone[0] == 1 {
        for i in 0..4 {
            hand_clone[i] = hand_clone[i+1].clone();
        }
        hand_clone[4] = 14;
        ace_to_14(hand_clone)
    }
    else {
        hand_clone
    }
}

//breaks a tie between the two given hands (of rank royalFlush, straightFlush, flush, straight, twoPairs and onePair) by 
//determining the highest cards of each and comparing them. If all the values are identical, it calls highestSuitRank
fn highest_card(hand1: [String; 5], hand2: [String; 5]) -> [String; 5] {
    let hand1_clone: [String; 5] = hand1.clone();
    let hand2_clone: [String; 5] = hand2.clone();
    let sorted_hand1: [u32; 5] = ace_to_14(sort_ascending(isolate_values(hand1)));
    let sorted_hand2: [u32; 5] = ace_to_14(sort_ascending(isolate_values(hand2)));
    for i in (0..5).rev() {
        if sorted_hand1[i] > sorted_hand2[i] {
            return hand1_clone;
        }
        else if sorted_hand1[i] < sorted_hand2[i] {
            return hand2_clone;
        }
    }
    highest_suit_rank(hand1_clone, hand2_clone)
}

//breaks a tie between the two given hands (of rank fourOfAKind fullHouse and threeOfAKind) by determining 
//the highest cards of each and comparing them. If all the values are identical, it calls highestSuitRank
fn highest_card2(hand1: [String; 5], hand2: [String; 5]) -> [String; 5] {
    let hand1_clone: [String; 5] = hand1.clone();
    let hand2_clone: [String; 5] = hand2.clone();
    let sorted_hand1: [u32; 5] = ace_to_14(sort_ascending(isolate_values(hand1)));
    let sorted_hand2: [u32; 5] = ace_to_14(sort_ascending(isolate_values(hand2)));
    if sorted_hand1[2] > sorted_hand2[2] {
        return hand1_clone;
    }
    else if sorted_hand1[2] < sorted_hand2[2] {
        return hand2_clone;
    } 
    else if sorted_hand1[4] > sorted_hand2[4] {
        return hand1_clone;
    }
    else if sorted_hand1[4] < sorted_hand2[4] {
        return hand2_clone;
    }
    else if sorted_hand1[0] > sorted_hand2[0] {
        return hand1_clone;
    }
    else if sorted_hand1[0] < sorted_hand2[0] {
        return hand2_clone;
    }
    else {
        highest_suit_rank(hand1_clone, hand2_clone)
    }
}

//driver function that takes in an array of integers, breaks it into two Poker hands
//then determines the winning hand based on their ranks
//if the ranks of both hands are the same, tie-breaking is implemented
fn deal (array: [u32;10]) -> [String;5]{
    let hand1: [String;5] = string_form(sort_ascending(make_hand_one(array)));
    let hand2: [String;5] = string_form(sort_ascending(make_hand_two(array)));

    let hand1_clone1: [String;5] = hand1.clone();
    let hand1_clone2: [String;5] = hand1.clone();
    let hand1_clone3: [String;5] = hand1.clone();
    let hand1_clone4: [String;5] = hand1.clone();
    let hand1_clone5: [String;5] = hand1.clone();
    let hand1_clone6: [String;5] = hand1.clone();
    let hand1_clone7: [String;5] = hand1.clone();
    let hand1_clone8: [String;5] = hand1.clone();
    let hand1_clone9: [String;5] = hand1.clone();
    let hand1_clone10: [String;5] = hand1.clone();
    let hand2_clone1: [String;5] = hand2.clone();
    let hand2_clone2: [String;5] = hand2.clone();
    let hand2_clone3: [String;5] = hand2.clone();
    let hand2_clone4: [String;5] = hand2.clone();
    let hand2_clone5: [String;5] = hand2.clone();
    let hand2_clone6: [String;5] = hand2.clone();
    let hand2_clone7: [String;5] = hand2.clone();
    let hand2_clone8: [String;5] = hand2.clone();
    let hand2_clone9: [String;5] = hand2.clone();
    let hand2_clone10: [String;5] = hand2.clone();

    let hand1_rank: u32 = rank(hand1);
    let hand2_rank: u32 = rank(hand2);

    if hand1_rank < hand2_rank {hand1_clone1}
    else if hand1_rank > hand2_rank  {hand2_clone1}
    else if hand1_rank == 10 {highest_card(hand1_clone2, hand2_clone2)}
    else if hand1_rank == 1 {highest_card(hand1_clone3,hand2_clone3)}
    else if hand1_rank == 2 {highest_card(hand1_clone4,hand2_clone4)}
    else if hand1_rank == 5 {highest_card(hand1_clone5,hand2_clone5)}
    else if hand1_rank == 6 {highest_card(hand1_clone6,hand2_clone6)}
    else if hand1_rank == 3 {highest_card2(hand1_clone7,hand2_clone7)}
    else if hand1_rank == 4 {highest_card2(hand1_clone8,hand2_clone8)}
    else if hand1_rank == 7 {highest_card2(hand1_clone9,hand2_clone9)}
    else {highest_suit_rank(hand1_clone10,hand2_clone10)}


}