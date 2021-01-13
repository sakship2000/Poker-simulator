--AUTHOR: Sakshi Padhiar

module Poker where

import Data.Set as Set

--creates hand one using cards at even indices in the array
createHandOne (x:xs) = x:createHandTwo xs
createHandOne _ = []

--creates hand two using cards at odd indices in the array
createHandTwo (_:xs) = createHandOne xs
createHandTwo _ = []

--sorts a given array of numbers from smallest to largest
sortAscending :: (Ord a) => [a] -> [a]  
sortAscending [] = []  
sortAscending (x:xs) =   
    let smallerSorted = sortAscending [a | a <- xs, a <= x]  
        biggerSorted = sortAscending [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

--converts a given hand of integers into their associated card values
stringForm hand = 
    [ 
        if i > 0 && i <= 13
            then if (i `mod` 13) /= 0
                then show (i `mod` 13) ++ "C"
                else "13C"
        else if i > 13 && i <= 26
            then if (i `mod` 13) /= 0
                then show (i `mod` 13) ++ "D"
                else "13D" 
        else if i > 26 && i <= 39
            then if (i `mod` 13) /= 0
                then show (i `mod` 13) ++ "H"
                else "13H"
        else if i > 39 && i <= 52
            then if (i `mod` 13) /= 0
                then show (i `mod` 13) ++ "S"
                else "13S"
        else error "Invalid Card" 
        | i <- hand 
    ]

--mapping function that applies a given function to each element in the given list
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs  

--filters out the suits in a given hand, leaving just the values
isolateValues hand = map' init hand

--filters out the values in a given hand, leaving just the suits
isolateSuits (hand) = map' last hand

--checks if the given hand has a rank of flush 
flush hand = let suits = isolateSuits hand in Set.size (Set.fromList suits) <=1

--checks if the given hand has a rank of straight 
straight hand = 
    let ranks = sortAscending(isolateValues(hand))
    in 
        if head(ranks) == "1" && ((ranks !! 1 == "2") && (ranks !! 2 == "3") 
            && (ranks !! 3 == "4") && (ranks !! 4 == "5")) 
            then True
        else if head(ranks) == "1" && ((ranks !! 1 == "10") && (ranks !! 2 == "11") 
            && (ranks !! 3 == "12") && (ranks !! 4 == "13")) 
            then True
        else 
            let rank1 = succ (read (ranks !! 0) :: Integer)
                rank2 = succ (read (ranks !! 1) :: Integer)
                rank3 = succ (read (ranks !! 2) :: Integer)
                rank4 = succ (read (ranks !! 3) :: Integer)
                rank5 = succ (read (ranks !! 4) :: Integer)
            in 
                if ((read (ranks !! 1) :: Integer) == rank1) && ((read (ranks !! 2) :: Integer) == rank2) && 
                    ((read (ranks !! 3) :: Integer) == rank3) && ((read (ranks !! 4) :: Integer) == rank4) 
                    then True
                else False

--checks if the given hand has a rank of straight flush
straightFlush hand = flush hand && straight hand

--checks if the given hand has a rank of royal flush
royalFlush hand = ((sortAscending(isolateValues(hand)) !! 0) == "1") && ((sortAscending(isolateValues(hand)) !! 1) == "10") && flush hand && straight hand

--checks if the given hand has a rank of four of a kind
fourOfAKind hand = 
    let sortedHand = sortAscending(isolateValues(hand))
    in (((sortedHand !! 0) == (sortedHand !! 1)) &&
    (((sortedHand !! 1) == (sortedHand !! 2)) &&
    ((sortedHand !! 2) == (sortedHand !! 3)))) ||

    (((sortedHand !! 1) == (sortedHand !! 2)) && 
    (((sortedHand !! 2) == (sortedHand !! 3)) &&
    ((sortedHand !! 3) == (sortedHand !! 4))))

--checks if the given hand has a rank of full house
fullHouse hand = 
    let sortedHand = sortAscending(isolateValues(hand))
    in (((sortedHand !! 0) == (sortedHand !! 1)) && 
    (((sortedHand !! 1) == (sortedHand !! 2)) &&
    ((sortedHand !! 3) == (sortedHand !! 4)))) ||

    (((sortedHand !! 0) == (sortedHand !! 1)) && 
    (((sortedHand !! 2) == (sortedHand !! 3)) &&
    ((sortedHand !! 3) == (sortedHand !! 4))))

--checks if the given hand has a rank of three of a kind
threeOfAKind hand =
    let sortedHand = sortAscending(isolateValues(hand))
    in if fourOfAKind(hand) || fullHouse(hand)
        then False
        else (((sortedHand !! 0) == (sortedHand !! 1)) &&
            ((sortedHand !! 1) == (sortedHand !! 2))) ||
        
            ((((sortedHand !! 1) == (sortedHand !! 2)) &&
            ((sortedHand !! 2) == (sortedHand !! 3))) ||

            (((sortedHand !! 2) == (sortedHand !! 3)) &&
            ((sortedHand !! 3) == (sortedHand !! 4))))

--checks if the given hand has a rank of two pairs
twoPairs hand =
    let sortedHand = sortAscending(isolateValues(hand))
    in if fourOfAKind(hand) || (fullHouse(hand) || threeOfAKind(hand))
        then False
        else (((sortedHand !! 0) == (sortedHand !! 1)) &&
            ((sortedHand !! 2) == (sortedHand !! 3))) ||
            
            ((((sortedHand !! 0) == (sortedHand !! 1)) &&
            ((sortedHand !! 3) == (sortedHand !! 4))) ||
            
            (((sortedHand !! 1) == (sortedHand !! 2)) &&
            ((sortedHand !! 3) == (sortedHand !! 4))))

--checks if the given hand has a rank of one pair
onePair hand =
    let sortedHand = sortAscending(isolateValues(hand))
    in if (fourOfAKind(hand) || (fullHouse(hand) || threeOfAKind(hand))) || twoPairs(hand)
        then False
        else ((sortedHand !! 0) == (sortedHand !! 1)) ||
            (((sortedHand !! 1) == (sortedHand !! 2)) ||
            (((sortedHand !! 2) == (sortedHand !! 3)) ||
            ((sortedHand !! 3) == (sortedHand !! 4))))

--assigns an integer value to a given hand based on the rank it belongs to
rank :: [[Char]] -> Integer
rank hand = 
    if royalFlush hand 
        then 1
    else if straightFlush hand 
        then 2
    else if fourOfAKind hand 
        then 3
    else if fullHouse hand 
        then 4
    else if flush hand 
        then 5
    else if straight hand 
        then 6
    else if threeOfAKind hand 
        then 7
    else if twoPairs hand 
        then 8
    else if onePair hand 
        then 9
    else 10

--checks if given hand contains an ace, if yes it adds 14 to the end (helper function for highestCard)
aceTo14 hand = 
    if (hand !! 0) == "1"
        then tail(hand) ++ ["14"]
    else hand

--checks if given hand contains an ace, if yes it moves the ace to the end of the list 
--(helper function for final output of royalFlush)
acesToEnd hand = 
    if (isolateValues(hand) !! 0) == "1"
        then acesToEnd(tail(hand) ++ [hand !! 0])
    else hand

--converts a given hand to Int
toInt hand = [read x :: Int | x <- hand]

--breaks a tie between the two given hands (of rank royalFlush, straightFlush, flush, straight, twoPairs and onePair) by 
--determining the highest cards of each and comparing them. If all the values are identical, it calls highestSuitRank
highestCard hand1 hand2
    | (sortedHand1 !! 4) > (sortedHand2 !! 4) = hand1
    | (sortedHand1 !! 4) < (sortedHand2 !! 4) = hand2
    | (sortedHand1 !! 3) > (sortedHand2 !! 3) = hand1
    | (sortedHand1 !! 3) < (sortedHand2 !! 3) = hand2
    | (sortedHand1 !! 2) > (sortedHand2 !! 2) = hand1
    | (sortedHand1 !! 2) < (sortedHand2 !! 2) = hand2
    | (sortedHand1 !! 1) > (sortedHand2 !! 1) = hand1
    | (sortedHand1 !! 1) < (sortedHand2 !! 1) = hand2
    | (sortedHand1 !! 0) > (sortedHand2 !! 0) = hand1
    | (sortedHand1 !! 0) < (sortedHand2 !! 0) = hand2
    | otherwise = highestSuitRank hand1 hand2
    where sortedHand1 = toInt(aceTo14(sortAscending(isolateValues(hand1))))
          sortedHand2 = toInt(aceTo14(sortAscending(isolateValues(hand2))))

--breaks a tie between the two given hands (of rank fourOfAKind fullHouse and threeOfAKind) by determining 
--the highest cards of each and comparing them. If all the values are identical, it calls highestSuitRank
highestCard2 hand1 hand2
    | (sortedHand1 !! 2) > (sortedHand2 !! 2) = hand1
    | (sortedHand1 !! 2) < (sortedHand2 !! 2) = hand2
    | (sortedHand1 !! 4) > (sortedHand2 !! 4) = hand1
    | (sortedHand1 !! 4) < (sortedHand2 !! 4) = hand2
    | (sortedHand1 !! 0) > (sortedHand2 !! 0) = hand1
    | (sortedHand1 !! 0) < (sortedHand2 !! 0) = hand2
    | otherwise = highestSuitRank hand1 hand2
    where sortedHand1 = toInt(sortAscending(isolateValues(hand1)))
          sortedHand2 = toInt(sortAscending(isolateValues(hand2)))

--breaks a tie between the two given hands of rank flush, royalFlush or straightFlush based on their suits 
--precedence of suits being Spades > Hearts > Diamonds > Clubs
breakFlushTieSuit :: [[Char]] -> [[Char]] -> [[Char]]
breakFlushTieSuit hand1 hand2 =
    let h1suit = isolateSuits hand1
        h2suit = isolateSuits hand2
    in
        if h1suit == "CCCCC" then hand2
        else if h2suit == "CCCCC" then hand1
        else if h1suit == "SSSSS" then hand1
        else if h2suit == "SSSSS" then hand2
        else if (h1suit == "DDDDD") && (h2suit == "HHHHH") then hand2
        else if (h1suit == "HHHHH") && (h2suit == "DDDDD") then hand1
        else []

--breaks a tie between the two given hands of rank onePair based on their suits 
--precedence of suits being Spades > Hearts > Diamonds > Clubs
breakPairTieSuit :: [[Char]] -> [[Char]] -> [[Char]]
breakPairTieSuit hand1 hand2 = 
    let h1suit = isolateSuits hand1
        h1rank = isolateValues hand1
        h2rank = isolateValues hand2
    in
        if h1rank !! 0 == h1rank !! 1 
            then if show (h1suit !! 0) == "'S'" || show (h1suit !! 1) == "'S'"
                then hand1
                else hand2
        else if h1rank !! 1 == h1rank !! 2 
            then if show (h1suit !! 1) == "'S'" || show (h1suit !! 2) == "'S'"
                then hand1
                else hand2
        else if h1rank !! 2 == h1rank !! 3 
            then if show (h1suit !! 2) == "'S'" || show (h1suit !! 3) == "'S'"
                then hand1
                else hand2
        else if h1rank !! 3 == h1rank !! 4 
            then if show (h1suit !! 3) == "'S'" || show (h1suit !! 4) == "'S'"
                then hand1
                else hand2
        else []

--breaks a tie between the two given hands based on their suits at a given card
--precedence of suits being Spades > Hearts > Diamonds > Clubs
breakSuitTie :: [[Char]] -> [[Char]] -> Int -> [[Char]]
breakSuitTie hand1 hand2 card = 
    let h1suit = isolateSuits hand1
        h2suit = isolateSuits hand2
        h1card = show (h1suit !! card)
        h2card = show (h2suit !! card)
    in
        if h1card == "'C'" then hand2
        else if h2card == "'C'" then hand1
        else if h1card == "'S'" then hand1
        else if h2card == "'S'" then hand2
        else if h1card == "'D'" && h2card == "'H'" then hand2
        else if h1card == "'H'" && h2card == "'D'" then hand1
        else []

--breaks a tie between the two given hands of rank twoPair based on their suits 
--precedence of suits being Spades > Hearts > Diamonds > Clubs
breakTwoPairTieSuit :: [[Char]] -> [[Char]] -> [[Char]]
breakTwoPairTieSuit hand1 hand2 = 
    let h1suit = isolateSuits hand1
        h2suit = isolateSuits hand2
        h1rank = isolateValues hand1
        h2rank = isolateValues hand2
    in
        if (h1rank !! 0 == h1rank !! 1) && (h1rank !! 2 == h1rank !! 3)
            then if (show (h1suit !! 0) == "'S'" || show (h1suit !! 1) == "'S'") 
                && (show (h1suit !! 2) == "'S'" || show (h1suit !! 3) == "'S'") 
                then hand1 
                else breakSuitTie hand1 hand2 4
        else if (h1rank !! 0 == h1rank !! 1) && (h1rank !! 3 == h1rank !! 4)
            then if (show (h1suit !! 0) == "'S'" || show (h1suit !! 1) == "'S'") 
                && (show (h1suit !! 3) == "'S'" || show (h1suit !! 4) == "'S'") 
                then hand1 
                else breakSuitTie hand1 hand2 2
        else if (h1rank !! 1 == h1rank !! 2) && (h1rank !! 3 == h1rank !! 4)
            then if (show (h1suit !! 1) == "'S'" || show (h1suit !! 2) == "'S'") 
                && (show (h1suit !! 3) == "'S'" || show (h1suit !! 4) == "'S'") 
                then hand1 
                else breakSuitTie hand1 hand2 0
        else if (h2rank !! 0 == h2rank !! 1) && (h2rank !! 2 == h2rank !! 3)
            then if (show (h2suit !! 0) == "'S'" || show (h2suit !! 1) == "'S'") 
                && (show (h2suit !! 2) == "'S'" || show (h2suit !! 3) == "'S'") 
                then hand2 
                else breakSuitTie hand1 hand2 4
        else if (h2rank !! 0 == h2rank !! 1) && (h2rank !! 3 == h2rank !! 4)
            then if (show (h2suit !! 0) == "'S'" || show (h2suit !! 1) == "'S'") 
                && (show (h2suit !! 3) == "'S'" || show (h2suit !! 4) == "'S'") 
                then hand2 
                else breakSuitTie hand1 hand2 2
        else if (h2rank !! 1 == h2rank !! 2) && (h2rank !! 3 == h2rank !! 4)
            then if (show (h2suit !! 1) == "'S'" || show (h2suit !! 2) == "'S'") 
                && (show (h2suit !! 3) == "'S'" || show (h2suit !! 4) == "'S'") 
                then hand2
                else breakSuitTie hand1 hand2 0
        else []

--breaks a tie between the two given hands of rank straight based on their suits 
--precedence of suits being Spades > Hearts > Diamonds > Clubs
breakStraightTieSuit :: [[Char]] -> [[Char]] -> [[Char]]
breakStraightTieSuit hand1 hand2 =
    let h1suit = isolateSuits hand1
        h2suit = isolateSuits hand2
        h1rank = isolateValues hand1
        h2rank = isolateValues hand2
        h1HighestCard = show(h1suit !! 4)
        h2HighestCard = show(h2suit !! 4)
        h1Ace = show(h1suit !! 0)
        h2Ace = show(h2suit !! 0)
    in
        if (h1rank !! 0 == "1") && (h2rank !! 0 == "1") 
            then 
                if h1Ace == "'C'" then hand2
                else if h2Ace == "'C'" then hand1
                else if h1Ace == "'S'" then hand1
                else if h2Ace == "'S'" then hand2
                else if h1Ace == "'D'" && h2Ace == "'H'" then hand2
                else if h1Ace == "'H'" && h2Ace == "'D'" then hand1
                else []
        else if h1HighestCard == "'C'" then hand2
        else if h2HighestCard == "'C'" then hand1
        else if h1HighestCard == "'S'" then hand1
        else if h2HighestCard == "'S'" then hand2
        else if h1HighestCard == "'D'" && h2HighestCard == "'H'" then hand2
        else if h1HighestCard == "'H'" && h2HighestCard == "'D'" then hand1
        else []

--breaks a tie between two given hands of any rank based on their suits
--precedence of suits being Spades > Hearts > Diamonds > Clubs
highestSuitRank :: [[Char]] -> [[Char]] -> [[Char]]
highestSuitRank hand1 hand2 = 
    if (rank hand1 == 1 && rank hand2 == 1 )
        || (rank hand1 == 2 && rank hand2 == 2)
        || (rank hand1 == 5 && rank hand2 == 5)
        then breakFlushTieSuit hand1 hand2
    else if rank hand1 == 8 && rank hand2 == 8
        then breakPairTieSuit hand1 hand2
    else if rank hand1 == 9 && rank hand2 == 9
        then breakTwoPairTieSuit hand1 hand2
    else if rank hand1 == 6 && rank hand2 == 6
        then breakStraightTieSuit hand1 hand2
    else []

--driver function that takes in an array of integers, breaks it into two Poker hands
--then determines the winning hand based on their ranks
--if the ranks of both hands are the same, tie-breaking is implemented
deal array =
    let hand1 = stringForm(sortAscending(createHandOne(array)))
        hand2 = stringForm(sortAscending(createHandTwo(array)))
        handRank1 = rank(hand1)
        handRank2 = rank(hand2)
    in if handRank1 < handRank2
        then 
            if handRank1 == 1 
                then acesToEnd hand1
            else hand1
    else if handRank1 > handRank2
        then 
            if handRank2 == 1
                then acesToEnd hand2
            else hand2
    else if handRank1 == 10
            then highestCard hand1 hand2
        else if handRank1 == 1
            then acesToEnd (highestCard hand1 hand2)
        else if handRank1 == 2
            then highestCard hand1 hand2
        else if handRank1 == 5
            then highestCard hand1 hand2
        else if handRank1 == 6
            then highestCard hand1 hand2
        else if handRank1 == 3
            then highestCard2 hand1 hand2
        else if handRank1 == 4
            then highestCard2 hand1 hand2
        else if handRank1 == 7
            then highestCard2 hand1 hand2
    else highestSuitRank hand1 hand2