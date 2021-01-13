deal: array
	"the driver method"
			
    | hand1 hand2 stringHand1 stringHand2 handRank1 handRank2 |
    
    "Create Hands"
    hand1 := (self createHandOne: array) asSortedCollection.
    hand2 := (self createHandTwo: array) asSortedCollection.

    "Convert cards to string form"
    stringHand1 := (self stringForm: hand1) asOrderedCollection.
    stringHand2 := (self stringForm: hand2) asOrderedCollection.

    "Sort the hands"
    hand1 sort.
    hand2 sort.

   "Get rank of hand"
    handRank1 := self rank: stringHand1.
    handRank2 := self rank: stringHand2.

    "Tie-breaking"

    (handRank1 == handRank2) ifTrue: [
			(handRank1 == 10)ifTrue: [ ^self highestCard: stringHand1  vs: stringHand2 ].
			(handRank1 == 1) ifTrue: [ ^self highestCard: stringHand1  vs: stringHand2 ].
			(handRank1 == 2) ifTrue: [ ^self highestCard: stringHand1  vs: stringHand2 ].
			(handRank1 == 5) ifTrue: [ ^self highestCard: stringHand1  vs: stringHand2 ].
			(handRank1 == 6) ifTrue: [ ^self highestCard: stringHand1  vs: stringHand2 ].
			(handRank1 == 3) ifTrue: [ ^self highestCard2: stringHand1  vs: stringHand2 ].
			(handRank1 == 4) ifTrue: [ ^self highestCard2: stringHand1  vs: stringHand2 ].
			(handRank1 == 7) ifTrue: [ ^self highestCard2: stringHand1  vs: stringHand2 ].
	]
	ifFalse: [
		   "Print the winnner"
    (handRank1 > handRank2) ifTrue: [^stringHand1 asArray]
    ifFalse: [ ^stringHand2 asArray]. 
	].

✓✓ createHandOne: array
	"creates hand one using alternate cards in the array"
	
	|h1|
	h1 := OrderedCollection new.
	1 to: 9 by: 2 do: [ :i | h1 addLast: (array at: i) ].
	^h1

✓✓ createHandTwo: array
	"creates hand two using alternate cards in the array"
			
	| h2 |
	h2 := OrderedCollection new.
	2 to: 10 by: 2 do: [ :i | h2 addLast: (array at: i) ].
	^h2

✓✓ isolateSuits: hand
	"takes in the hand in string form and returns an array with just the suits"
			
	|elem suits|
	suits := OrderedCollection new.
	1 to: (hand size) do: [ :i | elem := (hand at: i). 
									suits addLast: (elem copyFrom: (elem size) to: (elem size))]. 
	^suits asArray.

✓✓ sortAscending: array
	"sorts a hand in ascending order"

	| sortedHand temp|
	sortedHand := SortedCollection new.
	array do: [ :i | temp := i allButLast. sortedHand addLast: temp asInteger].
	sortedHand sort.
	^ sortedHand.

✓✓ sortDescending: array
	"sorts a hand in descending order"

	|sortedHand temp|
	sortedHand := SortedCollection new.
	array do: [ :i | temp := i allButLast. sortedHand addLast: temp asInteger].
	sortedHand sortBlock: [ :a :c | a >= c ].
	^ sortedHand.

✓ stringForm: array
	"Change cards of form 52 to form 13S"
			
	| newArray temp|
	newArray := OrderedCollection new.
	array do: [ :a | 
		((a > 0) and: (a <= 13))
			ifTrue: [
				temp := a rem: 13.
				temp == 0 ifTrue: [temp := 13]. 
				newArray addLast: temp asString, 'C'
			].
		((a > 13) and: (a <= 26))
			ifTrue: [
				temp := a rem: 13.
				temp == 0 ifTrue: [temp := 13]. 
				newArray addLast: temp asString, 'D'
			].
		((a > 26) and: (a <= 39))
			ifTrue: [
				temp := a rem: 13.
				temp == 0 ifTrue: [temp := 13]. 
				newArray addLast: temp asString, 'H'
			].
		((a > 39) and: (a <= 52))
			ifTrue: [temp := a rem: 13.
				temp == 0 ifTrue: [temp := 13]. 
				newArray addLast: temp asString, 'S'
			].
	].
	
	^ newArray.

✓✓ flush: hand
	"comment stating purpose of instance-side message"
	"scope: class-variables  &  instance-variables"	
	|suits numSuits |
	suits := self isolateSuits: hand.

	numSuits := (suits asSet) size.

	( numSuits == 1) ifTrue: [ ^true ] 
		  				  ifFalse: [ ^false ].		

✓ fourOfAKind: hand
	"Check if a hand is four-of-a-kind"
	"scope: class-variables  &  instance-variables"	
			
	|sortedHand |
	
	sortedHand := self sortAscending: hand.
	
	^ (((sortedHand at: 1) == (sortedHand at: 2)) and: 
      (((sortedHand at: 2) == (sortedHand at: 3)) and:
	  ((sortedHand at: 3) == (sortedHand at: 4)))) or:
							
	  (((sortedHand at: 2) == (sortedHand at: 3)) and: 
	  (((sortedHand at: 3) == (sortedHand at: 4)) and:
	  ((sortedHand at: 4) == (sortedHand at: 5))))

✓ fullHouse: hand
	"Check if a hand is full house"
	"scope: class-variables  &  instance-variables"	
			
	|sortedHand |
	
	sortedHand := self sortAscending: hand.
	
	^ (((sortedHand at: 1) == (sortedHand at: 2)) and: 
	  (((sortedHand at: 2) == (sortedHand at: 3)) and:
	  ((sortedHand at: 4) == (sortedHand at: 5)))) or:
							
	  (((sortedHand at: 1) == (sortedHand at: 2)) and: 
	  (((sortedHand at: 3) == (sortedHand at: 4)) and:
	  ((sortedHand at: 4) == (sortedHand at: 5))))

✓ onePair: hand
	"Check if a hand has two pairs"
	"scope: class-variables  &  instance-variables"	
			
	|sortedHand |
	
	sortedHand := self sortAscending: hand.
	
	((self fourOfAKind: hand) or: 
	((self fullHouse: hand) or: 
	((self threeOfAKind: hand) or: 
	 (self twoPairs: hand))))
		ifTrue: [ ^ false ]
		ifFalse: [ 
			^ ((sortedHand at: 1) == (sortedHand at: 2)) or:
			  (((sortedHand at: 2) == (sortedHand at: 3)) or:
			  (((sortedHand at: 3) == (sortedHand at: 4)) or:
			  ((sortedHand at: 4) == (sortedHand at: 5)))).
		]

✓✓ rank: hand 
	"comment stating purpose of instance-side message"
	"scope: class-variables  &  instance-variables"
	(self royalFlush: hand ) ifTrue: [ ^1 ]
	ifFalse:	[(self straightFlush: hand ) ifTrue: [ ^2 ]
		ifFalse: [(self fourOfAKind: hand) ifTrue: [ ^3 ]
			ifFalse: [ (self fullHouse: hand) ifTrue: [ ^4 ]
				ifFalse: [ (self flush: hand) ifTrue: [ ^5 ]
					ifFalse: [ (self straight: hand) ifTrue: [ ^6 ]
						ifFalse: [ (self threeOfAKind: hand) ifTrue: [ ^7 ]
							ifFalse: [ (self twoPairs: hand) ifTrue: [ ^8 ]
								ifFalse: [ (self onePair: hand) ifTrue: [ ^9 ]
									ifFalse: [ ^10 ] 
									]
								] 
							]
						]
					]
				]
			]
		]

✓✓ royalFlush: hand
	"comment stating purpose of instance-side message"
	"scope: class-variables  &  instance-variables"	
	| sortedHand highestCard|
	sortedHand := self sortAscending: hand.
	highestCard := sortedHand at: 1.
	(highestCard == 1) ifTrue: 
	[ ^(self flush: hand) and: (self straight: hand) ]
	ifFalse: [ ^false ].

✓✓ straight: hand
	"comment stating purpose of instance-side message"
	"scope: class-variables  &  instance-variables"	
	|sortedHand testRank|
	sortedHand := self sortAscending: hand.
	((sortedHand at: 1) == 1) ifTrue: [
		^ ((((sortedHand at: 2) == 2) and: ((sortedHand at: 3) == 3)) and: 
		(((sortedHand at: 4) == 4) and: ((sortedHand at: 5) == 5))) or:
		((((sortedHand at: 2) == 10) and: ((sortedHand at: 3) == 11)) and: 
		(((sortedHand at: 4) == 12) and: ((sortedHand at: 5) == 13)))
		]
	ifFalse: [  
		testRank := ((sortedHand at: 1) asInteger) + 1.
		sortedHand do: [ :i |
			(i ~~ testRank)	ifFalse: [ ^false ].
				testRank := (testRank + 1).
			].
			^true.
		].	 

✓✓ straightFlush: hand
	"comment stating purpose of instance-side message"
	"scope: class-variables  &  instance-variables"	
	((self flush: hand) and: (self straight: hand)) ifTrue: [ ^true]
	ifFalse: [ ^false ].

✓ threeOfAKind: hand
	"Check if a hand is three-of-a-kind"
	"scope: class-variables  &  instance-variables"	
			
	|sortedHand |
	
	sortedHand := self sortAscending: hand.
	
	((self fourOfAKind: hand) or: (self fullHouse: hand))
		ifTrue: [ ^ false ]
		ifFalse: [ 
			^ (((sortedHand at: 1) == (sortedHand at: 2)) and:
			  ((sortedHand at: 2) == (sortedHand at: 3))) or:
			
			  ((((sortedHand at: 2) == (sortedHand at: 3)) and:
			  ((sortedHand at: 3) == (sortedHand at: 4))) or:
			
			  (((sortedHand at: 3) == (sortedHand at: 4)) and:
			  ((sortedHand at: 4) == (sortedHand at: 5)))).
		]

✓ twoPairs: hand
	"Check if a hand has two pairs"
	"scope: class-variables  &  instance-variables"	
			
	|sortedHand |
	
	sortedHand := self sortAscending: hand.
	
	((self fourOfAKind: hand) or: ((self fullHouse: hand) or: (self threeOfAKind: hand)))
		ifTrue: [ ^ false ]
		ifFalse: [ 
			^ (((sortedHand at: 1) == (sortedHand at: 2)) and:
			  ((sortedHand at: 3) == (sortedHand at: 4))) or:
			
			  ((((sortedHand at: 1) == (sortedHand at: 2)) and:
			  ((sortedHand at: 4) == (sortedHand at: 5))) or:
			
			  (((sortedHand at: 2) == (sortedHand at: 3)) and:
			  ((sortedHand at: 4) == (sortedHand at: 5)))).
		]

✓✓ breakFlushTieSuit: hand1 vs: hand2
	"break a tie using the highest ranking suit between hands with the rank: royal flush and same card 	numbers"
			
	| h1suit h2suit h1card1 h2card1|
	h1suit := self isolateSuits: hand1.
	h2suit := self isolateSuits: hand2.
	h1card1 := (h1suit at: 1) asString .
	h2card1 := (h2suit at: 1) asString .
	
	(h1card1 = 'C') ifTrue: [ ^hand2 ]. 
	(h2card1 = 'C') ifTrue: [ ^hand1 ].
	(h1card1 = 'S') ifTrue: [ ^hand1 ]. 
	(h2card1 = 'S') ifTrue: [ ^hand2 ].
	((h1card1 = 'D') and: (h2card1 = 'H')) ifTrue: [ ^hand2 ].
	((h1card1 = 'H') and: (h2card1 = 'D')) ifTrue: [ ^hand1 ].

✓✓ breakPairTieSuit: hand1 vs: hand2
	"break a tie using the highest ranking suit between hands with the rank: one pair and same card 	numbers"
			
	| h1suits|
	h1suits := self isolateSuits: hand1.

	(((hand1 at: 1 ) allButLast ) = ((hand1 at: 2) allButLast)) ifTrue: [ 
				(((h1suits at: 1 ) = 'S' ) or: ((h1suits at: 2) = 'S')) ifTrue: [ ^hand1 ] 
				ifFalse: [ ^hand2 ]]. 
	
	(((hand1 at: 2 ) allButLast ) = ((hand1 at: 3) allButLast )) ifTrue: [ 
				(((h1suits at: 2 ) = 'S' ) or: ((h1suits at: 3) = 'S')) ifTrue: [ ^hand1 ]
				ifFalse: [ ^hand2 ]].
	
	(((hand1 at: 3 ) allButLast ) = ((hand1 at: 4) allButLast )) ifTrue: [ 
				(((h1suits at: 3 ) = 'S' ) or: ((h1suits at: 4) = 'S')) ifTrue: [ ^hand1 ]
				ifFalse: [ ^hand2 ]].
	
	(((hand1 at: 4 ) allButLast ) = ((hand1 at: 5) allButLast )) ifTrue: [ 
				(((h1suits at: 4 ) = 'S' ) or: ((h1suits at: 5) = 'S')) ifTrue: [ ^hand1 ]
				ifFalse: [ ^hand2 ]].

✓✓ breakSuitTie: hand1 vs: hand2 at: card
	"break a tie between to hands at a given card"

	| h1suit h2suit h1card h2card|
	h1suit := self isolateSuits: hand1.
	h2suit := self isolateSuits: hand2.
	h1card := (h1suit at: card) asString .
	h2card := (h2suit at: card) asString .
	
	(h1card = 'C') ifTrue: [ ^hand2 ]. 
	(h2card = 'C') ifTrue: [ ^hand1 ].
	(h1card = 'S') ifTrue: [ ^hand1 ]. 
	(h2card = 'S') ifTrue: [ ^hand2 ].
	((h1card = 'D') and: (h2card = 'H')) ifTrue: [ ^hand2 ].
	((h1card = 'H') and: (h2card = 'D')) ifTrue: [ ^hand1 ].
	
✓✓ breakTwoPairTieSuit: hand1 vs: hand2
	"break a tie using the highest ranking suits, between hands with the rank: two pair and the same 	card numbers"
			
	| h1suits h2suits|
	h1suits := self isolateSuits: hand1.
	h2suits := self isolateSuits: hand2.

	((((hand1 at: 1) allButLast) = ((hand1 at: 2 ) allButLast)) and: 
		(((hand1 at: 3) allButLast ) = ((hand1 at: 4) allButLast))) ifTrue: [
			((((h1suits at: 1 ) = 'S' ) or: ((h1suits at: 2) = 'S')) 
				and: ((((h1suits at: 3 ) = 'S' ) or: ((h1suits at: 4) = 'S')))) ifTrue: [ ^hand1 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 5 ]]. 

	((((hand1 at: 1) allButLast) = ((hand1 at: 2 ) allButLast)) and: 
		(((hand1 at: 4) allButLast ) = ((hand1 at: 5) allButLast))) ifTrue: [
			((((h1suits at: 1 ) = 'S' ) or: ((h1suits at: 2) = 'S')) 
				and: ((((h1suits at: 4 ) = 'S' ) or: ((h1suits at: 5) = 'S')))) ifTrue: [ ^hand1 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 3 ]]. 

	((((hand1 at: 2) allButLast) = ((hand1 at: 3 ) allButLast)) and: 
		(((hand1 at: 4) allButLast ) = ((hand1 at: 5) allButLast))) ifTrue: [
			((((h1suits at: 2 ) = 'S' ) or: ((h1suits at: 3) = 'S')) 
				and: ((((h1suits at: 4 ) = 'S' ) or: ((h1suits at: 5) = 'S')))) ifTrue: [ ^hand1 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 1 ]]. 

	"hand 2 checks"
	((((hand2 at: 1) allButLast) = ((hand2 at: 2 ) allButLast)) and: 
		(((hand2 at: 3) allButLast ) = ((hand2 at: 4) allButLast))) ifTrue: [
			((((h2suits at: 1 ) = 'S' ) or: ((h2suits at: 2) = 'S')) 
				and: ((((h2suits at: 3 ) = 'S' ) or: ((h2suits at: 4) = 'S')))) ifTrue: [ ^hand2 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 5 ]]. 
			
	((((hand2 at: 1) allButLast) = ((hand2 at: 2 ) allButLast)) and: 
		(((hand2 at: 4) allButLast ) = ((hand2 at: 5) allButLast))) ifTrue: [
			((((h2suits at: 1 ) = 'S' ) or: ((h2suits at: 2) = 'S')) 
				and: ((((h2suits at: 4 ) = 'S' ) or: ((h2suits at: 5) = 'S')))) ifTrue: [ ^hand2 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 3 ]]. 

	((((hand2 at: 2) allButLast) = ((hand2 at: 3 ) allButLast)) and: 
		(((hand2 at: 4) allButLast ) = ((hand2 at: 5) allButLast))) ifTrue: [
			((((h2suits at: 2 ) = 'S' ) or: ((h2suits at: 3) = 'S')) 
				and: ((((h2suits at: 4 ) = 'S' ) or: ((h2suits at: 5) = 'S')))) ifTrue: [ ^hand2 ]
					ifFalse: [ ^self breakSuitTie: hand1 vs: hand2 at: 1 ]]. 

(not necessary) makeDictionary: hand
	"makes a dictionary with the card number as the key and its suit as the value"

	| d suit subStr|
	d := Dictionary new.
	1 to: 5 do: [ :x | subStr := hand at: x.
		suit := subStr copyFrom: (subStr size) to: (subStr size).
		d at: ((subStr allButLast) asInteger ) put: suit ].	
	^d

✓✓ breakStraightTieSuits: hand1 vs: hand2
	"break a tie using the highest ranking suit between hands with the rank: straight and same card numbers"

	| h1Sorted h2Sorted h1dic h2dic h1HighestCard h2HighestCard|
	h1Sorted := self sortDescending: hand1.
	h2Sorted := self sortDescending: hand2.
	h1dic := self makeDictionary: hand1.
	h2dic := self makeDictionary: hand2.
	h1HighestCard := h1Sorted at: 1.
	h2HighestCard := h2Sorted at: 1.
	
	((h1Sorted at: 5) = 1 and: (h2Sorted at: 5) = 1) ifTrue: [
		((h1dic at: 1) = 'C') ifTrue: [ ^ hand2 ].
		((h2dic at: 1) = 'C') ifTrue: [ ^ hand1 ].
		((h1dic at: 1) = 'S') ifTrue: [ ^ hand1 ].
		((h2dic at: 1) = 'S') ifTrue: [ ^ hand2 ].
		(((h1dic at: 1) = 'D') and: ((h2dic at: 1) = 'H')) ifTrue: [ ^ hand2 ].
		(((h1dic at: 1) = 'H') and: ((h2dic at: 1) = 'D')) ifTrue: [ ^ hand1 ].
		]
	ifFalse: [ 
		((h1dic at: h1HighestCard) = 'C' ) ifTrue: [ ^ hand2 ].
		((h2dic at: h2HighestCard) = 'C') ifTrue: [ ^ hand1 ].
		((h1dic at: h1HighestCard) = 'S') ifTrue: [ ^ hand1 ].
		((h2dic at: h2HighestCard) = 'S') ifTrue: [ ^ hand2 ].
		(((h1dic at: h1HighestCard) = 'D') and: ((h2dic at: h2HighestCard) = 'H')) ifTrue: [ ^ hand2 ].
		(((h1dic at: h1HighestCard) = 'H') and: ((h2dic at: h2HighestCard) = 'D')) ifTrue: [ ^ hand1 ].
		].

✓ highestCard2: hand1 vs: hand2
	"breaks the tie between two hands with the ranks: four of a kind, full house and three of a kind 	using the highest card"

    | sortedHand1 sortedHand2 |

    sortedHand1 := self sortAscending: hand1.
    sortedHand2 := self sortAscending: hand2.

    ((sortedHand1 at: 3) > (sortedHand2 at: 3)) 
    ifTrue: [
        ^ hand1.
    ].
    ((sortedHand1 at: 3) < (sortedHand2 at: 3))
    ifTrue: [
        ^ hand2.
    ].

    ((sortedHand1 at: 5) > (sortedHand2 at: 5)) 
    ifTrue: [
        ^ hand1.
    ].
    ((sortedHand1 at: 5) < (sortedHand2 at: 5))
    ifTrue: [
        ^ hand2.
    ].

    ((sortedHand1 at: 1) > (sortedHand2 at: 1)) 
    ifTrue: [
        ^ hand1.
    ].
    ((sortedHand1 at: 1) < (sortedHand2 at: 1))
    ifTrue: [
        ^ hand2.
    ].

	^(self highestSuitRank: hand1  vs: hand2) asArray
	
✓ highestCard: hand1 vs: hand2
	"breaks the tie between two hands with the ranks: royal flush, straight flush, flush and straight, 	using the highest card"
    | sortedHand1 sortedHand2 |

    sortedHand1 := self sortAscending: hand1.
    sortedHand2 := self sortAscending: hand2.

    ((sortedHand1 at: 1) == 1) 
    ifTrue: [
        sortedHand1 addLast: 14.
        sortedHand1 removeFirst.
    ].

    ((sortedHand2 at: 1) == 1) 
    ifTrue: [
        sortedHand2 addLast: 14.
        sortedHand2 removeFirst.
    ].

    5 to: 1 by: -1 do: [ :i |
        ((sortedHand1 at: i) > (sortedHand2 at: i)) 
        ifTrue: [
            ^ hand1 asArray .
        ].
        ((sortedHand1 at: i) < (sortedHand2 at: i))
        ifTrue: [
            ^ hand2 asArray .
        ].
    ].

	^(self highestSuitRank: hand1  vs: hand2) asArray

✓✓ highestSuitRank: hand1 vs: hand2
	"breaks a tie between two hands with the same rank and card numbers using the highest ranking suit(s)"

	((((self rank: hand1 ) == 1 ) and: ((self rank: hand2 ) == 1 )) or: 
	((((self rank: hand1 ) == 2 ) and: ((self rank: hand2 ) == 2 )) or: 
	(((self rank: hand1 ) == 5 ) and: ((self rank: hand2 ) == 5 )))) 
		ifTrue: [ ^self breakFlushTieSuit: hand1 vs: hand2 ].
			
	(((self rank: hand1 ) == 6 ) and: ((self rank: hand1 ) == 6 )) 
		ifTrue: [ ^self breakStraightTieSuits: hand1 vs: hand2 ].
		
	(((self rank: hand1 ) == 8 ) and: ((self rank: hand1 ) == 8 )) 
		ifTrue: [ ^self breakPairTieSuit: hand1 vs: hand2 ].
	
	(((self rank: hand1 ) == 9 ) and: ((self rank: hand1 ) == 9 )) 
		ifTrue: [ ^self breakTwoPairTieSuit: hand1 vs: hand2 ].


