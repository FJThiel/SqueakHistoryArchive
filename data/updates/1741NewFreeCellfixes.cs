'From Squeak2.7alpha of 6 December 1999 [latest update: #1686] on 20 December 1999 at 8:42:16 pm'!"Change Set:		NewFreeCellFixesDate:			16 December 1999Author:			Torge HusfeldtFixes three more bugs in FreeCell.1) Picking a new game didn't update the statistics.2) Picking a new game disabled resuming elapsed Time.3) Quitting the game didn't count the game as lost.Removes code duplication (the cause for above bugs 1&2)in the three methods (and their counterparts for the board)for beginning new games.Cosmetical change: prevents overflow of elapsedTime by initially adding a digit.Note: Pausing of elapsed time still doesn't work for FreeCell morphswithout owning SystemWindow (don't know how to accomplish this).Torge"!!FreeCell methodsFor: 'actions' stamp: 'th 12/16/1999 18:04'!gameLost	| increaseStats |	state _ #lost.	"Don't count the same game more than once in a row"	increaseStats _ (lastGameLost == self currentGame) not.	lastGameLost _ self currentGame.	elapsedTimeDisplay stop.	cardsRemainingDisplay flash: true.	increaseStats ifTrue: [Statistics gameLost]! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 15:20'!modelSleep	"When fixing #contains: calls beware of reinventing #includes:"	(#(newGame sameGame pickGame won lost ) includes: state)		ifTrue: [elapsedTimeDisplay pause]! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 15:22'!modelWakeUp	"Maybe less performant but more readable"	(#(won lost) includes: state)		ifFalse: [elapsedTimeDisplay resume]! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 15:05'!newGame	self newGameNumber: nil.	state _ #newGame! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 16:01'!newGameNumber: aSeedOrNil 	cardsRemainingDisplay value ~~ 0 ifTrue: [self gameLost].	cardsRemainingDisplay value: 52;	 flash: false.	"board handles nil case"	self board pickGame: aSeedOrNil.	elapsedTimeDisplay reset; start.	gameNumberDisplay value: self currentGame! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 15:05'!pickGame	| seed |	seed _ self promptForSeed.	seed isNil ifTrue: [^ self].	self newGameNumber: seed.	state _ #pickGame! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 14:56'!promptForSeed	| s i |		[s _ FillInTheBlank request: 'Pick a game number between 1 and 32000'.	"Let the user cancel."	s isEmpty ifTrue:[^nil].	[i _ s asNumber asInteger]		on: Error do: [i _ 0].	i between: 1 and: 32000] whileFalse.	^ i! !!FreeCell methodsFor: 'actions' stamp: 'th 12/16/1999 18:14'!quit	cardsRemainingDisplay value ~~ 0 ifTrue: [self gameLost].	self owner == self world		ifTrue: [self delete]		ifFalse: [self owner delete].	Statistics close! !!FreeCell methodsFor: 'actions' stamp: 'th 12/15/1999 15:03'!sameGame	self newGameNumber: self currentGame.	state _ #sameGame.! !!FreeCellBoard methodsFor: 'initialization' stamp: 'th 12/15/1999 15:57'!pickGame: aSeedOrNil	cardDeck _ PlayingCardDeck newDeck.	aSeedOrNil ifNotNil:[cardDeck seed: aSeedOrNil].	cardDeck shuffle.	self resetBoard! !!FreeCellBoard methodsFor: 'layout' stamp: 'th 12/15/1999 16:14'!freeCell	| freeCell |	freeCell _ self cardCell.	freeCell stackingPolicy: #single;	 emptyDropPolicy: #any;	 target: self;	 cardDroppedSelector: #cardMoved;	 acceptCardSelector: #acceptSingleCard:on:.	^ freeCell! !!FreeCellBoard methodsFor: 'layout' stamp: 'th 12/15/1999 16:12'!homeCell	| homeCell |	homeCell _ self cardCell.	homeCell stackingPolicy: #straight;	 stackingOrder: #ascending;	 emptyDropPolicy: #inOrder;	 target: self;	 cardDroppedSelector: #cardMovedHome;	 cardDraggedSelector: #dragCard:fromHome:;	 acceptCardSelector: #acceptSingleCard:on:.	^ homeCell! !!FreeCellBoard methodsFor: 'layout' stamp: 'th 12/15/1999 16:16'!stack	^ PlayingCardDeck new color: self color;	 layout: #stagger;	 orientation: #vertical;	 openToDragNDrop: true;	 stackingPolicy: #altStraight;	 stackingOrder: #descending;	 emptyDropPolicy: #any;	 target: self;	 cardDroppedSelector: #cardMoved;	 cardDraggedSelector: #dragCard:fromStack:;	 acceptCardSelector: #acceptCard:onStack:;	 cardDoubleClickSelector: #doubleClickInStack:OnCard:! !!FreeCellBoard methodsFor: 'actions' stamp: 'th 12/15/1999 16:17'!acceptSingleCard: aCard on: aDeck 	"Home cells and free cells don't accept multiple cards on a home cell, 	defer to deck for other cases"	aCard hasSubmorphs		ifTrue: [^ false]		ifFalse: [^ nil]! !!FreeCellBoard methodsFor: 'actions' stamp: 'th 12/15/1999 16:15'!cardMoved	"Free cells and stacks do nothing special here - yet - th 12/15/1999 	16:15 "	self autoMoveCardsHome! !!FreeCellStatistics methodsFor: 'printing' stamp: 'th 12/20/1999 20:37'!print: aNumber type: type on: aStream 	"I moved the code from #printWins:on: and #printLosses:on: here because it is basically	the same. I hope this increases the maintainability. - th 12/20/1999 20:37"	aStream print: aNumber.	type = #wins		ifTrue: [aNumber = 1				ifTrue: [aStream nextPutAll: ' win']				ifFalse: [aStream nextPutAll: ' wins']].	type = #losses		ifTrue: [aNumber = 1				ifTrue: [aStream nextPutAll: ' loss']				ifFalse: [aStream nextPutAll: ' losses']]! !!FreeCellStatistics methodsFor: 'printing' stamp: 'th 12/20/1999 19:50'!printSessionOn: aStream 	| total |	aStream nextPutAll: 'This session: ' , String tab.	self print: sessionWins type: #wins on: aStream.	aStream nextPutAll: ', '.	self print: sessionLosses type: #losses on: aStream.	total _ sessionWins + sessionLosses.	total ~~ 0 ifTrue: [aStream nextPutAll: ', ';		 print: (sessionWins / total * 100) asInteger;		 nextPut: $%]! !!FreeCellStatistics methodsFor: 'printing' stamp: 'th 12/20/1999 19:53'!printStreaksOn: aStream 	aStream nextPutAll: 'Streaks: ';	 tab;	 tab.	self		print: streakWins		type: #wins		on: aStream.	aStream nextPutAll: ', '.	self		print: streakLosses		type: #losses		on: aStream.	aStream cr; tab; tab; tab; tab; nextPutAll: 'Current: '.	self		print: currentCount		type: currentType		on: aStream! !!FreeCellStatistics methodsFor: 'printing' stamp: 'th 12/20/1999 19:48'!printTotalOn: aStream 	| total |	aStream nextPutAll: 'Total: ';	 tab;	 tab;	 tab.	self print: totalWins type: #wins on: aStream.	aStream nextPutAll: ', '.	self print: totalLosses type: #losses on: aStream.	total _ totalWins + totalLosses.	total ~~ 0 ifTrue: [aStream nextPutAll: ', ';		 print: (totalWins / total * 100) asInteger;		 nextPut: $%]! !!FreeCellStatistics methodsFor: 'actions' stamp: 'th 12/17/1999 12:22'!gameLost	sessionLosses _ sessionLosses + 1.	totalLosses _ totalLosses + 1.	currentType = #losses		ifTrue: [currentCount _ currentCount + 1]		ifFalse: 			[currentCount _ 1.			currentType _ #losses].	self updateStreak.	self changed! !!FreeCellStatistics methodsFor: 'actions' stamp: 'th 12/17/1999 12:22'!gameWon	sessionWins _ sessionWins + 1.	totalWins _ totalWins + 1.	currentType = #wins		ifTrue: [currentCount _ currentCount + 1]		ifFalse: 			[currentCount _ 1.			currentType _ #wins].	self updateStreak.	self changed! !!FreeCellStatistics methodsFor: 'actions' stamp: 'th 12/20/1999 20:42'!updateStreak	"I moved the code from #printWins:on: and #printLosses:on: here because 	 it is basically the same. I hope this increases the maintainability. 	th 12/20/1999 20:41"	currentType = #losses ifTrue: [streakLosses _ streakLosses max: currentCount].	currentType = #wins ifTrue: [streakWins _ streakWins max: currentCount]! !!MorphExtension methodsFor: 'other properties' stamp: 'th 12/15/1999 14:18'!valueOfProperty: propName 	^ self valueOfProperty: propName ifAbsent: [nil]! !FreeCellBoard removeSelector: #acceptCard:onHome:!FreeCellBoard removeSelector: #cardMovedToStack!FreeCellBoard removeSelector: #acceptCard:onFree:!FreeCellBoard removeSelector: #newGame!FreeCellBoard removeSelector: #cardMovedToFreeCell!FreeCellBoard removeSelector: #sameGame!FreeCellStatistics removeSelector: #printWins:on:!FreeCellStatistics removeSelector: #printLosses:on:!FreeCellStatistics removeSelector: #updateStreakWins!FreeCellStatistics removeSelector: #updateStreakLosses!