'From Squeak2.7alpha of 29 November 1999 [latest update: #1660] on 29 November 1999 at 5:41:09 pm'!!FreeCell methodsFor: 'actions' stamp: 'djp 11/29/1999 17:29'!gameLost	state _ #lost.	lastGameLost _ self currentGame.	elapsedTimeDisplay highlighted: true; stop.	cardsRemainingDisplay flash: true.	Statistics gameLost.! !!FreeCell methodsFor: 'actions' stamp: 'djp 11/29/1999 17:28'!gameWon	state _ #won.	elapsedTimeDisplay stop; highlighted: true.	cardsRemainingDisplay flash: true.	Statistics gameWon.! !!FreeCell methodsFor: 'actions' stamp: 'djp 11/29/1999 17:28'!newGame	(cardsRemainingDisplay value ~~ 0) ifTrue: [self gameLost].	cardsRemainingDisplay value: 52; flash: false.	self board newGame.	elapsedTimeDisplay reset; highlighted: false; start.	gameNumberDisplay value: self currentGame.	state _ #newGame.! !!LedMorph methodsFor: 'accessing' stamp: 'djp 11/29/1999 17:27'!highlighted: aBoolean	self submorphsDo: [:m | m highlighted: aBoolean]! !!LedMorph methodsFor: 'stepping' stamp: 'djp 11/29/1999 17:27'!step	(flash or: [flashing])		ifTrue:			[flashing _ flashing not.			self highlighted: flashing]! !