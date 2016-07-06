'From Squeak 2.5 of August 6, 1999 [latest update: #1548] on 18 October 1999 at 5:16:28 pm'!"Change Set:		fixes-bfDate:			12 October 1999Author:			Bert Freudenberg* Form>>dominantColor never really worked!! (maybe we should introduce SequencableCollection>>indexOfMax?)* FatBitsPaint now switches colors on mousedown like bitEdit does* Cursor #showWhile: now restores cursor on unwind, too* Adjust the rotation handle's icon color while moving* Makes TTSampleStringMorphs 'toggle smoothing' menu option work* Makes target of 'change color' be the morph itself so permanent menu is ok"!!Form methodsFor: 'image manipulation' stamp: 'bf 10/12/1999 18:07'!dominantColor	| tally max maxi |	depth > 16 ifTrue:		[^(self asFormOfDepth: 16) dominantColor].	tally _ self tallyPixelValues.	max _ maxi _ 0.	tally withIndexDo: [:n :i | n > max ifTrue: [max _ n. maxi _ i]].	^ Color colorFromPixelValue: maxi - 1 depth: depth! !!Cursor methodsFor: 'displaying' stamp: 'bf 10/13/1999 13:05'!showWhile: aBlock 	"While evaluating the argument, aBlock, make the receiver be the cursor 	shape."	| oldcursor |	oldcursor _ Sensor currentCursor.	self show.	^aBlock ensure: [oldcursor show]! !!Morph methodsFor: 'menus' stamp: 'bf 10/18/1999 17:12'!addFillStyleMenuItems: aMenu hand: aHand	"Add the items for changing the current fill style of the Morph"	| menu |	self canHaveFillStyles ifFalse:[^aMenu add: 'change color...' target: self action: #changeColor].	menu _ MenuMorph new defaultTarget: self.	self fillStyle addFillStyleMenuItems: menu hand: aHand from: self.	menu addLine.	menu add: 'solid fill' action: #useSolidFill.	menu add: 'gradient fill' action: #useGradientFill.	menu add: 'bitmap fill' action: #useBitmapFill.	menu add: 'default fill' action: #useDefaultFill.	aMenu add: 'fill style' subMenu: menu.	"aMenu add: 'change color...' action: #changeColor"! !!FatBitsPaint methodsFor: 'events' stamp: 'bf 10/13/1999 18:22'!mouseDown: evt	lastMouse _ nil.	originalForm depth = 1 ifTrue: [		self brushColor: (originalForm colorAt: (self griddedPoint: evt)) negated]! !!TTSampleFontMorph methodsFor: 'accessing' stamp: 'bf 10/18/1999 16:19'!smoothing: aNumber	smoothing _ aNumber.	self changed! !!TTSampleFontMorph methodsFor: 'menu' stamp: 'bf 10/18/1999 16:28'!nextSmoothingLevel	"Menu support"	smoothing = 1 ifTrue: [^self smoothing: 2].	smoothing = 2 ifTrue: [^self smoothing: 4].	self smoothing: 1! !