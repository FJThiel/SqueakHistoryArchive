'From Squeak3.2alpha of 4 October 2001 [latest update: #4421] on 5 October 2001 at 6:28:02 am'!!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 10/3/2001 08:39'!insertUniversalTilesForClass: aClass selector: aSelector	"Add a submorph which holds the universal-tiles script for the given class and selector"	| source tree syn widget header |	source _ aClass sourceCodeAt: aSelector ifAbsent: [		Transcript cr; show: aClass name, 'could not find selector ', aSelector.		^ self delete].    	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	aSelector numArgs = 0 ifTrue: [		"remove method header line"		(header _ syn findA: SelectorNode) ifNotNil: [header delete]].	syn removeReturnNode.		"if ^ self at end, remove it"	widget _ syn inAScrollPane.	widget hResizing: #spaceFill;		vResizing: #spaceFill;		color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true.	self addMorphBack: widget.	(self hasProperty: #autoFitContents) ifFalse:		[self valueOfProperty: #sizeAtHibernate ifPresentDo:			[:oldExtent | self extent: oldExtent]].	syn finalAppearanceTweaks.! !!StackMorph methodsFor: 'background' stamp: 'tk 10/5/2001 06:27'!reassessBackgroundShape	"Have the current page reconsider its cards' instance structure"	currentPage setProperty: #myStack toValue: self. 	"pointer cardMorph -> stack"	^ self currentPage reassessBackgroundShape ! !!StackMorph methodsFor: 'card access' stamp: 'tk 10/5/2001 06:19'!goToCard: destinationCard	"Install the indicated destinationCard as the current card in the receiver.  Any viewer currently open on the current card will get retargeted to look at the new one."	| aBackground existingCard oldViewers |	aBackground _ self backgroundWithCard: destinationCard.	existingCard _ aBackground currentDataInstance.	oldViewers _ existingCard ifNil: [#()] ifNotNil: [existingCard allOpenViewers].	aBackground installAsCurrent: destinationCard.	aBackground setProperty: #myStack toValue: self.	"pointer cardMorph -> stack"	aBackground ~~ currentPage ifTrue:		["concern that need to run opening/closing scripts even when we're not going to a different background"		self goToPageMorph: aBackground].	oldViewers do: [:aViewer | aViewer retargetFrom: existingCard to: destinationCard]! !