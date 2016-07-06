'From Squeak2.9alpha of 12 June 2000 [latest update: #2477] on 12 August 2000 at 3:07:10 pm'!"Change Set:		MoreRoundedCornersDate:			12 August 2000Author:			Dan IngallsAllows any SketchMorph or subclass and any ImageMorph or subclass to have rounded corners.  It also makes it really easy to add this to just about any morph (but I didn't because it would slow down display of all the primitive morphs).  Just add or edit the following three methods as was done with ImageMorph and SketchMorph...	wantsRoundedCorners	addCustomMenuItems: aCustomMenu hand: aHandMorph	drawOn: aCanvas"!!Canvas methodsFor: 'testing' stamp: 'di 8/12/2000 15:04'!doesRoundedCorners 	^ true! !!Morph methodsFor: 'rounding' stamp: 'di 8/12/2000 14:52'!borderWidth	^ 0! !!Morph methodsFor: 'rounding' stamp: 'di 8/12/2000 14:44'!roundedCornersString	"Answer the string to put in a menu that will invite the user to switch to the opposite  corner-rounding mode"	^ self wantsRoundedCorners		ifTrue: ['stop rounding corners']		ifFalse: ['start rounding corners']! !!Morph methodsFor: 'rounding' stamp: 'di 8/12/2000 14:51'!toggleCornerRounding	self wantsRoundedCorners		ifTrue: [self removeProperty: #roundedCorners]		ifFalse: [self setProperty: #roundedCorners toValue: true].	self changed! !!Morph methodsFor: 'rounding' stamp: 'di 8/12/2000 14:44'!wantsRoundedCorners	"Default response is simple, fast, and false.	May be overridden to allow almost any morph to be rounded."	^ false! !!ImageMorph methodsFor: 'drawing' stamp: 'di 8/12/2000 15:03'!wantsRoundedCorners	^ self hasProperty: #roundedCorners! !!ImageMorph methodsFor: 'menu commands' stamp: 'di 8/12/2000 14:59'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'choose new graphic...' target: self action: #chooseNewGraphic.	aCustomMenu add: 'read from file' action: #readFromFile.	aCustomMenu add: 'grab from screen' action: #grabFromScreen.	aCustomMenu addUpdating: #roundedCornersString target: self action: #toggleCornerRounding.! !!ImageMorph methodsFor: 'drawing' stamp: 'di 8/12/2000 15:02'!drawOn: aCanvas	(self wantsRoundedCorners and: [aCanvas doesRoundedCorners])		ifTrue:			[CornerRounder roundCornersOf: self on: aCanvas				displayBlock: [aCanvas paintImage: image at: bounds origin]				borderWidth: 0]		ifFalse:			[aCanvas paintImage: image at: bounds origin]! !!PostscriptCanvas methodsFor: 'initialization' stamp: 'di 8/12/2000 15:04'!doesRoundedCorners 	^ false! !!SketchMorph methodsFor: 'drawing' stamp: 'di 8/12/2000 14:39'!wantsRoundedCorners	^ self hasProperty: #roundedCorners! !!SketchMorph methodsFor: 'drawing' stamp: 'di 8/12/2000 14:56'!drawOn: aCanvas	(self wantsRoundedCorners and: [aCanvas doesRoundedCorners])		ifTrue:			[CornerRounder roundCornersOf: self on: aCanvas				displayBlock: [aCanvas paintImage: self rotatedForm at: bounds origin]				borderWidth: 0]		ifFalse:			[aCanvas paintImage: self rotatedForm at: bounds origin]! !!SketchMorph methodsFor: 'menu' stamp: 'di 8/12/2000 14:53'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'choose new graphic...' target: self action: #chooseNewGraphic.	aCustomMenu add: 'set as background' target: rotatedForm action: #setAsBackground.	aCustomMenu addUpdating: #roundedCornersString target: self action: #toggleCornerRounding.	self addPaintingItemsTo: aCustomMenu hand: aHandMorph! !BorderedMorph removeSelector: #roundedCornersString!