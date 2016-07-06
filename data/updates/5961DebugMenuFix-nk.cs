'From Squeak3.7beta of ''1 April 2004'' [latest update: #5948] on 16 June 2004 at 2:39:52 pm'!"Change Set:		DebugMenuFix-nkDate:			14 June 2004Author:			Ned Konzmd: V2 without reorganization of Morph cat.Fixes a bug with the debug halo menu offering to let you view the player if there isn't one."!!Morph methodsFor: 'debug and other' stamp: 'nk 6/14/2004 16:14'!buildDebugMenu: aHand	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"	| aMenu aPlayer |	aMenu _ MenuMorph new defaultTarget: self.	aMenu addStayUpItem.	(self hasProperty: #errorOnDraw) ifTrue:		[aMenu add: 'start drawing again' translated action: #resumeAfterDrawError.		aMenu addLine].	(self hasProperty: #errorOnStep) ifTrue:		[aMenu add: 'start stepping again' translated action: #resumeAfterStepError.		aMenu addLine].	aMenu add: 'inspect morph' translated action: #inspectInMorphic:.	aMenu add: 'inspect owner chain' translated action: #inspectOwnerChain.	Smalltalk isMorphic ifFalse:		[aMenu add: 'inspect morph (in MVC)' translated action: #inspect].	self isMorphicModel ifTrue:		[aMenu add: 'inspect model' translated target: self model action: #inspect].	(aPlayer _ self player) ifNotNil:		[aMenu add: 'inspect player' translated target: aPlayer action: #inspect].     aMenu add: 'explore morph' translated target: self selector: #explore.	aMenu addLine.	aPlayer ifNotNil:		[ aMenu add: 'viewer for Player' translated target: self player action: #beViewed.	aMenu balloonTextForLastItem: 'Opens a viewer on my Player -- this is the same thing you get if you click on the cyan "View" halo handle' translated ].	aMenu add: 'viewer for Morph' translated target: self action: #viewMorphDirectly.	aMenu balloonTextForLastItem: 'Opens a Viewer on this Morph, rather than on its Player' translated.	aMenu addLine.	aPlayer ifNotNil:		[aPlayer class isUniClass ifTrue: [			aMenu add: 'browse player class' translated target: aPlayer action: #browseHierarchy]].	aMenu add: 'browse morph class' translated target: self selector: #browseHierarchy.	(self isMorphicModel)		ifTrue: [aMenu				add: 'browse model class'				target: self model				selector: #browseHierarchy].	aMenu addLine.	aPlayer ifNotNil:		[aMenu add: 'player protocol (tiles)' translated target: aPlayer action: #openInstanceBrowserWithTiles			"#browseProtocolForPlayer"].	aMenu add: 'morph protocol (text)' translated target: self selector: #haveFullProtocolBrowsed.	aMenu add: 'morph protocol (tiles)' translated target: self selector: #openInstanceBrowserWithTiles.	aMenu addLine.	self addViewingItemsTo: aMenu.	aMenu 		add: 'make own subclass' translated action: #subclassMorph;		add: 'internal name ' translated action: #choosePartName;		add: 'save morph in file' translated  action: #saveOnFile;		addLine;		add: 'call #tempCommand' translated action: #tempCommand;		add: 'define #tempCommand' translated action: #defineTempCommand;		addLine;		add: 'control-menu...' translated target: self selector: #invokeMetaMenu:;		add: 'edit balloon help' translated action: #editBalloonHelpText.	^ aMenu! !