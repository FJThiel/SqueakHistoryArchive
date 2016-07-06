'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:52 pm'!
"Change Set:		q02-trails-sw
Date:			4 March 2004
Author:			Scott Wallace

Created by merging the following four pen-trail-related updates from Squeakland, then integrating them with Squeak 3.7a/5764 (quite a few collisions were encountered):
  0148arrowShaft-sw
  0149arrowhead-sw
  0153dotsTrailStyle-sw
  0155dotsTrail-sw

NB:  Assumes the subsequent loading of the PasteUpMorph>>additionsToViewerCategories method in the 'textInEToys-sw' update.

Allows pen trails to be lines, linesAndArrows, or just arrows.  Consult the pen-trails category in viewers.

NB:  Restores method Preference.setArrowheads, which was erroneously removed by KCP update 5480KCP97NewPointOrNil.

Adds a new trail-style called 'dots' to the etoy system.

Also, guards SearchingViewer against putting up a gratuitous informer when Vocabulary is reinitialized

¥ Adds a new trail-style called 'dots' to the etoy system.
¥ Terminology changes:
  - 'arrows' is now 'arrowheads'
  -  'lines and arrows' is now 'arrows'.
¥ pen-trails submenu in the halo-menu is split off as a stand-alone menu, so that it can conveniently be made permanent if desired.
¥ 'dotSize' variable governs size of dot.  Defaults to 6, the smallest size at which dots start seeming round, but the user can set it to any value between 1 and 100.
¥ Also, guards SearchingViewer against putting up a gratuitous informer when Vocabulary is reinitialized."
!

Object subclass: #ActorState
	instanceVariableNames: 'owningPlayer penDown penSize penColor fractionalPosition instantiatedUserScriptsDictionary penArrowheads trailStyle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Scripting Support'!

!ActorState methodsFor: 'pen' stamp: 'sw 4/16/2003 12:26'!
trailStyle
	"Answer the receiver's trailStyle.  For backward compatibility, if the old penArrowheads slot is in found to be set, use it as a guide for initialization"

	^ trailStyle ifNil: [trailStyle _ penArrowheads == true ifTrue: [#arrows] ifFalse: [#lines]]! !

!ActorState methodsFor: 'pen' stamp: 'sw 3/11/2003 11:28'!
trailStyle: aSymbol
	"Set the trail style to the given symbol"

	trailStyle _ aSymbol! !


!Morph methodsFor: 'menus' stamp: 'sw 3/2/2004 22:11'!
addMiscExtrasTo: aMenu
	"Add a submenu of miscellaneous extra items to the menu."

	| realOwner realMorph subMenu |
	subMenu _ MenuMorph new defaultTarget: self.
	(self isWorldMorph not and: [(self renderedMorph isSystemWindow) not])
		ifTrue: [subMenu add: 'put in a window' translated action: #embedInWindow].

	self isWorldMorph ifFalse:
		[subMenu add: 'adhere to edge...' translated action: #adhereToEdge.
		subMenu addLine].

	realOwner _ (realMorph _ self topRendererOrSelf) owner.
	(realOwner isKindOf: TextPlusPasteUpMorph) ifTrue:
		[subMenu add: 'GeeMail stuff...' translated subMenu: (realOwner textPlusMenuFor: realMorph)].

	subMenu
		add: 'add mouse up action' translated action: #addMouseUpAction;
		add: 'remove mouse up action' translated action: #removeMouseUpAction;
		add: 'hand me tiles to fire this button' translated action: #handMeTilesToFire.
	subMenu addLine.
	subMenu add: 'arrowheads on pen trails...' translated action: #setArrowheads.
	subMenu addLine.

	subMenu defaultTarget: self topRendererOrSelf.
	subMenu add: 'draw new path' translated action: #definePath.
	subMenu add: 'follow existing path' translated action: #followPath.
	subMenu add: 'delete existing path' translated action: #deletePath.
	subMenu addLine.

	self addGestureMenuItems: subMenu hand: ActiveHand.

	aMenu add: 'extras...' translated subMenu: subMenu! !

!Morph methodsFor: 'menus' stamp: 'sw 3/2/2004 22:11'!
setArrowheads
	"Let the user edit the size of arrowheads for this object"

	| aParameter result  |
	aParameter _ self renderedMorph valueOfProperty:  #arrowSpec ifAbsent:
		[Preferences parameterAt: #arrowSpec ifAbsent: [5 @ 4]].
	result _ Morph obtainArrowheadFor: 'Head size for arrowheads: ' defaultValue: aParameter asString.
	result ifNotNil:
			[self renderedMorph  setProperty: #arrowSpec toValue: result]
		ifNil:
			[Beeper beep]! !


!Morph class methodsFor: 'scripting' stamp: 'sw 4/17/2003 12:05'!
additionsToViewerCategoryPenUse
	"Answer viewer additions for the 'pen use' category"

	^#(
		#'pen use' 
		(
			(slot penColor 'the color of ink used by the pen' Color readWrite Player getPenColor Player setPenColor:) 
			(slot penSize 'the width of the pen' Number readWrite Player getPenSize Player setPenSize:) 
			(slot penDown 'whether the pen is currently down' Boolean readWrite Player getPenDown Player setPenDown:)
			(slot trailStyle 'determines whether lines, arrows, arrowheads, or dots are used when I put down a pen trail' TrailStyle readWrite Player getTrailStyle Player setTrailStyle:)
			(slot dotSize 'diameter of dot to use when trailStyle is dots' Number readWrite Player getDotSize Player setDotSize:)
			(command clearOwnersPenTrails 'clear all pen trails in my containing playfield')
		)
	)
! !


!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 3/3/2004 01:14'!
addPenMenuItems: menu hand: aHandMorph
	"Add a pen-trails-within submenu to the given menu"

	menu add: 'penTrails within...' translated target: self action: #putUpPenTrailsSubmenu! !

!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 3/3/2004 01:15'!
addPenTrailsMenuItemsTo: aMenu
	"Add items relating to pen trails to aMenu"

	| oldTarget |
	oldTarget _ aMenu defaultTarget.
	aMenu defaultTarget: self.
	aMenu add: 'clear pen trails' translated action: #clearTurtleTrails.
	aMenu addLine.
	aMenu add: 'all pens up' translated action: #liftAllPens.
	aMenu add: 'all pens down' translated action: #lowerAllPens.
	aMenu addLine.
	aMenu add: 'all pens show lines' translated action: #linesForAllPens.
	aMenu add: 'all pens show arrowheads' translated action: #arrowsForAllPens.
	aMenu add: 'all pens show arrows' translated action: #linesAndArrowsForAllPens.
	aMenu add: 'all pens show dots' translated action: #dotsForAllPens.
	aMenu defaultTarget: oldTarget! !

!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 3/3/2004 01:17'!
putUpPenTrailsSubmenu
	"Put up the pen trails menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu title: 'pen trails' translated.
	aMenu addStayUpItem.
	self addPenTrailsMenuItemsTo: aMenu.
	aMenu popUpInWorld: ActiveWorld! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 4/16/2003 12:45'!
arrowsForAllPens
	"Set the trail style for all my objects to show arrowheads only"

	self trailStyleForAllPens: #arrowheads! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 4/10/2003 21:15'!
dotsForAllPens
	"Set the trail style for all my objects to show dots"

	self trailStyleForAllPens: #dots! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 4/17/2003 12:01'!
drawPenTrailFor: aMorph from: oldPoint to: targetPoint
	"Draw a pen trail for aMorph, using its pen state (the pen is assumed to be down)."
	"The turtleTrailsForm is created on demand when the first pen is put down and removed (to save space) when turtle trails are cleared."

	| origin mPenSize offset turtleTrailsDelta newPoint aPlayer trailStyle aRadius dotSize |
	turtleTrailsDelta _ self valueOfProperty: #turtleTrailsDelta ifAbsent:[0@0].
	newPoint _ targetPoint - turtleTrailsDelta.
	oldPoint = newPoint ifTrue: [^ self].
	self createOrResizeTrailsForm.
	origin _ self topLeft.
	mPenSize _ aMorph getPenSize.
	turtlePen color: aMorph getPenColor.
	turtlePen sourceForm width ~= mPenSize
		ifTrue: [turtlePen squareNib: mPenSize].
	offset _ (mPenSize // 2)@(mPenSize // 2).
	(#(lines arrows) includes: (trailStyle _ (aPlayer _ aMorph player) getTrailStyle))
		ifTrue:
			[turtlePen drawFrom: (oldPoint - origin - offset) asIntegerPoint
				to: (newPoint - origin - offset) asIntegerPoint].
	((#(arrowheads arrows) includes: trailStyle) and: [oldPoint ~= newPoint]) ifTrue:
		[turtlePen
			arrowHeadFrom: (oldPoint - origin - offset) 
			to: (newPoint - origin - offset)
			forPlayer: aPlayer].
	(#(dots) includes: trailStyle)
		ifTrue:
			[dotSize _ aPlayer getDotSize.
			turtlePen
				putDotOfDiameter: dotSize at: (oldPoint - origin).
			turtlePen
				putDotOfDiameter: dotSize at: (targetPoint - origin).
			aRadius _ (dotSize // 2) + 1.
			dotSize _ dotSize + 1.  "re round-off-derived gribblies"
			self invalidRect: ((oldPoint - origin - (aRadius @ aRadius)) extent: (dotSize @ dotSize)).
			self invalidRect: ((targetPoint - origin - (aRadius @ aRadius)) extent: (dotSize @ dotSize))]
		ifFalse:
			[self invalidRect: ((oldPoint rect: newPoint) expandBy: mPenSize)]! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 4/16/2003 12:27'!
linesAndArrowsForAllPens
	"Set the trail style for all my objects to show arrows"

	self trailStyleForAllPens: #arrows! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 3/11/2003 11:57'!
linesForAllPens
	"Set the trail style for all my objects to show lines only"

	self trailStyleForAllPens: #lines! !

!PasteUpMorph methodsFor: 'pen' stamp: 'sw 3/11/2003 11:40'!
trailStyleForAllPens: aTrailStyle
	"Ascribe the given trail style to all pens of objects within me"

	submorphs do: [:m | m assuredPlayer setTrailStyle: aTrailStyle]
! !


!Pen methodsFor: 'operations' stamp: 'sw 4/10/2003 22:37'!
putDotOfDiameter: aDiameter at: aPoint
	"Put a dot of the given size at the given point, using my colot"

	(FormCanvas on: destForm) 
			fillOval: (Rectangle center: aPoint extent: (aDiameter @ aDiameter))
			color: self color! !


!Player methodsFor: 'pen' stamp: 'sw 4/17/2003 12:26'!
getDotSize
	"Answer the receiver's dotSize"

	^ self costume renderedMorph valueOfProperty: #trailDotSize ifAbsentPut: [6]! !

!Player methodsFor: 'pen' stamp: 'sw 3/11/2003 11:28'!
getTrailStyle
	"Answer the receiver's trailStyle"

	^ self actorState trailStyle! !

!Player methodsFor: 'pen' stamp: 'sw 4/17/2003 11:56'!
setDotSize: aNumber
	"Set the trail dot size as indicated, but confine matters to a reasonable range"

	self costume renderedMorph setProperty: #trailDotSize toValue: ((aNumber max: 1) min: 100)! !

!Player methodsFor: 'pen' stamp: 'sw 3/11/2003 11:23'!
setTrailStyle: aTrailStyle
	"Set the trail style"

	self actorState trailStyle: aTrailStyle
! !

!Player methodsFor: 'pen' stamp: 'sw 3/11/2003 11:22'!
trailStyleForAllPens: aTrailStyle
	"Only for the Player of a World"

	self costume renderedMorph trailStyleForAllPens: aTrailStyle! !

!Player methodsFor: 'slots-kernel' stamp: 'sw 3/11/2003 11:52'!
usableMethodInterfacesIn: methodInterfaceList
	"Filter the list given by methodInterfaceList, to remove items inappropriate to the receiver"

	self hasCostumeThatIsAWorld ifTrue:
		[^ methodInterfaceList select: [:anInterface |
			#(append: prepend: beep: clearTurtleTrails doScript: getColor "color" getCursor "cursor" deleteCard doMenuItem emptyScript firstPage goToFirstCardInBackground goToFirstCardOfStack goToLastCardInBackground goToLastCardOfStack goToNextCardInStack goToPreviousCardInStack initiatePainting insertCard  liftAllPens lowerAllPens trailStyleForAllPens: arrowheadsOnAllPens noArrowheadsOnAllPens getMouseX getMouseY "mouseX mouseY" pauseScript: reverse roundUpStrays shuffleContents startScript: stopScript: unhideHiddenObjects getValueAtCursor "valueAtCursor"
startAll: pauseAll: stopAll:  
viewAllMessengers clobberAllMessengers openAllScriptsTool handScriptControlButtons viewAllReferencedObjects jumpToProject:)

includes: anInterface selector]].

	self hasAnyBorderedCostumes ifTrue: [^ methodInterfaceList].

	^ self hasOnlySketchCostumes
		ifTrue:
			[methodInterfaceList select: [:anInterface | (#(getColor getSecondColor getBorderColor getBorderWidth getBorderStyle  getRoundedCorners getUseGradientFill getRadialGradientFill ) includes: anInterface selector) not]]
		ifFalse:
			[methodInterfaceList select: [:anInterface | (#(getBorderColor getBorderWidth) includes: anInterface selector) not]]! !


!Preferences class methodsFor: 'misc' stamp: 'sw 3/2/2004 22:11'!
setArrowheads
	"Let the user edit the size of arrowheads"

	| aParameter result  |
	aParameter _ self parameterAt: #arrowSpec ifAbsent: [5 @ 4].
	result _ Morph obtainArrowheadFor: 'Default size of arrowheads on pen trails ' translated defaultValue: aParameter asString.
	result ifNotNil:
			[self setParameter: #arrowSpec to: result]
		ifNil:
			[Beeper beep]! !


!SearchingViewer methodsFor: 'categories' stamp: 'sw 4/10/2003 21:36'!
updateCategoryNameTo: aName
	"Update the category name, because of a language change."

	self doSearchFrom: (namePane findA: PluggableTextMorph) text interactive: false.
	self flag: #deferred.  "A nice touch would be to change the Button wording here"
! !

!SearchingViewer methodsFor: 'search' stamp: 'sw 4/10/2003 21:36'!
doSearchFrom:  aSource
	"Perform the search operation"

	^ self doSearchFrom: aSource interactive: true! !

!SearchingViewer methodsFor: 'search' stamp: 'sw 3/3/2004 01:29'!
doSearchFrom:  aSource interactive: isInteractive
	"Perform the search operation.  If interactive is true, this actually happened because a search button was pressed; if false, it was triggered some other way for which an informer would be inappropriate."

	| searchFor aVocab aList all anInterface useTranslations scriptNames addedMorphs |
	searchString _ (aSource isKindOf: PluggableTextMorph)
		ifFalse:
			[aSource]
		ifTrue:
			[aSource text string].
	searchFor _ searchString asString asLowercase withBlanksTrimmed.

	aVocab _ self outerViewer currentVocabulary.
	(useTranslations _ (scriptedPlayer isKindOf: Player) and: [aVocab isKindOf: EToyVocabulary])
		ifTrue:
			[all _ scriptedPlayer costume selectorsForViewer.
			all addAll: (scriptNames _ scriptedPlayer class namedTileScriptSelectors)]
		ifFalse:
			[all _ scriptNames _ scriptedPlayer class allSelectors].
	aList _ all select:
		[:aSelector | (aVocab includesSelector: aSelector forInstance: scriptedPlayer ofClass: scriptedPlayer class limitClass: ProtoObject) and:
			[(useTranslations and: [(anInterface _ aVocab methodInterfaceAt: aSelector ifAbsent: [nil]) notNil and: [anInterface elementWording includesSubstring: searchFor caseSensitive: false]])
				or:
					[((scriptNames includes: aSelector) or: [useTranslations not]) and:
						[aSelector includesSubstring: searchFor caseSensitive: false]]]].
	aList _ aList asSortedArray.

	self removeAllButFirstSubmorph. "that being the header"
	self addAllMorphs:
		((addedMorphs _ scriptedPlayer tilePhrasesForSelectorList: aList inViewer: self)).
	self enforceTileColorPolicy.
	self secreteCategorySymbol.
	self world ifNotNil: [self world startSteppingSubmorphsOf: self].
	self adjustColorsAndBordersWithin.

	owner ifNotNil: [owner isStandardViewer ifTrue: [owner fitFlap].

	(isInteractive and: [addedMorphs isEmpty]) ifTrue:
		[self inform: ('No matches found for "' translated), searchFor, '"']]! !


!Vocabulary class methodsFor: 'class initialization' stamp: 'sw 3/11/2003 12:36'!
initializeSilently
	"Initialize a few standard vocabularies and place them in the AllVocabularies list."

	self initializeStandardVocabularies.
	self embraceAddedTypeVocabularies.

	"Vocabulary initializeSilently"

! !

!Vocabulary class methodsFor: 'class initialization' stamp: 'sw 4/16/2003 12:27'!
initializeStandardVocabularies
	"Initialize a few standard vocabularies and place them in the AllStandardVocabularies list."

	AllStandardVocabularies _ nil.
	self allStandardVocabularies.

	self addEToyVocabulary.
	self addEToyVectorVocabulary.

	self addStandardVocabulary: self newPublicVocabulary.
	self addStandardVocabulary: FullVocabulary new.

	self addStandardVocabulary: self newQuadVocabulary.

	self addStandardVocabulary: ColorType new.
	self addStandardVocabulary: BooleanType new.
	self addStandardVocabulary: GraphicType new.
	self addStandardVocabulary: PlayerType new.
	self addStandardVocabulary: SoundType new.
	self addStandardVocabulary: StringType new.
	self addStandardVocabulary: MenuType new.
	self addStandardVocabulary: UnknownType new.
	self addStandardVocabulary: ScriptNameType new.

	self addStandardVocabulary: (SymbolListType new symbols: #(simple raised inset complexFramed complexRaised complexInset complexAltFramed complexAltRaised complexAltInset); vocabularyName: #BorderStyle; yourself).
	self addStandardVocabulary: (SymbolListType new symbols: #(lines arrows arrowheads dots); vocabularyName: #TrailStyle; yourself).
	self addStandardVocabulary: (SymbolListType new symbols: #(leftToRight rightToLeft topToBottom bottomToTop); vocabularyName: #ListDirection; yourself).

	self addStandardVocabulary: (SymbolListType new symbols: #(topLeft bottomRight center justified); vocabularyName: #ListCentering; yourself).

	self addStandardVocabulary: (SymbolListType new symbols: #(buttonDown whilePressed buttonUp); vocabularyName: #ButtonPhase; yourself).

	self addStandardVocabulary: (SymbolListType new symbols: #(rigid spaceFill shrinkWrap); vocabularyName: #Resizing; yourself).

	self addStandardVocabulary: self newSystemVocabulary.  "A custom vocabulary for Smalltalk -- still under development)"

	self numberVocabulary.  		"creates and adds it"
	self wonderlandVocabulary.  	"creates and adds it"
	self vocabularyForClass: Time.   "creates and adds it"

	"Vocabulary initialize"! !

"Postscript:"
Vocabulary initializeSilently.
!

