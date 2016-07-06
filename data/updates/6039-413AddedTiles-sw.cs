'From Squeak3.7-m17n of 21 July 2004 [latest update: #19] on 25 July 2004 at 8:38:55 pm'!"Change Set:		e13AddedTiles-swDate:			1 August 2004Author:			Scott Wallace, Diego Gomez Deck, Ned KonzDerived from Squeakland update 0222addedTiles-sw, dated 7 July 2004; adapted for Squeak 3.8a, sw 7/25/2004 -- three methods had to change in the reintegration.  Also, this update subsumes the 3.7gamma update 5978ViewerFontFixes-nk, which itself therefore need not/should not be published to 3.l8a.* Restores the 'is over color' test to the Viewer.  This was removed long ago when it was discovered that refreshing the readout was gobbling up an unconscionable amount of cpu time.  In this update, we adopt Andreas's suggestion that the tile be restored to the vocabulary, but that its readout in the Viewer be suppressed (in the same manner as is done for #copy and #color:sees:)* Adds Diego's 'turn toward' to the miscellaneous category.* Adds 'erase', allowing an object to be removed from the screen.  This has always been available via the doMenuCommand tile (i.e. use 'doMenuCommand delete') but a demand for having it more directly available has been heard in the Squeakland community.* Adds 'stamp', the first part of the long-standing 'stampAndErase' tile.  Now you can stamp without erasing -- actually rather nice to do with a moving or changing object sometimes.With the visibility of #stamp and #erase, the pre-existing #stampAndErase becomes redundant, but is retained for historical reasons.Objects removed via 'erase' are moved to the trash, from whence they can subsequently be retrieved manually if need be.Caution:  The individual etoy-translation methods and the etoy-translation template are not been changed by this update, so they all become a shade more incomplete, since they will lack #turnToward:, #stamp, and #erase."!!CategoryViewer methodsFor: 'entries' stamp: 'sw 7/25/2004 15:37'!addIsOverColorDetailTo: aRow	"Special-casee code for the boolean-valued phrase variously known as is-over-color or sees-color."	| clrTile |	aRow addMorphBack: (Morph new color: self color; extent: 2@10).  "spacer"	aRow addMorphBack: (clrTile _ Color blue newTileMorphRepresentative)."The following commented-out code put a readout up; the readout was very nice, but was very consumptive of cpu time, which is why the is-over-color tile got removed from the viewer long ago.  Now is-over-color is reinstated to the viewer, minus the expensive readout...""	aRow addMorphBack: (AlignmentMorph new beTransparent).	readout _ UpdatingStringMorphWithArgument new			target: scriptedPlayer; getSelector: #seesColor:; growable: false; putSelector: nil;			argumentTarget: clrTile colorSwatch argumentGetSelector: #color.	readout useDefaultFormat.	aTile _ StringReadoutTile new typeColor: Color lightGray lighter.	aTile addMorphBack: readout.	aRow addMorphBack: aTile.	aTile setLiteralTo: (scriptedPlayer seesColor: clrTile colorSwatch color) printString width: 30"! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 8/1/2004 15:39'!phraseForVariableFrom: aMethodInterface
	"Return a structure consisting of tiles and controls and a readout representing a 'variable' belonging to the player, complete with an appropriate readout when indicated.  Functions in both universalTiles mode and classic mode.  Slightly misnamed in that this path is used for any methodInterface that indicates an interesting resultType."

	| anArrow slotName getterButton cover inner aRow doc setter tryer universal hotTileForSelf spacer buttonFont |

	aRow _ ViewerLine newRow
		color: self color;
		beSticky;
		elementSymbol: (slotName _ aMethodInterface selector);
		wrapCentering: #center;
		cellPositioning: #leftCenter.

	(universal _ scriptedPlayer isUniversalTiles) ifFalse:
		[buttonFont _ Preferences standardEToysFont.
			aRow addMorphBack: (Morph new color: self color;
					 extent: (((buttonFont widthOfString: '!!') + 8) @ (buttonFont height + 6));
					 yourself)].  "spacer"

	aRow addMorphBack: (self infoButtonFor: slotName).
	aRow addMorphBack: (Morph new color: self color; extent: 0@10).  " spacer"

	universal
		ifTrue:
			[inner _ scriptedPlayer universalTilesForGetterOf: aMethodInterface.
			cover _ Morph new color: Color transparent.
			cover extent: inner fullBounds extent.
			(getterButton _ cover copy) addMorph: cover; addMorphBack: inner.
			cover on: #mouseDown send: #makeUniversalTilesGetter:event:from: 
					to: self withValue: aMethodInterface.
			aRow addMorphFront:  (tryer _ ScriptingSystem tryButtonFor: inner).
			tryer color: tryer color lighter lighter]
		ifFalse:
			[hotTileForSelf _ self tileForSelf bePossessive.
			hotTileForSelf  on: #mouseDown send: #makeGetter:event:from:
				to: self
				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).

			aRow addMorphBack: hotTileForSelf.
			aRow addMorphBack: (spacer _ Morph new color: self color; extent: 2@10).
			spacer on: #mouseEnter send: #addGetterFeedback to: aRow.
			spacer on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			spacer on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.
			spacer  on: #mouseDown send: #makeGetter:event:from:
				to: self
				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).
			hotTileForSelf on: #mouseEnter send: #addGetterFeedback to: aRow.
			hotTileForSelf on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			hotTileForSelf on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

			getterButton _ self getterButtonFor: aMethodInterface selector type: aMethodInterface resultType].

	aRow addMorphBack: getterButton.
	getterButton on: #mouseEnter send: #addGetterFeedback to: aRow.
	getterButton on: #mouseLeave send: #removeHighlightFeedback to: aRow.
	getterButton on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

	(doc _ aMethodInterface documentation) ifNotNil:
		[getterButton setBalloonText: doc].

	universal ifFalse:
		[(slotName == #seesColor:) ifTrue:
			[self addIsOverColorDetailTo: aRow.
			^ aRow].
		(slotName == #touchesA:) ifTrue:
			[self addTouchesADetailTo: aRow.
			^ aRow].
		(slotName == #overlaps:) ifTrue:
			[self addOverlapsDetailTo: aRow.
			^ aRow]].

	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"
	(setter _ aMethodInterface companionSetterSelector) ifNotNil:
		[aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"
		anArrow _ universal 
			ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from:  
						args: aMethodInterface]
			ifFalse: [self arrowSetterButton: #makeSetter:from:forPart:
						args: (Array with: slotName with: aMethodInterface resultType)].
		anArrow beTransparent.
		universal ifFalse:
			[anArrow on: #mouseEnter send: #addSetterFeedback to: aRow.
			anArrow on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			anArrow on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow].

		aRow addMorphBack: anArrow].
	(#(color:sees: playerSeeingColor copy touchesA: overlaps:) includes: slotName) ifFalse:
 		[(universal and: [slotName == #seesColor:]) ifFalse:
			[aMethodInterface wantsReadoutInViewer ifTrue: 
				[aRow addMorphBack: (self readoutFor: slotName type: aMethodInterface resultType readOnly: setter isNil getSelector: aMethodInterface selector putSelector: setter)]]].

	anArrow ifNotNil: [anArrow step].
	^ aRow! !!MethodInterface methodsFor: 'initialization' stamp: 'sw 8/1/2004 15:29'!initializeFromEToySlotSpec: tuple	"tuple holds an old etoy slot-item spec, of the form found in #additionsToViewerCategories methods.   Initialize the receiver to hold the same information"	| setter |	selector _ tuple seventh.	self		wording: (ScriptingSystem wordingForOperator: tuple second);		helpMessage: tuple third.	receiverType _ #Player.	resultSpecification _ ResultSpecification new.	resultSpecification resultType: tuple fourth.	(#(getNewClone seesColor: isOverColor: "etc.") includes: selector)		ifTrue:			[self setNotToRefresh]  "actually should already be nil"		ifFalse:			[self setToRefetch].	((tuple fifth == #readWrite) and: [((tuple size >= 9) and: [(setter _ tuple at: 9) ~~ #unused])]) ifTrue:		[resultSpecification companionSetterSelector: setter].		"An example of an old slot-item spec:(slot numericValue 'A number representing the current position of the knob.' number readWrite Player getNumericValue Player setNumericValue:)	1	#slot	2	wording	3	balloon help	4	type	5	#readOnly or #readWrite	6	#Player (not used -- ignore)	7	getter selector	8	#Player (not used -- ignore)	9	setter selector"	! !!Morph class methodsFor: 'scripting' stamp: 'sw 7/8/2004 00:20'!additionsToViewerCategoryMiscellaneous	"Answer viewer additions for the 'miscellaneous' category"	^#(		miscellaneous 		(			(command doMenuItem: 'do the menu item' Menu)			(command show 'make the object visible')			(command hide 'make the object invisible')			(command wearCostumeOf: 'wear the costume of...' Player)			(command fire 'trigger any and all of this object''s button actions')			(slot copy 'returns a copy of this object' Player readOnly Player getNewClone	 unused unused)			(slot elementNumber 'my index in my container' Number readWrite Player getIndexInOwner Player setIndexInOwner:)			(slot holder 'the object''s container' Player readOnly Player getHolder Player setHolder:)			(command stamp 'add my image to the pen trails')			(command erase 'remove this object from the screen')			(command stampAndErase 'add my image to the pen trails and go away')		)	)! !!Morph class methodsFor: 'scripting' stamp: 'dgd 8/8/2003 22:17'!additionsToViewerCategoryMotion	"Answer viewer additions for the 'motion' category"	^#(		motion 		(			(slot x 'The x coordinate' Number readWrite Player getX Player setX:)			(slot y  	'The y coordinate' Number readWrite Player 	getY Player setY:)			(slot heading  'Which direction the object is facing.  0 is straight up' Number readWrite Player getHeading Player setHeading:)			(command forward: 'Moves the object forward in the direction it is heading' Number)			(slot obtrudes 'whether the object sticks out over its container''s edge' Boolean readOnly Player getObtrudes unused unused) 			(command turnToward: 'turn toward the given object' Player) 			(command moveToward: 'move toward the given object' Player) 			(command turn: 'Change the heading of the object by the specified amount' Number)			(command bounce: 'bounce off the edge if hit' Sound) 			(command wrap 'wrap off the edge if appropriate') 			(command followPath 'follow the yellow brick road') 			(command goToRightOf: 'place this object to the right of another' Player)		)	)! !!Morph class methodsFor: 'scripting' stamp: 'sw 7/5/2004 23:30'!additionsToViewerCategoryTests	"Answer viewer additions for the 'tests' category.""Note:  Because of intractable performance problems in continuously evaluating isOverColor in a Viewer, the isOverColor entry is not given a readout"	^#(		#tests 		(			(slot isOverColor 'whether any part of the object is over the given color' Boolean	readOnly Player seesColor: unused unused)			(slot isUnderMouse 'whether the object is under the current mouse position' Boolean readOnly	Player getIsUnderMouse unused unused)			(slot colorSees	'whether the given color sees the given color' Boolean readOnly	Player color:sees:	unused	unused)			(slot overlaps    'whether I overlap a given object' Boolean readOnly Player overlaps: unused unused)			(slot touchesA	'whether I overlap any  Sketch that is showing the same picture as a particular prototype.' Boolean readOnly Player touchesA:	unused	unused)			(slot obtrudes 'whether the object sticks out over its container''s edge' Boolean readOnly Player getObtrudes unused unused)		)	)! !!Player methodsFor: 'misc' stamp: 'sw 7/8/2004 01:29'!erase	"Dismiss the receiver from the screen.  It can subsequently be found in the trash if need be, provided the preserveTrash preference is set to true"	self costume topRendererOrSelf dismissViaHalo! !!Player methodsFor: 'scripts-standard' stamp: 'dgd 8/8/2003 22:15'!moveToward: aPlayer	"Move a standard amount in the direction of the given player.  If the object has an instance variable named 'speed', the speed of the motion will be governed by that value"	self turnToward: aPlayer.	self forward: self getSpeed! !!Player methodsFor: 'scripts-standard' stamp: 'dgd 8/8/2003 22:15'!turnToward: aPlayer	"Turn to the direction of the given player."	| angle aCostume |	(aPlayer == nil or: [aPlayer == self]) ifTrue: [^ self].	((aCostume _ self costume) bounds intersects: aPlayer costume bounds) ifTrue: [^ self].	angle _ aCostume referencePosition bearingToPoint: aPlayer costume referencePosition.	self setHeading: angle.! !!Preferences class methodsFor: 'fonts' stamp: 'sw 7/25/2004 20:03'!attemptToRestoreClassicFonts	"If certain fonts formerly used in early versions of Squeak happen to be present in the image, restore them to their corresponding roles.  Not called by any other method -- intended to be invoked via do-it, possibly in a postscript"	"Preferences attemptToRestoreClassicFonts"	| aTextStyle |	#(	(setButtonFontTo:		NewYork		12)		(setCodeFontTo:			NewYork		12)		(setFlapsFontTo:			ComicBold		16)		(setEToysFontTo:			ComicBold		16)		(setListFontTo:			NewYork		12)		(setMenuFontTo:			NewYork		12)		(setWindowTitleFontTo:	NewYork		15)		(setSystemFontTo:		NewYork		12)) do:			[:triplet |				(aTextStyle _ TextStyle named: triplet second) ifNotNil:					[self perform: triplet first with: (aTextStyle fontOfSize: triplet third).					Transcript cr; show: triplet second, ' installed as ', (triplet first copyFrom: 4 to: triplet first size - 3)]]! !!Preferences class methodsFor: 'fonts' stamp: 'sw 7/25/2004 17:26'!setCodeFontTo: aFont	"Establish the code font."	Parameters at: #standardCodeFont put: aFont! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 7/25/2004 17:27'!restoreClassicEToyLook	"Restore classic EToy look, as closely as possible.  If ComicBold is present, restore it as the standard etoy and button font.  Substitute ComicSansMS and Accuny as respective alternatives if the classic fonts are absent.  If those also aren't available, do nothing."	| aTextStyle aFont | 	(aTextStyle _ TextStyle named: #ComicBold)		ifNotNil:			[aFont _ aTextStyle fontOfSize: 16.			Preferences setEToysFontTo: aFont.			Preferences setButtonFontTo: aFont]		ifNil:			[(aTextStyle _ TextStyle named: #ComicSansMS) ifNotNil:				[Preferences setEToysFontTo: (aTextStyle fontOfSize: 18)].			(aTextStyle _ TextStyle named: #Accuny) ifNotNil:				[Preferences setButtonFontTo: (aTextStyle fontOfSize: 12)]].	(aTextStyle _ TextStyle named: #NewYork)		ifNotNil:			[Preferences setSystemFontTo: (aTextStyle fontOfSize: 12)]! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 7/25/2004 15:29'!wordingForOperator: aString	"Answer the wording to be seen by the user for the given operator symbol/string"	| toTest |	toTest _ aString asString.	#(	(append:				'include at end')		(beep:					'make sound')		(bounce:				'bounce')		(clearTurtleTrails		'clear pen trails')		(clearOwnersPenTrails	'clear all pen trails')		(colorSees				'color  sees')		(color:sees:				'color sees')		(doMenuItem:			'do menu item')		(doScript:				'do')		(forward:				'forward by')		(turnToward:				'turn toward')		(moveToward:			'move toward')		(goToRightOf:			'align after')		(isDivisibleBy:			'is divisible by')		(liftAllPens				'lift all pens')		(lowerAllPens			'lower all pens')		(arrowheadsOnAllPens	'arrowheads on all pens')		(noArrowheadsOnAllPens	'no arrowheads on pens')		(pauseAll:				'pause all')		(pauseScript:			'pause script')		(max:					'max')		(min:					'min')		(seesColor:				'is over color')		(makeNewDrawingIn:	'start painting in')		(prepend:				'include at beginning')		(startAll:				'start all')		(startScript:				'start script')		(stopProgramatically	'stop')		(stopAll:					'stop all')		(stopScript:				'stop script')		(tellAllSiblings:			'tell all siblings')
		(tellSelfAndAllSiblings:	'send to all')		(turn:					'turn by')		(wearCostumeOf:		'look like'))	do:		[:pair | toTest = pair first ifTrue: [^ pair second]].	^ toTest	"StandardScriptingSystem initialize"! !!TextStyle class methodsFor: 'user interface' stamp: 'sw 7/25/2004 20:32'!importFontsFromStyleFiles	"Import any and all of the fonts found in the default directory in files named ComicBold.style, ComicPlain.style, NewYork.style, Palatino.style, Courier.style"	| aName |	#('ComicBold' 'ComicPlain' 'NewYork' 'Palatino' 'Courier') do:		[:frag |			(TextStyle knownTextStyles inlcudes: frag) ifFalse:				[(FileDirectory default fileExists: (aName _ frag, '.style'))						ifTrue:							[TextStyle default collectionFromFileNamed: aName]]].! !