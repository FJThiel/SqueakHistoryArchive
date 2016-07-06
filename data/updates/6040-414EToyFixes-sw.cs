'From Squeak3.7-m17n of 21 July 2004 [latest update: #19] on 25 July 2004 at 7:54:38 pm'!"Change Set:		e14EToyFixes-swDate:			25 July 2004Author:			Scott WallaceDerived from update 0223 of Squeakland; two methods needed to be changed in the integration.Several changes made as per the decisions at the etoy meetings of July 13 & 14, 2004:(1)  When a test/yes/no complex is obtained, attach it to the hand at its top-left corner rather than at its center.(2)  Make the default sound argument for the bounce: command be the sound of silence.(3)  When eToyFriendly is set, suppress the touchesA: test and the entire layout category.(4)  Add a new type called RotationStyle, which is used to govern how a sketch's picture is treated when the heading its heading changes.  This makes the rotationStyle feature accessible in viewers, and hence to user scripts.--- This version, for Squeak 3.8a, suppresses the flipHorizontal and flipVertical commands that evidently came into the system in update 5561flipHorizOrVert-mga, because use of those two commands could easily raise errors and strange results,when the sketch is wrapped in a TransformationMorph"!!EToyVocabulary methodsFor: 'method list' stamp: 'sw 7/14/2004 18:24'!phraseSymbolsToSuppress	"Answer a dictatorially-imposed list of phrase-symbols that are to be suppressed from viewers when the eToyFriendly preference is set to true.  This list at the moment corresponds to the wishes of Alan and Kim and the LA teachers using Squeak in school-year 2001-2"	^ Preferences eToyFriendly		ifTrue:			[#(moveToward: followPath goToRightOf:				getViewingByIcon initiatePainting				append: prepend: getClipSubmorphs touchesA:)]		ifFalse:			[#()]! !!Morph methodsFor: 'scripting' stamp: 'sw 7/14/2004 18:46'!categoriesForViewer	"Answer a list of symbols representing the categories to offer in the viewer, in order"	| aClass aList predetermined genericItems genericAdditions combined |	aClass _ self renderedMorph class.	aList _ OrderedCollection new.	[aClass == Morph] whileFalse:		[(aClass class includesSelector: #additionsToViewerCategories) ifTrue:			[aList addAllFirstUnlessAlreadyPresent: (aClass additionsToViewerCategories collect:				[:categorySpec | categorySpec first])].		aClass _ aClass superclass]. 	genericAdditions _ Morph additionsToViewerCategories.	genericItems _ genericAdditions collect:			[:categorySpec | categorySpec first].	aList removeAllFoundIn: genericItems.	aList addAllFirstUnlessAlreadyPresent: (genericAdditions collect:			[:categorySpec | categorySpec first]) asSet asOrderedCollection.	predetermined _ #(basic #'color & border' geometry motion #'pen use' tests layout #'drag & drop' scripting observation button search miscellaneous) select:		[:sym | aList includes: sym].  "bulletproof agains change in those names elsewhere"	aList removeAllFoundIn: predetermined.	combined _ predetermined, aList.	^ Preferences eToyFriendly		ifTrue:			[combined copyWithout: #layout]		ifFalse:			[combined]! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 8/1/2004 16:26'!phraseForCommandFrom: aMethodInterface
	"Answer a phrase for the non-slot-like command represented by aMethodInterface - classic tiles"

	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst aDocString universal tileBearingHelp |
	aDocString _ aMethodInterface documentation.
	names _ scriptedPlayer class namedTileScriptSelectors.

	resultType _ aMethodInterface resultType.
	cmd _ aMethodInterface selector.
	(universal _ scriptedPlayer isUniversalTiles)
		ifTrue:
			[aPhrase _ scriptedPlayer universalTilesForInterface: aMethodInterface]
		ifFalse: [cmd numArgs == 0
			ifTrue:
				[aPhrase _ PhraseTileMorph new vocabulary: self currentVocabulary.
				aPhrase setOperator: cmd
					type: resultType
					rcvrType: #Player]
			ifFalse:
				["only one arg supported in classic tiles, so if this is fed
				with a selector with > 1 arg, results will be very strange"
				argType _ aMethodInterface typeForArgumentNumber: 1.
				aPhrase _ PhraseTileMorph new vocabulary: self currentVocabulary.
				aPhrase setOperator: cmd
					type: resultType
					rcvrType: #Player
					argType: argType.
				argTile _ ScriptingSystem tileForArgType: argType.				(#(bounce: wrap:) includes: cmd) ifTrue:					["help for the embattled bj"					argTile setLiteral: #silence].
				argTile position: aPhrase lastSubmorph position.
				aPhrase lastSubmorph addMorph: argTile]].

	(scriptedPlayer slotInfo includesKey: cmd)
		ifTrue: [balloonTextSelector _ #userSlot].

	(scriptedPlayer belongsToUniClass and: [scriptedPlayer class includesSelector: cmd])
		ifTrue:
			[aDocString ifNil:
				[aDocString _ (scriptedPlayer class userScriptForPlayer: scriptedPlayer selector: cmd) documentation].
			aDocString ifNil:
				[balloonTextSelector _ #userScript]].

	tileBearingHelp _ universal ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]. 
	aDocString
		ifNotNil:
			[tileBearingHelp setBalloonText: aDocString translated]
		ifNil:
			[balloonTextSelector ifNil:
				[tileBearingHelp setProperty: #inherentSelector toValue: cmd.
				balloonTextSelector _ #methodComment].
			tileBearingHelp balloonTextSelector: balloonTextSelector].
	aPhrase markAsPartsDonor.
	cmd == #emptyScript ifTrue:
		[aPhrase setProperty: #newPermanentScript toValue: true.
		aPhrase setProperty: #newPermanentPlayer toValue: scriptedPlayer.
		aPhrase submorphs second setBalloonText: 
'drag and drop to 
add a new script' translated].

	universal ifFalse:
		[selfTile _ self tileForSelf.
		selfTile position: aPhrase firstSubmorph position.
		aPhrase firstSubmorph addMorph: selfTile].

	aRow _ ViewerLine newRow borderWidth: 0; color: self color.
	aRow elementSymbol: cmd asSymbol.

	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).
	aRow addMorphBack: (Morph new extent: 2@2; beTransparent).
	aRow addMorphBack: (self infoButtonFor: cmd).
	aRow addMorphBack: aPhrase.
	aPhrase on: #mouseEnter send: #addCommandFeedback to: aRow.
	aPhrase on: #mouseLeave send: #removeHighlightFeedback to: aRow.
	aPhrase on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

	(names includes: cmd) ifTrue:
		[aPhrase userScriptSelector: cmd.
		cmd numArgs == 0 ifTrue:
			[aPhrase beTransparent.
			aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.
			aRow addMorphBack: (stat _ (inst _ scriptedPlayer scriptInstantiationForSelector: cmd) statusControlMorph).
			inst updateStatusMorph: stat]].

	aRow beSticky; disableDragNDrop.

	^ aRow! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 7/14/2004 21:37'!getRotationStyle	"Answer the symbol representing the rotation style"	^ (#(rotate #'do not rotate' #'flip left right' #'flip up down') at:		(#(normal none leftRight upDown ) indexOf: costume renderedMorph rotationStyle))! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 7/14/2004 21:35'!setRotationStyle: aStyleSymbol	"Set the rotation style to the indicated symbol; the external symbols seen are different, as you'll observe..."	costume renderedMorph rotationStyle: 		(#(normal none leftRight upDown ) at:		(#(rotate #'do not rotate' #'flip left right' #'flip up down') indexOf: aStyleSymbol))! !!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sw 7/13/2004 14:30'!addYesNoToHand	"Place a test/yes/no complex in the hand of the beloved user"	| ms messageNodeMorph aMorph |	Preferences universalTiles		ifTrue:			[ms _ MessageSend receiver: true selector: #ifTrue:ifFalse: 						arguments: {['do nothing']. ['do nothing']}.			messageNodeMorph _ ms asTilesIn: playerScripted class globalNames: true.			self primaryHand attachMorph: messageNodeMorph]		ifFalse:			[aMorph _ CompoundTileMorph new.			ActiveHand attachMorph: aMorph.			aMorph position: ActiveHand position.			aMorph formerPosition: ActiveHand position]! !!SketchMorph class methodsFor: 'scripting' stamp: 'sw 7/14/2004 21:11'!additionsToViewerCategories	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."	^ #((graphics ((slot graphic 	'The picture currently being worn' Graphic	 readWrite Player getGraphic Player setGraphic:)(command wearCostumeOf: 'wear the costume of...' Player)(slot baseGraphic 	'The picture originally painted for this object, but can subsequently be changed via menu or script' Graphic	 readWrite Player getBaseGraphic Player setBaseGraphic:)(command restoreBaseGraphic 'Make my picture be the one I remember in my baseGraphic')(slot rotationStyle 'How the picture should change when the heading is modified' RotationStyle readWrite Player getRotationStyle Player setRotationStyle:))))! !!Vocabulary class methodsFor: 'class initialization' stamp: 'sw 8/1/2004 16:20'!initializeStandardVocabularies
	"Initialize a few standard vocabularies and place them in the AllStandardVocabularies list."

	AllStandardVocabularies _ nil.
	
self addStandardVocabulary: EToyVocabulary new.	self addStandardVocabulary: EToyVectorVocabulary new.

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
	self addStandardVocabulary: (SymbolListType new symbols: #(rotate #'do not rotate' #'flip left right' #'flip up down'); vocabularyName: #RotationStyle; yourself).
	self addStandardVocabulary: (SymbolListType new symbols: #(rigid spaceFill shrinkWrap); vocabularyName: #Resizing; yourself).

	self addStandardVocabulary: self newSystemVocabulary.  "A custom vocabulary for Smalltalk -- still under development)"

	self numberVocabulary.  		"creates and adds it"
	self wonderlandVocabulary.  	"creates and adds it"
	self vocabularyForClass: Time.   "creates and adds it"

	"Vocabulary initialize"! !"Postscript:"Vocabulary initialize.!