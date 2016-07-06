'From Squeak3.2alpha of 3 October 2001 [latest update: #4530] on 21 November 2001 at 4:25:35 pm'!"Change Set:		viewerAdditions-swDate:			21 November 2001Author:			Scott WallaceAdds layout-related items, drop-shadow-related items, etc. to the viewer."!!Morph methodsFor: 'scripting' stamp: 'sw 11/5/2001 21:34'!categoriesForViewer	"Answer a list of symbols representing the categories to offer in the viewer, in order"	| aClass aList predetermined genericItems |	aClass _ self renderedMorph class.	aList _ OrderedCollection new.	[aClass == Morph] whileFalse:		[(aClass class includesSelector: #additionsToViewerCategories) ifTrue:			[aList addAllFirstUnlessAlreadyPresent: (aClass additionsToViewerCategories collect:				[:categorySpec | categorySpec first])].		aClass _ aClass superclass]. 	genericItems _ Morph additionsToViewerCategories collect:			[:categorySpec | categorySpec first].	aList removeAllFoundIn: genericItems.	aList addAllFirstUnlessAlreadyPresent: (Morph additionsToViewerCategories collect:			[:categorySpec | categorySpec first]) asSet asOrderedCollection.	predetermined _ #(basic #'color & border' geometry motion #'pen use' tests layout #'drag & drop' scripting miscellaneous) select:		[:sym | aList includes: sym].  "bulletproof agains change in those names elsewhere"	aList removeAllFoundIn: predetermined.	^ predetermined, aList! !!AlignmentMorph methodsFor: 'visual properties' stamp: 'sw 11/5/2001 15:11'!canHaveFillStyles	"Return true if the receiver can have general fill styles; not just colors.	This method is for gradually converting old morphs."	^ self class == AlignmentMorph "no subclasses"! !!Morph class methodsFor: 'scripting' stamp: 'sw 11/21/2001 16:18'!additionsToViewerCategories	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."	^ {		self additionsToViewerCategoryBasic.		self additionsToViewerCategoryScripts.		self additionsToViewerCategoryColorAndBorder.		self additionsToViewerCategoryGeometry.		self additionsToViewerCategoryMiscellaneous.		self additionsToViewerCategoryMotion.		self additionsToViewerCategoryPenUse.		self additionsToViewerCategoryLayout.		self additionsToViewerCategoryDragAndDrop,		self additionsToViewerCategoryScripting.		self additionsToViewerCategoryTests.	}! !!Morph class methodsFor: 'scripting' stamp: 'sw 11/16/2001 08:28'!additionsToViewerCategoryColorAndBorder	"Answer viewer additions for the 'color & border' category"	^#(		#'color & border' 		(			(slot color 'The color of the object' Color readWrite Player getColor  Player  setColor:)			(slot colorUnder 'The color under the center of the object' Color readOnly Player getColorUnder unused  unused )			(slot luminanceUnder 'The luminance under the center of the object' Number readOnly Player getLuminanceUnder unused unused)			(slot saturationUnder 'The saturation under the center of the object' Number readOnly Player getSaturationUnder unused unused)			(slot brightnessUnder 'The brightness under the center of the object' Number readOnly Player getBrightnessUnder unused unused)			(slot borderColor 'The color of the object''s border' Color readWrite Player getBorderColor Player  setBorderColor:)			(slot borderWidth 'The width of the object''s border' Number readWrite Player getBorderWidth Player setBorderWidth:)			(slot roundedCorners 'Whether corners should be rounded' Boolean readWrite Player getRoundedCorners Player setRoundedCorners:)			(slot gradientFill 'Whether a gradient fill should be used' Boolean readWrite Player getUseGradientFill Player setUseGradientFill:)			(slot radialFill 'Whether the gradient fill, if used, should be radial' Boolean readWrite Player getRadialGradientFill Player setRadialGradientFill:)			(slot dropShadow 'Whether a drop shadow is shown' Boolean readWrite Player getDropShadow Player setDropShadow:)			(slot shadowColor 'The color of the drop shadow' Color readWrite Player getShadowColor Player setShadowColor:)		)	)! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/16/2001 10:01'!additionsToViewerCategories	"Answer viewer additions for the 'layout' category"	^#((		layout 		(			(slot cellInset 'The cell inset' Number readWrite Player getCellInset Player setCellInset:)			(slot layoutInset 'The layout inset' Number readWrite Player getLayoutInset Player setLayoutInset:)			(slot listCentering 'The list centering' ListCentering readWrite Player getListCentering Player setListCentering:)			(slot hResizing  	'Horizontal resizing' Resizing readWrite Player 	getHResizing Player setHResizing:)			(slot vResizing  	'Vertical resizing' Resizing readWrite Player 	getVResizing Player setVResizing:)			(slot listDirection  'List direction' ListDirection readWrite Player 	getListDirection Player setListDirection:)			(slot wrapDirection 'Wrap direction' ListDirection readWrite Player 	getWrapDirection Player setWrapDirection:)		)))! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/16/2001 09:34'!columnPrototype	"Answer a prototypical column"	| sampleMorphs aColumn |	sampleMorphs _ #(red yellow green) collect:		[:aColor | Morph new extent: 130 @ 38; color: (Color perform: aColor); setNameTo: aColor asString; yourself].	aColumn _ self inAColumn: sampleMorphs.	aColumn setNameTo: 'Column'.	aColumn color: Color veryVeryLightGray.	aColumn cellInset: 4; layoutInset: 6.	aColumn enableDragNDrop.	aColumn setBalloonText: 'Things dropped into here will automatically be organized into a column. Once you have added your own items here, you will want to remove the sample colored rectangles that this started with, and you will want to change this balloon help message to one of your own!!'.	^ aColumn! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/2/2001 04:45'!inAColumn: aCollectionOfMorphs	"Answer a columnar AlignmentMorph holding the given collection"	| col |	col _ self newColumn		color: Color transparent;		vResizing: #shrinkWrap;		hResizing: #shrinkWrap;		layoutInset: 1;		borderColor: Color black;		borderWidth: 1;		wrapCentering: #center;		cellPositioning: #topCenter.	aCollectionOfMorphs do: [:each | col addMorphBack: each].	^ col! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/5/2001 15:11'!inARow: aCollectionOfMorphs	"Answer a row-oriented AlignmentMorph holding the given collection"	| aRow |	aRow _ self newRow		color: Color transparent;		vResizing: #shrinkWrap;		hResizing: #shrinkWrap;		layoutInset: 1;		borderColor: Color black;		borderWidth: 1;		wrapCentering: #center;		cellPositioning: #topCenter.	aCollectionOfMorphs do: [ :each | aRow addMorphBack: each].	^ aRow! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/16/2001 09:36'!rowPrototype	"Answer a prototypical row"	| sampleMorphs aRow |	sampleMorphs _ (1 to: (2 + 3 atRandom)) collect:		[:integer | EllipseMorph new extent: ((60 + (20 atRandom)) @ (80 + ((20 atRandom)))); color: Color random; setNameTo: ('egg',  integer asString); yourself].	aRow _ self inARow: sampleMorphs.	aRow setNameTo: 'Row'.	aRow enableDragNDrop.	aRow cellInset: 6.	aRow layoutInset: 8.	aRow setBalloonText: 'Things dropped into here will automatically be organized into a row. Once you have added your own items here, you will want to remove the sample colored eggs that this started with, and you will want to change this balloon help message to one of your own!!'.	aRow color: Color veryVeryLightGray.	^ aRow			"AlignmentMorph rowPrototype openInHand"! !!AlignmentMorph class methodsFor: 'instance creation' stamp: 'sw 11/16/2001 09:16'!supplementaryPartsDescriptions	"Extra items for parts bins"	^ {DescriptionForPartsBin		formalName: 'Column'		categoryList: #('Presentation')		documentation: 'An object that presents the things within it in a column'		globalReceiverSymbol: #AlignmentMorph		nativitySelector: #columnPrototype.	DescriptionForPartsBin		formalName: 'Row'		categoryList: #('Presentation')		documentation: 'An object that presents the things within it in a row'		globalReceiverSymbol: #AlignmentMorph		nativitySelector: #rowPrototype}! !!Player methodsFor: 'slots-kernel' stamp: 'sw 11/16/2001 08:43'!usableMethodInterfacesIn: methodInterfaceList	"Filter the list given by methodInterfaceList, to remove items inappropriate to the receiver"	self hasCostumeThatIsAWorld ifTrue:		[^ methodInterfaceList select: [:anInterface |			#(append: beep: clearTurtleTrails doScript: getColor "color" getCursor "cursor" deleteCard doMenuItem emptyScript firstPage goToFirstCardInBackground goToFirstCardOfStack goToLastCardInBackground goToLastCardOfStack goToNextCardInStack goToPreviousCardInStack initiatePainting insertCard  liftAllPens lowerAllPens arrowheadsOnAllPens noArrowheadsOnAllPens getMouseX getMouseY "mouseX mouseY" pauseScript: reverse roundUpStrays shuffleContents startScript: stopScript: unhideHiddenObjects getValueAtCursor "valueAtCursor"startAll: pauseAll: stopAll:  viewAllMessengers clobberAllMessengers openAllScriptsTool handScriptControlButtons viewAllReferencedObjects jumpToProject:)includes: anInterface selector]].	self hasAnyBorderedCostumes ifTrue: [^ methodInterfaceList].	^ self hasOnlySketchCostumes		ifTrue:			[methodInterfaceList select: [:anInterface | (#(getColor getBorderColor getBorderWidth getRoundedCorners getUseGradientFill getRadialGradientFill ) includes: anInterface selector) not]]		ifFalse:			[methodInterfaceList select: [:anInterface | (#(getBorderColor getBorderWidth) includes: anInterface selector) not]]! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:31'!getCellInset	"Getter for costume's cellInset"	^ costume cellInset! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 10:22'!getClipSubmorphs	"Getter for costume's clipSubmorphs"	^ costume renderedMorph clipSubmorphs! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:29'!getDragEnabled	"Getter for costume's dragEnabled"	^ costume dragEnabled! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:32'!getDropEnabled	"Getter for costume's dropEnabled"	^ costume dropEnabled! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:29'!getDropShadow	"Getter for costume's hasDropShadow"	^ costume hasDropShadow! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:29'!getHResizing	"Getter for costume's hResizing"	^ costume hResizing! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:29'!getLayoutInset	"Getter for costume's layoutInset"	^ costume layoutInset! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:31'!getListCentering	"Getter for costume's listCentering"	^ costume listCentering! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:30'!getListDirection	"Getter for costume's listDirection"	^ costume listDirection! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 08:32'!getRadialGradientFill	"Geter for costume's useGradientFill"	| aStyle |	^ (aStyle _ costume renderedMorph fillStyle) isGradientFill and:		[aStyle isRadialFill]! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:28'!getShadowColor	"Getter for costume's shadowColor"	^ costume shadowColor! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:30'!getSticky	"Getter for costume's isSticky"	^ costume isSticky! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 08:33'!getUseGradientFill	"Geter for costume's useGradientFill"	^ costume renderedMorph fillStyle isGradientFill! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:29'!getVResizing	"Getter for costume's vResizing"	^ costume vResizing! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:27'!getWrapDirection	"Getter for costume's wrapDirection"	^ costume wrapDirection! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:34'!setCellInset: aValue	"Setter for costume's cellInset"	costume cellInset: aValue! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 10:23'!setClipSubmorphs: aBoolean	"Setter for costume's clipSubmorphs"	costume renderedMorph clipSubmorphs: aBoolean.	costume renderedMorph changed! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:32'!setDragEnabled: aValue	"Setter for costume's dragEnabled"	costume dragEnabled: aValue! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:34'!setDropEnabled: aValue	"Setter for costume's dropEnabled"	costume dropEnabled: aValue! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:32'!setDropShadow: aValue	"Setter for costume's dropShadow"	costume hasDropShadow ~~ aValue ifTrue: [costume toggleDropShadow]! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 09:55'!setHResizing: aValue	"Setter for costume's hResizing"	costume hResizing: aValue asSymbol! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:34'!setLayoutInset: aValue	"Setter for costume's layoutInset"	costume layoutInset: aValue! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:33'!setListCentering: val	"Setter for costume's listCentering"	costume listCentering: val! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:33'!setListDirection: aValue	"Setter for costume's listDirection"	costume listDirection: aValue asSymbol! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 08:51'!setRadialGradientFill: aBoolean	"Setter for costume's radialGradientFill"	| aStyle |	(aStyle _ costume renderedMorph fillStyle) isGradientFill		ifTrue:			[aStyle isRadialFill ~~ aBoolean ifTrue:				[aStyle radial: aBoolean.				costume renderedMorph changed]]! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:31'!setShadowColor: aValue	"Setter for costume's shadowColor"	costume shadowColor: aValue! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:33'!setSticky: val	"Setter for costume's sticky"	costume sticky: val! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 08:25'!setUseGradientFill: aBoolean	"Setter for costume's useGradientFill"	costume fillStyle isGradientFill		ifTrue:			[aBoolean ifFalse: [costume renderedMorph useSolidFill]]		ifFalse:			[aBoolean ifTrue: [costume renderedMorph useGradientFill]]! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 09:55'!setVResizing: aValue	"Setter for costume's vResizing"	costume vResizing: aValue asSymbol! !!Player methodsFor: 'slot getters/setters' stamp: 'sw 11/16/2001 07:32'!setWrapDirection: aValue	"Setter for costume's wrapDirection"	costume wrapDirection: aValue! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 07:25'!acceptNewLiteral: aLiteral	"Accept the new literal"	self labelMorph useStringFormat.	self literal: aLiteral.	self labelMorph setBalloonText: (ScriptingSystem helpStringForOperator: literal).	self acceptNewLiteral.  "so tile scriptor can recompile if necessary"	self labelMorph informTarget! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 07:08'!arrowAction: delta	"Do what is appropriate when an arrow on the tile is pressed; delta will be +1 or -1"	| index |	owner ifNil: [^ self].	literal ifNotNil:		[(index _ choices indexOf: literal) > 0			ifTrue:				[self literal: (choices atWrap: index + delta).				self labelMorph setBalloonText: (ScriptingSystem helpStringForOperator: literal).				self acceptNewLiteral.				self labelMorph informTarget]]! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 06:58'!initialize	"initialize the receiver.  Set up an event handler so that a click on the tile will produce a pop-up of alternatives"	super initialize.	literal _ #nothing.	self on: #mouseDown send: #offerAllChoicesInAPopUp to: self! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/6/2001 12:00'!literal: anObject	"Set the receiver's literal as indicated"	literal _ anObject asSymbol.	self updateLiteralLabel.	self flag: #deferred.  "The below formerly was necessary but now is problematical, leading to low-space condition etc.  May need to revisit, since as I comment this out now I am uncertain what if anything this may break"	"self labelMorph informTarget"! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 07:31'!offerAllChoicesInAPopUp	"Offer all choices in a pop-up menu"	| aMenu |	owner ifNil: [^ self].	aMenu _ MenuMorph new defaultTarget: self.	choices do:		[:aSym | aMenu add: aSym target: self selector: #acceptNewLiteral: argument: aSym].	aMenu popUpInWorld: ActiveWorld! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/6/2001 13:30'!setLiteralInitially: anObject	"Establish the initial literal.  Get the label correct, but do *not* send the value back to the target via the setter (unlike #literal:)"	literal _ anObject ifNotNil: [anObject asSymbol].	self updateLiteralLabel! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 07:06'!updateLiteralLabel	"Update the wording emblazoned on the tile, if needed.  Copied down, for jimmying, unfortunately"	| myLabel |	(myLabel _ self labelMorph) ifNil: [^ self].	myLabel useStringFormat.	myLabel acceptValue: literal asString.	self changed.! !!SymbolListTile methodsFor: 'user interface' stamp: 'sw 11/16/2001 07:31'!wantsKeyboardFocusFor: aSubmorph	"Answer whether a plain mouse click on aSubmorph, a text-edit-capable thing, should result in a text selection there"	^ false! !!SymbolListTile methodsFor: 'private' stamp: 'sw 11/16/2001 07:08'!line1: line1	"Emblazon the receiver with the requested label.  If the receiver already has a label, make the new label be of the same class"	super line1: line1.	self labelMorph useStringFormat! !!SymbolListTile methodsFor: 'mouse handling' stamp: 'sw 11/16/2001 07:01'!mouseStillDown: evt	"The mouse is still down on the receiver.  Copied down uningratiatingly to get an urgently-needed asSymbol  call interjected where you see the arrow below"	| aPoint label |	self flag: #arNote. "Fix 'significant' events below"	upArrow ifNotNil:		[aPoint _ evt cursorPoint.		(label _ self labelMorph) ifNotNil:			[label step. literal _ label valueFromContents asSymbol].  "<---"		(upArrow containsPoint: aPoint) ifTrue:			[self abandonLabelFocus.			self variableDelay:				[self arrowAction: 1].			^ evt "hand noteSignificantEvent: evt"].		(downArrow containsPoint: aPoint) ifTrue:			[self abandonLabelFocus.			self variableDelay:				[self arrowAction: -1].			^ evt "hand noteSignificantEvent: evt"]].	super mouseStillDown: evt.! !!SymbolListType methodsFor: 'tiles' stamp: 'sw 11/16/2001 07:30'!symbols: symbolList	"Set the receiver's list of symbols as indicated"	symbols _ symbolList! !!Vocabulary class methodsFor: 'class initialization' stamp: 'sw 11/19/2001 12:13'!initializeStandardVocabularies	"Initialize a few standard vocabularies and place them in the AllStandardVocabularies list."	AllStandardVocabularies _ nil.	self allStandardVocabularies.	self addEToyVocabulary.	self addEToyVectorVocabulary.	self addStandardVocabulary: self newPublicVocabulary.	self addStandardVocabulary: FullVocabulary new.	self addStandardVocabulary: self newQuadVocabulary.	self addStandardVocabulary: ColorType new.	self addStandardVocabulary: BooleanType new.	self addStandardVocabulary: GraphicType new.	self addStandardVocabulary: PlayerType new.	self addStandardVocabulary: SoundType new.	self addStandardVocabulary: StringType new.	self addStandardVocabulary: MenuType new.	self addStandardVocabulary: UnknownType new.	self addStandardVocabulary: (SymbolListType new symbols: #(leftToRight rightToLeft topToBottom bottomToTop); vocabularyName: #ListDirection; yourself).	self addStandardVocabulary: (SymbolListType new symbols: #(topLeft bottomRight center justified); vocabularyName: #ListCentering; yourself).	self addStandardVocabulary: (SymbolListType new symbols: #(buttonDown whilePressed buttonUp); vocabularyName: #ButtonPhase; yourself).	self addStandardVocabulary: (SymbolListType new symbols: #(rigid spaceFill shrinkWrap); vocabularyName: #Resizing; yourself).	self addStandardVocabulary: self newSystemVocabulary.  "A custom vocabulary for Smalltalk -- still under development)"	self numberVocabulary.  		"creates and adds it"	self wonderlandVocabulary.  	"creates and adds it"	self vocabularyForClass: Time.   "creates and adds it"	"Vocabulary initialize"! !Player removeSelector: #usablePhraseSpecsIn:!"Postscript:"Vocabulary initialize.!