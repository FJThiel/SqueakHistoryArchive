'From Squeak3.3alpha of 18 January 2002 [latest update: #4969] on 6 September 2002 at 1:23:24 pm'!"Change Set:		overlaps-swDate:			6 September 2002Author:			Scott Wallace, Leo Burd, John Maloney, Ted KaehlerPublished as 4970overlaps-sw.cs to 3.3aAdds an 'overlaps' test to the etoy vocabulary.  This is simple object-to-object collision detection, and is smart about jagged edges, transparency, etc.NOTE:  The 'touchesA:' test is changed to exploit this better collision detection (previously it had only looked for bounding-box intersection) but is otherwise left with its quite inscrutable semantics (consult comment in Player>>touchesA: for details.)Also, this update changes the behavior of the 'show categories' function from the viewer-entry menu; now, instead of replacing an existing viewer pane with one pointing at the new category, it *adds* a new viewer pane showing the new category -- per Kim's suggestion"!!Viewer commentStamp: 'sw 9/6/2002 13:14' prior: 0!An abstract superclass for both CategoryViewer and StandardViewer.  A viewer is always associated with a particular 'scriptedPlayer' -- the object whose protocol it shows in tile form.!!Object methodsFor: 'scripts-kernel' stamp: 'sw 9/6/2002 11:33'!universalTilesForGetterOf: aMethodInterface	"Return universal tiles for a getter on the given method interface."	| ms argTile argArray itsSelector |	itsSelector _ aMethodInterface selector.	argArray _ #().	"Four gratuituous special cases..."	(itsSelector == #color:sees:) ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _ Array with: argTile colorSwatch color with: argTile colorSwatch color copy].	itsSelector == #seesColor: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _  Array with: argTile colorSwatch color].	(#(touchesA: overlaps:) includes: itsSelector) ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Player.		argArray _ Array with: argTile actualObject].	ms _ MessageSend receiver: self selector: itsSelector arguments: argArray.	^ ms asTilesIn: self class globalNames: (self class officialClass ~~ CardPlayer)			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !!DataType methodsFor: 'tiles' stamp: 'sw 9/6/2002 11:32'!addWatcherItemsToMenu: aMenu forGetter: aGetter	"Add watcher items to the menu if appropriate, provided the getter is not an odd-ball one for which a watcher makes no sense"	(#(colorSees copy newClone getNewClone color:sees: touchesA: overlaps:) includes: aGetter) ifFalse:		[aMenu add: 'simple watcher' selector: #tearOffWatcherFor: argument: aGetter]! !!Form methodsFor: 'image manipulation' stamp: 'LB 8/26/2002 18:08'!stencil	"return a 1-bit deep, black-and-white stencil of myself"	| canvas |	canvas _ FormCanvas extent: self extent depth: 1.	canvas fillColor: (Color white).	canvas stencil: self at: 0@0  				sourceRect: (Rectangle origin: 0@0 corner: self extent) color: Color black.	^ canvas form! !!Morph class methodsFor: 'scripting' stamp: 'sw 9/6/2002 13:20'!additionsToViewerCategoryTests	"Answer viewer additions for the 'tests' category.  Note that isOverColor is commented out owing to intractable performance problems in continuously evaluating it in a Viewer -- someone should attend to this someday."	^#(		#tests 		(			"(slot isOverColor 'whether any part of the object is over the given color' Boolean	readOnly Player seesColor: unused unused) "			(slot isUnderMouse 'whether the object is under the current mouse position' Boolean readOnly	Player getIsUnderMouse unused unused)			(slot colorSees	'whether the given color sees the given color' Boolean readOnly	Player color:sees:	unused	unused)			(slot overlaps    'whether I overlap a given object' Boolean readOnly Player overlaps: unused unused)			(slot touchesA	'whether I overlap any  Sketch that is showing the same picture as a particular prototype.' Boolean readOnly Player touchesA:	unused	unused)			(slot obtrudes 'whether the object sticks out over its container''s edge' Boolean readOnly Player getObtrudes unused unused)		)	)! !!Player methodsFor: 'slots-kernel' stamp: 'sw 9/6/2002 11:33'!typeForSlotWithGetter: aGetter	"Answer the data type for values of the instance variable of the given name"	| getter inherentSelector |	(#(color:sees: touchesA: overlaps:) includes: aGetter) ifTrue: [^ #Boolean].  "Annoying special cases"	inherentSelector _ Utilities inherentSelectorForGetter: aGetter.	(self slotInfo includesKey: inherentSelector) ifTrue: [^ (self slotInfoAt: inherentSelector) type].	getter _ (aGetter beginsWith: 'get')		ifTrue:			[aGetter]		ifFalse:			[Utilities getterSelectorFor: aGetter].	^ (Vocabulary eToyVocabulary methodInterfaceAt: getter ifAbsent: [self error: 'Unknown slot name: ', aGetter]) resultType! !!Player methodsFor: 'misc' stamp: 'sw 9/6/2002 12:53'!overlaps: aPlayer	"Answer whether my costume overlaps that of another player"	| goalCostume intersection myShadow goalShadow bb myRect goalRect |	aPlayer ifNil: [^ false].	goalCostume _ aPlayer costume.	"check if the 2 player costumes intersect"	intersection _ costume bounds intersect: goalCostume bounds.	((intersection width = 0) or: [ intersection height = 0])		ifTrue:			[^ false]		ifFalse:			["check if the overlapping region is non-transparent"			"compute 1-bit, black and white versions (stencils) of the intersecting  			part of each morph's costume"			myRect _ intersection translateBy: 0@0 - costume topLeft.			myShadow _ (costume imageForm contentsOfArea: myRect) stencil.			goalRect _ intersection translateBy: 0@0 - goalCostume topLeft.			goalShadow _ (goalCostume imageForm contentsOfArea: goalRect) stencil.			"compute a pixel-by-pixel AND of the two stencils.  Result will be black 			(pixel value = 1) where black parts of the stencils overlap"			bb _ BitBlt toForm: myShadow.			bb copyForm: goalShadow to: 0@0 rule: Form and.			"return TRUE if resulting form contains any black pixels"			^ (bb destForm tallyPixelValues at: 2) > 0]! !!Player methodsFor: 'misc' stamp: 'sw 9/6/2002 13:11'!touchesA: aPrototypicalPlayer	"Answer whether the receiver overlaps any player who wears a Sketch costume and who is of the same class as the prototypicalPlayer and who is wearing the same bitmap, but who is *not that player itself*!!  This is an extreme case of a function highly customized (by Bob Arning) to suit a single, idiosycratic, and narrow demo need of Alan's.  Consult:http://groups.yahoo.com/group/squeak/message/40560"	| envelope trueNeighbor trueGoal trueSelf itsPlayer |	aPrototypicalPlayer ifNil: [^ false].	envelope _ costume owner ifNil: [^ false].	trueSelf _ costume renderedMorph.	trueGoal _ aPrototypicalPlayer costume renderedMorph.	envelope submorphs do: [:each |		trueNeighbor _ each renderedMorph.		(trueNeighbor == trueGoal or: [trueNeighbor == trueSelf]) ifFalse:			[(itsPlayer _ each player) ifNotNil:				[(itsPlayer overlaps: self) ifTrue:					[(trueGoal appearsToBeSameCostumeAs: trueNeighbor) ifTrue: [^ true]]]]].	^ false! !!Viewer methodsFor: 'special phrases' stamp: 'sw 9/6/2002 11:29'!overlapsPhrase	"Answer a conjured-up overlaps phrase in classic tile"	| outerPhrase |	outerPhrase _ PhraseTileMorph new setOperator: #+ "temp dummy" 				type: #Boolean rcvrType: #Player argType: #Player.	(outerPhrase submorphs at: 2) delete.	"operator"	outerPhrase addMorphBack: (TileMorph new setOperator: #overlaps:).	(outerPhrase submorphs at: 2) goBehind.		"Make it third"	outerPhrase submorphs last addMorph: scriptedPlayer tileToRefer.	^ outerPhrase! !!CategoryViewer methodsFor: 'categories' stamp: 'sw 9/6/2002 10:55'!showCategoriesFor: aSymbol	"Put up a pop-up list of categories in which aSymbol is filed; replace the receiver with a CategoryViewer for the one the user selects, if any"	| allCategories aVocabulary hits meths chosen |	aVocabulary _ self currentVocabulary.	allCategories _ scriptedPlayer categoriesForVocabulary: aVocabulary limitClass: ProtoObject.	hits _ allCategories select:		[:aCategory | 			meths _ aVocabulary allMethodsInCategory: aCategory forInstance: scriptedPlayer ofClass: scriptedPlayer class.			meths includes: aSymbol].	chosen _ (SelectionMenu selections: hits) startUp.	chosen isEmptyOrNil ifFalse:		[self outerViewer addCategoryViewerFor: chosen atEnd: true]	! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 9/6/2002 11:54'!addOverlapsDetailTo: aRow	"Disreputable magic: add necessary items to a viewer row abuilding for the overlaps phrase"	aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer".	aRow addMorphBack:  self tileForSelf.	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 9/6/2002 11:50'!phraseForVariableFrom: aMethodInterface	"Return a structure consisting of tiles and controls and a readout representing a 'variable' belonging to the player, complete with an appropriate readout when indicated.  Functions in both universalTiles mode and classic mode.  Slightly misnamed in that this path is used for any methodInterface that indicates an interesting resultType."	| anArrow slotName getterButton cover inner aRow doc setter tryer universal |	aRow _ ViewerLine newRow		color: self color;		beSticky;		elementSymbol: (slotName _ aMethodInterface selector);		wrapCentering: #center;		cellPositioning: #leftCenter.	(universal _ scriptedPlayer isUniversalTiles) ifFalse:		[aRow addMorphBack: (Morph new color: self color;		extent: 11 @ 22; yourself)].  "spacer"	aRow addMorphBack: (self infoButtonFor: slotName).	aRow addMorphBack: (Morph new color: self color; extent: 0@10).  " spacer"	universal ifTrue:			[inner _ scriptedPlayer universalTilesForGetterOf: aMethodInterface.			cover _ Morph new color: Color transparent.			cover extent: inner fullBounds extent.			(getterButton _ cover copy) addMorph: cover; addMorphBack: inner.			cover on: #mouseDown send: #makeUniversalTilesGetter:event:from: 					to: self withValue: aMethodInterface.			aRow addMorphFront:  (tryer _ ScriptingSystem tryButtonFor: inner).			tryer color: tryer color lighter lighter]		ifFalse:			[aRow addMorphBack: self tileForSelf bePossessive.			aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"			getterButton _ self getterButtonFor: aMethodInterface selector type: aMethodInterface resultType].	aRow addMorphBack: getterButton.	(doc _ aMethodInterface documentationOrNil) ifNotNil:		[getterButton setBalloonText: doc].	universal ifFalse:		[(slotName == #seesColor:) ifTrue:			[self addIsOverColorDetailTo: aRow.			^ aRow].		(slotName == #touchesA:) ifTrue:			[self addTouchesADetailTo: aRow.			^ aRow].		(slotName == #overlaps:) ifTrue:			[self addOverlapsDetailTo: aRow.			^ aRow]].	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"	(setter _ aMethodInterface companionSetterSelector) ifNotNil:		[aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"		anArrow _ universal 			ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from:  						args: aMethodInterface]			ifFalse: [self arrowSetterButton: #makeSetter:from:forPart:						args: (Array with: slotName with: aMethodInterface resultType)].		aRow addMorphBack: anArrow].	(#(color:sees: playerSeeingColor copy touchesA: overlaps:) includes: slotName) ifFalse: 		[(universal and: [slotName == #seesColor:]) ifFalse:			[aRow addMorphBack: (self readoutFor: slotName type: aMethodInterface resultType readOnly: setter isNil getSelector: aMethodInterface selector putSelector: setter)]].	anArrow ifNotNil: [anArrow step].	^ aRow! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 9/6/2002 11:31'!wantsRowMenuFor: aSymbol	"Answer whether a viewer row for the given symbol should have a menu button on it"	| elementType |	true ifTrue: [^ true].  "To allow show categories item.  So someday this method can be removed, and its sender can stop sending it..."	elementType _ scriptedPlayer elementTypeFor: aSymbol vocabulary: self currentVocabulary.	(elementType == #systemScript) ifTrue: [^ false].	((elementType == #systemSlot) and:		[#(color:sees: touchesA: overlaps:) includes: aSymbol]) ifTrue: [^ false].	^ true! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'sw 9/6/2002 11:57'!getterTilesFor: getterSelector type: aType	"Answer classic getter for the given name/type"	|  selfTile selector  aPhrase |	"aPhrase _ nil, assumed"	(#(color:sees: colorSees) includes: getterSelector) ifTrue: [aPhrase _ self colorSeesPhrase].	(#(seesColor: isOverColor) includes: getterSelector) ifTrue: [aPhrase _ self seesColorPhrase].	(#(overlaps: overlaps) includes: getterSelector) ifTrue: [aPhrase _ self overlapsPhrase].	(#(touchesA: touchesA) includes: getterSelector) ifTrue: [aPhrase _ self touchesAPhrase].	aPhrase ifNil: [aPhrase _ PhraseTileMorph new setSlotRefOperator: getterSelector asSymbol type: aType].	selfTile _ self tileForSelf bePossessive.	selfTile position: aPhrase firstSubmorph position.	aPhrase firstSubmorph addMorph: selfTile.	selector _ aPhrase submorphs at: 2.	(Vocabulary vocabularyNamed: aType capitalized)		ifNotNilDo:			[:aVocab | aVocab wantsSuffixArrow ifTrue:				[selector addSuffixArrow]].	selector updateLiteralLabel.	aPhrase enforceTileColorPolicy.	^ aPhrase! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'sw 9/6/2002 11:30'!newGetterTilesFor: aPlayer methodInterface: aMethodInterface	"Return universal tiles for a getter on this property.  Record who self is."	| ms argTile argArray |	ms _ MessageSend receiver: aPlayer selector: aMethodInterface selector arguments: #().	"Handle three idiosyncratic cases..."	aMethodInterface selector == #color:sees: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _ Array with: argTile colorSwatch color with: argTile colorSwatch color copy. 		ms arguments: argArray].	aMethodInterface selector == #seesColor: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		ms arguments: (Array with: argTile colorSwatch color)].	aMethodInterface selector == #touchesA: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Player.		ms arguments: (Array with: argTile actualObject)].	aMethodInterface selector == #overlaps: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Player.		ms arguments: (Array with: argTile actualObject)].	^ ms asTilesIn: aPlayer class globalNames: (aPlayer class officialClass ~~ CardPlayer)			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !