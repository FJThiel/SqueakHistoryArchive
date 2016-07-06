'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #347] on 14 October 2004 at 11:52:08 am'!"Change Set:		overlapsAny-nkDate:			14 October 2004Author:			Ned KonzAdds an 'overlaps any' test to the 'tests' Etoy vocabulary category.This test reports whether a Player's costume overlaps that of:	- another Player, or	- any of the other Player's siblings (if the other Player is a scripted player), or	- any other morphs of the same class as the other Player's costume (if the other Player is unscripted).This is probably more useful than touchesA:."!!Object methodsFor: 'scripts-kernel' stamp: 'nk 10/14/2004 10:55'!universalTilesForGetterOf: aMethodInterface	"Return universal tiles for a getter on the given method interface."	| ms argTile argArray itsSelector |	itsSelector _ aMethodInterface selector.	argArray _ #().	"Four gratuituous special cases..."	(itsSelector == #color:sees:) ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _ Array with: argTile colorSwatch color with: argTile colorSwatch color copy].	itsSelector == #seesColor: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _  Array with: argTile colorSwatch color].	(#(touchesA: overlaps: overlapsAny:) includes: itsSelector) ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Player.		argArray _ Array with: argTile actualObject].	ms _ MessageSend receiver: self selector: itsSelector arguments: argArray.	^ ms asTilesIn: self class globalNames: (self class officialClass ~~ CardPlayer)			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !!DataType methodsFor: 'tiles' stamp: 'nk 10/14/2004 10:54'!addWatcherItemsToMenu: aMenu forGetter: aGetter	"Add watcher items to the menu if appropriate, provided the getter is not an odd-ball one for which a watcher makes no sense"	(#(colorSees copy isOverColor: seesColor: newClone getNewClone color:sees: touchesA: overlaps: overlapsAny:) includes: aGetter) ifFalse:		[aMenu add: 'simple watcher' translated selector: #tearOffWatcherFor: argument: aGetter]! !!Morph class methodsFor: 'scripting' stamp: 'nk 10/14/2004 10:59'!additionsToViewerCategoryTests	"Answer viewer additions for the 'tests' category.""Note:  Because of intractable performance problems in continuously evaluating isOverColor in a Viewer, the isOverColor entry is not given a readout"	^#(		#tests 		(			(slot isOverColor 'whether any part of the object is over the given color' Boolean	readOnly Player seesColor: unused unused)			(slot isUnderMouse 'whether the object is under the current mouse position' Boolean readOnly	Player getIsUnderMouse unused unused)			(slot colorSees	'whether the given color sees the given color' Boolean readOnly	Player color:sees:	unused	unused)			(slot overlaps    'whether I overlap a given object' Boolean readOnly Player overlaps: unused unused)			(slot overlapsAny    'whether I overlap a given object or one of its siblings or similar objects' Boolean readOnly Player overlapsAny: unused unused)			(slot touchesA	'whether I overlap any  Sketch that is showing the same picture as a particular prototype.' Boolean readOnly Player touchesA:	unused	unused)			(slot obtrudes 'whether the object sticks out over its container''s edge' Boolean readOnly Player getObtrudes unused unused)		)	)! !!Player methodsFor: 'scripts-standard' stamp: 'nk 10/14/2004 11:09'!overlapsAny: aPlayer 	"Answer true if my costume overlaps that of aPlayer, or any of its  	siblings (if aPlayer is a scripted player)  	or if my costume overlaps any morphs of the same class (if aPlayer is  	unscripted)."	| possibleCostumes itsCostume itsCostumeClass myShadow |	(self overlaps: aPlayer)		ifTrue: [^ true].	possibleCostumes := IdentitySet new.	aPlayer belongsToUniClass		ifTrue: [aPlayer class				allSubInstancesDo: [:anInstance | (anInstance ~~ aPlayer							and: [itsCostume := anInstance costume.								itsCostume bounds intersects: costume bounds])						ifTrue: [possibleCostumes add: itsCostume]]]		ifFalse: [itsCostumeClass := aPlayer costume class.			self world allExtantPlayers				do: [:ep | ep costume						ifNotNilDo: [:ea | (ea class == itsCostumeClass									and: [ea bounds intersects: costume bounds])								ifTrue: [possibleCostumes add: ea]]]].	possibleCostumes isEmpty		ifTrue: [^ false].	myShadow := costume shadowForm.	^ possibleCostumes		anySatisfy: [:m | m overlapsShadowForm: myShadow bounds: costume fullBounds]! !!Player methodsFor: 'slots-kernel' stamp: 'nk 10/14/2004 10:56'!typeForSlotWithGetter: aGetter	"Answer the data type for values of the instance variable of the given name"	| getter inherentSelector |	(#(color:sees: seesColor: touchesA: overlaps: overlapsAny:) includes: aGetter) ifTrue: [^ #Boolean].  "Annoying special cases"	inherentSelector _ Utilities inherentSelectorForGetter: aGetter.	(self slotInfo includesKey: inherentSelector) ifTrue: [^ (self slotInfoAt: inherentSelector) type].	getter _ (aGetter beginsWith: 'get')		ifTrue:			[aGetter]		ifFalse:			[Utilities getterSelectorFor: aGetter].	^ (Vocabulary eToyVocabulary methodInterfaceAt: getter ifAbsent: [self error: 'Unknown slot name: ', aGetter]) resultType! !!Viewer methodsFor: 'special phrases' stamp: 'nk 10/14/2004 10:59'!overlapsAnyPhrase	"Answer a conjured-up overlaps phrase in classic tile"	| outerPhrase |	outerPhrase := PhraseTileMorph new 				setOperator: #+				type: #Boolean				rcvrType: #Player				argType: #Player.	"temp dummy"	(outerPhrase submorphs second) delete.	"operator"	outerPhrase addMorphBack: (TileMorph new setOperator: #overlapsAny:).	(outerPhrase submorphs second) goBehind.	"Make it third"	outerPhrase submorphs last addMorph: scriptedPlayer tileToRefer.	^outerPhrase! !!CategoryViewer methodsFor: 'entries' stamp: 'nk 10/14/2004 11:32'!phraseForVariableFrom: aMethodInterface	"Return a structure consisting of tiles and controls and a readout representing a 'variable' belonging to the player, complete with an appropriate readout when indicated.  Functions in both universalTiles mode and classic mode.  Slightly misnamed in that this path is used for any methodInterface that indicates an interesting resultType."	| anArrow slotName getterButton cover inner aRow doc setter tryer universal hotTileForSelf spacer buttonFont |	aRow _ ViewerLine newRow		color: self color;		beSticky;		elementSymbol: (slotName _ aMethodInterface selector);		wrapCentering: #center;		cellPositioning: #leftCenter.	(universal _ scriptedPlayer isUniversalTiles) ifFalse:		[buttonFont _ Preferences standardEToysFont.			aRow addMorphBack: (Morph new color: self color;					 extent: (((buttonFont widthOfString: '!!') + 8) @ (buttonFont height + 6));					 yourself)].  "spacer"	aRow addMorphBack: (self infoButtonFor: slotName).	aRow addMorphBack: (Morph new color: self color; extent: 0@10).  " spacer"	universal		ifTrue:			[inner _ scriptedPlayer universalTilesForGetterOf: aMethodInterface.			cover _ Morph new color: Color transparent.			cover extent: inner fullBounds extent.			(getterButton _ cover copy) addMorph: cover; addMorphBack: inner.			cover on: #mouseDown send: #makeUniversalTilesGetter:event:from: 					to: self withValue: aMethodInterface.			aRow addMorphFront:  (tryer _ ScriptingSystem tryButtonFor: inner).			tryer color: tryer color lighter lighter]		ifFalse:			[hotTileForSelf _ self tileForSelf bePossessive.			hotTileForSelf  on: #mouseDown send: #makeGetter:event:from:				to: self				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).			aRow addMorphBack: hotTileForSelf.			aRow addMorphBack: (spacer _ Morph new color: self color; extent: 2@10).			spacer on: #mouseEnter send: #addGetterFeedback to: aRow.			spacer on: #mouseLeave send: #removeHighlightFeedback to: aRow.			spacer on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.			spacer  on: #mouseDown send: #makeGetter:event:from:				to: self				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).			hotTileForSelf on: #mouseEnter send: #addGetterFeedback to: aRow.			hotTileForSelf on: #mouseLeave send: #removeHighlightFeedback to: aRow.			hotTileForSelf on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.			getterButton _ self getterButtonFor: aMethodInterface selector type: aMethodInterface resultType].	aRow addMorphBack: getterButton.	getterButton on: #mouseEnter send: #addGetterFeedback to: aRow.	getterButton on: #mouseLeave send: #removeHighlightFeedback to: aRow.	getterButton on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.	(doc _ aMethodInterface documentation) ifNotNil:		[getterButton setBalloonText: doc].	universal ifFalse:		[(slotName == #seesColor:) ifTrue:			[self addIsOverColorDetailTo: aRow.			^ aRow].		(slotName == #touchesA:) ifTrue:			[self addTouchesADetailTo: aRow.			^ aRow].		(slotName == #overlaps: or: [ slotName == #overlapsAny:]) ifTrue:			[self addOverlapsDetailTo: aRow.			^ aRow]].	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"	(setter _ aMethodInterface companionSetterSelector) ifNotNil:		[aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"		anArrow _ universal 			ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from:  						args: aMethodInterface]			ifFalse: [self arrowSetterButton: #makeSetter:from:forPart:						args: (Array with: slotName with: aMethodInterface resultType)].		anArrow beTransparent.		universal ifFalse:			[anArrow on: #mouseEnter send: #addSetterFeedback to: aRow.			anArrow on: #mouseLeave send: #removeHighlightFeedback to: aRow.			anArrow on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow].		aRow addMorphBack: anArrow].	(#(color:sees: playerSeeingColor copy touchesA: overlaps:) includes: slotName) ifFalse: 		[(universal and: [slotName == #seesColor:]) ifFalse:			[aMethodInterface wantsReadoutInViewer ifTrue: 				[aRow addMorphBack: (self readoutFor: slotName type: aMethodInterface resultType readOnly: setter isNil getSelector: aMethodInterface selector putSelector: setter)]]].	anArrow ifNotNil: [anArrow step].	^ aRow! !!CategoryViewer methodsFor: 'entries' stamp: 'nk 10/14/2004 10:54'!wantsRowMenuFor: aSymbol	"Answer whether a viewer row for the given symbol should have a menu button on it"	| elementType |	true ifTrue: [^ true].  "To allow show categories item.  So someday this method can be removed, and its sender can stop sending it..."	elementType _ scriptedPlayer elementTypeFor: aSymbol vocabulary: self currentVocabulary.	(elementType == #systemScript) ifTrue: [^ false].	((elementType == #systemSlot) and:		[#(color:sees: touchesA: overlaps: overlapsAny:) includes: aSymbol]) ifTrue: [^ false].	^ true! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'nk 10/14/2004 11:32'!getterTilesFor: getterSelector type: aType 	"Answer classic getter for the given name/type"	"aPhrase _ nil, assumed"	| selfTile selector aPhrase |	(#(#color:sees: #colorSees) includes: getterSelector) 		ifTrue: [aPhrase := self colorSeesPhrase].	(#(#seesColor: #isOverColor) includes: getterSelector) 		ifTrue: [aPhrase := self seesColorPhrase].	(#(#overlaps: #overlaps) includes: getterSelector) 		ifTrue: [aPhrase := self overlapsPhrase].	(#(#overlapsAny: #overlapsAny) includes: getterSelector) 		ifTrue: [aPhrase := self overlapsAnyPhrase].	(#(#touchesA: #touchesA) includes: getterSelector) 		ifTrue: [aPhrase := self touchesAPhrase].	aPhrase ifNil: 			[aPhrase := PhraseTileMorph new setSlotRefOperator: getterSelector asSymbol						type: aType].	selfTile := self tileForSelf bePossessive.	selfTile position: aPhrase firstSubmorph position.	aPhrase firstSubmorph addMorph: selfTile.	selector := aPhrase submorphs second.	(Vocabulary vocabularyNamed: aType capitalized) 		ifNotNilDo: [:aVocab | aVocab wantsSuffixArrow ifTrue: [selector addSuffixArrow]].	selector updateLiteralLabel.	aPhrase enforceTileColorPolicy.	^aPhrase! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'nk 10/14/2004 10:53'!newGetterTilesFor: aPlayer methodInterface: aMethodInterface	"Return universal tiles for a getter on this property.  Record who self is."	| ms argTile argArray |	ms _ MessageSend receiver: aPlayer selector: aMethodInterface selector arguments: #().	"Handle three idiosyncratic cases..."	aMethodInterface selector == #color:sees: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		argArray _ Array with: argTile colorSwatch color with: argTile colorSwatch color copy. 		ms arguments: argArray].	aMethodInterface selector == #seesColor: ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Color.		ms arguments: (Array with: argTile colorSwatch color)].	(#(touchesA: overlaps: overlapsAny:) includes: aMethodInterface selector) ifTrue:		[argTile _ ScriptingSystem tileForArgType: #Player.		ms arguments: (Array with: argTile actualObject)].	^ ms asTilesIn: aPlayer class globalNames: (aPlayer class officialClass ~~ CardPlayer)			"For CardPlayers, use 'self'.  For others, name it, and use its name."! !