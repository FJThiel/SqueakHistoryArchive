'From Squeak3.1alpha of 28 February 2001 [latest update: #3886] on 28 March 2001 at 4:46:14 pm'!"Change Set:		univUnbreakage-swDate:			28 March 2001Author:			Scott WallaceFixes several things that got broken regarding universal-tiles scripting by the recent integration with Vocabularies.* Fixes error seen when grabbing a getter from a univ-tiles viewer.* Fixes error seen when dragging a value tile over a tile directly following a setter statement in a univ-tiles scriptor -- the methodInterfaces for the setters of etoy slots had been inadequately set up so the type-checking of arguments ran into an error.* Finds the right morph representing the MethodNode upon script rename.* Fixes bug that wrongly set a MethodInterface's playerClass to the player itself upon a script rename -- and bulletproofs against bad data resulting from that bug."!!CategoryViewer methodsFor: 'entries' stamp: 'sw 3/28/2001 13:26'!phraseForVariableFrom: aMethodInterface	"Return a structure consisting of tiles and controls and a readout representing a 'variable' belonging to the player, complete with an appropriate readout when indicated.  Functions in both universalTiles mode and classic mode.  Slightly misnamed in that this path is used for any methodInterface that indicates an interesting resultType."	| anArrow slotName getterButton cover inner aRow doc setter tryer universal |	aRow _ ViewerRow newRow		color: self color;		beSticky;		elementSymbol: (slotName _ aMethodInterface elementWording);		wrapCentering: #center;		cellPositioning: #leftCenter.	(universal _ scriptedPlayer isUniversalTiles) ifFalse:		[aRow addMorphBack: (Morph new color: self color;		extent: 11 @ 22; yourself)].  "spacer"	aRow addMorphBack: (self infoButtonFor: slotName).	aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"	universal ifTrue:			[inner _ scriptedPlayer universalTilesForGetterOf: aMethodInterface.			cover _ Morph new color: Color transparent.			cover extent: inner fullBounds extent.			(getterButton _ cover copy) addMorph: cover; addMorphBack: inner.			cover on: #mouseDown send: #makeUniversalTilesGetter:event:from: 					to: self withValue: aMethodInterface.			aRow addMorphFront:  (tryer _ ScriptingSystem tryButtonFor: inner).			tryer color: tryer color lighter lighter]		ifFalse:			[aRow addMorphBack: self tileForSelf bePossessive.			aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"			getterButton _ self getterButtonFor: aMethodInterface elementWording type: aMethodInterface resultType].	aRow addMorphBack: getterButton.	(doc _ aMethodInterface documentationOrNil) ifNotNil:		[getterButton setBalloonText: doc].	universal ifFalse:		[(slotName == #isOverColor) ifTrue:			[self addIsOverColorDetailTo: aRow.			^ aRow].		(slotName == #touchesA) ifTrue:			[self addTouchesADetailTo: aRow.			^ aRow]].	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"	(setter _ aMethodInterface companionSetterSelector) ifNotNil:		[aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"		anArrow _ universal 			ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from:  						args: aMethodInterface]			ifFalse: [self arrowSetterButton: #makeSetter:from:forPart:						args: (Array with: slotName with: aMethodInterface resultType)].		aRow addMorphBack: anArrow].	(#(colorSees playerSeeingColor copy touchesA) includes: slotName) ifFalse: 		[(universal and: [slotName == #isOverColor]) ifFalse:			[aRow addMorphBack: (self readoutFor: slotName type: aMethodInterface resultType readOnly: setter isNil getSelector: aMethodInterface selector putSelector: setter)]].	anArrow ifNotNil: [anArrow step].	^ aRow! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'sw 3/28/2001 12:54'!makeUniversalTilesGetter: aMethodInterface event: evt from: aMorph	"Button in viewer performs this to make a universal-tiles getter and attach it to hand."	| newTiles |	newTiles _ self newGetterTilesFor: scriptedPlayer methodInterface: aMethodInterface.	owner		ifNotNil:			[ActiveHand attachMorph: newTiles.			newTiles align: newTiles topLeft with: evt hand position + (7@14)]		ifNil:			[^ newTiles]! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'sw 3/28/2001 14:17'!newMakeGetter: arg event: evt from: aMorph	"Button in viewer performs this to makea universal-tiles header tile and attach to hand."	^ self makeUniversalTilesGetter: arg event: evt from: aMorph! !!CategoryViewer methodsFor: 'get/set slots' stamp: 'sw 3/28/2001 13:04'!newMakeGetter: arg1 from: arg2 forMethodInterface: arg3	"Button in viewer performs this to make a new style tile and attach to hand. (Reorder the arguments for existing event handlers)"	(arg3 isMorph and: [arg3 eventHandler notNil]) ifTrue:		[arg3 eventHandler fixReversedValueMessages].	 ^ self makeUniversalTilesGetter: arg1 event: arg2 from: arg3! !!EToyVocabulary methodsFor: 'initialization' stamp: 'sw 3/28/2001 13:59'!addGetterAndSetterInterfacesFromOldSlotSpec: aCommandSpec	"Create, given an old etoy-style command spec, appropriate MethodInterfaces, and store those interfaces in my method-interface dictionary"	| aMethodInterface |	aMethodInterface _ MethodInterface new initializeFromEToySlotSpec: aCommandSpec.	self atKey: aCommandSpec seventh putMethodInterface: aMethodInterface.	(aCommandSpec size >= 9 and: [(#(unused missing) includes: aCommandSpec ninth) not])		ifTrue:			[aMethodInterface _ MethodInterface new initializeSetterFromEToySlotSpec: aCommandSpec.			self atKey: aMethodInterface selector putMethodInterface: aMethodInterface]"	1	#slot  -- indicates that is a slot specification rather than a method specification	2	slot name	3	help message	4	type	5	#readOnly or #readWrite (if #readWrite, items 8 and 9 should be present & meaningful)	6	<future use -- target for getter -- currently always set to #player>	7	getter selector	8	<future use -- target for setter -- currently always set to #player>	9	setter selector"! !!EventHandler methodsFor: 'fixups' stamp: 'sw 3/28/2001 14:22'!fixReversedValueMessages	"ar 3/18/2001: Due to the change in the ordering of the value parameter old event handlers may have messages that need to be fixed up. Do this here."	self replaceSendsIn: #( renameCharAction:sourceMorph:requestor: makeGetter:from:forPart: makeSetter:from:forPart: newMakeGetter:from:forPart: newMakeSetter:from:forPart: clickOnLine:evt:envelope: limitHandleMoveEvent:from:index: mouseUpEvent:linkMorph:formData: mouseUpEvent:linkMorph:browserAndUrl: mouseDownEvent:noteMorph:pitch: mouseMoveEvent:noteMorph:pitch: mouseUpEvent:noteMorph:pitch: dragVertex:fromHandle:vertIndex: dropVertex:fromHandle:vertIndex: newVertex:fromHandle:afterVert: prefMenu:rcvr:pref: event:arrow:upDown:newMakeGetter:from:forMethodInterface:)			with: #( renameCharAction:event:sourceMorph: makeGetter:event:from: makeSetter:event:from: newMakeGetter:event:from: newMakeSetter:event:from: clickOn:evt:from: limitHandleMove:event:from: mouseUpFormData:event:linkMorph: mouseUpBrowserAndUrl:event:linkMorph: mouseDownPitch:event:noteMorph: mouseMovePitch:event:noteMorph: mouseUpPitch:event:noteMorph: dragVertex:event:fromHandle: dropVertex:event:fromHandle: newVertex:event:fromHandle: prefMenu:event:rcvr: upDown:event:arrow: makeUniversalTilesGetter:event:from:)."sw 3/28/2001 extended Andreas's original lists by one item"! !!MethodInterface methodsFor: 'initialization' stamp: 'sw 3/28/2001 13:51'!initializeSetterFromEToySlotSpec: tuple	"tuple holds an old etoy slot-item spec, of the form found in #additionsToViewerCategories methods.   Initialize the receiver to represent the getter of this item"	selector _ tuple ninth.	elementWording _ 'set ', tuple second.	self documentation: 'setter for', tuple third.	receiverType _ #player.	argumentVariables _ Array with: (Variable new variableType: tuple fourth)	! !!MethodWithInterface methodsFor: 'rename' stamp: 'sw 3/28/2001 16:27'!renameScript: newSelector fromPlayer: aPlayer	"The receiver's selector has changed to the new selector.  get various things right, including the physical appearance of any Scriptor open on this method"	self allScriptEditors do:		[:aScriptEditor | aScriptEditor renameScriptTo: newSelector].	selector _ newSelector asSymbol.	self bringUpToDate.	self playerClass atSelector: selector putScript: self.! !!MethodWithInterface methodsFor: 'updating' stamp: 'sw 3/28/2001 16:26'!bringUpToDate	"Bring all scriptors related to this method up to date.  Note that this will not change the senders of this method if the selector changed -- that's something still ahead."	(ScriptEditorMorph allInstances select:		[:m | (m playerScripted isMemberOf: self playerClass) and: [m scriptName == selector]])			do:				[:m | m bringUpToDate]! !!MethodWithInterface methodsFor: 'access' stamp: 'sw 3/28/2001 16:25'!playerClass	"Answer the playerClass associated with the receiver.  Note: fixes up cases where the playerClass slot was a Playerxxx object because of an earlier bug"	^ (playerClass isKindOf: Class)		ifTrue:			[playerClass]		ifFalse:			[playerClass _ playerClass class]! !!MethodWithInterface methodsFor: 'script editor' stamp: 'sw 3/28/2001 16:26'!allScriptEditors	"Answer all the script editors that exist for the class and selector of this interface"	^ ScriptEditorMorph allInstances select: 		[:aScriptEditor | aScriptEditor playerScripted class == self playerClass and:			[aScriptEditor scriptName == selector]]! !!MethodWithInterface methodsFor: 'script editor' stamp: 'sw 3/28/2001 16:26'!instantiatedScriptEditorForPlayer: aPlayer	"Return a new script editor for the player and selector"	| aScriptEditor |	aScriptEditor _ (self playerClass includesSelector: selector) 			ifTrue: [ScriptEditorMorph new 				fromExistingMethod: selector 				forPlayer: aPlayer]			ifFalse: [ScriptEditorMorph new				setMorph: aPlayer costume				scriptName: selector].		defaultStatus == #ticking ifTrue:			[aPlayer costume arrangeToStartStepping].		^ aScriptEditor! !!Player class methodsFor: 'scripts' stamp: 'sw 3/28/2001 16:18'!userScriptForPlayer: aPlayer selector: aSelector	"Answer the user script for the player (one copy for all instances of the uniclass) and selector"	|  newEntry existingEntry |	scripts ifNil: [scripts _ IdentityDictionary new].	existingEntry _ scripts at: aSelector ifAbsent: [nil].	"Sorry for all the distasteful isKindOf: and isMemberOf: stuff here, folks; it arises out of concern for preexisting content saved on disk from earlier stages of this architecture.  Someday much of it could be cut loose"	Preferences universalTiles		ifTrue:			[(existingEntry isMemberOf: MethodWithInterface) ifTrue: [^ existingEntry].			newEntry _ (existingEntry isKindOf: UniclassScript)				ifTrue:					[existingEntry as: MethodWithInterface] "let go of extra stuff if it was UniclassScript"				ifFalse:					[MethodWithInterface new playerClass: aPlayer class selector: aSelector].			scripts at: aSelector put: newEntry.			^ newEntry]		ifFalse:			[(existingEntry isKindOf: UniclassScript)				ifTrue:					[^ existingEntry]				ifFalse:					[newEntry _ UniclassScript new playerClass: self selector: aSelector.					scripts at: aSelector put: newEntry.					existingEntry ifNotNil: "means it is a grandfathered UserScript that needs conversion"						[newEntry convertFromUserScript: existingEntry].					^ newEntry]]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 3/28/2001 16:09'!methodNodeMorph	"Answer the morph that constitutes the receiver's method node"	submorphs size < 2  ifTrue: [^ nil].	^ self findDeepSubmorphThat:		[:aMorph | (aMorph isKindOf: SyntaxMorph) and:				[aMorph parseNode isKindOf: MethodNode]]			ifAbsent: [nil]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 3/28/2001 16:06'!renameScriptTo: newSelector	"Rename the receiver's script so that it has given new selector"	| selectorReadout aMethodNodeMorph |	scriptName _ newSelector.	selectorReadout _ self firstSubmorph findDeepSubmorphThat:		[:m | m externalName = 'script name'] ifAbsent: [nil].	selectorReadout ifNotNil:		[selectorReadout contents: self scriptTitle; font: Preferences standardButtonFont].	Preferences universalTiles		ifFalse:			[self install]		ifTrue:			[(aMethodNodeMorph _ self methodNodeMorph) ifNotNil: [aMethodNodeMorph accept]]! !!UniclassScript methodsFor: 'script editor' stamp: 'sw 3/28/2001 16:27'!instantiatedScriptEditorForPlayer: aPlayer	"Return the current script editor, creating it if necessary"	currentScriptEditor ifNil:		[currentScriptEditor _ (self playerClass includesSelector: selector) 			ifTrue: [ScriptEditorMorph new 				fromExistingMethod: selector 				forPlayer: aPlayer]			ifFalse: [ScriptEditorMorph new				setMorph: aPlayer costume				scriptName: selector].		defaultStatus == #ticking ifTrue:			[aPlayer costume arrangeToStartStepping]].		^ currentScriptEditor! !"Postscript:"Vocabulary initialize.!