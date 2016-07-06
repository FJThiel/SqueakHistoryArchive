'From Squeak3.2alpha of 3 October 2001 [latest update: #4519] on 15 November 2001 at 8:48:38 pm'!"Change Set:		userEditsAllowed-swDate:			15 November 2001Author:			Scott WallaceRestores the ability of UpdatingStringMorphs to reject manual user editing, while making it still possible for instances that lack putSelectors to allow such editing by bearing the property #okToTextEdit.Also fixes a long-standing bug in simple watchers -- those associated with read-only variables mistakenly would generate an error if a new value were submitted for them."!!NumericReadoutTile methodsFor: 'initialization' stamp: 'sw 11/15/2001 20:22'!initializeToStandAlone	"Enclose my prototype in a SyntaxMorph.  For the ObjectTool"	| aWatcher aTile aLine aColor ms slotMsg |	super initializeToStandAlone.	aColor _ Color r: 0.387 g: 0.581 b: 1.0.	aTile _ self typeColor: aColor.	aWatcher _ UpdatingStringMorph new.	aWatcher growable: true;		getSelector: nil;		putSelector: nil;		setToAllowTextEdit.	aWatcher target: nil.	aTile addMorphBack: aWatcher.	aTile addArrows.	aTile setLiteralTo: 5 width: 30.	ms _ MessageSend receiver: nil selector: #aNumber arguments: #().	slotMsg _ ms asTilesIn: Player globalNames: false.		"For CardPlayers, use 'aPlayer'.  For others, name it, and use its name."	ms _ MessageSend receiver: 3 selector: #= asSymbol arguments: #(5).	aLine _ ms asTilesIn: Player globalNames: false.	aLine firstSubmorph delete.		"A little over-complicated?  Yes?"	aLine addMorphFront: (slotMsg submorphs second) firstSubmorph.	aLine addMorphFront: (Morph new transparentSpacerOfSize: 3@3).	aLine lastSubmorph delete.	aLine lastSubmorph delete.	aLine color: aColor.	aLine addMorphBack: (Morph new transparentSpacerOfSize: 3@3).	aLine addMorphBack: aTile.	aLine cellPositioning: #leftCenter.	aWatcher step; fitContents.	^ aLine markAsPartsDonor.! !!NumericReadoutTile class methodsFor: 'instance creation' stamp: 'sw 11/15/2001 20:21'!authoringPrototype	"Enclose my prototype in a SyntaxMorph."	| aWatcher aTile aLine aColor ms slotMsg |	aColor _ Color r: 0.387 g: 0.581 b: 1.0.	aTile _ self new typeColor: aColor.	aWatcher _ UpdatingStringMorph new.	aWatcher growable: true;		setToAllowTextEdit;		getSelector: nil;		putSelector: nil.	aWatcher target: nil.	aTile addMorphBack: aWatcher.	aTile addArrows.	aTile setLiteralTo: 5 width: 30.	"This is the long way around to do this..."	ms _ MessageSend receiver: nil selector: #aNumber arguments: #().	slotMsg _ ms asTilesIn: Player globalNames: false.		"For CardPlayers, use 'aPlayer'.  For others, name it, and use its name."	ms _ MessageSend receiver: 3 selector: #= asSymbol arguments: #(5).	aLine _ ms asTilesIn: Player globalNames: false.	aLine firstSubmorph delete.		aLine addMorphFront: (slotMsg submorphs second) firstSubmorph.	aLine addMorphFront: (Morph new transparentSpacerOfSize: 3@3).	aLine lastSubmorph delete.	aLine lastSubmorph delete.	aLine color: aColor.	aLine addMorphBack: (Morph new transparentSpacerOfSize: 3@3).	aLine addMorphBack: aTile.	aLine cellPositioning: #leftCenter.	aWatcher step; fitContents.	^ aLine markAsPartsDonor.! !!NumericReadoutTile class methodsFor: 'instance creation' stamp: 'sw 11/15/2001 20:21'!borderedPrototype	"Just number and up/down arrows"	| aWatcher aTile |	aTile _ self new typeColor: (Color r: 0.387 g: 0.581 b: 1.0).	aWatcher _ UpdatingStringMorph new.	aWatcher growable: true.	aTile addMorphBack: aWatcher.	aTile addArrows.	aTile setLiteralTo: 5 width: 30.	aWatcher step; fitContents; setToAllowTextEdit.	^ aTile extent: 30@24; markAsPartsDonor! !!NumericReadoutTile class methodsFor: 'instance creation' stamp: 'sw 11/15/2001 20:20'!simplePrototype	"Bare number readout.  Will keep up to data with a number once it has target, getterSelector, setterSelector."	^ (UpdatingStringMorph new) contents: '5'; growable: true; setToAllowTextEdit; step; fitContents; markAsPartsDonor! !!Player methodsFor: 'slots-user' stamp: 'sw 11/15/2001 09:11'!tearOffWatcherFor: aSlotGetter	"Tear off a watcher for the slot whose getter is provided"	| aWatcher precision anInterface info isNumeric |	info _ self slotInfoForGetter: aSlotGetter.	info		ifNotNil:			[isNumeric _ info type == #Number]		ifNil:			[anInterface _ Vocabulary eToyVocabulary methodInterfaceAt: aSlotGetter ifAbsent: [nil].			isNumeric _ anInterface notNil and: [anInterface resultType == #Number]].	aWatcher _ UpdatingStringMorph new.		isNumeric		ifFalse:			[aWatcher useStringFormat]		ifTrue:			[precision _ self defaultFloatPrecisionFor: aSlotGetter.			precision ~= 1 ifTrue: [aWatcher floatPrecision: precision]].	aWatcher		growable: true;		getSelector: aSlotGetter;		putSelector: (info notNil			ifTrue:				[ScriptingSystem setterSelectorForGetter: aSlotGetter]			ifFalse:				[anInterface companionSetterSelector]);		setNameTo: (info notNil			ifTrue:				[Utilities inherentSelectorForGetter: aSlotGetter]			ifFalse:				[anInterface elementWording]); 		target: self;		step;		fitContents.	self currentHand attachMorph: aWatcher! !!UpdatingRectangleMorph methodsFor: 'accessing' stamp: 'sw 11/15/2001 16:22'!userEditsAllowed	"Answer whether it is suitable for a user to change the value represented by this readout"	^ putSelector notNil! !!UpdatingStringMorph methodsFor: 'editing' stamp: 'sw 11/15/2001 18:17'!handlerForMouseDown: evt	"Answer an object to field the mouseDown event provided, or nil if none"	| aHandler |	aHandler _ super handlerForMouseDown: evt.	aHandler == self ifTrue:	[^ self]. "I would get it anyways"	"Note: This is a hack to allow value editing in viewers"	((owner wantsKeyboardFocusFor: self) and:		[self userEditsAllowed]) ifTrue: [^ self].	^ aHandler! !!UpdatingStringMorph methodsFor: 'editing' stamp: 'sw 11/15/2001 10:15'!mouseDown: evt	"The mouse went down over the receiver.  If appropriate, launch a mini-editor so that the user can commence text-editing here"	(owner wantsKeyboardFocusFor: self) ifTrue:		[self userEditsAllowed ifTrue:			[(owner respondsTo: #parseNode)					ifTrue: 	"leave space for editing"						[minimumWidth _ (49 max: minimumWidth)].			self launchMiniEditor: evt]]! !!UpdatingStringMorph methodsFor: 'editing' stamp: 'sw 11/15/2001 20:20'!setToAllowTextEdit	"Set up the receiver so that it will be receptive to text editing, even if there is no putSelector provided"	self setProperty: #okToTextEdit toValue: true! !!UpdatingStringMorph methodsFor: 'editing' stamp: 'sw 11/15/2001 09:43'!userEditsAllowed	"Answer whether user-edits are allowed to this field"	^ putSelector notNil or: [self hasProperty: #okToTextEdit]! !