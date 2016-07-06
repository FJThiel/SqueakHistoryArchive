'From Squeak 2.1 of June 30, 1998 on 13 August 1998 at 1:33:01 pm'!"Change Set:		OwnerBug-tkDate:			13 August 1998Author:			Ted KaehlerWe now use weak referneces for copying the owner field.  It works to the disk, but two tricky bugs in in-memory copying made it go ahead and copy the owner no matter what.  This fixes that.Then I discovered that BookMorph's prototype saving was using fullCopy instead of veryDeepCopy.  This was causing Players to be created with two identical costumes.  They held costume A, but costume B was installed in the world.  Bad for script execution.  This file changes some of them to veryDeepCopy.We now have a rule that tile scriptors cannot contain any Players.  Thus we can use fullCopy for scripts.  Yes, an object's own tile may pioint to a Player, but that is OK, that Player should not be copied when a script is copied."!!Morph methodsFor: 'apr98 additions' stamp: 'tk 8/13/1998 12:57'!usableDuplicateIn: aWorld	|  aName usedNames newPlayer newMorph |	self error: 'I thought this method was not used anymore'.	newMorph _ self fullCopy.	self player ifNotNil:		[newPlayer _ self player duplicatedPlayerForCostume: newMorph.			"nb newPlayer has had his costume set to newMorph in the above-called method"		self isFlexMorph ifTrue: [newMorph renderedMorph player: newPlayer]. "???"		newMorph actorState: (self player actorState shallowCopy initializeFor: newPlayer)].	(aName _ self knownName) == nil ifTrue:		[self player ~~ nil ifTrue: [aName _ newMorph innocuousName]].			"Force a difference here"	aName ~~ nil ifTrue:		[usedNames _ aWorld allKnownNames copyWith: aName.		newMorph setNameTo: (			Utilities keyLike: aName satisfying: [:f | (usedNames includes: f) not])].	newMorph justDuplicatedFrom: self.	newMorph isPartsDonor: false.	newMorph privateOwner: nil.	(newPlayer ~~ nil and: [newMorph renderedMorph eventHandler ~~ nil]) ifTrue:		[newPlayer assureEventHandlerRepresentsStatus].	newPlayer ifNotNil: [self presenter flushPlayerListCache].	^ newMorph! !!BookMorph methodsFor: 'menu' stamp: 'tk 8/13/1998 12:09'!setNewPagePrototype	"Record the current page as the prototype to be copied when inserting new pages."	currentPage ifNotNil:		[newPagePrototype _ currentPage veryDeepCopy].! !!BookMorph methodsFor: 'other' stamp: 'tk 8/13/1998 12:49'!makeMinimalControlsWithColor: aColor title: aString	| aButton aColumn aRow but |	aButton _ SimpleButtonMorph new target: self; borderColor: Color black; color: aColor; borderWidth: 0.	aColumn _ AlignmentMorph newColumn.	aColumn color: aButton color; borderWidth: 0; inset: 0.	aColumn hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	aRow _ AlignmentMorph newRow.	aRow color: aButton color; borderWidth: 0; inset: 0.	aRow hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	aRow addTransparentSpacerOfSize: 40@0.	aRow addMorphBack: (but _ aButton fullCopy label: ' < ' ; actionSelector: #previousPage).		"fullCopy is OK, since we just made it and it can't own any Players"	but setBalloonText: 'Go to previous page'.	aRow addTransparentSpacerOfSize: 82@0.	aRow addMorphBack: (StringMorph contents: aString) lock.	aRow addTransparentSpacerOfSize: 82@0.	aRow addMorphBack: (but _ aButton fullCopy label: ' > ' ; actionSelector: #nextPage).	but setBalloonText: 'Go to next page'.	aRow addTransparentSpacerOfSize: 40@0.	aColumn addMorphBack: aRow.	aColumn setNameTo: 'Page Controls'.		^ aColumn! !!ButtonMorph methodsFor: 'copying' stamp: 'tk 8/13/1998 12:53'!prepareToBeSaved	"SmartRefStream will not write any morph that is owned by someone outside the root being written.  (See DataStream.typeIDFor:)  Open Scripts are like that.  Make a private copy of the scriptEditor."	super prepareToBeSaved.	lastAcceptedScript ifNotNil: [		lastAcceptedScript owner ifNotNil: ["open on the screen"			lastAcceptedScript _ lastAcceptedScript fullCopy setMorph: self.			self flag: #noteToTed.  "What if some tile has a Player?  				Need an invariant that says no script part can have a Player.  Enforce it."			"lastAcceptedScript privateOwner: nil" "fullCopy does it"]].	"lastScriptEditor will not be written out"! !!EToyPalette methodsFor: 'initialization' stamp: 'tk 8/13/1998 12:54'!addPalettes	paintPalette _ PaintBoxMorph newSticky.	controlsPalette _ self controlsBook.	suppliesPalette _ self suppliesBook.	"Later share the userStuff across EToys?  Not now."	userStuffPalette ifNil: [		userStuffPalette _ EToyHolder userStuffBook veryDeepCopy beSticky].! !!HandMorph methodsFor: 'meta menu' stamp: 'tk 8/13/1998 12:56'!writeInitMethodForModel	| model |	model _ self world model.	model class chooseNewName.	model veryDeepCopy compileInitMethods.! !!MorphicModel class methodsFor: 'instance creation' stamp: 'tk 8/13/1998 12:58'!new	"Return a copy of the prototype, if there is one.	Otherwise create a new instance normally."	self hasPrototype ifTrue: [^ prototype veryDeepCopy].	^ super new! !!MorphicModel class methodsFor: 'prototype access' stamp: 'tk 8/13/1998 12:58'!prototype: aMorph	"Store a copy of the given morph as a prototype to be copied to make new instances."	aMorph ifNil: [prototype _ nil. ^ self].	prototype _ aMorph veryDeepCopy.	(prototype isKindOf: MorphicModel) ifTrue: 		[prototype model: nil slotName: nil].! !!PaintBoxMorph class methodsFor: 'all' stamp: 'tk 8/13/1998 13:00'!new	| pb button dualUse formCanvas rect |	pb _ Prototype fullCopy.		"Assume that the PaintBox does not contain any scripted Players!!"	pb stampHolder normalize.	"Get the stamps to show"	"Get my own copies of the brushes so I can modify them"	#(brush1: brush2: brush3: brush4: brush5: brush6:) do: [:sel |		button _ pb findButton: sel.		button offImage: button offImage deepCopy.		dualUse _ button onImage == button pressedImage.	"sometimes shared"		button onImage: button onImage deepCopy.		dualUse			ifTrue: [button pressedImage: button onImage]			ifFalse: [button pressedImage: button pressedImage deepCopy].		"force color maps for later mapping"		button offImage.		button onImage.		button pressedImage.		formCanvas _ FormCanvas on: button onImage.		formCanvas _ formCanvas			copyOrigin: 0@0			clipRect: (rect _ 0@0 extent: button onImage extent).		(#(brush1: brush3:) includes: sel) ifTrue: [			rect _ rect origin corner: rect corner - (2@2)].		(#brush2: == sel) ifTrue: [			rect _ rect origin corner: rect corner - (2@4)].		formCanvas frameAndFillRectangle: rect fillColor: Color transparent			borderWidth: 2 borderColor: (Color r: 0.599 g: 0.8 b: 1.0).		].	pb showColor.	^ pb! !!PartsBinMorph methodsFor: 'extraction support' stamp: 'tk 8/13/1998 13:01'!rootForGrabOf: aMorph	"If open to drag-n-drop, allow submorph to be extracted. Otherwise, copy the submorph."	| root |	root _ aMorph.	[root = self] whileFalse:		[root owner = self ifTrue:			[^ openToDragNDrop				ifTrue: [root]				ifFalse: [root veryDeepCopy isPartsDonor: false]].		root _ root owner].	^ super rootForGrabOf: aMorph! !!PartsViewer methodsFor: 'private' stamp: 'tk 8/13/1998 12:39'!nextCostume	"The receiver's player is currently being viewed via a particular costume.  Now switch to a plausible different costume."	| ind acn c |	ind _ (acn _ scriptedPlayer availableCostumeNamesForArrows) indexOf: scriptedPlayer costume renderedMorph class name.	ind _ ind + 1.	ind > acn size ifTrue: [ind _ 1].	scriptedPlayer costume:		(c _ scriptedPlayer costumeNamed: (acn at: ind)) veryDeepCopy.		"Assume that the costume in the library has player = nil"	c isInWorld ifTrue: [self presenter updatePartsViewer: self]! !!PartsViewer methodsFor: 'private' stamp: 'tk 8/13/1998 12:39'!previousCostume	"The receiver's player is currently being viewed via a particular costume.  Now switch to a plausible different costume."	| ind acn c |	ind _ (acn _ scriptedPlayer availableCostumeNamesForArrows) indexOf: scriptedPlayer costume renderedMorph class name.	ind _ ind - 1.	ind < 1 ifTrue: [ind _ acn size].	scriptedPlayer costume:		(c _ scriptedPlayer costumeNamed: (acn at: ind)) veryDeepCopy.		"Assume that the costume in the library has player = nil"	c isInWorld ifTrue: [self presenter updatePartsViewer: self]! !!Player methodsFor: 'costume' stamp: 'tk 8/13/1998 12:48'!costume: aMorph	"Make aMorph be the receiver's current costume"	| itsBounds |	costume == aMorph ifTrue: [^ self].	((costume isKindOf: SketchMorph) and: [(aMorph isKindOf: SketchMorph)])		ifTrue:			[^ costume wearCostume: aMorph].	aMorph player: nil.		"So another player won't be created by the copying.  OK to smash the old player."	self costumeDictionary		at: aMorph formalCostumeName		put: (aMorph veryDeepCopy player: nil).	costume ifNotNil:		[itsBounds _ costume bounds.		costume pasteUpMorph replaceSubmorph: costume topRendererOrSelf by: aMorph.		aMorph position: itsBounds origin.		aMorph actorState: costume actorState.		aMorph setNameTo: costume externalName].	aMorph player: self.	costume _ aMorph.	aMorph arrangeToStartStepping! !!Player methodsFor: 'costume' stamp: 'tk 8/13/1998 12:35'!wearCostumeOf: anotherPlayer	"Put on a costume similar to the one currently worn by anotherPlayer"	anotherPlayer costume player: nil.	self costume: anotherPlayer costume veryDeepCopy.	anotherPlayer costume player: anotherPlayer.! !!Player methodsFor: 'costume' stamp: 'tk 8/13/1998 12:36'!wearCostumeOfClass: aClass	"Assume that the costume in the library has player = nil"	self costume:		(self costumeNamed: aClass formalCostumeName) veryDeepCopy! !!ScrollPane methodsFor: 'initialization' stamp: 'tk 8/13/1998 13:05'!fullCopy	| copy |	self mouseEnter: nil.		 "Make sure scrollBar is in morphic structure"	copy _ super fullCopy.		"So that references are updated properly"		"Will fail of any Players with scripts are in the ScrollPane"	self mouseLeave: nil.	^ copy mouseLeave: nil! !!SmartRefStream methodsFor: 'read write' stamp: 'tk 8/13/1998 12:20'!veryDeepCopy: anObject	"Do a complete tree copy using a dictionary.  An object in the tree twice is only copied once.  Both pointers point to one new copy.  Uses ReferenceStream.  To see where the copying stops, see DataStream.typeIDFor: and implementors of objectToStoreOnDataStream and storeDataOn:"	| dummy refs class new uniClasses uc old stillWeakRefs |	dummy _ DeepCopyStream on: (DummyStream on: nil).		"Write to a fake Stream, not a file"	"Collect all objects"	dummy rootObject: anObject.	"inform him about the root"  		"Note that dummy is a ReferenceStream.  instVarInfo: will not be called."	dummy nextPut: anObject.	"Do the traverse of the tree"	uniClasses _ Dictionary new.	"UniClass -> new UniClass"	renamed _ Dictionary new.	stillWeakRefs _ OrderedCollection new: 30.	refs _ dummy references.	"all the objects"	"For each object, make a simple copy, then replace all fields with new copy from dict"	refs keysDo: [:each | 		class _ each class.		class == Metaclass ifTrue: ["object is a metaclass" self error: 'can''t copy a class'].		class isMeta 			ifTrue: ["a class" self error: 'can''t copy a class']			ifFalse: ["an instance" class isSystemDefined 				ifFalse: [uc _ uniClasses at: class ifAbsent: [nil].					uc ifNil: [uniClasses at: class put: (uc _ each copyUniClass)]]				ifTrue: [(refs at: each) class == OrderedCollection ifTrue: [						"Is a weak reference that was not confirmed"						stillWeakRefs add: each]]]].	uniClasses do: [:uniCls | 		dummy nextPut: uniCls scripts].	"more objects to veryDeepCopy"	refs associationsDo: [:assoc | 		(uniClasses includesKey: assoc key class)			ifFalse: [(#(Form Color StrikeFont) includes: assoc key class name)				ifFalse: [assoc key class == DiskProxy					ifFalse: [assoc value: assoc key clone]					ifTrue: [assoc value: nil "assoc key comeFullyUpOnReload"]]						"a DiskProxy will never be looked up -- its not in any field.						Old value of field is used."				ifTrue: [assoc value: assoc key]]		"Don't copy Forms, Colors"			ifTrue: [old _ assoc key.				assoc value: (new _ (uc _ uniClasses at: assoc key class) new).				1 to: uc instSize do: [:ii | new instVarAt: ii put: (old instVarAt: ii)]]].		"Watch out for classes that do extra things in copy but not in clone"	stillWeakRefs do: [:each | refs at: each put: nil]. 	"nil out weak refs"	self veryDeepRectify: refs.	"rehashes Sets and Dictionaries"	self veryDeepClassVars: refs.		"ScannedObject _ refs.	debug"	^ refs at: anObject! !!SmartRefStream methodsFor: 'read write' stamp: 'tk 8/13/1998 09:19'!veryDeepRectify: refs	"refs is a dictionary of (old object -> shallow copy of it).  For each field, map the value to a new object.  This is for copying, so if an object is not in refs, use its old value.	Rehash the Sets.  If any other class depends on the hashes of its inst vars, let is recompute also."	| hashers new class index sub subAss |	hashers _ OrderedCollection new.	refs associationsDo: [:assoc | 		assoc key == assoc value ifFalse: ["is a new object"			new _ assoc value.			class _ new class.			class isVariable				ifTrue: 					[index _ new basicSize.					[index > 0] whileTrue: 						[sub _ new basicAt: index.						(subAss _ refs associationAt: sub ifAbsent: [nil]) ifNotNil: [								"If not in refs, then the right value is already in the field"								new basicAt: index put: subAss value].						index _ index - 1]].			index _ class instSize.			[index > 0] whileTrue: 				[sub _ new instVarAt: index.				(subAss _ refs associationAt: sub ifAbsent: [nil]) ifNotNil: [						"If not in refs, then the right value is already in the field"						new instVarAt: index put: subAss value].				index _ index - 1].			(new respondsTo: #rehash) ifTrue: [hashers add: new].			]].	"Force new Sets and Dictionaries to rehash"	hashers do: [:each | each rehash].! !!UserScript methodsFor: 'initialization' stamp: 'tk 8/13/1998 13:06'!initializeForPlayer: aPlayer afterShallowCopyFrom: aDonorUserScript	player _ aPlayer.	formerScriptEditors _ nil.	aDonorUserScript isTextuallyCoded		ifFalse:			[currentScriptEditor _ currentScriptEditor fullCopy.				"We have a rule that ScriptEditors can't have Players in them"			currentScriptEditor playerScripted: aPlayer.			currentScriptEditor donorActor: aDonorUserScript player ownActor: aPlayer]		! !!WebBookMorph methodsFor: 'commands' stamp: 'tk 8/13/1998 13:11'!saveBookToFile	"Save this book in a file."	| fileName fileStream |	self ensurePagesAreExternal ifFalse: [^ self].	fileName _ FillInTheBlank request: 'File name for this Book?'.	fileName isEmpty ifTrue: [^ self].  "abort"	self allMorphsDo: [:m | m prepareToBeSaved].	fileStream _ FileStream newFileNamed: fileName,'.morph'.	fileStream fileOutClass: nil andObject: self.	"Puts UniClass definitions out anyway"! !!WebBookMorph methodsFor: 'commands' stamp: 'tk 8/13/1998 13:16'!setNewPagePrototype	"Record the current page as the prototype to be copied when inserting new pages."	currentContents ifNotNil:		[newPagePrototype _ currentContents veryDeepCopy].! !!WebBookMorph methodsFor: 'private' stamp: 'tk 8/13/1998 13:07'!insertPageColored: aColor	| sz bw bc newContents newURL |	newPagePrototype		ifNil: [			currentContents				ifNil: [					sz _ pageSize.					bw _ 0.					bc _ Color blue muchLighter]				ifNotNil: [					sz _ currentContents extent.					bw _ currentContents borderWidth.					bc _ currentContents borderColor].			newContents _ PasteUpMorph new extent: sz; color: aColor.			newContents borderWidth: bw; borderColor: bc]		ifNotNil: [			newContents _ newPagePrototype veryDeepCopy].	newContents resizeToFit: false.	newURL _ SqueakPage newURLAndPageFor: newContents.	urls isEmpty		ifTrue: [urls add: newURL]		ifFalse: [urls add: newURL after: currentURL].	self nextPage.! !!WebBookMorph methodsFor: 'private' stamp: 'tk 8/13/1998 13:07'!revealPage: aMorph ascending: ascending	| w r oldOrigin |	w _ self world.	w ifNotNil: [self primaryHand newKeyboardFocus: nil].	currentContents ifNotNil: [		(r _ currentContents screenRectangle) ifNotNil: [oldOrigin _ r origin].		currentContents delete].	originalContents _ aMorph.	currentContents _ aMorph veryDeepCopy.	self addMorphBack: currentContents.	w ifNotNil: [		w startSteppingSubmorphsOf: currentContents.		self showPageTurningFeedbackFromOrigin: oldOrigin ascending: ascending].	currentContentsChanged _ false.! !!WebBookMorph methodsFor: 'private' stamp: 'tk 8/13/1998 13:16'!savePageIfNecessary	"About to go to a new page; save the current page if necessary."	| oldPage morphToSave |	currentContentsChanged ifNil: [currentContentsChanged _ false].	currentContentsChanged ifFalse: [		"no need to save, but do release cached state of viewed page"		originalContents allMorphsDo: [:m | m releaseCachedState].		^ self].	oldPage _ SqueakPageCache atURL: currentURL ifAbsent: [^ self].	oldPage contentsMorph ~~ originalContents ifTrue: [		(self confirm: 'Someone else has changed this page; save this copy?')			ifFalse: [^ self]].	morphToSave _ currentContents fullCopy.		self flag: #noteToTed.	"Must use veryDeepCopy.  Saving must write Player code too."  	currentContentsChanged _ false.	originalContents _ morphToSave.	oldPage saveMorph: morphToSave author: Utilities authorInitials. ! !!WebBookMorph class methodsFor: 'all' stamp: 'tk 8/13/1998 13:06'!newFromOldBook: oldBookMorph	"Make a new instance of me whose pages are copies of the pages in the given book. Does not modify the original book."	| urlList |	urlList _ oldBookMorph pages collect:		[:oldPg | SqueakPage newURLAndPageFor: oldPg veryDeepCopy].	^ self new urls: urlList! !