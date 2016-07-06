'From Squeak2.9alpha of 16 June 2000 [latest update: #3271] on 18 January 2001 at 1:42:19 pm'!!Player methodsFor: 'scripts-standard' stamp: 'tk 1/16/2001 18:26'!append: aPlayer	"Add aPlayer to the list of objects logically 'within' me.  This is visually represented by its morph becoming my costume's last submorph.   Also allow text to be appended."	| aCostume |	(aPlayer == nil or: [aPlayer == self]) ifTrue: [^ self].	(aPlayer class == Text or: [aPlayer class == String]) ifTrue: [		self costume class == TextFieldMorph 			ifTrue: [^ self costume append: aPlayer]			ifFalse: [^ self]].	(aCostume _ self costume topRendererOrSelf) addMorphNearBack:  aPlayer costume.	aPlayer costume goHome.  "assure it's in view"	(aCostume isKindOf: PasteUpMorph) ifTrue:		[self setCursor: (aCostume submorphs indexOf: aPlayer costume)]! !!CardPlayer methodsFor: 'card data' stamp: 'tk 1/16/2001 16:12'!installPrivateMorphsInto: aBackground	"The receiver is being installed as the current card in a given pasteup morph being used as a background.  Install the receiver's private morphs into that playfield"	| prior originToUse |	self flag: #deferred.  "not robust if the background is showing a list view"	privateMorphs ifNotNil: [privateMorphs do:		[:aMorph |			originToUse _ aBackground topLeft.			prior _ aMorph valueOfProperty: #priorMorph ifAbsent: [nil].			aMorph position: (aMorph position + originToUse).			(prior notNil and: [aBackground submorphs includes: prior])				ifTrue:					[aBackground addMorph: aMorph after: prior]				ifFalse:					[aBackground addMorphFront: aMorph].		aMorph removeProperty: #priorMorph]]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 1/16/2001 17:51'!renameScript	"Invoked at user menu request"	| reply aPosition oldSelector dflt oldStatus oldOwner newSelector aUserScript methodNode isNewTiles |	oldSelector _ self scriptName.	aUserScript _ playerScripted class userScriptForPlayer: self selector: oldSelector.	oldStatus _ self scriptInstantiation status.	dflt _ self isAnonymous ifTrue: [''] ifFalse: [self scriptTitle].	reply _   FillInTheBlank request: 'Script Name' initialAnswer: dflt. 	reply size == 0 ifTrue: [^ self].	newSelector _ ScriptingSystem acceptableScriptNameFrom: reply		forScriptCurrentlyNamed: oldSelector		asScriptNameIn: self playerScripted		world: self world.	isNewTiles _ aUserScript isTextuallyCoded and: [					(submorphs atPin: 2) class = TwoWayScrollPane].	isNewTiles ifTrue: ["allow colons"		(reply copyWithout: $:) = newSelector 			ifTrue: [newSelector _ reply asSymbol]			ifFalse: [self inform: 'name will be modified']].		self titleMorph borderColor: Color black.	scriptName _ newSelector.	"aUserScript isTextuallyCoded ifFalse: ["		playerScripted class atSelector: newSelector putScriptEditor: self  "]"  .	self scriptInstantiation status: oldStatus.	playerScripted class removeScriptNamed: oldSelector.	playerScripted actorState instantiatedUserScriptsDictionary removeKey: oldSelector.	submorphs first delete.  "the button row"	self addMorphFront: self buttonRowForEditor.  "up to date"	isNewTiles ifTrue: [			methodNode _ submorphs second scroller "transform" submorphs first.			methodNode rename: newSelector; accept.			"self refresh"].	self install.	aPosition _ self position.	oldOwner _ self topRendererOrSelf owner.	self delete.	playerScripted costume viewAfreshIn: oldOwner		showingScript: scriptName at: aPosition.! !!SyntaxMorph methodsFor: 'event handling' stamp: 'tk 1/17/2001 15:15'!mouseMove: evt	| dup selection |	owner isSyntaxMorph ifFalse: [^ self].	self currentSelectionDo:		[:innerMorph :mouseDownLoc :outerMorph |		mouseDownLoc ifNotNil: [			(evt cursorPoint dist: mouseDownLoc) > 4 ifTrue:				["If drag 5 pixels, then tear off a copy of outer selection."				selection _ outerMorph ifNil: [self].				evt hand attachMorph: (dup _ selection duplicate).				dup align: dup topLeft with: evt hand position + self cursorBaseOffset.				self setSelection: nil.				(self firstOwnerSuchThat: [:m | m isSyntaxMorph and: [m isBlockNode]])					ifNotNilDo: [:m | "Activate enclosing block."								m startStepping]]]].! !!SyntaxMorph methodsFor: 'selection' stamp: 'tk 1/17/2001 15:07'!isSelectable	| ss |	"Spacer morphs enclose other morphs with the same parseNode"	self submorphs size > 1 ifTrue: [		ss _ self submorphs second.		ss isSyntaxMorph ifTrue: [			ss parseNode == parseNode ifTrue: [				^ self submorphs first class ~~ Morph]]].		"	(self nodeClassIs: SelectorNode) ifTrue: [^ false].	(self nodeClassIs: KeyWordNode) ifTrue: [^ false]."	self isMethodNode ifTrue: [^ false].	parseNode ifNil: [^ false].	^ true! !