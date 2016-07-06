'From Squeak2.9alpha of 5 August 2000 [latest update: #2580] on 8 September 2000 at 9:37:39 pm'!"Change Set:		misc-swDate:			8 September 2000Author:			Scott Wallace� Drop the long-standing feature (more often an annoying unfeature than not nowadays) that used the label of a submorph that is a StringMorph to help document a morph in printOn:.� When the user chooses cancel in a tool code pane that has an associated annotation pane, that annotaiton pane is now updated.� Subtle improvement to the wording on the message informing the user that a method may have been changed elsewhere.� Some changes to the standard Supplies flap� Add #seventh, #eighth, & #ninth to SequenceableCollection protocol (responding to an explicit need"!!Morph methodsFor: 'printing' stamp: 'sw 9/5/2000 06:49'!printOn: aStream	| aName |	super printOn: aStream.	(aName _ self knownName) ~~ nil ifTrue:		[aStream nextPutAll: '<', aName, '>'].	aStream nextPutAll: '('.	aStream print: self identityHash;			nextPutAll: ')'.! !!PluggableTextController methodsFor: 'as yet unclassified' stamp: 'sw 9/6/2000 04:58'!accept 	view hasUnacceptedEdits ifFalse: [^ view flash].	view hasEditingConflicts ifTrue:		[(self confirm: 'Caution!! This method may have beenchanged elsewhere since you startedediting it here.  Accept anyway?') ifFalse: [^ self flash]].	(view setText: paragraph text from: self) ifTrue:		[initialText _ paragraph text copy.		view ifNotNil: [view hasUnacceptedEdits: false]]    .! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'sw 9/6/2000 04:57'!accept 	"Inform the model of text to be accepted, and return true if OK."	| textToAccept ok |	self canDiscardEdits ifTrue: [^ self flash].	self hasEditingConflicts ifTrue:		[(self confirm: 'Caution!! This method may have beenchanged elsewhere since you startedediting it here.  Accept anyway?') ifFalse: [^ self flash]].	textToAccept _ textMorph asText.	ok _ (setTextSelector == nil) or:		[setTextSelector numArgs = 2			ifTrue: [model perform: setTextSelector with: textToAccept with: self]			ifFalse: [model perform: setTextSelector with: textToAccept]].	ok ifTrue:		[self setText: self getText.		self hasUnacceptedEdits: false.		(model dependents detect: [:dep | (dep isKindOf: PluggableTextMorph) and: [dep getTextSelector == #annotation]] ifNone: [nil]) doIfNotNil:			[:aPane | model changed: #annotation]]! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'sw 9/6/2000 04:56'!cancel	self setText: self getText.	self setSelection: self getSelection.	getTextSelector == #annotation ifFalse:		[(model dependents detect: [:dep | (dep isKindOf: PluggableTextMorph) and: [dep getTextSelector == #annotation]] ifNone: [nil]) doIfNotNil:			[:aPane | model changed: #annotation]]! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sw 9/8/2000 11:23'!eighth	"Answer the eighth element of the receiver.	Raise an error if there are not enough elements."	^ self checkedAt: 8! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sw 9/7/2000 18:10'!ninth	"Answer the ninth element of the receiver.	Raise an error if there are not enough elements."	^ self checkedAt: 9! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sw 9/7/2000 18:11'!seventh	"Answer the seventh element of the receiver.	Raise an error if there are not enough elements."	^ self checkedAt: 7! !!Utilities class methodsFor: 'flaps' stamp: 'sw 9/8/2000 21:23'!standardBottomFlap	"Answer a fully-instantiated flap named 'Supplies' to be placed at the bottom of the screen"	|  aFlapTab aPage |	aPage _ self newPartsFlapPage.	aPage setProperty: #maximumThumbnailWidth toValue: 80.	aFlapTab _ FlapTab new referent: aPage beSticky.	aFlapTab color: Color red lighter.	aFlapTab setToPopOutOnDragOver: true.	aFlapTab setToPopOutOnMouseOver: true.	aFlapTab assumeString: 'Supplies' font: Preferences standardFlapFont orientation: #horizontal color: Color red lighter.	aFlapTab edgeToAdhereTo: #bottom; inboard: false.	aPage extent: self currentWorld width @ 100.	aPage addMorphBack: Command undoRedoButtons markAsPartsDonor.	aPage addMorphBack: TrashCanMorph new markAsPartsDonor.	aPage addMorphBack: ScriptingSystem scriptControlButtons markAsPartsDonor.	#(PaintInvokingMorph RectangleMorph EllipseMorph StarMorph  CurveMorph PolygonMorph TextMorph ) do:		[:sym | aPage addMorphBack: (Smalltalk at: sym) authoringPrototype].	aPage addMorphBack: ScriptingSystem prototypicalHolder.	aPage addMorphBack: RectangleMorph roundRectPrototype.	#(ImageMorph BasicButton SimpleSliderMorph		PasteUpMorph    BookMorph TabbedPalette 		JoystickMorph  ) do:		[:sym | aPage addMorphBack: (Smalltalk at: sym) authoringPrototype].	aPage addMorphBack: Morph new previousPageButton markAsPartsDonor.	aPage addMorphBack: Morph new nextPageButton markAsPartsDonor.	aPage addMorphBack: ScriptingSystem holderWithAlphabet markAsPartsDonor.	aPage addMorphBack: (ClockMorph authoringPrototype showSeconds: false) step.	aPage replaceTallSubmorphsByThumbnails.	aPage fixLayout.	aFlapTab position: ((Display width - aFlapTab width) // 2 @ (self currentWorld height - aFlapTab height)).	aPage setProperty: #flap toValue: true.	aPage color: (Color red muchLighter "alpha: 0.2").	aPage extent: self currentWorld width @ 100.		^ aFlapTab! !