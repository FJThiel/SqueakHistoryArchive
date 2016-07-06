'From Squeak3.1alpha of 28 February 2001 [latest update: #3880] on 26 March 2001 at 3:41:19 pm'!"Change Set:		scriptPerformer-swDate:			26 March 2001Author:			Scott WallaceA cleanup of the 'scriptPerformer' imbroglio.  This mechanism has long been used to revector communications between readouts and the players or morphs they feed off of.  Now, with a certain amount of trepidation about consequences for pre-existing content, we disenfranchise that mechanism altogether."!StringMorph subclass: #UpdatingStringMorph	instanceVariableNames: 'format target lastValue getSelector putSelector floatPrecision growable stepTime autoAcceptOnFocusLoss minimumWidth maximumWidth targetProvider '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Widgets'!!Morph methodsFor: 'e-toy support' stamp: 'sw 3/23/2001 12:43'!defaultFloatPrecisionFor: aGetSelector	"Answer a number indicating the default float precision to be used in a numeric readout for which the receiver provides the data.   Individual morphs can override this.  Showing fractional values for readouts of getCursor was in response to an explicit request from ack"	(#(getCursor getNumericValue getNumberAtCursor getCursorWrapped) includes: aGetSelector)		ifTrue:			[^ 0.01].	^ 1! !!CategoryViewer methodsFor: 'entries' stamp: 'sw 3/23/2001 13:56'!readoutFor: partName type: partType readOnly: readOnly getSelector: getSelector putSelector: putSelector	"Answer a readout morph for the given part"	| readout | 	(partType == #player) ifTrue:		[readout _ PlayerReferenceReadout new objectToView: scriptedPlayer viewSelector: getSelector putSelector: putSelector].	(partType == #color) ifTrue:		[readout _ UpdatingRectangleMorph new.		readout			getSelector: (ScriptingSystem getterSelectorFor: partName);			target: scriptedPlayer;			borderWidth: 1;			extent:  22@22.		putSelector == #unused ifFalse:			[readout putSelector: (ScriptingSystem setterSelectorFor: partName)]].	readout ifNil:  "player and color types handled above, the rest fall here"		[readout _ scriptedPlayer updatingTileForArgType: partType partName: partName getSelector: getSelector putSelector: putSelector].	readout step.	^ readout! !!Player methodsFor: 'scripts-kernel' stamp: 'sw 3/23/2001 22:40'!updatingTileForArgType: typeSymbol partName: partName getSelector: getSelector putSelector: putSelector	"Answer a readout tile representing the given part's value, given the putter, getter, and type information"	| aColor aTile displayer |	aColor _ Color lightGray lighter.	aTile _ typeSymbol == #number		ifTrue:			[NumericReadoutTile new typeColor: aColor]		ifFalse:				[typeSymbol == #sound				ifTrue:					[SoundReadoutTile new typeColor: aColor]				ifFalse:					[typeSymbol == #buttonPhase						ifTrue:							[SymbolListTile new choices: #(buttonDown whilePressed buttonUp) dataType:  typeSymbol]						ifFalse:							[StringReadoutTile new typeColor: aColor]]]. 	displayer _ UpdatingStringMorph new		getSelector: getSelector;		target: self;		growable: true;		minimumWidth: 24;		putSelector: ((putSelector == #unused) ifTrue: [nil] ifFalse: [putSelector]).	"Note that when typeSymbol = #number, the #target: call above will have dealt with floatPrecision details"	typeSymbol == #string		ifTrue:			[displayer useStringFormat.			displayer growable: true]		ifFalse:			[(typeSymbol == #sound)				ifTrue: 	[displayer useStringFormat]				ifFalse:	[displayer useDefaultFormat]].	aTile addMorphBack: displayer.	((putSelector ~~ #unused) and: [#(number sound boolean buttonPhase) includes: typeSymbol])  ifTrue: [aTile addArrows].	getSelector numArgs == 0 ifTrue:		[aTile setLiteralInitially: (self perform: getSelector)].	^ aTile! !!Player methodsFor: 'misc' stamp: 'sw 3/23/2001 12:41'!defaultFloatPrecisionFor: aGetSelector	"Answer the float position to use in conjunction with a readout for aGetSelector, which will be of the form 'getXXX'"	| aSlotName |	aSlotName _ Utilities inherentSelectorForGetter: aGetSelector.	((self elementTypeFor: aSlotName) == #userSlot)		ifTrue:			[^ (self slotInfoAt: aSlotName) floatPrecision].	self costume ifNotNil:		[^ self costume defaultFloatPrecisionFor: aGetSelector].	^ 1! !!UpdatingRectangleMorph methodsFor: 'target access' stamp: 'sw 3/26/2001 15:01'!readFromTarget	"Read the color value from my target"	| v |	((target == nil) or: [getSelector == nil]) ifTrue: [^ contents].	target isMorph ifTrue: [target isInWorld ifFalse: [^ contents]].	v _ self valueProvider perform: getSelector.	lastValue _ v.	^ v ! !!UpdatingRectangleMorph methodsFor: 'setting' stamp: 'sw 3/23/2001 23:26'!setTargetColor: aColor	"Set my target's color as indicated"	putSelector ifNotNil:		[self color: aColor.		contents _ aColor.		self valueProvider perform: self putSelector withArguments: (Array with: aColor)]! !!UpdatingRectangleMorph methodsFor: 'setting' stamp: 'sw 3/23/2001 13:24'!valueProvider	"Answer the object to which my get/set messages should be sent.  This is inefficient and contorted in order to support grandfathered content for an earlier design"	^ target isMorph		ifTrue:			[target topRendererOrSelf player ifNil: [target]]		ifFalse:			[target]! !!ColorSwatch methodsFor: 'as yet unclassified' stamp: 'sw 3/23/2001 12:13'!readFromTarget	"Obtain a value from the target and set it into my lastValue"	| v |	((target == nil) or: [getSelector == nil]) ifTrue: [^ contents].	v _ target perform: getSelector with: argument.	lastValue _ v.	^ v ! !!ColorSwatch methodsFor: 'as yet unclassified' stamp: 'sw 3/23/2001 12:12'!setTargetColor: aColor	"Set the target color as indicated"	putSelector ifNotNil:		[self color: aColor.		contents _ aColor.		target perform: self putSelector withArguments: (Array with: argument with: aColor)]! !!UpdatingStringMorph methodsFor: 'initialization' stamp: 'sw 3/26/2001 15:14'!initWithContents: aString font: aFont emphasis: emphasisCode	"Initialize the receiver to bear the given emphasis, font, and emphasis"	super initWithContents: aString font: aFont emphasis: emphasisCode.	targetProvider _ #none! !!UpdatingStringMorph methodsFor: 'initialization' stamp: 'sw 3/23/2001 23:30'!initialize	"Initialie the receiver to have default values in its instance variables"	super initialize.	format _ #default.  "formats: #string, #default"	target _ getSelector _ putSelector _  nil.	floatPrecision _ 1.	growable _ true.	stepTime _ 50.	autoAcceptOnFocusLoss _ true.	minimumWidth _ 8.	maximumWidth _ 300.	targetProvider _ #none! !!UpdatingStringMorph methodsFor: 'target access' stamp: 'sw 3/26/2001 15:10'!checkTarget	"If targetProvider is nil, it indicates a grandfathered item, so fixup is indicated"	| aPlayer |		(targetProvider == nil) ifTrue:		[(target isMorph and: [(aPlayer _ target topRendererOrSelf player) notNil])		ifTrue:			[target _ aPlayer].		targetProvider _ #none]! !!UpdatingStringMorph methodsFor: 'target access' stamp: 'sw 3/23/2001 22:38'!informTarget	"Obtain a value from my contents, and tell my target about it"	| newValue |	((target ~~ nil) and: [putSelector ~~ nil]) ifTrue:		[(newValue _ self valueFromContents) ifNotNil:			[self checkTarget.			target perform: putSelector with: newValue.			target isMorph ifTrue: [target changed]].			self fitContents]! !!UpdatingStringMorph methodsFor: 'target access' stamp: 'sw 3/23/2001 22:39'!readFromTarget	"Update my readout from my target"	| v |	((target == nil) or: [getSelector == nil]) ifTrue: [^ contents].	self checkTarget.	v _ target "scriptPerformer" perform: getSelector.	^ self acceptValueFromTarget: v! !!UpdatingStringMorph methodsFor: 'copying' stamp: 'sw 3/23/2001 22:42'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared."	super veryDeepInner: deepCopier.	format _ format veryDeepCopyWith: deepCopier.	target _ target.					"Weakly copied"	lastValue _ lastValue veryDeepCopyWith: deepCopier.	getSelector _ getSelector.			"Symbol"	putSelector _ putSelector.		"Symbol"	floatPrecision _ floatPrecision veryDeepCopyWith: deepCopier.	growable _ growable veryDeepCopyWith: deepCopier.	stepTime _ stepTime veryDeepCopyWith: deepCopier.	autoAcceptOnFocusLoss _ autoAcceptOnFocusLoss veryDeepCopyWith: deepCopier.	minimumWidth _ minimumWidth veryDeepCopyWith: deepCopier.	maximumWidth _ maximumWidth veryDeepCopyWith: deepCopier.	targetProvider _ targetProvider veryDeepCopyWith: deepCopier.! !!UpdatingBooleanStringMorph methodsFor: 'as yet unclassified' stamp: 'sw 3/23/2001 12:58'!informTarget	"Determine a value by evaluating my readout, and send that value to my target"	| newValue |	((target ~~ nil) and: [putSelector ~~ nil]) ifTrue:		[newValue _ self valueFromContents.		newValue ifNotNil:			[target perform: putSelector with: getSelector with: newValue.			target isMorph ifTrue: [target changed]].			self growable ifTrue:				[self readFromTarget; fitContents.				owner updateLiteralLabel]]! !Morph removeSelector: #updatingTileForArgType:partName:getSelector:putSelector:!