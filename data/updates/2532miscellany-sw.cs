'From Squeak2.9alpha of 5 August 2000 [latest update: #2531] on 29 August 2000 at 4:14:11 pm'!"Change Set:		miscellany-swDate:			29 August 2000Author:			Scott Wallace� A workaround for a recently-introduced bug in Morph.submorphNamed:ifNone:.� Don't step the models of collapsed system windows.� The programmatic compilations of Preferences methods no longer clutter up the current change set. � Performance improvements relating to computationof referencePlayfield"!!Morph methodsFor: 'submorphs-accessing' stamp: 'sw 8/29/2000 16:10'!submorphNamed: aName ifNone: aBlock	"Find the first submorph with this name, or a button with an action selector of that name"	| sub args |	self submorphs do: [:p | p knownName = aName ifTrue: [^ p]].	self submorphs do: [:button |		(button respondsTo: #actionSelector) ifTrue:			[button actionSelector == aName ifTrue: [^ button]].		((button respondsTo: #arguments) and: [(args _ button arguments) notNil])			ifTrue: [(args at: 2 ifAbsent: [nil]) == aName ifTrue: [^ button]].		(button isKindOf: AlignmentMorph) ifTrue:			[(sub _ button submorphNamed: aName ifNone: [nil]) ifNotNil: [^ sub]]].	^ aBlock value! !!Morph methodsFor: 'e-toy support' stamp: 'sw 8/29/2000 16:09'!referencePlayfield	"Answer the PasteUpMorph to be used for cartesian-coordinate reference"	| former |	owner ifNotNil:		[owner isPlayfieldLike ifTrue: [^ owner].		(owner isHandMorph and: [(former _ owner formerOwner) notNil])			ifTrue:				[^ former isPlayfieldLike 					ifTrue: [former]					ifFalse: [former referencePlayfield]]].	^ self world! !!PasteUpMorph methodsFor: 'world state' stamp: 'sw 8/29/2000 16:11'!referencePlayfield	"Answer a pasteup morph to be used as the reference for cartesian coordinates"	self isWorldMorph		ifTrue: [^ self submorphNamed: 'playfield' ifNone: [self]]		ifFalse: [^ super referencePlayfield]! !!Preferences class methodsFor: 'preferences dictionary' stamp: 'sw 8/29/2000 16:12'!wantsChangeSetLogging	"Answer whether method changes in the receiver should be logged to current change set.  This circumlocution avoids such logging for programmatically-compiled methods in Preferences, removing an annoyance"	^ Utilities authorInitialsPerSe  ~= 'programmatic'! !!SystemWindow methodsFor: 'stepping' stamp: 'sw 8/29/2000 16:12'!step	"Step the receiver.  Send it to the model, the receiver is open and if it **has* a model"	open ifTrue: [model ifNotNil: [model stepIn: self]]! !!SystemWindow methodsFor: 'stepping' stamp: 'sw 8/29/2000 16:13'!stepAt: millisecondClockValue	"Step the receiver at the given point in time.  Send it to the model, the receiver is open and if it **has* a model"	open ifTrue: [model ifNotNil: [model stepAt: millisecondClockValue in: self]]! !!SystemWindow methodsFor: 'stepping' stamp: 'sw 8/29/2000 16:13'!wantsSteps	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"	self isPartsDonor ifTrue: [^ false].	self player wantsSteps ifTrue: [^ true].	^ open and: [model wantsStepsIn: self]! !