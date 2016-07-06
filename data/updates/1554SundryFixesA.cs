'From Squeak2.6 of 12 October 1999 [latest update: #1549] on 15 October 1999 at 3:34:23 pm'!"Change Set:		SundryFixesADate:			15 October 1999Author:			VariousMark Wai and Chris Norton's fix to correctly file out all selectors for the '-- all --' category.  Markus Denker's fix to Morph>>debuggingMenuFor:.DI's tweak so SystemDictionary>>version is consistent with EToySystem."!!ClassDescription methodsFor: 'fileIn/Out' stamp: 'di 10/15/1999 14:45'!fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex 	"File a description of the receiver's category, aString, onto aFileStream. If 	moveSource, is true, then set the method source pointer to the new file position.	Note when this method is called with moveSource=true, it is condensing the	.sources file, and should only write one preamble per method category."	| selectors |	aFileStream cr.	selectors := (aSymbol asString = ClassOrganizer allCategory)				ifTrue: [ self organization allMethodSelectors ]				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].	"Overridden to preserve author stamps in sources file regardless"	selectors do: [:sel |		self printMethodChunk: sel 			withPreamble: true			on: aFileStream 			moveSource: moveSource 			toFile: fileIndex].	^ self! !!Morph methodsFor: 'debug and other' stamp: 'di 10/15/1999 14:28'!debuggingMenuFor: aHandMorph	| aMenu aPlayer |	aMenu _ MenuMorph new defaultTarget: self.	(self hasProperty: #errorOnDraw) ifTrue:		[aMenu add: 'start drawing again' action: #resumeAfterDrawError.		aMenu addLine].	(self hasProperty: #errorOnStep) ifTrue:		[aMenu add: 'start stepping again' action: #resumeAfterStepError.		aMenu addLine].	aMenu add: 'control-menu...' target: aHandMorph selector: #invokeMetaMenuFor: argument: self.	aMenu add: 'inspect morph' action: #inspectInMorphic.	Smalltalk isMorphic ifFalse:		[aMenu add: 'inspect morph (in MVC)' action: #inspect].     aMenu add: 'explore morph' target: aHandMorph selector: #exploreArgument.	aMenu add: 'browse morph class' target: aHandMorph selector: #browseMorphClassFor: argument: self.	(aPlayer _ self player) ifNotNil:		[aMenu add: 'inspect player' target: aPlayer action: #inspect.		World ifNil: [aMenu add: 'inspect player (morphic)' action: #inspectArgumentsPlayerInMorphic].		aMenu add: 'browse player class' target: aPlayer action: #inspect].	aMenu add: 'make own subclass' target: aHandMorph action: #subclassMorph.	aMenu add: 'internal name ' action: #choosePartName.	aMenu add: 'save morph in file'  action: #saveOnFile.	aMenu addLine.	aMenu add: 'call #tempCommand' target: aHandMorph action: #callTempCommand.	aMenu add: 'define #tempCommand' target: aHandMorph action: #defineTempCommand.	aMenu addLine.	aMenu add: 'edit balloon help' action: #editBalloonHelpText.	^ aMenu! !!SystemDictionary methodsFor: 'sources, change log' stamp: 'di 10/15/1999 14:48'!version	"Answer the version of this release."	^ EToySystem version , ' of ' , EToySystem versionDate! !