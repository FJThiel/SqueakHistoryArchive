'From Squeak 2.1 of June 30, 1998 on 23 July 1998 at 3:59:25 am'!"Change Set:		uiBugsDate:			23 July 1998Author:			Lex SpoonSmall UI things:1. PluggableTextMorphs get reasonable scroll settings after a setText:, so you can use the scroll bars for huge texts.  The scroll setting still become incorrect if the text changes size by some other means, for instance by a big paste.2. Inspectors don't constantly update their display if the text is >1000 characters--it can get really slow.3. allow writing to a collapsed Transcript"!!Inspector methodsFor: 'accessing' stamp: 'ls 7/23/1998 03:49'!step	"Continuously update the value of the selected item"	| newText |	contents ifNotNil: [ contents size > 1000 ifTrue: [ 		"too much text--forget the automatic updating"		^self  ] ].	newText _ (selectionIndex = 2) | (selectionIndex = 0)		ifTrue: [self selection]		ifFalse: [self selection printString].	newText = contents ifFalse:		[contents _ newText.		self changed: #contents]! !!PluggableTextMorph methodsFor: 'initialization' stamp: 'ls 7/20/1998 22:56'!setScrollDeltas	| range |	scroller hasSubmorphs ifFalse: [^ self].	range _ self totalScrollRange.	range = 0 ifTrue: [^ scrollBar scrollDelta: 0.02 pageDelta: 0.2].	scrollBar scrollDelta: (10 / range) asFloat 			pageDelta: (self innerBounds height / range * 0.9) asFloat ! !!PluggableTextMorph methodsFor: 'model access' stamp: 'ls 7/20/1998 22:50'!setText: aText	scrollBar setValue: 0.0.	textMorph		ifNil: [textMorph _ TextMorphForEditView new						contents: aText wrappedTo: self innerBounds width-6.				textMorph setEditView: self.				scroller addMorph: textMorph]		ifNotNil: [textMorph newContents: aText].	self hasUnacceptedEdits: false.	self setScrollDeltas.! !!PluggableTextMorph methodsFor: 'updating' stamp: 'ls 7/23/1998 03:47'!update: aSymbol	aSymbol == #flash ifTrue: [^ self flash].	aSymbol == getTextSelector ifTrue:			[self setText: self getText.			^ self setSelection: self getSelection].	aSymbol == getSelectionSelector ifTrue: [^ self setSelection: self getSelection].	aSymbol == #autoSelect ifTrue:			[self handleEdit:				[textMorph editor setSearch: model autoSelectString;							againOrSame: true]].	aSymbol == #clearUserEdits ifTrue: [^ self hasUnacceptedEdits: false].	aSymbol == #wantToChange ifTrue:			[self canDiscardEdits ifFalse: [^ self promptForCancel].			^ self].	aSymbol == #appendEntry ifTrue:			[self handleEdit: [self appendEntry].			^ self world ifNotNil: [ self world displayWorld]].	aSymbol == #clearText ifTrue:			[self handleEdit: [self changeText: Text new].			^ self world displayWorld].! !