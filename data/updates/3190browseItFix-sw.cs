'From Squeak2.9alpha of 5 August 2000 [latest update: #3249] on 10 January 2001 at 3:47:51 pm'!"Change Set:		browseItFix-swDate:			10 January 2001Author:			Scott WallaceIn ParagraphEditor.selectedSymbol and Utilities.classFromPattern:withCaption:, strip blanks and tabs from the pattern before doing the match.  This allows 'browse it' (cmd-b) to work as expected on a line that has a class name preceded by one or more tabs, and/or followed by one or more space characters, for example.Issuing cmd b with the selection being an insertion point after the space character after the word Utilities in the following line had formerly not opened a browser as it should; after this update, it will:		Utilities "!!ParagraphEditor methodsFor: 'menu messages' stamp: 'sw 1/9/2001 05:33'!selectedSymbol	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"	| aString |	startBlock = stopBlock ifTrue: [^ nil].	aString _ self selection string copyWithoutAll:		{Character space.  Character cr.  Character tab}.	aString size == 0 ifTrue: [^ nil].	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].	^ nil! !!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 1/9/2001 05:34'!classFromPattern: pattern withCaption: aCaption	"If there is a class whose name exactly given by pattern, return it.	If there is only one class in the system whose name matches pattern, return it.	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.	This method ignores tab, space, & cr characters in the pattern"	| toMatch potentialClassNames classNames exactMatch index |	(toMatch _  pattern copyWithoutAll:			{Character space.  Character cr.  Character tab})		isEmpty ifTrue: [^ nil].	Symbol hasInterned: toMatch ifTrue:		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:			[:maybeClass | (maybeClass isKindOf: Class) ifTrue: [^ maybeClass]]].	toMatch _ (toMatch copyWithout: $.) asLowercase.	potentialClassNames _ Smalltalk classNames asOrderedCollection.	classNames _ pattern last = $. 		ifTrue: [potentialClassNames select:					[:nm |  nm asLowercase = toMatch]]		ifFalse: [potentialClassNames select: 					[:n | n includesSubstring: toMatch caseSensitive: false]].	classNames isEmpty ifTrue: [^ nil].	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: [nil].	index _ classNames size = 1		ifTrue:	[1]		ifFalse:	[exactMatch			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]			ifNotNil: [classNames addFirst: exactMatch.				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].	index = 0 ifTrue: [^ nil].	^ Smalltalk at: (classNames at: index) asSymbol"	Utilities classFromPattern: 'CharRecog'	Utilities classFromPattern: 'rRecog'	Utilities classFromPattern: 'znak'	Utilities classFromPattern: 'orph'"! !