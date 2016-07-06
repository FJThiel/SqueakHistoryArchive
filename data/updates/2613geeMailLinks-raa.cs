'From Squeak2.9alpha of 17 July 2000 [latest update: #2612] on 13 September 2000 at 4:03:29 pm'!"Change Set:		geeMailLinksDate:			13 September 2000Author:			Bob Arning- add several text navigations commands to GeeMail (invoked by option/yellowButton menu)	'Go to top of document'	'Move selection to top of page'	'Define as jump start'	'Define as jump end'The last two allow creation of hyperlinks within the document."!TextAction subclass: #TextPlusJumpEnd	instanceVariableNames: 'jumpLabel '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-GeeMail'!TextAction subclass: #TextPlusJumpStart	instanceVariableNames: 'evalString jumpLabel '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-GeeMail'!TextMorph subclass: #TextPlusMorph	instanceVariableNames: 'scrollerOwner ignoreNextUp '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-GeeMail'!!AlansTextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:11'!scrollSelectionIntoView: event alignTop: alignTop	"Scroll my text into view if necessary and return true, else return false"	| selRects delta selRect rectToTest transform cpHere |	selRects _ theTextMorph paragraph selectionRects.	selRects isEmpty ifTrue: [^ false].	rectToTest _ selRects first merge: selRects last.	transform _ scroller transformFrom: self.	(event notNil and: [event anyButtonPressed]) ifTrue:  "Check for autoscroll"		[cpHere _ transform localPointToGlobal: event cursorPoint.		cpHere y <= self top			ifTrue: [rectToTest _ selRects first topLeft extent: 2@2]			ifFalse: [cpHere y >= self bottom					ifTrue: [rectToTest _ selRects last bottomRight extent: 2@2]					ifFalse: [^ false]]].	selRect _ transform localBoundsToGlobal: rectToTest.	selRect height > bounds height		ifTrue: [^ false].  "Would not fit, even if we tried to scroll"	alignTop ifTrue: [		self scrollBy: 0@(bounds top - selRect top).		^ true	].	selRect bottom > bounds bottom ifTrue: [		self scrollBy: 0@(bounds bottom - selRect bottom - 30).		^ true	].	(delta _ selRect amountToTranslateWithin: self bounds) y ~= 0 ifTrue: [		"Scroll end of selection into view if necessary"		self scrollBy: 0@delta y.		^ true].	^ false! !!TextPlusJumpEnd methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:33'!emphasizeScanner: scanner	"none for me, thanks"! !!TextPlusJumpEnd methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:08'!jumpLabel	^jumpLabel! !!TextPlusJumpEnd methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:08'!jumpLabel: aString	jumpLabel _ aString! !!TextPlusJumpEnd methodsFor: 'object fileIn' stamp: 'RAA 9/13/2000 13:28'!converte0: varDict j0: smartRefStrm	"These variables are automatically stored into the new instance #().	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('jumpLabel')  If a non-nil value is needed, please assign it."	"These are going away #('evalString').  Possibly store their info in another variable?"! !!TextPlusJumpStart methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 13:06'!actOnClickFor: model	"Subclasses may override to provide, eg, hot-spot actions"	model doJumpTo: jumpLabel.	^ true! !!TextPlusJumpStart methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:34'!emphasizeScanner: scanner	"Set the emphasist for text scanning"	scanner addEmphasis: 4! !!TextPlusJumpStart methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:21'!jumpLabel	^jumpLabel! !!TextPlusJumpStart methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:21'!jumpLabel: aString	jumpLabel _ aString! !!TextPlusJumpStart methodsFor: 'object fileIn' stamp: 'RAA 9/13/2000 15:45'!converte0: varDict j0: smartRefStrm	"These variables are automatically stored into the new instance #().	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('jumpLabel')  If a non-nil value is needed, please assign it."	"These are going away #('evalString').  Possibly store their info in another variable?"! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:01'!addJumpBeginning	| ed attribute jumpEnd mySelection a1 ax |	ed _ self editor.	(mySelection _ ed selection) isEmpty ifTrue: [^self inform: 'Please select something first'].	jumpEnd _ self chooseOneJumpEnd.	jumpEnd isEmptyOrNil ifTrue: [^self].	attribute _ TextPlusJumpStart new jumpLabel: jumpEnd.	a1 _ (mySelection attributesAt: 1) reject: [ :each | each isKindOf: TextPlusJumpStart].	ax _ (mySelection attributesAt: mySelection size) reject: [ :each | each isKindOf: TextPlusJumpStart].	ed replaceSelectionWith: 		(Text string: '*' attributes: a1),		(mySelection addAttribute: attribute),		(Text string: '*' attributes: ax).	self releaseParagraphReally.	self layoutChanged.! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:18'!addJumpEnd	| ed attribute jumpLabel selectedString |	ed _ self editor.	selectedString _ ed selection asString.	selectedString isEmpty ifTrue: [^self inform: 'Please select something first'].	jumpLabel _ FillInTheBlank request: 'Name this place' initialAnswer: selectedString.	jumpLabel isEmpty ifTrue: [^self].	self removeJumpEndFor: jumpLabel.	attribute _ TextPlusJumpEnd new jumpLabel: jumpLabel.	ed replaceSelectionWith: (ed selection addAttribute: attribute).! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 14:41'!allJumpEndStrings	| answer |	answer _ OrderedCollection new.	text runs withStartStopAndValueDo: [:start :stop :attributes |		attributes do: [:att |			(att isMemberOf: TextPlusJumpEnd) ifTrue: [				(answer includes: att jumpLabel) ifFalse: [answer add: att jumpLabel].			]		]	].	^answer! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:28'!chooseOneJumpEnd	| menu |	menu _ CustomMenu new.	self allJumpEndStrings do: [ :each |		menu 			add: each 			action: each	].	^menu build startUpCenteredWithCaption: 'Possible jump ends'.	! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:10'!doJumpTo: aString	| myStart myStop |	myStart _ myStop _ nil.	text runs withStartStopAndValueDo: [:start :stop :attributes |		attributes do: [:att |			((att isMemberOf: TextPlusJumpEnd) and: [att jumpLabel = aString]) ifTrue: [				myStart 					ifNil: [myStart _ start. myStop _ stop] 					ifNotNil: [myStart _ myStart min: start. myStop _ myStop max: stop].			]		]	].	myStart ifNil: [^self].	self editor selectFrom: myStart to: myStop.	ignoreNextUp _ true.	self changed.	scrollerOwner ifNil: [^self].	scrollerOwner scrollSelectionIntoView: nil alignTop: true.! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:52'!doYellowButtonPress: evt	| menu |	menu _ CustomMenu new.	menu 		add: 'Go to top of document'				action: [self jumpToDocumentTop];		add: 'Move selection to top of page'		action: [self scrollSelectionToTop];		add: 'Define as jump start'				action: [self addJumpBeginning];		add: 'Define as jump end'				action: [self addJumpEnd].	((menu build startUpCenteredWithCaption: 'Text navigation options') ifNil: [^self]) value.! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:07'!handleInteraction: interactionBlock fromEvent: evt		super handleInteraction: interactionBlock fromEvent: evt.	scrollerOwner ifNil: [^self].	scrollerOwner scrollSelectionIntoView: nil alignTop: false.! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 16:00'!jumpToDocumentTop	self editor selectFrom: 1 to: 0.	self changed.	scrollerOwner ifNil: [^self].	scrollerOwner scrollSelectionIntoView: nil alignTop: true.! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:53'!mouseDown: evt	ignoreNextUp _ false.	evt yellowButtonPressed ifTrue: [		^self doYellowButtonPress: evt	].	^super mouseDown: evt! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:53'!mouseMove: evt	ignoreNextUp == true ifTrue: [^self].	^super mouseMove: evt! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:53'!mouseUp: evt	ignoreNextUp == true ifTrue: [ignoreNextUp _ false. ^self].	^super mouseUp: evt! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 12:08'!removeJumpEndFor: aString	| anchors |	anchors _ OrderedCollection new.	text runs withStartStopAndValueDo: [:start :stop :attributes |		attributes do: [:att |			(att isMemberOf: TextPlusJumpEnd) ifTrue: [				att jumpLabel == aString ifTrue: [					anchors add: {att. start. stop}				]			]		]	].	anchors do: [ :old |		text removeAttribute: old first from: old second to: old third.	].! !!TextPlusMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/13/2000 15:36'!scrollSelectionToTop	scrollerOwner ifNil: [^self].	scrollerOwner scrollSelectionIntoView: nil alignTop: true.! !!TextPlusMorph methodsFor: 'object fileIn' stamp: 'RAA 9/13/2000 13:28'!convertbosfcettwpecpss0: varDict bosfcettwpecpssi0: smartRefStrm	"These variables are automatically stored into the new instance #('bounds' 'owner' 'submorphs' 'fullBounds' 'color' 'extension' 'textStyle' 'text' 'wrapFlag' 'paragraph' 'editor' 'container' 'predecessor' 'successor' 'scrollerOwner').	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('ignoreNextUp')  If a non-nil value is needed, please assign it."! !TextAction subclass: #TextPlusJumpStart	instanceVariableNames: 'jumpLabel '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-GeeMail'!AlansTextPlusMorph removeSelector: #scrollSelectionIntoView:!