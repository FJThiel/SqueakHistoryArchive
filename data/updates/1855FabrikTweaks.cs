'From Squeak2.8alpha of 12 January 2000 [latest update: #1851] on 11 February 2000 at 10:10:52 am'!"Change Set:		FabrikTweaksDate:			11 February 2000Author:			Dan IngallsThis change set makes a number of tweaks to restore the Component framework to basic operability:	Create model in ComponentLayout initialize	Don't activate scroll bars when mouseover pins	PinMorph drag (position:) has been fixed so it is pixel-perfect.	PlaceFromSpec now uses position:	Sharing problems now fixed in text components.	Used removeKey:ifAbsent: in Player class removeInstVar to fix delete."!!Component methodsFor: 'variables' stamp: 'di 1/18/2000 16:10'!addVariableNamed: varName	| otherNames i partName |	"Adjust name if necessary and add it"	otherNames _ self class allInstVarNames.	i _ nil.	[i == nil ifTrue: [partName _ varName] ifFalse: [partName _ varName, i printString].	otherNames includes: partName]		whileTrue: [i == nil ifTrue: [i _ 1] ifFalse: [i _ i + 1]].	self class addInstVarName: partName.	"Now compile read method and write-with-change method"	self class compile: (String streamContents:			[:s | s nextPutAll: partName; cr;			tab; nextPutAll: '^', partName])		classified: 'view access'		notifying: nil.	self class compile: (String streamContents:			[:s | s nextPutAll: partName, 'Set: newValue'; cr;				tab; nextPutAll: partName, ' _ newValue.'; cr;				tab; nextPutAll: 'self changed: #', partName, '.'; cr;				tab; nextPutAll: '^ true' "for components that expect a boolean for accept"])		classified: 'view access'		notifying: nil.	^ Array with: partName asSymbol with: (partName , 'Set:') asSymbol! !!ComponentLayout methodsFor: 'as yet unclassified' stamp: 'di 1/17/2000 16:36'!initialize	super initialize.	self createCustomModel.	self extent: 384@256! !!PinMorph methodsFor: 'geometry' stamp: 'di 1/18/2000 12:31'!placeFromSpec	| side corners c1 c2 |	side _ pinSpec pinLoc asInteger.  "1..4 ccw from left"	corners _ owner bounds corners.	c1 _ corners at: side.	c2 _ corners atWrap: side+1.	self position: (c1 + (c2 - c1 * pinSpec pinLoc fractionPart)).	self updateImage.! !!PinMorph methodsFor: 'geometry' stamp: 'di 1/18/2000 12:28'!position: p	| r side p1 corners c1 c2 sideIndex |	"Adhere to owner bounds, and apply gridding"	r _ owner bounds.	side _ r sideNearestTo: p.	p1 _ r pointNearestTo: p.  "a point on the border"	(side = # top or: [side = #left])		ifTrue: [p1 _ r topLeft + (p1 - r topLeft grid: 4@4)]		ifFalse: [p1 _ r bottomRight + (p1 - r bottomRight grid: 4@4)].	"Update pin spec(5) = side index + fraction along side"	corners _ r corners.	sideIndex _ #(left bottom right top) indexOf: side.	c1 _ corners at: sideIndex.	c2 _ corners atWrap: sideIndex+1.	pinSpec pinLoc: sideIndex + ((p1 dist: c1) / (c2 dist: c1) min: 0.99999).	"Set new position with appropriate offset."	side = #top ifTrue: [super position: p1 - (0@8)].	side = #left ifTrue: [super position: p1 - (8@0)].	side = #bottom ifTrue: [super position: p1].	side = #right ifTrue: [super position: p1].	wires do: [:w | w pinMoved]! !!Player class methodsFor: 'slots' stamp: 'di 1/19/2000 17:18'!removeInstVarName: aName	self removeInstVarAccessorsFor: aName.	super removeInstVarName: aName.	self slotInfo removeKey: aName asSymbol ifAbsent: []! !!ScrollPane methodsFor: 'pane events' stamp: 'di 1/18/2000 15:29'!handlesMouseOver: evt	"Could just ^ true, but this ensures that scroll bars won't flop out	if you mouse-over appendages such as connecting pins."	| cp |	cp _ evt cursorPoint.	(bounds containsPoint: cp)		ifTrue: [^ true]					ifFalse: [self submorphsDo:					[:m | (m containsPoint: cp) ifTrue:							[m == scrollBar								ifTrue: [^ true]								ifFalse: [^ false]]].				^ false]! !!PluggableTextMorph methodsFor: 'model access' stamp: 'di 1/29/2000 14:34'!getText 	"Retrieve the current model text"	| newText |	getTextSelector == nil ifTrue: [^ Text new].	newText _ model perform: getTextSelector.	newText ifNil: [^Text new].	^ newText shallowCopy! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'di 1/29/2000 14:33'!accept	"Inform the model of text to be accepted, and return true if OK."	| textToAccept ok |	self canDiscardEdits ifTrue: [^ self flash].	self hasEditingConflicts ifTrue:		[(self confirm: 'Caution!! This method has been changed elsewheresince you started editing it here.  Accept anyway?') ifFalse: [^ self flash]].	textToAccept _ textMorph asText.	ok _ (setTextSelector == nil) or:		[setTextSelector numArgs = 2			ifTrue: [model perform: setTextSelector with: textToAccept with: self]			ifFalse: [model perform: setTextSelector with: textToAccept]].	ok ifTrue:		[self setText: textToAccept shallowCopy.		self hasUnacceptedEdits: false]! !!Text methodsFor: 'comparing' stamp: 'di 1/29/2000 14:15'!= other	| otherRuns |	^ other isText		ifTrue:	["This is designed to run fast even for megabytes"				otherRuns _ other asText runs.				(string == other string or: [string = other string])					and: [runs == otherRuns or: [runs = otherRuns]]]		ifFalse: [false]! !!TextMorph methodsFor: 'private' stamp: 'di 1/19/2000 15:47'!updateFromParagraph	"A change has taken place in my paragraph, as a result of editing and I must be updated.  If a line break causes recomposition of the current paragraph, or it the selection has entered a different paragraph, then the current editor will be release, and must be reinstalled with the resulting new paragraph, while retaining any editor state, such as selection, undo state, and current typing emphasis."	| newStyle sel oldLast oldEditor |	paragraph ifNil: [^ self].	wrapFlag ifNil: [wrapFlag _ true].	editor ifNotNil: [oldEditor _ editor.					sel _ editor selectionInterval.					editor storeSelectionInParagraph].	text _ paragraph text.	paragraph textStyle = textStyle		ifTrue: [self fit]		ifFalse: ["Broadcast style changes to all morphs"				newStyle _ paragraph textStyle.				(self firstInChain text: text textStyle: newStyle) recomposeChain.				editor ifNotNil: [self installEditorToReplace: editor]].	super layoutChanged.	sel ifNil: [^ self].	"If selection is in top line, then recompose predecessor for possible ripple-back"	predecessor ifNotNil:		[sel first <= (self paragraph lines first last+1) ifTrue:			[oldLast _ predecessor lastCharacterIndex.			predecessor paragraph recomposeFrom: oldLast to: text size delta: 0.			oldLast = predecessor lastCharacterIndex				ifFalse: [predecessor changed. "really only last line"						self predecessorChanged]]].	((predecessor~~nil and: [sel first <= self paragraph firstCharacterIndex])		or: [successor~~nil and: [sel first > (self paragraph lastCharacterIndex+1)]])		ifTrue:		["The selection is no longer inside this paragraph.		Pass focus to the paragraph that should be in control."		self firstInChain withSuccessorsDo:			[:m |  (sel first between: m firstCharacterIndex								and: m lastCharacterIndex+1)					ifTrue: [m installEditorToReplace: editor.							^ self passKeyboardFocusTo: m]].		self error: 'Inconsistency in text editor' "Must be somewhere in the successor chain"].	editor ifNil:		["Reinstate selection after, eg, style change"		self installEditorToReplace: oldEditor]! !