'From Squeak 2.4b of April 23, 1999 on 28 April 1999 at 11:42:21 am'!"Change Set:		FasterTextDate:			28 April 1999Author:			Dan IngallsThis change set speeds up several text-related operations:1.  FileLists now fetch up to 100,000 bytes in a single operation instead of showing just the beginning.  The old limit had been 30,000.2.  When typing into text, any type-ahead is fetched from the keyboard buffer before initiating each text replacement.  This reduces the update latency to approximately the time for a single replacement.3.  The kernel of text composition has been reworked to detect when recomposition of unchanged text following an insertion of deletion finds the same line breaks.  At this point, if the text is in a rectangular container, no further recomposition is necessary, and the old lines can simply be slid up or down by adjusting their character index and Y value.Also eliminates a gratuitous dependence on class Paragraph in the code for copy to ClipBoard."!!BookMorph methodsFor: 'menu' stamp: 'di 4/28/1999 11:32'!copyUrl	"Copy this page's url to the clipboard"	| str |	str _ currentPage url ifNil: [str _ 'Page does not have a url.  Send page to server first.'].	ParagraphEditor clipboardTextPut: str asText.! !!CharacterScanner methodsFor: 'private' stamp: 'di 3/20/1999 22:39'!setFont	"Set the font and other emphasis."	emphasisCode _ 0.	kern _ 0.	indentationLevel _ 0.	alignment _ textStyle alignment.	font _ nil.	(text attributesAt: lastIndex) do: 		[:att | att emphasizeScanner: self].	font == nil ifTrue: [self setFont: 1].	font _ font emphasized: emphasisCode.	"Install various parameters from the font."	spaceWidth _ font widthOf: Space. 	sourceForm _ font glyphs.  "Should only be needed in DisplayScanner"	height _ font height.			" ditto "	xTable _ font xTable.	stopConditions _ font stopConditions.	stopConditions at: Space asciiValue + 1 put: #space.	stopConditions at: Tab asciiValue + 1 put: #tab.	stopConditions at: CR asciiValue + 1 put: #cr.	stopConditions at: EndOfRun put: #endOfRun.	stopConditions at: CrossedX put: #crossedX! !!FormSetFont class methodsFor: 'examples' stamp: 'di 4/28/1999 11:33'!copy: charForm toClipBoardAs: char ascent: ascent	ParagraphEditor clipboardTextPut:		(Text string: char asString			attribute: (TextFontReference toFont: 				(FormSetFont new					fromFormArray: (Array with: charForm)					asciiStart: char asciiValue					ascent: ascent)))"	The S in the Squeak welcome window was installed by doing the following	in a workspace (where the value of, eg, charForm will persist through BitEdit...	f _ TextStyle default fontAt: 4.	oldS _ f characterFormAt: $S.	charForm _ Form extent: oldS extent depth: 8.	oldS displayOn: charForm.	charForm bitEdit.	...Play around with the BitEditor, then accept and close...	FormSetFont copy: charForm toClipBoardAs: $S ascent: f ascent.	...Then do a paste into the Welcome window"! !!HandMorph methodsFor: 'event handling' stamp: 'di 4/27/1999 21:44'!checkForMoreKeyboard	"Quick check for more keyboard activity -- Allows, eg, many characters	to be accumulated into a single replacement during type-in."	| oldPoint |	Sensor keyboardPressed ifFalse: [^ nil].	oldPoint _ lastEvent cursorPoint.	lastEvent _ MorphicEvent new		setKeyValue: Sensor keyboard asciiValue		mousePoint: oldPoint		buttons: Sensor primMouseButtons		hand: self.	remoteConnections size > 0 ifTrue: [self transmitEvent: lastEvent].	^ lastEvent! !!HandMorph methodsFor: 'event handling' stamp: 'di 4/27/1999 21:41'!processEvents	"Process user input events from the local input devices."	| griddedPoint evt currentExtent |	griddedPoint _ Sensor cursorPoint - owner viewBox topLeft.	gridOn ifTrue: [griddedPoint _ griddedPoint grid: grid].	evt _ MorphicEvent new		setMousePoint: griddedPoint		buttons: Sensor primMouseButtons		lastEvent: lastEvent		hand: self.	remoteConnections size > 0 ifTrue: [		currentExtent _ self worldBounds extent.		lastWorldExtent ~= currentExtent ifTrue: [			self transmitEvent: (MorphicEvent newWorldExtent: currentExtent).			lastWorldExtent _ currentExtent].		self transmitEvent: evt].	self handleEvent: evt.	[Sensor keyboardPressed] whileTrue: [		evt _ MorphicEvent new			setKeyValue: Sensor keyboard asciiValue			mousePoint: griddedPoint			buttons: Sensor primMouseButtons			hand: self.		remoteConnections size > 0 ifTrue: [self transmitEvent: evt].		self handleEvent: evt].! !!KeyboardBuffer methodsFor: 'as yet unclassified' stamp: 'di 4/27/1999 21:49'!keyboardPressed	| evt |	eventUsed ifFalse: [^ true].	(evt _ event hand checkForMoreKeyboard) ifNil: [^ false].	event _ evt.	eventUsed _ false.	^ true! !!NewParagraph methodsFor: 'composition' stamp: 'di 4/28/1999 11:07'!composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines	atY: startingY	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"	| charIndex lineY lineHeight scanner line row firstLine lineHeightGuess saveCharIndex hitCR maybeSlide sliding bottom priorIndex priorLine |	charIndex _ start.	lines _ lineColl.	lineY _ startingY.	lineHeightGuess _ textStyle lineGrid.	maxRightX _ container left.	maybeSlide _ stop < text size and: [container isMemberOf: Rectangle].	sliding _ false.	priorIndex _ 1.	bottom _ container bottom.	scanner _ CompositionScanner new text: text textStyle: textStyle.	firstLine _ true.	[charIndex <= text size and: [(lineY + lineHeightGuess) <= bottom]]		whileTrue:		[sliding			ifTrue:			["Having detected the end of rippling recoposition, we are only sliding old lines"			(priorIndex _ priorIndex + 1) <= priorLines size				ifTrue: ["Adjust and re-use previously composed line"						priorLine _ (priorLines at: priorIndex)									slideIndexBy: delta andMoveTopTo: lineY.						lineColl addLast: priorLine.						lineY _ priorLine bottom.						charIndex _ priorLine last + 1]				ifFalse: ["There are no more priorLines to slide."						sliding _ maybeSlide _ false]]			ifFalse:			[lineHeight _ lineHeightGuess.			saveCharIndex _ charIndex.			hitCR _ false.			row _ container rectanglesAt: lineY height: lineHeight.			1 to: row size do:				[:i | (charIndex <= text size and: [hitCR not]) ifTrue:						[line _ scanner composeFrom: charIndex inRectangle: (row at: i)								firstLine: firstLine leftSide: i=1 rightSide: i=row size.					lines addLast: line.					(text at: line last) = Character cr ifTrue: [hitCR _ true].					lineHeight _ lineHeight max: line lineHeight.  "includes font changes"					charIndex _ line last + 1]].			row size >= 1 ifTrue:			[lineY _ lineY + lineHeight.			lineY > bottom				ifTrue: ["Oops -- the line is really too high to fit -- back out"						charIndex _ saveCharIndex.						row do: [:r | lines removeLast]]				ifFalse: ["It's OK -- the line still fits."						maxRightX _ maxRightX max: scanner rightX.						1 to: row size - 1 do:  "Adjust heights across row if necess"							[:i | (lines at: lines size - row size + i)									lineHeight: lines last lineHeight									baseline: lines last baseline].						charIndex > text size ifTrue:							["end of text"							hitCR ifTrue:								["If text ends with CR, add a null line at the end"								((lineY + lineHeightGuess) <= container bottom) ifTrue:									[row _ container rectanglesAt: lineY height: lineHeightGuess.									row size > 0 ifTrue:										[line _ (TextLine start: charIndex stop: charIndex-1											internalSpaces: 0 paddingWidth: 0)										rectangle: row first;										lineHeight: lineHeightGuess baseline: textStyle baseline.										lines addLast: line]]].							lines _ lines asArray.							^ maxRightX].						firstLine _ false]]				ifFalse:				[lineY _ lineY + lineHeight].			(maybeSlide and: [charIndex > stop]) ifTrue:				["Check whether we are now in sync with previously composed lines"				 [priorIndex <= priorLines size					and: [(priorLines at: priorIndex) first < (charIndex - delta)]]						whileTrue: [priorIndex _ priorIndex + 1].				(priorLines at: priorIndex) first = (charIndex - delta)					ifTrue: ["Yes -- next line will have same start as prior line."							priorIndex _ priorIndex - 1.							maybeSlide _ false.							sliding _ true]					ifFalse: [priorIndex = priorLines size ifTrue:								["Weve reached the end of priorLines,								so no use to keep looking for lines to slide."								maybeSlide _ false]]]]].	firstLine ifTrue:		["No space in container or empty text"		line _ (TextLine start: start stop: start-1						internalSpaces: 0 paddingWidth: 0)				rectangle: (container topLeft extent: 0@lineHeightGuess);				lineHeight: lineHeightGuess baseline: textStyle baseline.		lines _ Array with: line].	"end of container"	lines _ lines asArray.	^ maxRightX! !!NewParagraph methodsFor: 'composition' stamp: 'di 4/28/1999 10:13'!recomposeFrom: start to: stop delta: delta	"Recompose this paragraph.  The altered portion is between start and stop.	Recomposition may continue to the end of the text, due to a ripple effect.	Delta is the amount by which the current text is longer than it was	when its current lines were composed."	| startLine newLines |	"Have to recompose line above in case a word-break was affected."	startLine _ (self lineIndexForCharacter: start) - 1 max: 1.	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]		whileTrue: [startLine _ startLine - 1].  "Find leftmost of line pieces"	newLines _ OrderedCollection new: lines size + 1.	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].	self composeLinesFrom: (lines at: startLine) first to: stop delta: delta			into: newLines priorLines: lines			atY: (lines at: startLine) top! !!NewParagraph methodsFor: 'editing' stamp: 'di 4/28/1999 10:14'!replaceFrom: start to: stop with: aText displaying: displayBoolean 	"Edit the text, and then recompose the lines." 	text replaceFrom: start to: stop with: aText.	self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop-start+1)! !!NewParagraph methodsFor: 'composition' stamp: 'di 4/28/1999 10:26'!composeAll	self composeLinesFrom: firstCharacterIndex to: text size delta: 0		into: OrderedCollection new priorLines: Array new atY: container top! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 4/28/1999 11:39'!clipboardText	^ self class clipboardText! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 4/28/1999 11:40'!clipboardTextPut: text	^ self class clipboardTextPut: text! !!ParagraphEditor class methodsFor: 'clipboard access' stamp: 'di 4/28/1999 11:30'!clipboardText	"Return the text currently in the clipboard. If the system clipboard is empty, or if it differs from the Smalltalk clipboard text, use the Smalltalk clipboard. This is done since (a) the Mac clipboard gives up on very large chunks of text and (b) since not all platforms support the notion of a clipboard."	| s |	s _ Smalltalk clipboardText.	(s isEmpty or: [s = CurrentSelection string])		ifTrue: [^ CurrentSelection]		ifFalse: [^ s asText]! !!ParagraphEditor class methodsFor: 'clipboard access' stamp: 'di 4/28/1999 11:31'!clipboardTextPut: text	"Set text currently on the clipboard.  Also export to Mac"	CurrentSelection _ text.	Smalltalk clipboardText: CurrentSelection string! !!StringHolder methodsFor: 'message list menu' stamp: 'di 4/28/1999 11:34'!copyName	"Copy the current selector to the clipboard"	| selector |	(selector _ self selectedMessageName) ifNotNil:		[ParagraphEditor clipboardTextPut: selector asString asText]! !!FileList methodsFor: 'file list menu' stamp: 'di 4/28/1999 11:33'!copyName	listIndex = 0 ifTrue: [^ self].	ParagraphEditor clipboardTextPut: self fullName asText.! !!FileList methodsFor: 'private' stamp: 'di 4/27/1999 20:47'!readContentsBrief: brevityFlag	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."	| f fileSize first5000 |	f _ directory oldFileOrNoneNamed: self fullName.	f ifNil: [^ 'For some reason, this file cannot be read'].	(brevityFlag not or: [(fileSize _ f size) <= 100000]) ifTrue:		[contents _ f contentsOfEntireFile.		brevityState _ #fullFile.   "don't change till actually read"		^ contents].	"if brevityFlag is true, don't display long files when first selected"	first5000 _ f next: 5000.	f close.	contents _ 'File ''', fileName, ''' is ', fileSize printString, ' bytes long.You may use the ''get'' command to read the entire file.Here are the first 5000 characters...------------------------------------------', first5000 , '------------------------------------------... end of the first 5000 characters.'.	brevityState _ #briefFile.   "don't change till actually read"	^ contents.! !!Inspector methodsFor: 'menu commands' stamp: 'di 4/28/1999 11:33'!copyName	"Copy the name of the current variable, so the user can paste it into the window below and work with is.  If collection, do (xxx at: 1). "	| sel aClass |	self selectionUnmodifiable ifTrue: [^ self changed: #flash].	(aClass _ self object class) isVariable ifTrue: [^ self changed: #flash].	sel _ aClass allInstVarNames at: selectionIndex - 2.	(self selection isKindOf: Collection) ifTrue: [sel _ '(',sel,' at: 1)'].	ParagraphEditor clipboardTextPut: sel asText.	"no undo allowed"! !!DictionaryInspector methodsFor: 'menu' stamp: 'di 4/28/1999 11:32'!copyName	"Copy the name of the current variable, so the user can paste it into the window below and work with is.  If collection, do (xxx at: 1). "	| sel |	sel _ '(self at: ', 		(String streamContents: [:strm | (keyArray at: selectionIndex) storeOn: strm]) ,		')'.	ParagraphEditor clipboardTextPut: sel asText.	"no undo allowed"! !!TextLine methodsFor: 'updating' stamp: 'di 4/28/1999 11:12'!slideIndexBy: delta andMoveTopTo: newTop	"Relocate my character indices and y-values.	Used to slide constant text up or down in the wake of a text replacement."	firstIndex _ firstIndex + delta.	lastIndex _ lastIndex + delta.	bottom _ bottom + (newTop - top).	top _ newTop.! !!TextMorph methodsFor: 'private' stamp: 'di 4/28/1999 10:37'!updateFromParagraph  	"A change has taken place in my paragraph, as a result of editing and I must be updated.  If a line break causes recomposition of the current paragraph, or it the selection has entered a different paragraph, then the current editor will be release, and must be reinstalled with the resulting new paragraph, while retaining any editor state, such as selection, undo state, and current typing emphasis."	| newStyle sel oldLast oldEditor |	paragraph ifNil: [^ self].	wrapFlag ifNil: [wrapFlag _ true].	editor ifNotNil: [oldEditor _ editor.					sel _ editor selectionInterval.					editor storeSelectionInParagraph].	paragraph textStyle = textStyle		ifTrue: [self fit]		ifFalse: ["Broadcast style changes to all morphs"				newStyle _ paragraph textStyle.				(self firstInChain text: text textStyle: newStyle) recomposeChain.				editor ifNotNil: [self installEditorToReplace: editor]].	sel ifNil: [^ self].	"If selection is in top line, then recompose predecessor for possible ripple-back"	predecessor ifNotNil:		[sel first <= (paragraph lines first last+1) ifTrue:			[oldLast _ predecessor lastCharacterIndex.			predecessor paragraph recomposeFrom: oldLast to: text size delta: 0.			oldLast = predecessor lastCharacterIndex				ifFalse: [predecessor changed. "really only last line"						self predecessorChanged]]].	((predecessor~~nil and: [sel first <= paragraph firstCharacterIndex])		or: [successor~~nil and: [sel first > (paragraph lastCharacterIndex+1)]])		ifTrue:		["The selection is no longer inside this paragraph.		Pass focus to the paragraph that should be in control."		self firstInChain withSuccessorsDo:			[:m |  (sel first between: m firstCharacterIndex								and: m lastCharacterIndex+1)					ifTrue: [m installEditorToReplace: editor.							^ self passKeyboardFocusTo: m]].		self error: 'Inconsistency in text editor' "Must be somewhere in the successor chain"].	editor ifNil:		["Reinstate selection after, eg, style change"		self installEditorToReplace: oldEditor]! !!TextStyle methodsFor: 'private' stamp: 'di 3/20/1999 22:31'!fontAt: index 	"This is private because no object outside TextStyle should depend on the 	representation of the font family in fontArray."	^ fontArray atPin: index! !NewParagraph removeSelector: #recomposeFrom:orLineAbove:!NewParagraph removeSelector: #composeLinesFrom:withLines:atY:!