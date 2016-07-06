'From Squeak 2.5 of August 6, 1999 [latest update: #1549] on 15 October 1999 at 2:05:31 pm'!"Change Set:		PageUpDnDate:			15 October 1999Author:			Stephan B. WesselsAdds pageUp and pageDown controls for MVC and Morphic text editors."!!NewParagraph methodsFor: 'access' stamp: 'sbw 10/13/1999 22:31'!numberOfLines	^lines size! !!ParagraphEditor methodsFor: 'typing/selecting keys' stamp: 'sbw 10/14/1999 16:37'!cursorDown: characterStream "Private - Move cursor from position in current line to same position innext line. If next line too short, put at end. If shift key down,select."	| shift string right left start position textSize|	shift := sensor leftShiftDown.	sensor keyboard.	string _ paragraph text string.	textSize _ string size.	left _ right _ stopBlock stringIndex.	[left > 1 and: [(string at: (left - 1)) ~= Character cr]] whileTrue:[left _ left - 1].	position _ stopBlock stringIndex - left.	[right < textSize and: [(string at: right) ~= Character cr]] whileTrue:[right _ right + 1].	right _ start _ right + 1.	[right < textSize and: [(string at: right) ~= Character cr]] whileTrue:[right _ right + 1].	shift		ifTrue: 			[			start + position > right				ifTrue: [self selectFrom: startBlock stringIndex to: right - 1]				ifFalse: [self selectFrom: startBlock stringIndex to: start +position - 1]			]		ifFalse: 			[			start + position > right				ifTrue: [self selectFrom: right to: right - 1]				ifFalse: [self selectFrom: start + position to: start + position -1]			].	^true! !!ParagraphEditor methodsFor: 'typing/selecting keys' stamp: 'sbw 10/12/1999 12:11'!cursorPageDown: characterStream 	^self cursorPageJump: characterStream down: true! !!ParagraphEditor methodsFor: 'typing/selecting keys' stamp: 'sbw 10/13/1999 22:40'!cursorPageJump: characterStream down: aBoolean"Private - Move cursor from position in current line to same position in the line on the next page up or down (direction is controlled by <aBoolean>. If next line too short, put at end. If shift key down, select.  This method is similar to #cursorDown:.  Haven't figured out how to intercept the shift key yet.See Utilities createPageTestWorkspace to create a test MVC workspace."	| string right left start position textSize currentLineNumber howManyLines visibleHeight totalHeight ratio deltaLines targetLine offsetAtTargetLine |	sensor keyboard.  "Eat the key stroke."	string _ paragraph text string.	textSize _ string size.	left _ right _ stopBlock stringIndex.	"Calculate the position of the left edge."	[left > 1 and: [(string at: (left - 1)) ~= Character cr]] whileTrue: [left _ left - 1].	"Calculate the offset from the left edge where cursor is now."	position _ stopBlock stringIndex - left.	"Calculate the current line number."	currentLineNumber _ paragraph lineIndexOfCharacterIndex: stopBlock stringIndex.	howManyLines _ paragraph numberOfLines.	visibleHeight _ self visibleHeight.	totalHeight _ self totalTextHeight.	ratio _ visibleHeight / totalHeight.	deltaLines _ (ratio * howManyLines) rounded - 2.	targetLine _ aBoolean		ifTrue: [(currentLineNumber + deltaLines) min: howManyLines]		ifFalse: [(currentLineNumber - deltaLines) max: 1].	offsetAtTargetLine _ (paragraph lines at: targetLine) first.	"Calculate the position of the right edge of text of target line."	right _ offsetAtTargetLine.	[right < textSize and: [(string at: right) ~= Character cr]] whileTrue: [right _ right + 1].	start _ offsetAtTargetLine.	start + position > right			ifTrue: [self selectForTopFrom: right to: right - 1]			ifFalse: [self selectForTopFrom: start + position to: start + position - 1].	^true! !!ParagraphEditor methodsFor: 'typing/selecting keys' stamp: 'sbw 10/12/1999 12:11'!cursorPageUp: characterStream 	^self cursorPageJump: characterStream down: false! !!ParagraphEditor methodsFor: 'current selection' stamp: 'sbw 10/12/1999 16:51'!selectAndScrollToTop	"Scroll until the selection is in the view and then highlight it."	| lineHeight deltaY clippingRectangle |	self select.	lineHeight _ paragraph textStyle lineGrid.	clippingRectangle _ paragraph clippingRectangle.	deltaY _ stopBlock top - clippingRectangle top.	deltaY ~= 0 		ifTrue: [self scrollBy: (deltaY abs + lineHeight - 1 truncateTo: lineHeight)									* deltaY sign]! !!ParagraphEditor methodsFor: 'as yet unclassified' stamp: 'sbw 10/13/1999 22:40'!totalTextHeight	^paragraph boundingBox height! !!ParagraphEditor methodsFor: 'as yet unclassified' stamp: 'sbw 10/13/1999 22:33'!visibleHeight	^paragraph clippingRectangle height! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'sbw 10/8/1999 21:28'!initializeCmdKeyShortcuts	"Initialize the (unshifted) command-key shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"ParagraphEditor initialize"	| cmdMap cmds |	cmdMap _ Array new: 256.  "use temp in case of a crash"	cmdMap atAllPut: #noop:.	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #backspace:.			"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.			"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"cmd-Return"	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	'0123456789-=' do: [ :char | cmdMap at: (char asciiValue + 1) put: #changeEmphasis: ].	'([{''"<'         do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose: ].	cmdMap at: ($, asciiValue + 1) put: #shiftEnclose:.	cmds _ #(		$a	selectAll:		$b	browseIt:		$c	copySelection:		$d	doIt:		$e	exchange:		$f	find:		$g	findAgain:		$h	setSearchString:		$i	inspectIt:		$j	doAgainOnce:		$k  offerFontMenu:		$l	cancel:		$m	implementorsOfIt:		$n	sendersOfIt:		$o	spawnIt:		$p	printIt:		$q	querySymbol:		$r	recognizer:		$s	save:		$t	tempCommand:		$u	align:		$v	paste:		$w	backWord:		$x	cut:		$y	swapChars:		$z	undo:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).	].	CmdActions _ cmdMap.! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'sbw 10/9/1999 08:44'!initializeShiftCmdKeyShortcuts	"Initialize the shift-command-key (or control-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 	capitalized versions of the letters.	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."	| cmdMap cmds |	"shift-command and control shortcuts"	cmdMap _ Array new: 256.  "use temp in case of a crash"	cmdMap atAllPut: #noop:.	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #forwardDelete:.			"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.			"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."	cmdMap at: (27 + 1) put: #shiftEnclose:.	"ctrl-["	cmds _ #(		$a	argAdvance:		$b	browseItHere:		$c	compareToClipboard:		$d	duplicate:		$e	methodStringsContainingIt:		$f	displayIfFalse:		$j	doAgainMany:		$k	changeStyle:		$n	referencesToIt:		$r	indent:		$l	outdent:		$s	search:		$t	displayIfTrue:		$u	changeLfToCr:		$v	pasteInitials:		$w	methodNamesContainingIt:		$x	makeLowercase:		$y	makeUppercase:		$z	makeCapitalized:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).		"plain keys"		cmdMap at: ((cmds at: i) asciiValue - 32 + 1) put: (cmds at: i + 1).		"shifted keys"		cmdMap at: ((cmds at: i) asciiValue - 96 + 1) put: (cmds at: i + 1).		"ctrl keys"	].	ShiftCmdActions _ cmdMap.! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'sbw 10/8/1999 21:42'!specialShiftCmdKeys"Private - return array of key codes that represent single keys actingas if shift-command were also being pressed"^#(	1	"home"	3	"enter"	4	"end"	8	"backspace"	11	"page up"	12	"page down"	27	"escape"	28	"left arrow"	29	"right arrow"	30	"up arrow"	31	"down arrow"	127	"delete"	)! !!PluggableTextController methodsFor: 'as yet unclassified' stamp: 'sbw 10/12/1999 16:46'!selectForTopFrom: start to: stop	"Deselect, then select the specified characters inclusive.	 Be sure the selection is in view."	(start = startBlock stringIndex and: [stop + 1 = stopBlock stringIndex]) ifFalse:		[view superView ifNotNil: [self deselect].		self selectInvisiblyFrom: start to: stop].	view superView ifNotNil: [self selectAndScrollToTop]! !!TextMorphEditor methodsFor: 'mvc compatibility' stamp: 'sbw 10/14/1999 17:06'!selectAndScrollToTop	"Scroll until the selection is in the view and then highlight it."	| lineHeight deltaY rect deltaX |	lineHeight _ paragraph textStyle lineGrid.	rect _ morph owner bounds.	deltaY _ stopBlock top - rect top.	deltaY ~= 0 ifTrue: [		deltaX _ 0.		deltaY _ (deltaY abs + lineHeight - 1 truncateTo: lineHeight) negated.		morph editView scrollBy: deltaX@deltaY]! !!TextMorphEditor methodsFor: 'mvc compatibility' stamp: 'sbw 10/14/1999 16:51'!selectForTopFrom: start to: stop	self selectFrom: start to: stop.	morph editView ifNotNil: [self selectAndScrollToTop]! !!TextMorphEditor methodsFor: 'as yet unclassified' stamp: 'sbw 10/13/1999 22:41'!totalTextHeight	^paragraph lines last bottom! !!TextMorphEditor methodsFor: 'as yet unclassified' stamp: 'sbw 10/13/1999 22:43'!visibleHeight	^morph owner bounds height! !!Utilities class methodsFor: 'miscellaneous' stamp: 'sbw 10/13/1999 22:28'!createPageTestWorkspace	"Used to generate a workspace window for testing page up and page down stuff."	"Utilities createPageTestWorkspace"	| numberOfLines maxStringLength minLineCounterSize lineCounterSize offsetSize stream headerConstant prevStart prevStrLen prevLineNumber stringLen lineNumber start log pad charIndex char |	numberOfLines _ 400.	maxStringLength _ 22.	minLineCounterSize _ 3.	lineCounterSize _ (numberOfLines log asInteger + 1) max: minLineCounterSize.	offsetSize _ 5.	stream _ ReadWriteStream on: ''.	headerConstant _ lineCounterSize + 1 + offsetSize + 1.	prevStart _ headerConstant negated.	prevStrLen _ 0.	prevLineNumber _ 0.	numberOfLines timesRepeat: [		stringLen _ maxStringLength atRandom max: 1.		lineNumber _ prevLineNumber + 1.		start _ prevStart + prevStrLen + headerConstant + 1.		prevStart _ start.		prevStrLen _ stringLen.		prevLineNumber _ lineNumber.		log _ lineNumber log asInteger.		pad _ lineCounterSize - log - 1.		pad timesRepeat: [stream nextPutAll: '0'].		stream nextPutAll: lineNumber printString.		stream space.		log _ start log asInteger.		pad _ offsetSize - log - 1.		pad timesRepeat: [stream nextPutAll: '0'].		stream nextPutAll: start printString.		stream space.		charIndex _ 'a' first asInteger.		stringLen timesRepeat: [			char _ Character value: charIndex.			charIndex _ charIndex + 1.			stream nextPut: char].		lineNumber = numberOfLines ifFalse: [stream cr]		].	(Workspace new contents: stream contents) openLabel: 'Test Data'.! !"Postscript:Initialize key tables."ParagraphEditor initialize.!