"      NAME	EmacsKeys
       AUTHOR	cgreuter@calum.csclub.uwaterloo.ca (Chris Reuter)
       URL	(none)
       FUNCTION	Provides basic emacs key-bindings
       KEYWORDS	emacs keyboard
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.0.1
       DATE	03-Sep-98

SUMMARY

Adds methods to switch between the default key-bindings and one supporting a few emacsbindings.  Only CONTROL+key combinations arecurrently implemented, but those should bemore than helpful to those of us who spend ourdays using emacs, bash and X text-editingwidgets.

				Chris Reuter
"!
'From Squeak 2.0 of May 22, 1998 on 23 August 1998 at 4:31:31 am'!

!ParagraphEditor reorganize!
('initialize-release' changeParagraph: initialize resetState)
('accessing' initialText replace:with:and: replaceSelectionWith: selection selectionAsStream selectionInterval setSearch: text userHasEdited zapSelectionWith:)
('controlling' controlActivity controlInitialize controlTerminate isControlActive)
('scrolling' computeMarkerRegion markerDelta scrollAmount scrollBar scrollBy: scrollRectangleHeight scrollToBottom scrollToTop scrollView: updateMarker viewDelta)
('sensor access' processBlueButton processKeyboard processMouseButtons processRedButton processYellowButton)
('displaying' display flash)
('menu messages' accept again align browseIt browseItHere cancel changeAlignment changeEmphasis changeStyle clipboardText clipboardTextPut: compareToClipboard copySelection cut exchange experimentalCommand explain fileItIn find findAgain fit format implementorsOfIt lineSelectAndEmptyCheck: methodNamesContainingIt methodSourceContainingIt methodStringsContainingit mvcRedisplay paste performMenuMessage: presentSpecialMenu referencesToIt selectedSelector selectedSymbol sendersOfIt setSearchString shiftedYellowButtonMenu shiftedYellowButtonMessages spawn spawnWorkspace specialMenuItems undo)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainScan: explainTemp:)
('editing keys' align: browseIt: browseItHere: cancel: changeEmphasis: changeLfToCr: compareToClipboard: copyHiddenInfo copySelection: cut: doIt: duplicate: enclose: exchange: implementorsOfIt: inOutdent:delta: indent: inspectIt: makeCapitalized: makeLowercase: makeUppercase: methodNamesContainingIt: methodStringsContainingIt: noop: offerFontMenu offerFontMenu: outdent: paste: pasteInitials: printIt: recognizer: referencesToIt: save: sendersOfIt: setEmphasis: shiftEnclose: spawnIt: swapChars: tempCommand: undo:)
('typing/selecting keys' argAdvance: backWord: backspace: changeStyle: crWithIndent: cursorDown: cursorEnd: cursorHome: cursorLeft: cursorRight: cursorUp: displayIfFalse: displayIfTrue: doAgainMany: doAgainOnce: find: findAgain: forwardDelete: normalCharacter: querySymbol: search: selectAll: selectCurrentTypeIn: setSearchString: simulatedBackspace)
('emacs keys' cursorBeginningOfLine: cursorEndOfLine: cutToEndOfLine:)
('typing support' backTo: closeTypeIn closeTypeIn: dispatchOnCharacter:with: dispatchOnEnterWith: doneTyping insertTypeAhead: openTypeIn readKeyboard recognizeCharacters recognizeCharactersWhileMouseIn: setEmphasisHere simulatedKeystroke:)
('undoers' undoAgain:andReselect:typedKey: undoAndReselect:redoAndReselect: undoCutCopy: undoQuery:lastOffering: undoReplace)
('undo support' isDoing isRedoing isUndoing noUndoer undoMessage:forRedo: undoer: undoer:with: undoer:with:with: undoer:with:with:with:)
('current selection' deselect initializeSelection recomputeInterval recomputeSelection reverseSelection select selectAndScroll)
('new selection' afterSelectionInsertAndSelect: computeIntervalFrom:to: correctFrom:to:with: insertAndSelect:at: nextTokenFrom:direction: notify:at:in: selectAt: selectFrom:to: selectInterval: selectInvisiblyFrom:to: selectLine selectPrecedingIdentifier selectWord)
('private' againOnce: againOrSame: againOrSame:many: completeSymbol:lastOffering: exchangeWith: indent:fromStream:toStream: initializeYellowButtonMenu isDisjointFrom: nullText)
('do-its' doIt evaluateSelection inspectIt objectsReferencingIt printIt)
!


!ParagraphEditor methodsFor: 'emacs keys' stamp: 'cr 8/23/1998 02:28'!
cursorBeginningOfLine: characterStream 
	"Private - Move cursor from position in current line to beginning of
	current line. If cursor already at beginning of line, do nothing."
	| string left |

	sensor keyboard.

	string _ paragraph text string.
	left _ startBlock stringIndex.
	[left > 1 and: [(string at: (left - 1)) ~= Character cr]] whileTrue:
		[left _ left - 1].

	startBlock stringIndex == left
		ifFalse: [self selectAt: left].

	^true! !

!ParagraphEditor methodsFor: 'emacs keys' stamp: 'cr 8/23/1998 02:27'!
cursorEndOfLine: characterStream 
	"Private - Move cursor end of current line. If cursor already at end of
	 line, do nothing."
	| string right stringSize |

	sensor keyboard.

	string _ paragraph text string.
	stringSize _ string size.
	right _ stopBlock stringIndex.
	[right <= stringSize and: [(string at: right) ~= Character cr]]
		whileTrue: [right _ right + 1].

	stopBlock stringIndex == right
		ifFalse: [self selectAt: right].

	^true! !

!ParagraphEditor methodsFor: 'emacs keys' stamp: 'cr 8/23/1998 03:44'!
cutToEndOfLine: characterStream 
	"Cut from cursor position to end of line a la emacs CTRL-y. Flushes typeahead."

	| string stringSize right |
	sensor keyboard.		"flush character"

	string _ paragraph text string.
	stringSize _ string size.
	right _ stopBlock stringIndex.
	[right <= stringSize and: [(string at: right) ~= Character cr]]
		whileTrue: [right _ right + 1].

	"Only delete the trailing newline if we're at the end of the line."

	"Unfortunately, this doesn't behave as expected--merging consecutive CTRL-y's--so
	 I've removed it, as there's less chance of data loss."
[
	(right > startBlock stringIndex and: [(string at: right) = Character cr])
		ifTrue: [
			right _ right - 1.
		].
].
	self selectFrom: startBlock stringIndex to: right.
	self cut.
	^true! !

!ParagraphEditor methodsFor: 'typing support' stamp: 'cr 8/23/1998 02:10'!
dispatchOnCharacter: char with: typeAheadStream
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."

	char = Character enter
		ifTrue: [^ self dispatchOnEnterWith: typeAheadStream].

	"enter, backspace, and escape keys (ascii 3, 8, and 27) are command keys"
	(sensor commandKeyPressed 
		or: [(self class specialShiftCmdKeys includes: char asciiValue) 
			and: [sensor controlKeyPressed not]]) ifTrue:
		[^ sensor leftShiftDown
			ifTrue:
				[self perform: (ShiftCmdActions at: char asciiValue + 1) with: typeAheadStream]
			ifFalse:
				[self perform: (CmdActions at: char asciiValue + 1) with: typeAheadStream]].

	"the control key can be used to invoke shift-cmd shortcuts"
	sensor controlKeyPressed ifTrue:
		[^ self perform: (ShiftCmdActions at: char asciiValue + 1) with: typeAheadStream].
	^ self perform: #normalCharacter: with: typeAheadStream! !


!ParagraphEditor class reorganize!
('class initialization' initialize initializeTextEditorMenus yellowButtonMenu yellowButtonMessages)
('instance creation' new newParagraph:)
('keyboard shortcut tables' initializeCmdKeyShortcuts initializeShiftCmdKeyShortcuts specialShiftCmdKeys)
('emacs key bindings' defaultBindings emacsBindings emacsControlKeyShortcuts)
('clipboard access' clipboardContents)
!


!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'cr 8/23/1998 04:10'!
initializeCmdKeyShortcuts
	"Initialize the (unshifted) command-key shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"
	"ParagraphEditor initialize"

	| cmdMap cmds |
	cmdMap _ Array new: 256.  "use temp in case of a crash"
	cmdMap atAllPut: #noop:.
	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"
	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"
	cmdMap at: ( 8 + 1) put: #backspace:.			"ctrl-H or delete key"
	cmdMap at: (13 + 1) put: #crWithIndent:.			"cmd-Return"
	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"
	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"
	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"
	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"
	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"

	'0123456789-=' do: [ :char | cmdMap at: (char asciiValue + 1) put: #changeEmphasis: ].
	'([{''"<'         do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose: ].
	cmdMap at: ($, asciiValue + 1) put: #shiftEnclose:.

	cmds _ #(
		$a	selectAll:
		$b	browseIt:
		$c	copySelection:
		$d	doIt:
		$e	exchange:
		$f	find:
		$g	findAgain:
		$h	setSearchString:
		$i	inspectIt:
		$j	doAgainOnce:
		$k  offerFontMenu:
		$l	cancel:
		$m	implementorsOfIt:
		$n	sendersOfIt:
		$o	spawnIt:
		$p	printIt:
		$q	querySymbol:
		$r	recognizer:
		$s	save:
		$t	tempCommand:
		$u	align:
		$v	paste:
		$w	backWord:
		$x	cut:
		$y	swapChars:
		$z	undo:
	).
	1 to: cmds size by: 2 do: [ :i |
		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).
	].

	"Make the changes take effect immediately on all instances."
	CmdActions notNil
		ifTrue: [
			cmdMap withIndexDo: [ :value :index | CmdActions at: index put: value]
		] ifFalse: [	
			CmdActions _ cmdMap.
		].! !

!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'cr 8/23/1998 04:12'!
initializeShiftCmdKeyShortcuts
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	| cmdMap cmds |
	"shift-command and control shortcuts"
	cmdMap _ Array new: 256.  "use temp in case of a crash"
	cmdMap atAllPut: #noop:.
	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"
	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"
	cmdMap at: ( 8 + 1) put: #forwardDelete:.			"ctrl-H or delete key"
	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"
	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"
	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"
	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"
	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"
	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"
	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"
	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"
	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"

	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("
	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"
	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."
	cmdMap at: (27 + 1) put: #shiftEnclose:.	"ctrl-["

	cmds _ #(
		$a	argAdvance:
		$b	browseItHere:
		$c	compareToClipboard:
		$d	duplicate:
		$e	methodStringsContainingIt:
		$f	displayIfFalse:
		$j	doAgainMany:
		$k	changeStyle:
		$n	referencesToIt:
		$r	indent:
		$l	outdent:
		$s	search:
		$t	displayIfTrue:
		$u	changeLfToCr:
		$v	pasteInitials:
		$w	methodNamesContainingIt:
		$x	makeLowercase:
		$y	makeUppercase:
		$z	makeCapitalized:
	).
	1 to: cmds size by: 2 do: [ :i |
		cmdMap at: ((cmds at: i) asciiValue + 1)			put: (cmds at: i + 1).
		cmdMap at: (((cmds at: i) asciiValue - 96) + 1)	put: (cmds at: i + 1).
	].

	"Make the changes take effect immediately on all instances."
	ShiftCmdActions notNil
		ifTrue: [
			cmdMap withIndexDo: [ :value :index | ShiftCmdActions at: index put: value]
		] ifFalse: [	
			ShiftCmdActions _ cmdMap.
		].
! !

!ParagraphEditor class methodsFor: 'emacs key bindings' stamp: 'cr 8/23/1998 04:26'!
defaultBindings
	"Use default Squeak bindings."

	self initializeCmdKeyShortcuts.
	self initializeShiftCmdKeyShortcuts.
! !

!ParagraphEditor class methodsFor: 'emacs key bindings' stamp: 'cr 8/23/1998 04:06'!
emacsBindings
	"Use emacs bindings.  Yay!!"
	self emacsControlKeyShortcuts
! !

!ParagraphEditor class methodsFor: 'emacs key bindings' stamp: 'cr 8/23/1998 04:19'!
emacsControlKeyShortcuts
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	| cmdMap cmds |
	"shift-command and control shortcuts"
	cmdMap _ Array new: 256.  "use temp in case of a crash"
	cmdMap atAllPut: #noop:.
	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"
	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"
	cmdMap at: ( 8 + 1) put: #forwardDelete:.			"ctrl-H or delete key"
	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"
	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"
	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"
	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"
	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"
	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"
	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"
	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"
	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"

	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("
	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"
	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."
	cmdMap at: (27 + 1) put: #shiftEnclose:.	"ctrl-["

	cmds _ #(
		$a	cursorBeginningOfLine:
		$b	cursorLeft:
		$f	cursorRight:
		$n	cursorDown:
		$p	cursorUp:
		$d	forwardDelete:
		$e	cursorEndOfLine:
		$s	find:
		$k	cutToEndOfLine:
		$y	paste:
		$t	swapChars:
		$w	cut:
	).
	1 to: cmds size by: 2 do: [ :i |
		cmdMap at: ((cmds at: i) asciiValue + 1)			put: (cmds at: i + 1).
		cmdMap at: (((cmds at: i) asciiValue - 96) + 1)	put: (cmds at: i + 1).
	].

	"Cleverness to make the changes take effect immediately."
	ShiftCmdActions class = Array
		ifTrue: [
			cmdMap withIndexDo: [ :value :index | ShiftCmdActions at: index put: value]
		] ifFalse: [	
			ShiftCmdActions _ cmdMap.
		].! !



