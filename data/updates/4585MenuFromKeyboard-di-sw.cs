'From Squeak3.2alpha of 2 October 2001 [latest update: #4584] on 5 December 2001 at 10:23:56 pm'!"Change Set:		MenuFromKeyboardDate:			2 December 2001Author:			Dan Ingalls and Scott WallaceThis changeSet introduces keyboard control of menus in morphic.First of all, there is now the ability to move the selection up and down using arrow keys in any menu, and to confirm with CR or cancel with ESC, and to pass control into a sub-menu with right- or left- arrow (and to return back with ESC).That's not all.  If you type in a menu, it builds a pattern string, which appears at the bottom of the menu.  Whenever the pattern is not empty, only items that match are presented.  If only one item matches the pattern (or in any menu with only one item), the option is given to confirm with CR or cancel with ESC.But wait, there's more...  In most contexts, hitting ESC now brings up the world menu, (and, of course, hitting ESC again will make it go away again).  This enables all sorts of TECO-like incantations such as	'<ESC>d q<CR>' which would, of course, invoke 'save and quit', or	'<ESC>he<CR>spa<CR>' to show the amount of free space.[A free bottle of Ripple will be awarded to the first Squeaker to successfully perform majorShrink on a computer with no pointing device].Finally, two cosmetic tweaks are also included:	1.  The selection in a menu is now shown slightly darker than before, and	2.  Menus indicate keyboard focus with a thin green border (useful with arrow control)."!!MenuItemMorph methodsFor: 'drawing' stamp: 'di 12/4/2001 23:16'!drawOn: aCanvas	| selectionColor |	isSelected & isEnabled		ifTrue:			[selectionColor _ Display depth <= 2				ifTrue: [Color gray]				ifFalse: [owner color darker darker].			aCanvas fillRectangle: self bounds color: selectionColor].	super drawOn: aCanvas.	subMenu ifNotNil:		[aCanvas			paintImage: SubMenuMarker			at: self right - 8 @ (self top + self bottom - SubMenuMarker height // 2)]! !!MenuItemMorph methodsFor: 'events' stamp: 'sw 12/4/2001 19:11'!mouseLeaveDragging: evt	"The mouse left the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."	owner ifNil: [^ self].	evt hand mouseFocus == owner ifFalse: [^ self].	"If we have a submenu, make sure we've got some time to enter it before actually leaving the menu item"	subMenu == nil		ifTrue:	[owner selectItem: nil event: evt]		ifFalse:	[self addAlarm: #deselectTimeOut: with: evt after: 500].! !!MenuMorph methodsFor: 'accessing' stamp: 'sw 12/4/2001 21:22'!commandKeyHandler	"Answer the receiver's commandKeyHandler"	^ self valueOfProperty: #commandKeyHandler ifAbsent: [nil]! !!MenuMorph methodsFor: 'accessing' stamp: 'sw 12/4/2001 21:23'!commandKeyHandler: anObject	"Set the receiver's commandKeyHandler.  Whatever you set here needs to be prepared to respond to the message #commandKeyTypedIntoMenu: "	self setProperty: #commandKeyHandler toValue: anObject! !!MenuMorph methodsFor: 'keyboard control' stamp: 'di 12/4/2001 22:48'!displayFiltered: evt	| pattern matchStr originals matchingItems |	pattern _ '*' , (matchStr _ self valueOfProperty: #matchString) , '*'.	originals _ self valueOfProperty: #originalSubmorphs.	self removeAllMorphs.	matchStr isEmpty ifTrue: [^ self addAllMorphs: originals].	matchingItems _ originals select:		[:m | (m isKindOf: MenuItemMorph) and: [pattern match: m contents]].	(originals first isKindOf: MenuItemMorph)		ifTrue: [self addAllMorphs: matchingItems]		ifFalse: [self addAllMorphs: {originals first} , matchingItems].  "include title (we hope)"	self addLine.		matchingItems size = 1		ifTrue: [self addMorphBack: (TextMorph new autoFit: true;					backgroundColor: Color green lighter;					contents: ' CR = yes / ESC = no ') lock.				self fullBounds.  "Lay out for submorph adjacency"				self selectItem: matchingItems first event: evt]		ifFalse: [self selectItem: nil event: evt.				self addMorphBack: (TextMorph new autoFit: true;					backgroundColor: Color blue lighter lighter lighter;					contents: ' items matching ' , matchStr printString, ' ') lock].! !!MenuMorph methodsFor: 'keyboard control' stamp: 'sw 12/4/2001 20:13'!handlesKeyboard: evt	"Answer whether the receiver handles the keystroke represented by the event"	^ evt anyModifierKeyPressed not or: [evt commandKeyPressed and: [self commandKeyHandler notNil]]! !!MenuMorph methodsFor: 'keyboard control' stamp: 'di 12/5/2001 21:40'!keyStroke: evt	| matchString char asc selectable |	self deleteBalloon.	(evt commandKeyPressed and: [self commandKeyHandler notNil]) ifTrue:			[self commandKeyHandler commandKeyTypedIntoMenu: evt.			^ self deleteIfPopUp: evt].	char _ evt keyCharacter.	char = Character cr ifTrue:		[selectedItem ifNotNil:			[selectedItem hasSubMenu			ifTrue: [evt hand newMouseFocus: selectedItem subMenu.					^ evt hand newKeyboardFocus: selectedItem subMenu]			ifFalse: ["self delete."					^ selectedItem invokeWithEvent: evt]].		(selectable _ self items) size = 1			ifTrue: [^ selectable first invokeWithEvent: evt].		^ self].	((asc _ char asciiValue) between: 27 and: 31) ifTrue:		[asc = 27 ifTrue:   "escape key"			[(self hasProperty: #matchString) ifTrue:				["If filtered, first ESC removes filter"				self selectItem: nil event: evt.				self removeProperty: #matchString.				self removeAllMorphs.				^ self addAllMorphs: (self valueOfProperty: #originalSubmorphs)].			"If a stand-alone menu, just delete it"			popUpOwner == nil ifTrue: [^ self delete].			"If a sub-menu, then deselect, and return focus to outer menu"			self selectItem: nil event: evt.			evt hand newMouseFocus: popUpOwner owner.			^ evt hand newKeyboardFocus: popUpOwner owner].		(asc = 28 or: [asc = 29]) ifTrue:		"left or right arrow key"			[(selectedItem ~~ nil and: [selectedItem hasSubMenu]) ifTrue:					[evt hand newMouseFocus: selectedItem subMenu.					^ evt hand newKeyboardFocus: selectedItem subMenu]].		asc = 30 ifTrue: [^ self moveSelectionDown: -1 event: evt].		"up arrow key"		asc = 31 ifTrue: [^ self moveSelectionDown: 1 event: evt].		"down arrow key"		].	matchString _ self valueOfProperty: #matchString ifAbsent:		[self setProperty: #matchString toValue: String new.		self setProperty: #originalSubmorphs toValue: submorphs copy.		String new].	matchString _ char = Character backspace		ifTrue: [matchString isEmpty				ifTrue: [matchString]				ifFalse: [matchString allButLast]]		ifFalse: [matchString copyWith: evt keyCharacter].	self setProperty: #matchString toValue: matchString.	self displayFiltered: evt! !!MenuMorph methodsFor: 'keyboard control' stamp: 'di 12/5/2001 11:41'!keyboardFocusChange: aBoolean	"Notify change due to green border for keyboard focus"	self changed! !!MenuMorph methodsFor: 'keyboard control' stamp: 'di 12/4/2001 21:46'!moveSelectionDown: direction event: evt	"Move the current selection up or down by one, presumably under keyboard control.	direction = +/-1"	| index m |	index _ submorphs indexOf: selectedItem ifAbsent: [1-direction].	submorphs do: "Ensure finite"		[:unused | m _ submorphs atWrap: (index _ index + direction).		(m isKindOf: MenuItemMorph) ifTrue: [^ self selectItem: m event: evt]].	^ self selectItem: nil event: evt! !!MenuMorph methodsFor: 'control' stamp: 'di 12/2/2001 21:02'!popUpAt: aPoint forHand: hand in: aWorld	"Present this menu at the given point under control of the given hand."	self items isEmpty ifTrue: [^ self].	self positionAt: aPoint relativeTo: (selectedItem ifNil:[self items first]) inWorld: aWorld.	aWorld addMorphFront: self; startSteppingSubmorphsOf: self.	"Aquire focus for valid pop up behavior"	hand newMouseFocus: self.	hand newKeyboardFocus: self.	self changed! !!MenuMorph methodsFor: 'drawing' stamp: 'di 12/4/2001 23:33'!drawOn: aCanvas	super drawOn: aCanvas.	ActiveHand keyboardFocus == self ifTrue:		[aCanvas frameAndFillRectangle: self innerBounds fillColor: Color transparent borderWidth: 1 borderColor: Color green]! !!MenuMorph methodsFor: 'events' stamp: 'di 12/5/2001 10:26'!handleFocusEvent: evt	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."	self processEvent: evt.	"Need to handle keyboard input if we have the focus."	evt isKeyboard ifTrue: [^ self handleEvent: evt].	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"	(evt isMouseOver or:[evt isMouse not]) ifTrue:[^self].	"What remains are mouse buttons and moves"	evt isMove ifFalse:[^self handleEvent: evt]. "handle clicks outside by regular means"	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."	selectedItem ifNotNil:[(selectedItem activateSubmenu: evt) ifTrue:[^self]].	"Note: The following does not traverse upwards but it's the best I can do for now"	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: evt) ifTrue:[^self]].! !!ParagraphEditor methodsFor: 'nonediting/nontyping keys' stamp: 'di 12/3/2001 21:49'!escapeToDesktop: characterStream 	"Pop up a morph to field keyboard input in the context of the desktop"	Smalltalk isMorphic ifTrue: [ActiveWorld putUpWorldMenuFromEscapeKey].	^ true! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'di 12/3/2001 21:30'!initializeCmdKeyShortcuts 	"Initialize the (unshifted) command-key (or alt-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"ParagraphEditor initialize"	| cmdMap cmds |	cmdMap _ Array new: 256 withAll: #noop:.  "use temp in case of a crash"	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #backspace:.			"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.		"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"cmd-Return"	cmdMap at: (27 + 1) put: #escapeToDesktop:.		"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.			"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (32 + 1) put: #selectWord:.			"space bar key"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	'0123456789-=' do: [ :char | cmdMap at: (char asciiValue + 1) put: #changeEmphasis: ].	'([{''"<' do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose: ].	cmdMap at: ($, asciiValue + 1) put: #shiftEnclose:.	cmds _ #(		$a	selectAll:		$b	browseIt:		$c	copySelection:		$d	doIt:		$e	exchange:		$f	find:		$g	findAgain:		$h	setSearchString:		$i	inspectIt:		$j	doAgainOnce:		$k  offerFontMenu:		$l	cancel:		$m	implementorsOfIt:		$n	sendersOfIt:		$o	spawnIt:		$p	printIt:		$q	querySymbol:		$r	recognizer:		$s	save:		$t	tempCommand:		$u	align:		$v	paste:		$w	backWord:		$x	cut:		$y	swapChars:		$z	undo:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).	].	CmdActions _ cmdMap! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'di 12/3/2001 21:30'!initializeShiftCmdKeyShortcuts 	"Initialize the shift-command-key (or control-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 	capitalized versions of the letters.	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."	| cmdMap cmds |	"shift-command and control shortcuts"	cmdMap _ Array new: 256 withAll: #noop:.  "use temp in case of a crash"	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #forwardDelete:.		"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.			"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (32 + 1) put: #selectWord:.			"space bar key"	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."	cmdMap at: (27 + 1) put: #shiftEnclose:.	"ctrl-["	"'""''(' do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose:]."	cmds _ #(		$a	argAdvance:		$b	browseItHere:		$c	compareToClipboard:		$d	duplicate:		$e	methodStringsContainingIt:		$f	displayIfFalse:		$g	fileItIn:		$h	cursorTopHome:		$i	exploreIt:		$j	doAgainMany:		$k	changeStyle:		$l	outdent:		$m	selectCurrentTypeIn:		$n	referencesToIt:		$p	makeProjectLink:		$r	indent:		$s	search:		$t	displayIfTrue:		$u	changeLfToCr:		$v	pasteInitials:		$w	methodNamesContainingIt:		$x	makeLowercase:		$y	makeUppercase:		$z	makeCapitalized:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).		"plain keys"		cmdMap at: ((cmds at: i) asciiValue - 32 + 1) put: (cmds at: i + 1).		"shifted keys"		cmdMap at: ((cmds at: i) asciiValue - 96 + 1) put: (cmds at: i + 1).		"ctrl keys"	].	ShiftCmdActions _ cmdMap! !!PasteUpMorph methodsFor: 'world menu' stamp: 'di 12/3/2001 21:52'!keystrokeInWorld: evt	"A keystroke was hit when no keyboard focus was in set, so it is sent here to the world instead.  This current implementation is regrettably hard-coded; until someone cleans this up, you may wish to edit this method to suit your personal taste in interpreting cmd-keys issued to the desktop."	|  aChar isCmd |	aChar _ evt keyCharacter.	aChar asciiValue = 27 ifTrue: "escape key"		[^ ActiveWorld putUpWorldMenuFromEscapeKey].	isCmd _ evt commandKeyPressed and: [Preferences cmdKeysInText].	(evt commandKeyPressed and: [Preferences eToyFriendly])			ifTrue:				[(aChar == $W) ifTrue: [^ self putUpWorldMenu: evt]].	(isCmd and: [Preferences honorDesktopCmdKeys]) ifTrue:		[(aChar == $o) ifTrue: [^ ActiveWorld activateObjectsTool].		(aChar == $F) ifTrue: [^ CurrentProjectRefactoring currentToggleFlapsSuppressed].		(aChar == $N) ifTrue:			[Preferences classicNavigatorEnabled ifTrue: [^ Preferences togglePreference: #showProjectNavigator]].		(aChar == $r) ifTrue: [^ ActiveWorld restoreMorphicDisplay].		Preferences eToyFriendly			ifFalse:				[(aChar == $\) ifTrue: [^ SystemWindow sendTopWindowToBack].				(aChar == $b) ifTrue: [^ Browser openBrowser].				(aChar == $k) ifTrue: [^ Workspace open].				(aChar == $m) ifTrue: [^ TheWorldMenu new adaptToWorld: World; newMorph].				(aChar == $t) ifTrue: [^ self findATranscript: evt].				(aChar == $w) ifTrue: [^ SystemWindow closeTopWindow].				(aChar == $z) ifTrue: [^ self commandHistory undoOrRedoCommand].				(aChar == $C) ifTrue: [^ self findAChangeSorter: evt].				(aChar == $R) ifTrue: [^ self openRecentSubmissionsBrowser: evt].				(aChar == $P) ifTrue: [^ self findAPreferencesPanel: evt].				(aChar == $W) ifTrue: [^ self findAMessageNamesWindow: evt]]]! !!PasteUpMorph methodsFor: 'world menu' stamp: 'di 12/3/2001 21:45'!putUpWorldMenu: evt	"Put up a menu in response to a click on the desktop, triggered by evt."	| menu |	self bringFlapTabsToFront.	evt isMouse ifTrue:		[evt yellowButtonPressed			ifTrue: [^ self yellowButtonClickOnDesktopWithEvent: evt].		evt shiftPressed ifTrue:[^ self findWindow: evt]].	"put up screen menu"	menu _ self buildWorldMenu: evt.	menu addTitle: Preferences desktopMenuTitle.	menu popUpEvent: evt in: self.	^ menu! !!PasteUpMorph methodsFor: 'world menu' stamp: 'di 12/3/2001 21:55'!putUpWorldMenuFromEscapeKey	(self putUpWorldMenu: ActiveEvent)		setBalloonText:'This menu was invokedby the ESC key.If you don''t want it,hit ESC again.'! !!PluggableListMorph methodsFor: 'model access' stamp: 'sw 12/4/2001 20:51'!commandKeyTypedIntoMenu: evt	"The user typed a command-key into a menu which has me as its command-key handler"	^ self modifierKeyPressed: evt keyCharacter! !!PluggableListMorph methodsFor: 'model access' stamp: 'di 12/3/2001 21:49'!specialKeyPressed: asciiValue 	| oldSelection nextSelection max howManyItemsShowing |	max _ self maximumSelection.	max > 0 ifFalse: [^ self].	nextSelection _ oldSelection _ self getCurrentSelectionIndex.	asciiValue = 31 ifTrue: 		[" down arrow"		nextSelection _ oldSelection + 1.		nextSelection > max ifTrue: [nextSelection _ 1]].	asciiValue = 30 ifTrue: 		[" up arrow"		nextSelection _ oldSelection - 1.		nextSelection < 1 ifTrue: [nextSelection _ max]].	asciiValue = 1 ifTrue:		[" home"		nextSelection _ 1].	asciiValue = 4 ifTrue:		[" end"		nextSelection _ max].	asciiValue = 27 ifTrue: 		[" escape key"		^ ActiveWorld putUpWorldMenuFromEscapeKey].	howManyItemsShowing _ self numSelectionsInView.	asciiValue = 11 ifTrue:		[" page up"		nextSelection _ 1 max: oldSelection - howManyItemsShowing].	asciiValue = 12 ifTrue:		[" page down"		nextSelection _ oldSelection + howManyItemsShowing min: max].	model okToChange ifFalse: [^ self].	"No change if model is locked"	oldSelection = nextSelection ifTrue: [^ self flash].	^ self changeModelSelection: nextSelection! !!PluggableListMorph methodsFor: 'menu' stamp: 'sw 12/4/2001 20:47'!getMenu: shiftKeyState	"Answer the menu for this text view, supplying an empty menu to be filled in. If the menu selector takes an extra argument, pass in the current state of the shift key."	| aMenu |	aMenu _ super getMenu: shiftKeyState.	aMenu commandKeyHandler: self.	^ aMenu! !!TheWorldMenu methodsFor: 'mechanics' stamp: 'sw 12/4/2001 20:22'!menu: titleString	"Create a menu with the given title, ready for filling"	| menu |	(menu _ MenuMorph entitled: titleString) 		defaultTarget: self; 		addStayUpItem;		commandKeyHandler: self.	self colorForDebugging: menu.	^ menu! !!TheWorldMenu methodsFor: 'construction' stamp: 'sw 12/4/2001 21:01'!buildWorldMenu	"Build the menu that is put up when the screen-desktop is clicked on"	| menu |	menu _ MenuMorph new defaultTarget: self.	menu commandKeyHandler: self.	self colorForDebugging: menu.	menu addStayUpItem.	self fillIn: menu from: {		{'previous project' . { #myWorld . #goBack }. 'return to the most-recently-visited project'}.		{'jump to project...' . { #myWorld . #jumpToProject }. 'put up a list of all projects, letting me choose one to go to' }.		{'save project on file...' . { #myWorld  . #saveOnFile }. 'save this project on a file' }.		{'load project from file...' . { self  . #loadProject }. 'load a project from a file' }.		nil}.	myWorld addUndoItemsTo: menu.		self fillIn: menu from: {		{'restore display (r)' . { World . #restoreMorphicDisplay }. 'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.' }.		nil}.	Preferences simpleMenus ifFalse:		[self fillIn: menu from: { 			{'open...' . { self  . #openWindow } }.			{'windows...' . { self  . #windowsDo } }.			{'changes...' . { self  . #changesDo } }}].	self fillIn: menu from: { 		{'help...' . { self  . #helpDo }.  'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.		{'appearance...' . { self  . #appearanceDo }. 'put up a menu offering many controls over appearance.' }}.	Preferences simpleMenus ifFalse:		[self fillIn: menu from: {			{'do...' . { Utilities . #offerCommonRequests} . 'put up an editible list of convenient expressions, and evaluate the one selected.' }}].	self fillIn: menu from: { 		nil.		{'objects (o)' . { #myWorld . #activateObjectsTool } . 'A tool for finding and obtaining many kinds of objects'}.		{'new morph...' . { self  . #newMorph }. 'Offers a variety of ways to create new objects'}.		nil.		{'authoring tools...' . { self  . #scriptingDo } . 'A menu of choices useful for authoring'}.		{'playfield options...' . { self  . #playfieldDo } . 'A menu of options pertaining to this object as viewed as a playfield' }.		{'flaps...'. { self . #flapsDo } . 'A menu relating to use of flaps.  For best results, use "keep this menu up"' }.		{'projects...' . { self  . #projectDo }. 'A menu of commands relating to use of projects' }}.	Preferences simpleMenus ifFalse:		[self fillIn: menu from: { 			{'print PS to file...' . { self  . #printWorldOnFile } . 'write the world into a postscript file'}.			{'debug...' . { self  . #debugDo } . 'a menu of debugging items' }}].	self fillIn: menu from: { 		nil.		{'save' . { self  . #saveSession } . 'save the current version of the image on disk' }.		{'save as...' . { Smalltalk . #saveAs }. 'save the current version of the image on disk under a new name.'}.		{'save as new version' . { Smalltalk . #saveAsNewVersion }. 'give the current image a new version-stamped name and save it under that name on disk.' }.		{'save and quit' . { self  . #saveAndQuit } . 'save the current image on disk, and quit out of Squeak.'}.		{'quit' . { self  . #quitSession } . 'quit out of Squeak.' }}.	^ menu! !!TheWorldMenu methodsFor: 'action' stamp: 'sw 12/4/2001 21:02'!commandKeyTypedIntoMenu: evt	"The user typed a command-key into the given menu; dispatch it"	myWorld keystrokeInWorld: evt ! !!Utilities class methodsFor: 'support windows' stamp: 'di 12/3/2001 21:34'!commandKeyMappings	^ self class firstCommentAt: #commandKeyMappings"Lower-case command keys(use with Cmd key on Mac and Alt key on other platforms)a	Select allb	Browse it (selection is a class name or cursor is over a class-list or message-list)c	Copy selectiond	Do it (selection is a valid expression)e	Exchange selection with prior selectionf	Findg	Find againh	Set selection as search string for find againi	Inspect it (selection is a valid expression, or selection is over an inspect-ilst)j	Again once (do the last text-related operation again)k	Set fontl	Cancelm	Implementors of it (selection is a message selector or cursor is over a class-list or message-list)n	Senders of it (selection is a message selector or cursor is over a class-list or message-list)o	Spawn current methodp	Print it (selection is a valid expression)q	Query symbol (toggle all possible completion for a given prefix)r	Recognizers	Save (i.e. accept)t	Finds a Transcript (when cursor is over the desktop)u	Toggle alignmentv	Pastew	Delete preceding word (over text);  Close-window (over morphic desktop)x	Cut selectiony	Swap charactersz	UndoNote: for Do it, Senders of it, etc., a null selection will be expanded to a word or to the current line in an attempt to do what you want.  Also note that Senders/Implementors of it will find the outermost keyword selector in a large selection, as when you have selected a bracketed expression or an entire line.  Finally note that the same cmd-m and cmd-n (and cmd-v for versions) work in the message pane of most browsers.Upper-case command keys	(use with Shift-Cmd, or Ctrl on Mac	or Shift-Alt on other platforms; sometimes Ctrl works too)A	Advance argumentB	Browse it in this same browser (in System browsers only)C	Compare argument to clipboardD	DuplicateE	Method strings containing itF	Insert 'ifFalse:'G	fileIn from it (a file name)H	cursor TopHome:I	Inspect via Object ExplorerJ	Again many (apply the previous text command repeatedly until the end of the text)K	Set styleL	Outdent (move selection one tab-stop left)M	Select current type-inN	References to it (selection is a class name, or cursor is over a class-list or message-list)O	Open single-message browser (in message lists)P	Make project linkR	Indent (move selection one tab-stap right)S	SearchT	Insert 'ifTrue:'U	Convert linefeeds to carriage returns in selectionV	Paste author's initialsW	Selectors containing it (in text); show-world-menu (when issued with cursor over desktop)X	Force selection to lowercaseY	Force selection to uppercaseZ	Capitalize all words in selectionOther special keysBackspace	Backward delete characterDel			Forward delete characterShift-Bksp	Backward delete wordShift-Del	Forward delete wordEsc			Pop up the Desktop Menu\			Send top window to backCursor keysleft, right,up, down	Move cursor left, right, up or downCtrl-left		Move cursor left one wordCtrl-right	Move cursor right one wordHome		Move cursor to begin of line or begin of textEnd			Move cursor to end of line or end of textPgUp, Ctrl-up	Move cursor up one pagePgDown, Ctrl-Dn	Move cursor down one pageNote all these keys can be used together with Shift to define or enlarge the selection. You cannot however shrink that selection again, as in some other systems.Other Cmd-key combinations (not available on all platforms)Return		Insert return followed by as many tabs as the previous line			(with a further adjustment for additional brackets in that line)Space		Select the current word as with double clickingEnclose the selection in a kind of bracket.  Each is a toggle.	(not available on all platforms)Ctrl-(	Enclose within ( and ), or remove enclosing ( and )Ctrl-[	Enclose within [ and ], or remove enclosing [ and ]Crtl-{	Enclose within { and }, or remove enclosing { and }Ctrl-<	Enclose within < and >, or remove enclosing < and >Ctrl-'	Enclose within ' and ', or remove enclosing ' and 'Ctrl-""	Enclose within "" and "", or remove enclosing "" and ""Note also that you can double-click just inside any of the above delimiters,or at the beginning or end of a line, to select the text enclosed.Text Emphasis	(not available on all platforms)Cmd-1	10 point fontCmd-2	12 point fontCmd-3	18 point fontCmd-4	24 point fontCmd-5	36 point fontCmd-6	color, action-on-click, link to class comment, link to method, url		Brings up a menu.  To remove these properties, select		more than the active part and then use command-0.Cmd-7	boldCmd-8	italicCmd-9	narrow (same as negative kern)Cmd-0	plain text (resets all emphasis)Cmd--	underlined (toggles it)Cmd-=	struck out (toggles it)Shift-Cmd--	(aka _) negative kern (letters 1 pixel closer)Shift-Cmd-+	positive kern (letters 1 pixel larger spread)"! !MenuMorph removeSelector: #moveSelectionDown:!MenuItemMorph removeSelector: #receiveNewFocus:!"Postscript:Install new command key bindings for ESC and cmd-sh-M."ParagraphEditor initialize.!