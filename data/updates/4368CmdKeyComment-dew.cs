'From Squeak3.1alpha of 6 February 2001 [latest update: #3848] on 19 March 2001 at 12:55:58 pm'!"Change Set:		CmdKeyComment-dewDate:			19 March 2001Author:			Doug WayImprove the comment of ParagraphEditor class>>initializeCmdKeyShortcuts to mention the alt-key."!!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'dew 3/19/2001 12:49'!initializeCmdKeyShortcuts	"Initialize the (unshifted) command-key (or alt-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"ParagraphEditor initialize"	| cmdMap cmds |	cmdMap _ Array new: 256 withAll: #noop:.  "use temp in case of a crash"	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #backspace:.			"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.			"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"cmd-Return"	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (32 + 1) put: #selectWord:.			"space bar key"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	'0123456789-=' do: [ :char | cmdMap at: (char asciiValue + 1) put: #changeEmphasis: ].	'([{''"<' do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose: ].	cmdMap at: ($, asciiValue + 1) put: #shiftEnclose:.	cmds _ #(		$a	selectAll:		$b	browseIt:		$c	copySelection:		$d	doIt:		$e	exchange:		$f	find:		$g	findAgain:		$h	setSearchString:		$i	inspectIt:		$j	doAgainOnce:		$k  offerFontMenu:		$l	cancel:		$m	implementorsOfIt:		$n	sendersOfIt:		$o	spawnIt:		$p	printIt:		$q	querySymbol:		$r	recognizer:		$s	save:		$t	tempCommand:		$u	align:		$v	paste:		$w	backWord:		$x	cut:		$y	swapChars:		$z	undo:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).	].	CmdActions _ cmdMap! !