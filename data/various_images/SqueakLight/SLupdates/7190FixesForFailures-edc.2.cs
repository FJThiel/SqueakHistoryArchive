'From SqueakLight|II of 31 May 2008 [latest update: #7188] on 1 July 2008 at 7:28 am'!"Change Set:		7190FixesForFailures-edcDate:			1 July 2008Author:			Edgar J. De CleeneThis is a first cleanup based on results of:| loader |    loader _ CodeLoader new.    loader baseURL: 'http://www.squeaksource.com/Ladrillos/'.    loader loadSourceFiles: #('SUnit-edc.1.mcz' 'SUnitGUI-edc.1.mcz''Tests-edc.36.mcz' 'Tests-Utilities-edc.1' 'KernelTests-edc.1.mcz''GraphicsTests-edc.1.mcz' 'CollectionsTests-edc.75.mcz''Tests-edc.1.mcz' 'SystemChangeNotification-Tests-edc.36.mcz''MorphicTests-edc.1.mcz');installSourceFiles."Object class removeSelector: #registeredServices.ParagraphEditor class removeSelector: #initializeTextEditorMenus.!!Color methodsFor: 'html' stamp: 'hmm 11/1/2006 23:28'!printHtmlString	"answer a string whose characters are the html representation  	of the receiver"	^ ((self red * 255) asInteger printStringBase: 16 length: 2 padded: true)	, ((self green * 255) asInteger printStringBase: 16 length: 2 padded: true)	, ((self blue * 255) asInteger printStringBase: 16 length: 2 padded: true)! !!Number methodsFor: 'printing' stamp: 'fcs 12/13/2006 19:47'!printShowingDecimalPlaces: placesDesired	"Print the receiver showing precisely the given number of places desired.  If placesDesired is positive, a decimal point and that many digits after the decimal point will always be shown.  If placesDesired is zero, a whole number will be shown, without a decimal point.  It now handles negative numbers between 0 and -1 and rounds correctly in more cases.  This method probably could be optimized -- improvements welcomed.  Category was/is 'converting' but should be 'printing' "	| precision rounded frac sign integerString fractionString result |	placesDesired <= 0 ifTrue: [^ self rounded printString].	precision _ Utilities floatPrecisionForDecimalPlaces: placesDesired.	rounded _ self roundTo: precision.	sign := rounded negative ifTrue: ['-'] ifFalse: [''].	integerString := rounded abs integerPart asInteger printString.	frac := ((rounded abs fractionPart roundTo: precision) * (10 raisedToInteger: placesDesired)) asInteger.	fractionString := frac printString padded: #right to: placesDesired with: $0.	result := sign , integerString , '.' , fractionString.	^result"23 printShowingDecimalPlaces: 223.5698 printShowingDecimalPlaces: 2-234.567 printShowingDecimalPlaces: 523.4567 printShowingDecimalPlaces: 023.5567 printShowingDecimalPlaces: 0-23.4567 printShowingDecimalPlaces: 0-23.5567 printShowingDecimalPlaces: 0100000000 printShowingDecimalPlaces: 10.98 printShowingDecimalPlaces: 2-0.98 printShowingDecimalPlaces: 22.567 printShowingDecimalPlaces: 2-2.567 printShowingDecimalPlaces: 20 printShowingDecimalPlaces: 2Number categoryForSelector: #printShowingDecimalPlaces:"! !!ParagraphEditor class methodsFor: 'class initialization' stamp: 'edc 7/1/2008 07:27'!yellowButtonExpertMenu	^ MenuMorph fromArray: {			{'set font... (k)' translated.				#offerFontMenu}.			{'set style... (K)' translated.				#changeStyle}.			{'set alignment... (u)' translated.		#chooseAlignment}.			#-.			{'make project link (P)' translated.	#makeProjectLink}.			#-.			{'find...(f)' translated.					#find}.			{'find again (g)' translated.				#findAgain}.			{'set search string (h)' translated.		#setSearchString}.			#-.			{'do again (j)' translated.				#again}.			{'undo (z)' translated.					#undo}.			#-.			{'copy (c)' translated.					#copySelection}.			{'cut (x)' translated.						#cut}.			{'paste (v)' translated.					#paste}.			{'paste...' translated.					#pasteRecent}.			#-.			{'do it (d)' translated.					#doIt}.			{'print it (p)' translated.				#printIt}.			{'inspect it (i)' translated.				#inspectIt}.			{'explore it (I)' translated.				#exploreIt}.			{'debug it' translated.					#debugIt}.			{'tally it' translated.			#tallyIt}.			#-.			{'accept (s)' translated.					#accept}.			{'cancel (l)' translated.					#cancel}.			#-.			{'show bytecodes' translated.			#showBytecodes}.			#-.			{'copy html' translated.					#copyHtml}.			#-.			{'more...' translated.					#shiftedTextPaneMenuRequest}.		}.! !!ParagraphEditor class methodsFor: 'instance creation' stamp: 'nk 9/3/2004 14:10'!new	"Answer a new instance of me with a null Paragraph to be edited."	| aParagraphEditor |	aParagraphEditor _ super new.	aParagraphEditor changeParagraph: '' asParagraph.	^aParagraphEditor! !!ParagraphEditor class methodsFor: 'instance creation'!newParagraph: aParagraph 	"Answer an instance of me with aParagraph as the text to be edited."	| aParagraphEditor |	aParagraphEditor _ super new.	aParagraphEditor initialize.	aParagraphEditor changeParagraph: aParagraph.	^aParagraphEditor! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'dgd 4/4/2006 16:04'!initializeCmdKeyShortcuts	"Initialize the (unshifted) command-key (or alt-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"ParagraphEditor initialize"	| cmdMap |	cmdMap := Array new: 256 withAll: #noop:.	"use temp in case of a crash"	cmdMap at: 1 + 1 put: #cursorHome:.			"home key"	cmdMap at: 4 + 1 put: #cursorEnd:.				"end key"	cmdMap at: 8 + 1 put: #backspace:.				"ctrl-H or delete key"	cmdMap at: 11 + 1 put: #cursorPageUp:.		"page up key"	cmdMap at: 12 + 1 put: #cursorPageDown:.	"page down key"	cmdMap at: 13 + 1 put: #crWithIndent:.			"cmd-Return"	cmdMap at: 27 + 1 put: #offerMenuFromEsc:.	"escape key"	cmdMap at: 28 + 1 put: #cursorLeft:.			"left arrow key"	cmdMap at: 29 + 1 put: #cursorRight:.			"right arrow key"	cmdMap at: 30 + 1 put: #cursorUp:.				"up arrow key"	cmdMap at: 31 + 1 put: #cursorDown:.			"down arrow key"	cmdMap at: 32 + 1 put: #selectWord:.			"space bar key"	cmdMap at: 127 + 1 put: #forwardDelete:.		"del key"	'0123456789-=' 		do: [:char | cmdMap at: char asciiValue + 1 put: #changeEmphasis:].	'([{''"<' do: [:char | cmdMap at: char asciiValue + 1 put: #enclose:].	cmdMap at: $, asciiValue + 1 put: #shiftEnclose:.	"triplet = {character. comment selector. novice appropiated}"	#(		($a		#selectAll:				true)		($b		#browseIt:				false)		($c		#copySelection:			true)		($d		#doIt:						false)		($e		#exchange:				true)		($f		#find:						true)		($g		#findAgain:				true)		($h		#setSearchString:		true)		($i		#inspectIt:				false)		($j		#doAgainOnce:			true)		($k		#offerFontMenu:		true)		($l		#cancel:					true)		($m	#implementorsOfIt:		false)		($n		#sendersOfIt:			false)		($o		#spawnIt:				false)		($p		#printIt:					false)		($q		#querySymbol:			false)		($s		#save:					true)		($t		#tempCommand:		false)		($u		#align:					true)		($v		#paste:					true)		($w	#backWord:				true)		($x		#cut:						true)		($y		#swapChars:				true)		($z		#undo:					true)	)		select:[:triplet | Preferences noviceMode not or:[triplet third]]		thenDo:[:triplet | cmdMap at: triplet first asciiValue + 1 put: triplet second].	CmdActions := cmdMap.! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'dgd 10/4/2004 13:53'!initializeShiftCmdKeyShortcuts 	"Initialize the shift-command-key (or control-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 	capitalized versions of the letters.	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."	| cmdMap |	"shift-command and control shortcuts"	cmdMap _ Array new: 256 withAll: #noop:.  "use temp in case of a crash"	cmdMap at: ( 1 + 1) put: #cursorHome:.				"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.				"end key"	cmdMap at: ( 8 + 1) put: #forwardDelete:.			"ctrl-H or delete key"	cmdMap at: (11 + 1) put: #cursorPageUp:.			"page up key"	cmdMap at: (12 + 1) put: #cursorPageDown:.		"page down key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"	cmdMap at: (27 + 1) put: #offerMenuFromEsc:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.				"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.				"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (32 + 1) put: #selectWord:.				"space bar key"	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."	"NB: sw 12/9/2001 commented out the idiosyncratic line just below, which was grabbing shift-esc in the text editor and hence which argued with the wish to have shift-esc be a universal gesture for escaping the local context and calling up the desktop menu."  	"cmdMap at: (27 + 1) put: #shiftEnclose:." 	"ctrl-["	"'""''(' do: [ :char | cmdMap at: (char asciiValue + 1) put: #enclose:]."	"triplet = {character. comment selector. novice appropiated}"	#(		($a		argAdvance:						false)		($b		browseItHere:					false)		($c		compareToClipboard:			false)		($d		duplicate:							true)		($e		methodStringsContainingIt:	false)		($f		displayIfFalse:					false)		($g		fileItIn:							false)		($h		cursorTopHome:					true)		($i		exploreIt:							false)		($j		doAgainMany:					true)		($k		changeStyle:						true)		($l		outdent:							true)		($m	selectCurrentTypeIn:			true)		($n		referencesToIt:					false)		($p		makeProjectLink:				true)		($r		indent:							true)		($s		search:							true)		($t		displayIfTrue:					false)		($u		changeLfToCr:					false)		($v		pasteInitials:						false)		($w	methodNamesContainingIt:	false)		($x		makeLowercase:					true)		($y		makeUppercase:					true)		($z		makeCapitalized:				true)	)		select:[:triplet | Preferences noviceMode not or:[triplet third]]		thenDo:[:triplet |			cmdMap at: (triplet first asciiValue         + 1) put: triplet second.		"plain keys"			cmdMap at: (triplet first asciiValue - 32 + 1) put: triplet second.		"shifted keys"			cmdMap at: (triplet first asciiValue - 96 + 1) put: triplet second.		"ctrl keys"		].	ShiftCmdActions _ cmdMap! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'sps 7/24/2003 17:25'!multiRedoOverride"Call this to set meta-r to perform the multilevel redo (or tweak the code below to have it bound to some other key sequence).""ParagraphEditor multiRedoOverride."	CmdActions at: $r asciiValue + 1 put: #multiRedo: ! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'sbw 10/8/1999 21:42'!specialShiftCmdKeys"Private - return array of key codes that represent single keys actingas if shift-command were also being pressed"^#(	1	"home"	3	"enter"	4	"end"	8	"backspace"	11	"page up"	12	"page down"	27	"escape"	28	"left arrow"	29	"right arrow"	30	"up arrow"	31	"down arrow"	127	"delete"	)! !!ReleaseBuilderFor3dot11 methodsFor: 'cleaning' stamp: 'edc 7/1/2008 07:06'!cleanupPhasePrepareself cleanUnwantedCs.                "SMSqueakMap default clearCaches."" Commented out for no Undeclared on image "#(zapMVCprojects zapAllOtherProjects discardFlash discardFFIcomputeImageSegmentation discardSpeech ) do:[:ea| SystemDictionary removeSelector:ea].#( reserveUrl: saveAsResource saveDocPane saveOnURL saveOnURL:saveOnURLbasic isTurtleRow objectViewed inATwoWayScrollPane) do:[:ea| Morph removeSelector: ea].#(playfieldOptionsMenu presentPlayfieldMenu allScriptEditorsattemptCleanupReporting: modernizeBJProjectscriptorForTextualScript:ofPlayer:) do:[:ea| PasteUpMorph removeSelector:   ea].#(isUniversalTiles noteDeletionOf:fromWorld: scriptorsForSelector:inWorld: tilesToCall: handMeTilesToFire) do:[:ea| Player removeSelector:   ea].Player class removeCategory: 'turtles'.Player removeCategory: 'slots-user'.Morph removeCategory: 'scripting'.ColorType removeCategory: 'tiles'.TheWorldMainDockingBar removeSelector: #hideAllViewersIn: .#(test1 test2) do:[:ea|WorldWindow class removeSelector:   ea].SystemOrganization removeCategoriesMatching: 'UserObjects'.FileList2 class organization classify: #morphicViewOnDirectory: under: 'morphic ui'.FileList2 class organization classify: #morphicView under: 'morphic ui'.SystemOrganization classifyAll: #(AbstractMediaEventMorph ColorSwatch) under: 'MorphicExtras-AdditionalSupport'.! !ParagraphEditor class removeSelector: #initializeTextEditorMenus!Object class removeSelector: #registeredServices!