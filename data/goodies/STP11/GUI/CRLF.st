"CRLF.st
This makes CR and LR display the same (real useful on UNIX machines).

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 19, 1996"!


!Text class methodsFor: 'class initialization'!
initTextConstants 
	"Initialize constants shared by classes associated with text display, e.g., 
	Space, Tab, Cr, Bs, ESC."
		"1/24/96 sw: in exasperation and confusion, changed cmd-g mapping from 231 to 232 to see if I could gain any relief?!!"


	| letter varAndValue tempArray width |
	"CtrlA..CtrlZ, Ctrla..Ctrlz"
	letter _ $A.
 	#(		212 230 228 196 194 226 241 243 214 229 200 217 246 
			245 216 202 210 239 211 240 197 198 209 215 242 231
	 		1 166 228 132 130 12 232 179 150 165 136 153 182 
			14 15 138 17 18 19 11 21 134 145 151 178 167 ) do:
		[:kbd |
		TextConstants at: ('Ctrl', letter asSymbol) asSymbol put: kbd asCharacter.
		letter _ letter == $Z ifTrue: [$a] ifFalse: [(letter asciiValue + 1) asCharacter]].

	varAndValue _ #(
		Space	32
		Tab		9
		CR		13
		LF		10
		Enter	3
		BS		8
		BS2		158
		ESC		160
		Clear 	173
	).

	varAndValue size odd ifTrue: [self notify: 'unpaired text constant'].
	(2 to: varAndValue size by: 2) do:
		[:i | TextConstants at: (varAndValue at: i - 1) put: (varAndValue at: i) asCharacter].

	varAndValue _ #(
		CtrlDigits 			(159 144 143 128 127 129 131 180 149 135)
		CtrlOpenBrackets	(201 7 218 249 219 15)
			"lparen gottn by ctrl-_ = 201; should be 213 but can't type that on Mac"

			"location of non-character stop conditions"
		EndOfRun	257
		CrossedX	258

			"values for alignment"
		LeftFlush	0
		RightFlush	1
		Centered	2
		Justified	3

			"subscripts for a marginTabsArray tuple"
		LeftMarginTab	1
		RightMarginTab	2

			"font faces"
		Basal	0
		Bold	1
		Italic	2

			"in case font doesn't have a width for space character"
			"some plausible numbers-- are they the right ones?"
		DefaultSpace			4
		DefaultTab				24
		DefaultLineGrid			16
		DefaultBaseline			12
		DefaultFontFamilySize	3	"basal, bold, italic"
	).

	varAndValue size odd ifTrue: [self notify: 'unpaired text constant'].
	(2 to: varAndValue size by: 2) do:
		[:i | TextConstants at: (varAndValue at: i - 1) put: (varAndValue at: i)].

	TextConstants at: #DefaultRule	put: Form over.
	TextConstants at: #DefaultMask	put: Form black.

	width _ Display width max: 720.
	tempArray _ Array new: width // DefaultTab.
	1 to: tempArray size do:
		[:i | tempArray at: i put: DefaultTab * i].
	TextConstants at: #DefaultTabsArray put: tempArray.
	tempArray _ Array new: (width // DefaultTab) // 2.
	1 to: tempArray size do:
		[:i | tempArray at: i put: (Array with: (DefaultTab*i) with: (DefaultTab*i))].
	TextConstants at: #DefaultMarginTabsArray put: tempArray.

"Text initTextConstants "! !

Text initTextConstants!

!CharacterScanner methodsFor: 'private'!
setFont
	"Set the font and other emphasis."
	self setFont: 1.
	emphasisCode _ 0.
	(text attributesAt: lastIndex) do: 
		[:att | att emphasizeScanner: self].
	font _ font emphasized: emphasisCode.

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Space. 
	sourceForm _ font glyphs.  "Should only be needed in DisplayScanner"
	height _ font height.			" ditto "
	xTable _ font xTable.
	stopConditions _ font stopConditions.
	stopConditions at: Space asciiValue + 1 put: #space.
	stopConditions at: Tab asciiValue + 1 put: #tab.
	stopConditions at: CR asciiValue + 1 put: #cr.
	stopConditions at: LF asciiValue + 1 put: #cr.
	stopConditions at: EndOfRun put: #endOfRun.
	stopConditions at: CrossedX put: #crossedX! !
