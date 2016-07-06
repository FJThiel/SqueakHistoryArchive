"      NAME	Flippy
       AUTHOR	cgreuter@calum.csclub.uwaterloo.ca (Chris Reuter)
       URL	(none)
       FUNCTION	silly game
       KEYWORDS	game Morphic demo
       ST-VERSIONS	Squeak
       PREREQUISITES	CurveButton 1.0 (or better?)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.0
       DATE	16-Apr-98

SUMMARY

This is a simple logic puzzle that uses the newMorphic UI in Squeak.  It's a great time-killerand looks pretty cool.

				Chris Reuter
"!
'From Squeak 1.31 of Feb 4, 1998 on 16 April 1998 at 12:48:00 pm'!
"Change Set:		Flippy
Date:			16 April 1998
Author:			Chris Reuter

This is a little logic puzzle written to use Morphic.  The object of the game is to
make the central square red and all the rest green.  Clicking on a square toggles it
and some others.

I've been told that it's provably possible to solve any Flippy configuration in seven
moves or less.
"!

SimpleButtonMorph subclass: #FlippyButtonMorph
	instanceVariableNames: 'isRed '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
Object subclass: #FlippyGame
	instanceVariableNames: 'state '
	classVariableNames: 'FinalState Masks '
	poolDictionaries: ''
	category: 'Flippy'!
CurveButton subclass: #FlippyHelpButton
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
CurveMorph subclass: #FlippyMorph
	instanceVariableNames: 'buttons countMorph counter gameData titleMorph titleColor flashCount '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Demo'!
CurveButton subclass: #FlippyQuitButton
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
CurveButton subclass: #FlippyScrambleButton
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
Object subclass: #FlippyState
	instanceVariableNames: 'top middle bottom '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Flippy'!

!FlippyButtonMorph commentStamp: 'cr 4/16/98 12:48' prior: 0!
One of the red or green square grid buttons.!

!FlippyButtonMorph reorganize!
('non-labels' label label: label:font:)
('colouring' colourByBit: green red toggle)
('initialization' initialize)
('accessing' isRed)
!


!FlippyButtonMorph methodsFor: 'non-labels' stamp: 'cr 3/22/98 04:27'!
label
	"Labels are not allowed, so this is a no-op."
	^''.
	! !

!FlippyButtonMorph methodsFor: 'non-labels' stamp: 'cr 3/22/98 04:25'!
label: aString
	"Labels are not allowed, so this is a no-op."
	^self
	! !

!FlippyButtonMorph methodsFor: 'non-labels' stamp: 'cr 3/22/98 04:27'!
label: aString font: aFont
	"Labels are not allowed, so this is a no-op."
	^self
	! !

!FlippyButtonMorph methodsFor: 'colouring' stamp: 'cr 3/22/98 07:09'!
colourByBit: colourRed
	colourRed
		ifTrue: [isRed or: [self red]]
		ifFalse: [self green].! !

!FlippyButtonMorph methodsFor: 'colouring' stamp: 'cr 3/22/98 07:00'!
green
	"color green."
	self color: (Color r: 0.4 g: 0.8 b: 0.6).
	isRed := false.! !

!FlippyButtonMorph methodsFor: 'colouring' stamp: 'cr 3/22/98 07:00'!
red
	"color red."
	self color: (Color r: 1.0 g: 0.199 b: 0.199).
	isRed := true.! !

!FlippyButtonMorph methodsFor: 'colouring' stamp: 'cr 3/22/98 07:01'!
toggle
	self isRed
		ifTrue: [self green]
		ifFalse: [self red].	! !

!FlippyButtonMorph methodsFor: 'initialization' stamp: 'cr 3/25/98 02:31'!
initialize

	super initialize.
	self borderWidth: 3.
	self borderColor: #raised.
	self green.
	target _ nil.
	actionSelector _ nil.
	arguments _ nil.
	actWhen _ #buttonUp.
	self extent: 34@34.
! !

!FlippyButtonMorph methodsFor: 'accessing' stamp: 'cr 3/22/98 07:00'!
isRed
	"Return value of instance variable 'isRed'"
	^isRed! !


!FlippyButtonMorph class reorganize!
('all' includeInNewMorphMenu)
!


!FlippyButtonMorph class methodsFor: 'all' stamp: 'cr 4/1/98 02:16'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"
	^ false! !


!FlippyGame reorganize!
('playing' clickedOn: colourAt: isDone reset)
('initialization' initialize)
('accessing' state)
!


!FlippyGame methodsFor: 'playing' stamp: 'cr 3/22/98 06:50'!
clickedOn: location
	"A square has been clicked.  Change the state. ('location' is an integer representing a square
	 arranged just like a phone keypad (i.e. 1 is top-left, 2 is top-middle, etc.))"
	state := state flipBy: (Masks at: location).
! !

!FlippyGame methodsFor: 'playing' stamp: 'cr 3/22/98 06:50'!
colourAt: location
	"Return the colour (1 or 0, that is) of the square indicated by location, an int, in
	 touch-tone phone layout."
	| bits |
	bits := (state bottom bitOr: (state middle << 3)) bitOr: state top << 6.
	^bits anyMask: (1 << (9 - location)).! !

!FlippyGame methodsFor: 'playing' stamp: 'cr 3/25/98 00:43'!
isDone
	"Do we have a finished state?"
	^self state = FinalState.! !

!FlippyGame methodsFor: 'playing' stamp: 'cr 1/30/98 19:44'!
reset
	"Initialize state."
	state := FlippyState from: ((1 to: 3) collect: [ :dummy | (0 to: 7) atRandom]). ! !

!FlippyGame methodsFor: 'initialization' stamp: 'cr 3/22/98 05:07'!
initialize
	state := FlippyState new.
	self reset.! !

!FlippyGame methodsFor: 'accessing' stamp: 'cr 3/25/98 00:43'!
state
	"Return value of instance variable 'state'"
	^state! !


!FlippyGame class methodsFor: 'instance creation' stamp: 'cr 3/22/98 05:06'!
new
	^super new initialize.! !

!FlippyGame class methodsFor: 'class initialization' stamp: 'cr 4/1/98 02:31'!
initialize
	"Initialize the class variables."
	| | 

	FinalState := FlippyState from: #( 2r000
							           2r010
                                            2r000 ).
			
	Masks := #(  
				#( 	2r110
					2r110
					2r000 )

				#(	2r111
					2r000
					2r000 )

				#(	2r011
					2r011
					2r000 )

				#(	2r100
					2r100
					2r100 )

				#(	2r010
					2r111
					2r010 )

				#(	2r001
					2r001
					2r001 )

				#(	2r000
					2r110
					2r110 )

				#(	2r000
					2r000
					2r111 )

				#(	2r000
					2r011
					2r011 )) collect: [ :item | FlippyState from: item]. 
		

			


	"FlippyGame initializeClass"! !


!FlippyHelpButton commentStamp: 'cr 4/16/98 12:48' prior: 0!
The help button.!

!FlippyHelpButton reorganize!
('initialization' defaultVertices initialize)
!


!FlippyHelpButton methodsFor: 'initialization' stamp: 'cr 3/22/98 05:24'!
defaultVertices
	"Return a collection of vertices to initialize self with.  The array was created by cutting from
	 the vertices list a Morph inspector produced."
	| result |
	result := WriteStream on: Array new.
	(#(310@54 323@41 338@51 341@79 341@167 338@179 331@189 315@180 307@96 )
			reject: [:item | item == #@]
	  ) pairsDo: [:x :y | result nextPut: x @ y].

	^ result contents.! !

!FlippyHelpButton methodsFor: 'initialization' stamp: 'cr 3/22/98 05:26'!
initialize
	super initialize.
	self color: (Color r: 0.599 g: 0.8 b: 1.0).
	self label: 'Help'.! !


!FlippyHelpButton class reorganize!
('all' includeInNewMorphMenu)
!


!FlippyHelpButton class methodsFor: 'all' stamp: 'cr 4/1/98 02:16'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"
	^ false! !

Smalltalk renameClassNamed: #FlippyMain as: #FlippyMorph!

!FlippyMorph reorganize!
('stepping' startStepping step stepTime)
('game play' helpScreen pressed: resetGame winner)
('display' updateDisplay)
('creation' initialize makeButtons)
('accessing' buttons counter countMorph gameData)
('handles' installModelIn:)
!


!FlippyMorph methodsFor: 'stepping' stamp: 'cr 4/1/98 02:04'!
startStepping
	"Stepping should only start when a game ends."
	flashCount := 0.
	self gameData isDone
		ifTrue: [super startStepping].! !

!FlippyMorph methodsFor: 'stepping' stamp: 'cr 4/1/98 02:08'!
step
	"Toggle the buttons."

	flashCount := flashCount + 1.
	flashCount > 10 ifTrue: [^self stopStepping].

	buttons do: [ :button | button toggle].
	titleMorph color: 
			(Color	r: (0.0 to: 1.0 by: 0.0001) atRandom
					g: (0.0 to: 1.0 by: 0.0001) atRandom
					b: (0.0 to: 1.0 by: 0.0001) atRandom)

! !

!FlippyMorph methodsFor: 'stepping' stamp: 'cr 3/25/98 01:07'!
stepTime
	^300! !

!FlippyMorph methodsFor: 'game play' stamp: 'cr 4/1/98 02:39'!
helpScreen
	"Bring up a help screen."
	| b |

	b := SimpleButtonMorph new
			label: 'To win, make the innermost button red and the rest green.';
			yourself.

	b target: b;
		actionSelector: #delete;
		bounds: (self bounds withWidth: (b findA: StringMorph) bounds width * 1.2).
	
	self addMorph: b.

! !

!FlippyMorph methodsFor: 'game play' stamp: 'cr 3/25/98 01:39'!
pressed: anInt
	"Button pressed."

	gameData isDone
		ifTrue: [^self].

	gameData clickedOn: anInt.
	counter := counter + 1.
	self updateDisplay.

	gameData isDone
		ifTrue: [
			self winner.
		].
	! !

!FlippyMorph methodsFor: 'game play' stamp: 'cr 3/25/98 01:43'!
resetGame
	"New game."
	gameData reset.
	counter := 0.
	self stopStepping.
	titleMorph 	contents: 'Flippy!!';
				color: titleColor.
	self updateDisplay.
! !

!FlippyMorph methodsFor: 'game play' stamp: 'cr 3/25/98 01:43'!
winner
	"The player has won."
	titleMorph contents: 'Solved!!!!!!'.
	self startStepping.! !

!FlippyMorph methodsFor: 'display' stamp: 'cr 3/22/98 06:20'!
updateDisplay
	"Make the display synch with the game."
	countMorph contents: counter printString.
	buttons withIndexDo: [ :button :index | button colourByBit: (gameData colourAt: index)]. 
		
	! !

!FlippyMorph methodsFor: 'creation' stamp: 'cr 4/1/98 02:11'!
initialize
	"Initialize to tweaked defaults."
	| pts |
	
	counter := 0.

	"Create the game object."
	gameData := FlippyGame new.

	pts := OrderedCollection new.
	pts add: 239@63;
			add: (365@76);
			add: (414@69);
			add:	 (436@239);
			add: (324@227);
			add: (234@233);
			add: (219@108).
		
	self vertices: pts asArray
		color: (Color r: 0.8 g: 0.599 b: 1.0)
		borderWidth: 2
		borderColor: #raised.

	self makeButtons.

	self addMorph: (FlippyScrambleButton new
						position: self bounds topLeft + (147@162);
						actionSelector: #resetGame;
						target: self;
						yourself).

	self addMorph: (FlippyHelpButton new
						position: self bounds topLeft + (5@22);
						actionSelector: #helpScreen;
						target: self;
						yourself).

	countMorph := StringMorph new.
	countMorph contents: '0';
				position: self bounds topLeft + (180@76);
				color: (Color r: 0 g: 0.4 b: 0.4).
	self addMorph: countMorph.

	titleColor := Color r: 1.0 g: 1.0 b: 0.4.
	titleMorph := TextMorph new
						contents: 'Flippy!!!!!!';
						position: self bounds topLeft + (87@34);
						color: titleColor;
						lock;
						yourself.
	self addMorph: titleMorph.

	self addMorph: (FlippyQuitButton new
						position: self bounds topLeft + (153@11);
						actionSelector: #delete;
						target: self;
						yourself).
	self stopStepping.
	self updateDisplay.
		! !

!FlippyMorph methodsFor: 'creation' stamp: 'cr 3/22/98 06:41'!
makeButtons
	"Create the 9 flippy buttons."
	| b bList n |
	bList := OrderedCollection new.
	
	n := 1.
	0 to: 2 do: [ :ypos |
		0 to: 2 do: [ :xpos |
			b := FlippyButtonMorph new.
			b position: self bounds topLeft + (46@54) + ((xpos @ ypos) * (b extent + 2));
				target: self;
				actionSelector: #pressed:;
				arguments: (Array with: n).
			self addMorph: b.
			bList add: b.
			n := n + 1.
		].
	].

	buttons := bList asArray.! !

!FlippyMorph methodsFor: 'accessing' stamp: 'cr 3/25/98 01:11'!
buttons
	"Return value of instance variable 'buttons'"
	^buttons! !

!FlippyMorph methodsFor: 'accessing' stamp: 'cr 3/25/98 01:11'!
counter
	"Return value of instance variable 'counter'"
	^counter! !

!FlippyMorph methodsFor: 'accessing' stamp: 'cr 3/25/98 01:11'!
countMorph
	"Return value of instance variable 'countMorph'"
	^countMorph! !

!FlippyMorph methodsFor: 'accessing' stamp: 'cr 3/25/98 01:11'!
gameData
	"Return value of instance variable 'gameData'"
	^gameData! !

!FlippyMorph methodsFor: 'handles' stamp: 'cr 3/25/98 02:28'!
installModelIn: mode
	"Override initially displaying those #$%@ handles."! !


!FlippyQuitButton commentStamp: 'cr 4/16/98 12:48' prior: 0!
The quit button.!

!FlippyQuitButton reorganize!
('creation' defaultVertices initialize)
!


!FlippyQuitButton methodsFor: 'creation' stamp: 'cr 3/25/98 01:58'!
defaultVertices
	"Return a collection of vertices to initialize self with.  The array was created by cutting from
	 the vertices list a Morph inspector produced."
	| result |
	result := WriteStream on: Array new.
	(#(246@173 270@165 281@199 262@198 238@196)
			reject: [:item | item == #@]
	  ) pairsDo: [:x :y | result nextPut: x @ y].

	^ result contents.! !

!FlippyQuitButton methodsFor: 'creation' stamp: 'cr 3/25/98 02:13'!
initialize
	super initialize.
	self color: (Color r: 1.0 g: 0.599 b: 1.0).
	self label: 'Quit'.! !


!FlippyQuitButton class reorganize!
('all' includeInNewMorphMenu)
!


!FlippyQuitButton class methodsFor: 'all' stamp: 'cr 4/1/98 02:16'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"
	^ false! !

Smalltalk renameClassNamed: #Flippy as: #FlippyScrambleButton!

!FlippyScrambleButton commentStamp: 'cr 4/16/98 12:48' prior: 0!
The Scramble button!

!FlippyScrambleButton reorganize!
('initialization' defaultVertices initialize)
!


!FlippyScrambleButton methodsFor: 'initialization' stamp: 'cr 3/22/98 04:32'!
defaultVertices
	"Return a collection of vertices to initialize self with.  The array was created by cutting from
	 the vertices list a Morph inspector produced."
	| result |
	result := WriteStream on: Array new.
	(#(477@182 503@182 520@185 512@206 488@209 454@199 449@195 448@189 455@183 )
			reject: [:item | item == #@]
	  ) pairsDo: [:x :y | result nextPut: x @ y].

	^ result contents.! !

!FlippyScrambleButton methodsFor: 'initialization' stamp: 'cr 3/22/98 05:18'!
initialize
	super initialize.
	self color: (Color r: 0.4 g: 1.0 b: 1.0).
	self label: 'Scramble'.! !


!FlippyScrambleButton commentStamp: 'cr 4/16/98 12:48' prior: 0!
The Scramble button!

!FlippyScrambleButton class reorganize!
('all' includeInNewMorphMenu)
!


!FlippyScrambleButton class methodsFor: 'all' stamp: 'cr 4/1/98 02:16'!
includeInNewMorphMenu
	"Return true for all classes that can be instantiated from the menu"
	^ false! !


!FlippyState methodsFor: 'operations' stamp: 'cr 1/30/98 19:20'!
flipBy: mask
	"Toggle all bits in self which are one in 'mask'."
	^FlippyState
		top: (mask top bitXor: self top)
		middle: (mask middle bitXor: self middle)
		bottom: (mask bottom bitXor: self bottom).
! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
bottom
	"Return value of instance variable 'bottom'"
	^bottom! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
bottom: newValue
	"Set value of instance variable 'bottom'."
	bottom := newValue! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
middle
	"Return value of instance variable 'middle'"
	^middle! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
middle: newValue
	"Set value of instance variable 'middle'."
	middle := newValue! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
top
	"Return value of instance variable 'top'"
	^top! !

!FlippyState methodsFor: 'accessing' stamp: 'cr 1/30/98 19:03'!
top: newValue
	"Set value of instance variable 'top'."
	top := newValue! !

!FlippyState methodsFor: 'initialization' stamp: 'cr 3/22/98 04:42'!
initialize
	"Create a blank state."
	top := 0.
	middle := 0.
	bottom := 0.! !

!FlippyState methodsFor: 'testing' stamp: 'cr 1/30/98 19:30'!
= aFlippyState
	"Basic = operation."
	^ (self top = aFlippyState top) & (self middle = aFlippyState middle) & (self bottom = aFlippyState bottom).! !


!FlippyState class reorganize!
('instance creation' from: new top:middle:bottom:)
!


!FlippyState class methodsFor: 'instance creation' stamp: 'cr 1/30/98 19:26'!
from: anArray
	"Create a new FlippyState using array contents."
	^FlippyState 
		top: (anArray at: 1)
		middle: (anArray at: 2)
		bottom: (anArray at: 3).! !

!FlippyState class methodsFor: 'instance creation' stamp: 'cr 3/22/98 04:43'!
new
	^super new initialize.! !

!FlippyState class methodsFor: 'instance creation' stamp: 'cr 1/30/98 19:04'!
top: t middle: m bottom: b
	"Create a new instance."
	^self new top: t; middle: m; bottom: b; yourself.
! !


FlippyGame initialize!
Smalltalk removeClassNamed: #FinMorph!
Smalltalk removeClassNamed: #FooBarMorph!
Smalltalk removeClassNamed: #FlippyGridButton!
FlippyGame initialize.!


