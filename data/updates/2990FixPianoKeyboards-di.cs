'From Squeak2.9alpha of 12 June 2000 [latest update: #3039] on 22 November 2000 at 1:43:59 pm'!"Change Set:		FixPianoKeyboardsDate:			18 November 2000Author:			Dan IngallsAdapts the keyboard to the new event dispatching logic.It was necessary to pass mouse focus from note to note."!!PianoKeyboardMorph methodsFor: 'simple keyboard' stamp: 'di 11/22/2000 13:39'!buildKeyboard	| wtWid bkWid keyRect octavePt nWhite nBlack |	self removeAllMorphs.	wtWid _ 8. bkWid _ 5.	self extent: 10@10.	1 to: nOctaves+1 do:		[:i | i <= nOctaves ifTrue: [nWhite _ 7.  nBlack _ 5]						ifFalse: [nWhite _ 1.  nBlack _ 0 "High C"].		octavePt _ self innerBounds topLeft + ((7*wtWid*(i-1)-1)@-1).		1 to: nWhite do:			[:j | keyRect _ octavePt + (j-1*wtWid@0) extent: (wtWid+1)@36.			self addMorph: ((RectangleMorph newBounds: keyRect color: whiteKeyColor)								borderWidth: 1;				on: #mouseDown send: #mouseDownEvent:noteMorph:pitch: to: self								withValue: i-1*12 + (#(1 3 5 6 8 10 12) at: j))].		1 to: nBlack do:			[:j | keyRect _ octavePt + ((#(6 15 29 38 47) at: j)@1) extent: bkWid@21.			self addMorph: ((Morph newBounds: keyRect color: blackKeyColor)				on: #mouseDown send: #mouseDownEvent:noteMorph:pitch: to: self								withValue: i-1*12 + (#(2 4 7 9 11) at: j))]].	self submorphsDo:		[:m | m on: #mouseMove send: #mouseMoveEvent:noteMorph:pitch: to: self;				on: #mouseUp send: #mouseUpEvent:noteMorph:pitch: to: self;				on: #mouseEnterDragging send: #mouseDownEvent:noteMorph:pitch: to: self;				on: #mouseLeaveDragging send: #mouseUpEvent:noteMorph:pitch: to: self].	self extent: (self fullBounds extent + borderWidth - 1)! !!PianoKeyboardMorph methodsFor: 'simple keyboard' stamp: 'di 11/22/2000 13:27'!mouseDownEvent: event noteMorph: noteMorph pitch: midiKey	| pitch |	event hand hasSubmorphs ifTrue: [^ self  "no response if drag something over me"].	event hand mouseFocus ifNil:		["If dragged into me, then establish focus so I'll see moves"		event hand newMouseFocus: noteMorph event: event].	noteMorph color: playingKeyColor.	pitch _ AbstractSound pitchForMIDIKey: midiKey + 23.	soundPlaying ifNotNil: [soundPlaying stopGracefully].	soundPlaying _ soundPrototype soundForPitch: pitch dur: 100.0 loudness: 0.3.	SoundPlayer resumePlaying: soundPlaying quickStart: true.! !!PianoKeyboardMorph methodsFor: 'simple keyboard' stamp: 'di 11/22/2000 13:28'!mouseMoveEvent: event noteMorph: noteMorph pitch: pitch	(noteMorph containsPoint: event cursorPoint) ifFalse:		["If drag out of me, zap focus so other morphs can see drag in."		event hand releaseMouseFocus: noteMorph]! !!PianoKeyboardMorph methodsFor: 'simple keyboard' stamp: 'di 11/22/2000 13:26'!mouseUpEvent: event noteMorph: noteMorph pitch: pitch	noteMorph color: ((#(0 1 3 5 6 8 10) includes: pitch\\12)					ifTrue: [whiteKeyColor]					ifFalse: [blackKeyColor]).	soundPlaying ifNotNil: [soundPlaying stopGracefully].! !"Postscript:Recompute all existing keyboards."PianoKeyboardMorph allSubInstancesDo: [:m | m buildKeyboard].!