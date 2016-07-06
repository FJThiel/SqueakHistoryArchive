'From Squeak 2.4c of May 10, 1999 on 22 June 1999 at 8:47:18 am'!!MIDIScore methodsFor: 'editing' stamp: 'di 6/21/1999 10:56'!insertEvents: events at: selection	| track selStartTime delta |	track _ tracks at: selection first.	selection second = 0		ifTrue: [selStartTime _ 0.				selection at: 2 put: 1]		ifFalse: [selStartTime _ (track at: selection second) time].	track _ track copyReplaceFrom: selection second to: selection second - 1				with: (events collect: [:e | e copy]).	track size >=  (selection second + events size) ifTrue:		["Adjust times of following events"		delta _ selStartTime - (track at: selection second) time.		selection second to: selection second + events size - 1 do:			[:i | (track at: i) adjustTimeBy: delta].		delta _ (self gridToNextQuarterNote: (track at: selection second + events size - 1) endTime)					- (track at: selection second + events size) time.		selection second + events size to: track size do:			[:i | (track at: i) adjustTimeBy: delta].		].	tracks at: selection first put: track! !!PianoRollScoreMorph methodsFor: 'editing' stamp: 'di 6/21/1999 10:49'!insertSelection	self selection == nil ifTrue: [^ self].	score insertEvents: NotePasteBuffer at: self selection.	scorePlayer updateDuration.	self rebuildFromScore ! !!WaveEditor methodsFor: 'menu' stamp: 'di 6/22/1999 08:46'!chooseLoopStart 	| bestLoops menu secs choice start |	possibleLoopStarts ifNil: [		Utilities			informUser: 'Finding possible loop points...'			during: [possibleLoopStarts _ self findPossibleLoopStartsFrom: graph cursor]].	bestLoops _ possibleLoopStarts copyFrom: 1 to: (100 min: possibleLoopStarts size).	menu _ CustomMenu new.	bestLoops do: [:entry |		secs _ ((loopEnd - entry first) asFloat / self samplingRate) roundTo: 0.01.		menu add: entry third printString, ' cycles; ', secs printString, ' secs' action: entry].	choice _ menu startUp.	choice ifNil: [^ self].	loopCycles _ choice at: 3.	start _ self fractionalLoopStartAt: choice first.	self loopLength: (loopEnd asFloat - start) + 1.0.! !