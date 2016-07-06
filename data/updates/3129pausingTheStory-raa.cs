'From Squeak2.9alpha of 17 July 2000 [latest update: #3186] on 18 December 2000 at 3:44:53 pm'!"Change Set:		pausingTheStoryDate:			18 December 2000Author:			Bob Arning- implement pause and resume for camera marks/thumbnails- allow camera marks to be picked up with normal click"!!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:38'!encounteredAtTime: ticks inScorePlayer: scorePlayer atIndex: index inEventTrack: track secsPerTick: secsPerTick	| nextAmbient m nextDurationInMs program now finalMark thisPage nextPage |	self gotoMark.	nextAmbient _ nil.	index to: track size do: [ :i |		(nextAmbient isNil and: [((m _ track at: i) morph) isKindOf: self class]) ifTrue: [			nextAmbient _ m.		].	].	nextAmbient ifNil: [^self].	nextDurationInMs _ (nextAmbient time - ticks * secsPerTick * 1000) rounded.	finalMark _ nextAmbient morph.	thisPage _ self valueOfProperty: #bookPage.	nextPage _ finalMark valueOfProperty: #bookPage.	(thisPage = nextPage or: [thisPage isNil | nextPage isNil]) ifFalse: [^finalMark gotoMark].	now _ Time millisecondClockValue.	program _ Dictionary new.	program		at: #startTime put: now;		at: #endTime put: now + nextDurationInMs;		at: #startPoint put: (self valueOfProperty: #cameraPoint);		at: #endPoint put: (finalMark valueOfProperty: #cameraPoint);		at: #startZoom put: (self valueOfProperty: #cameraScale);		at: #endZoom put: (finalMark valueOfProperty: #cameraScale).	self cameraController setProgrammedMoves: {program}.! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:39'!handlesMouseDown: evt	^true! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:43'!mouseDown: evt	evt shiftPressed ifTrue: [^self].	self isSticky ifTrue: [^self].	evt hand grabMorph: self.! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:43'!mouseUp: evt	evt shiftPressed ifTrue: [^self gotoMark].! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 14:56'!pauseFrom: scorePlayer	self cameraController pauseProgrammedMoves.! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:27'!resetFrom: scorePlayer	"self cameraController setProgrammedMoves: nil."! !!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:04'!resumeFrom: scorePlayer	self cameraController resumeProgrammedMoves! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:21'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu addLine.	aCustomMenu add: 'change tilt and zoom keys' action: #changeKeys.	aCustomMenu add: 'run an existing camera script' action: #runAScript.	aCustomMenu add: 'edit an existing camera script' action: #editAScript.! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 14:54'!currentCameraVersion	^2! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:30'!doProgrammedMoves	| thisMove startPoint endPoint startZoom endZoom newScale newPoint fractionLeft |	programmedMoves isEmptyOrNil ifTrue: [		^programmedMoves _ nil	].	thisMove _ programmedMoves first.	thisMove at: #pauseTime ifPresent: [ :ignore | ^self].	fractionLeft _ self fractionLeftInMove: thisMove.	fractionLeft ifNil: [^programmedMoves _ programmedMoves allButFirst].	startPoint _ thisMove at: #startPoint ifAbsentPut: [self cameraPoint].	endPoint _ thisMove at: #endPoint ifAbsentPut: [self cameraPoint].	startZoom _ thisMove at: #startZoom ifAbsentPut: [self cameraScale].	endZoom _ thisMove at: #endZoom ifAbsentPut: [self cameraScale].	newScale _ endZoom - (endZoom - startZoom * fractionLeft).	newPoint _ (endPoint - (endPoint - startPoint * fractionLeft)) "rounded".	target changeScaleTo: newScale.	target cameraPoint: newPoint.	fractionLeft <= 0 ifTrue: [^programmedMoves _ programmedMoves allButFirst].! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 14:55'!keyStroke: anEvent! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:30'!pauseProgrammedMoves	programmedMoves isEmptyOrNil ifTrue: [^self].	programmedMoves first		at: #pauseTime		put: Time millisecondClockValue! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:38'!resumeProgrammedMoves	| thisStep |	programmedMoves isEmptyOrNil ifTrue: [^self].	(thisStep _ programmedMoves first)		at: #pauseTime		ifPresent: [ :pauseTime |			thisStep 				at: #startTime 				put: (thisStep at: #startTime) + Time millisecondClockValue - pauseTime.			thisStep removeKey: #pauseTime ifAbsent: [].		].! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 15:38'!setProgrammedMoves: aCollection	programmedMoves _ aCollection! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/18/2000 14:55'!step	| delta halfDW action |	(self valueOfProperty: #currentCameraVersion ifAbsent: [0]) = 							self currentCameraVersion ifFalse: [		self patchOldVersion1.		self setProperty: #currentCameraVersion toValue: self currentCameraVersion.	].	super step.	self doProgrammedMoves.	(currentKeyDown ifNil: [#()]) do: [ :each |		action _ upDownCodes at: each ifAbsent: [#fugeddaboutit].		action == #in ifTrue: [			target scaleImageBy: -10.		].		action == #out ifTrue: [			target scaleImageBy: 10.		].		action == #up ifTrue: [			target tiltImageBy: -20.		].		action == #down ifTrue: [			target tiltImageBy: 20.		].	].	mouseMovePoint ifNil: [^self].	mouseDownPoint ifNil: [^self].	target ifNil: [^self].	halfDW _ self deadZoneWidth // 2.	delta _ mouseMovePoint - mouseDownPoint.	delta x abs <= halfDW ifTrue: [delta _ 0@delta y].	delta y abs <= halfDW ifTrue: [delta _ delta x@0].		target panImageBy: delta x.! !ZoomAndScrollControllerMorph removeSelector: #test!