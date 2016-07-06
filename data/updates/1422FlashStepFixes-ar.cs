'From Squeak 2.5 of August 6, 1999 on 5 September 1999 at 2:51:16 pm'!"Change Set:		FlashStepFixes-arDate:			5 September 1999Author:			Andreas RaabFixes the Flash players dependency on the Morph step frequency."!FlashMorph subclass: #FlashSpriteMorph	instanceVariableNames: 'playing maxFrames loadedFrames frameNumber stepTime damageRecorder sounds actions labels lastStepTime '	classVariableNames: 'UseTimeSync '	poolDictionaries: ''	category: 'MM-Flash-Morphs'!!FlashSpriteMorph methodsFor: 'stepping' stamp: 'ar 9/5/1999 13:50'!startPlaying	"Start playing from the current frame"	playing _ true.	loadedFrames = 0 ifTrue:[^nil].	frameNumber >= maxFrames ifTrue:[self frameNumber: 1].	lastStepTime _ Time millisecondClockValue.! !!FlashSpriteMorph methodsFor: 'stepping' stamp: 'ar 9/5/1999 14:51'!step	| nowStepTime maxSteps |	playing ifFalse:[^self].	UseTimeSync ifTrue:[		maxSteps _ 5.		nowStepTime _ Time millisecondClockValue.		[(lastStepTime + stepTime <= nowStepTime) and:[playing and:[maxSteps >= 0]]]			whileTrue:[				self stepForward.				lastStepTime _ lastStepTime + stepTime.				maxSteps _ maxSteps - 1.			].	] ifFalse:[self stepForward].	damageRecorder _ nil. "Insurance"! !!FlashSpriteMorph methodsFor: 'stepping' stamp: 'ar 9/5/1999 14:04'!stepTime	"If we're syncing with time step at double speed."	^UseTimeSync		ifTrue:[stepTime // 2]		ifFalse:[stepTime]! !!FlashSpriteMorph methodsFor: 'object fileIn' stamp: 'ar 9/5/1999 14:13'!convertbosfcetcpmlfsdsal0: varDict bosfcetcpmlfsdsall0: smartRefStrm	"These variables are automatically stored into the new instance ('playing' 'maxFrames' 'loadedFrames' 'frameNumber' 'stepTime' 'damageRecorder' 'sounds' 'actions' 'labels' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: ('lastStepTime' )  If a non-nil value is needed, please assign it."	lastStepTime _ 0.! !!FlashSpriteMorph class methodsFor: 'class initialization' stamp: 'ar 9/5/1999 14:02'!initialize	"FlashSpriteMorph initialize"	UseTimeSync _ true.! !!FlashSpriteMorph class methodsFor: 'accessing' stamp: 'ar 9/5/1999 14:05'!useTimeSync	"Return true if Flash should be synchronized by time	rather than step-frequency."	^UseTimeSync! !!FlashSpriteMorph class methodsFor: 'accessing' stamp: 'ar 9/5/1999 14:05'!useTimeSync: aBoolean	"Determine if Flash should be synchronized by time	rather than step-frequency."	UseTimeSync _ aBoolean! !FlashSpriteMorph initialize!