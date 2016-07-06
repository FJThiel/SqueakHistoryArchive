'From Squeak3.4alpha of ''11 November 2002'' [latest update: #5125] on 12 December 2002 at 10:08:54 am'!"Change Set:		TallyVMParam-yoDate:			12 December 2002Author:			Yoshiki OhshimaTwo methods in MessageTally do unnecessary arithmetic between integer and array of integer."!!MessageTally methodsFor: 'initialize-release' stamp: 'yo 12/12/2002 10:00'!spyEvery: millisecs on: aBlock 	"Create a spy and spy on the given block at the specified rate."	| myDelay value startTime time0 |	(aBlock isMemberOf: BlockContext)		ifFalse: [self error: 'spy needs a block here'].	self class: aBlock receiver class method: aBlock method.		"set up the probe"	ObservedProcess _ Processor activeProcess.	myDelay := Delay forMilliseconds: millisecs.	time0 := Time millisecondClockValue.	gcStats _ Smalltalk getVMParameters.	Timer :=		[[true] whileTrue: 			[startTime := Time millisecondClockValue.			myDelay wait.			self tally: ObservedProcess suspendedContext				"tally can be > 1 if ran a long primitive"				by: (Time millisecondClockValue - startTime) // millisecs].		nil] newProcess.	Timer priority: Processor userInterruptPriority.		"activate the probe and evaluate the block"	Timer resume.	value := aBlock value.	"Collect gc statistics"	Smalltalk getVMParameters keysAndValuesDo:		[:idx :gcVal| gcStats at: idx put: (gcVal - (gcStats at: idx))].		"cancel the probe and return the value"	Timer terminate.	time := Time millisecondClockValue - time0.	^value! !!MessageTally methodsFor: 'initialize-release' stamp: 'yo 12/12/2002 10:05'!spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration 	"Create a spy and spy on the given process at the specified rate."	| myDelay time0 endTime sem |	(aProcess isKindOf: Process)		ifFalse: [self error: 'spy needs a Process here'].	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.	"set up the probe"	ObservedProcess _ aProcess.	myDelay _ Delay forMilliseconds: millisecs.	time0 _ Time millisecondClockValue.	endTime _ time0 + msecDuration.	sem _ Semaphore new.	gcStats _ Smalltalk getVMParameters.	Timer _ [[| startTime | 			startTime _ Time millisecondClockValue.			myDelay wait.			self tally: ObservedProcess suspendedContext by: Time millisecondClockValue - startTime // millisecs.			startTime < endTime] whileTrue.			sem signal]				forkAt: (ObservedProcess priority + 1 min: Processor highestPriority).	"activate the probe and wait for it to finish"	sem wait.	"Collect gc statistics"	Smalltalk getVMParameters keysAndValuesDo:		[:idx :gcVal| gcStats at: idx put: (gcVal - (gcStats at: idx))].	time _ Time millisecondClockValue - time0! !