'From Squeak2.9alpha of 12 June 2000 [latest update: #2606] on 12 September 2000 at 9:52:16 am'!"Change Set:		MorphicAlarmsDate:			11 September 2000Author:			Andreas RaabThe change set adds alarms to Morphic so that we have a convenient way to say, e.g.,	World addAlarm: #addHalo after: 2000.	World addAlarm: #color: with: (Color red) after: 2000.and getting rid of it by	World removeAlarm: #addHalo.Alarms are simple actions to be executed at some point in the future and tremendously helpful for getting rid of all the #mouseOverTimes nonsense in HandMorph."!MessageSend subclass: #MorphicAlarm	instanceVariableNames: 'scheduledTime '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Support'!Object subclass: #WorldState	instanceVariableNames: 'hands activeHand viewBox canvas damageRecorder stepList lastStepTime lastCycleTime commandHistory alarms lastAlarmTime '	classVariableNames: 'CanSurrenderToOS DeferredUIMessages DisableDeferredUpdates LastCycleTime MinCycleLapse '	poolDictionaries: ''	category: 'Morphic-Worlds'!!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector after: delayTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: #() after: delayTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector at: scheduledTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: #() at: scheduledTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector with: arg1 after: delayTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: (Array with: arg1) after: delayTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector with: arg1 at: scheduledTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: (Array with: arg1) at: scheduledTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector with: arg1 with: arg2 after: delayTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: (Array with: arg1 with: arg2) after: delayTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector with: arg1 with: arg2 at: scheduledTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: (Array with: arg1 with: arg2) at: scheduledTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:35'!addAlarm: aSelector withArguments: args after: delayTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self addAlarm: aSelector withArguments: args at: Time millisecondClockValue + delayTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:34'!addAlarm: aSelector withArguments: args at: scheduledTime	"Add an alarm (that is an action to be executed once) with the given set of parameters"	^self alarmScheduler addAlarm: aSelector withArguments: args for: self at: scheduledTime! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:34'!alarmScheduler	"Return the scheduler being responsible for triggering alarms"	^self world! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:36'!removeAlarm: aSelector	"Remove the given alarm"	^self alarmScheduler removeAlarm: aSelector for: self! !!Morph methodsFor: 'events-alarms' stamp: 'ar 9/11/2000 16:36'!removeAlarm: aSelector at: scheduledTime	"Remove the given alarm"	^self alarmScheduler removeAlarm: aSelector at: scheduledTime for: self! !!MorphicAlarm methodsFor: 'accessing' stamp: 'ar 9/11/2000 16:44'!scheduledTime	"Return the time (in milliseconds) that the receiver is scheduled to be executed"	^scheduledTime! !!MorphicAlarm methodsFor: 'accessing' stamp: 'ar 9/11/2000 16:45'!scheduledTime: msecs	"Set the time (in milliseconds) that the receiver is scheduled to be executed"	scheduledTime _ msecs! !!MorphicAlarm class methodsFor: 'instance creation' stamp: 'ar 9/11/2000 16:44'!scheduledAt: scheduledTime receiver: aTarget selector: aSelector arguments: argArray	^(self receiver: aTarget selector: aSelector arguments: argArray)		scheduledTime: scheduledTime.! !!PasteUpMorph methodsFor: 'alarms-scheduler' stamp: 'ar 9/11/2000 16:40'!addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime	"Add a new alarm with the given set of parameters"	worldState addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime.! !!PasteUpMorph methodsFor: 'alarms-scheduler' stamp: 'ar 9/11/2000 16:39'!removeAlarm: aSelector for: aTarget	"Remove the alarm with the given selector"	worldState removeAlarm: aSelector for: aTarget! !!WorldState methodsFor: 'stepping' stamp: 'ar 9/11/2000 17:13'!runLocalStepMethodsIn: aWorld	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world.	ar 3/13/1999: Remove buggy morphs from the step list so that they don't raise repeated errors."	| now deletions wakeupTime morphToStep |	now _ Time millisecondClockValue.	self triggerAlarmsBefore: now.	stepList size = 0 ifTrue: [^ self].	((now < lastStepTime) or: [(now - lastStepTime) > 5000])		 ifTrue: [self adjustWakeupTimes].  "clock slipped"	deletions _ nil.	"Note: Put the following into an error handler to prevent errors happening on stepping"	[stepList do: [:entry |		wakeupTime _ entry at: 2.		morphToStep _ entry at: 1.		(morphToStep shouldGetStepsFrom: aWorld)			ifTrue:				[wakeupTime <= now					ifTrue:						[morphToStep stepAt: now.						entry at: 2 put: now + morphToStep stepTime]]			ifFalse:				[deletions ifNil: [deletions _ OrderedCollection new]. 				deletions addLast: morphToStep]]]	 ifError: [:err :rcvr |		self stopStepping: morphToStep. "Stop this guy right now"		morphToStep setProperty: #errorOnStep toValue: true. "Remember stepping"		Processor activeProcess errorHandler: nil. "So we don't handle this guy twice"		rcvr error: err. "And re-raise the error from here so the stack is still valid"].	deletions ifNotNil:		[deletions do: [:deletedM |			self stopStepping: deletedM.			deletedM stopStepping]].	lastStepTime _ now! !!WorldState methodsFor: 'object fileIn' stamp: 'ar 9/11/2000 17:23'!converthavcdsllc0: varDict havcdsllcal0: smartRefStrm	"These variables are automatically stored into the new instance #('hands' 'activeHand' 'viewBox' 'canvas' 'damageRecorder' 'stepList' 'lastStepTime' 'lastCycleTime' 'commandHistory').	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('alarms' 'lastAlarmTime')  If a non-nil value is needed, please assign it."! !!WorldState methodsFor: 'alarms' stamp: 'ar 9/11/2000 16:43'!addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime	"Add a new alarm with the given set of parameters"	self alarms add: 		(MorphicAlarm 			scheduledAt: scheduledTime			receiver: aTarget			selector: aSelector			arguments: argArray).! !!WorldState methodsFor: 'alarms' stamp: 'ar 9/11/2000 17:11'!adjustAlarmTimes: nowTime	"Adjust the alarm times after some clock weirdness (such as roll-over, image-startup etc)"	| deltaTime |	deltaTime _ nowTime - lastAlarmTime.	self alarms do:[:alarm| alarm scheduledTime: alarm scheduledTime + deltaTime].! !!WorldState methodsFor: 'alarms' stamp: 'ar 9/11/2000 16:48'!alarms	^alarms ifNil:[alarms _ Heap sortBlock:[:alarm1 :alarm2| alarm1 scheduledTime < alarm2 scheduledTime]]! !!WorldState methodsFor: 'alarms' stamp: 'ar 9/11/2000 16:47'!removeAlarm: aSelector for: aTarget	"Remove the alarm with the given selector"	| alarm |	alarm _ self alarms 		detect:[:any| any receiver == aTarget and:[any selector == aSelector]]		ifNone:[nil].	alarm == nil ifFalse:[self alarms remove: alarm].! !!WorldState methodsFor: 'alarms' stamp: 'ar 9/11/2000 17:11'!triggerAlarmsBefore: nowTime	"Trigger all pending alarms that are to be executed before nowTime."	| pending |	lastAlarmTime ifNil:[lastAlarmTime _ nowTime].	(nowTime < lastAlarmTime or:[nowTime - lastAlarmTime > 10000])		ifTrue:[self adjustAlarmTimes: nowTime].	pending _ self alarms.	[pending isEmpty not and:[pending first scheduledTime < nowTime]]		whileTrue:[pending removeFirst value].	lastAlarmTime _ nowTime.! !