'From SqueakLight|II of 31 May 2008 [latest update: #7174] on 12 June 2008 at 7:10:07 am'!"Change Set:		7174msSinceMidnight-M474-kphDate:			23 May 2008Author:			Keith HodgesThis change correct a calculation related to the millisecond clock correctly taking into account the fact that it's maximum value is Integer>>maxVal // 2."!Magnitude subclass: #DateAndTime	instanceVariableNames: 'seconds offset jdn nanos '	classVariableNames: 'LocalTimeZone ClockProvider LastTickSemaphore DaysSinceEpoch LastTick LastMilliSeconds MilliSecondOffset LocalOffset '	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!DateAndTime commentStamp: '<historical>' prior: 0!I represent a point in UTC time as defined by ISO 8601. I have zero duration.My implementation uses three SmallIntegers and a Duration:jdn		- julian day number.seconds	- number of seconds since midnight.nanos	- the number of nanoseconds since the second.offset	- duration from UTC.The nanosecond attribute is almost always zero but it defined for full ISO compliance and is suitable for timestamping.!!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 4/13/2006 10:21'!asUTC	^ offset isZero		ifTrue: [self]		ifFalse: [self utcOffset: 0]! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'gk 8/31/2006 00:55'!asDuration	"Answer the duration since midnight."	^ Duration seconds: seconds nanoSeconds: nanos! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'kph 10/13/2006 04:33'!setJdn: j seconds: s nano: n offset: ojdn := j.seconds := s.nanos :=  n.offset :=  o.! !!DateAndTime methodsFor: 'private' stamp: 'gk 8/30/2006 22:59'!normalize: i ticks: ticks base: base	| tick div quo rem |	tick := ticks at: i.	div := tick digitDiv: base neg: tick negative.	quo := (div at: 1) normalize.	rem := (div at: 2) normalize.	rem < 0 ifTrue: [ quo := quo - 1. rem := base + rem ].	ticks at: (i-1) put: ((ticks at: i-1) + quo).	ticks at: i put: rem! !!DateAndTime methodsFor: 'private' stamp: 'gk 8/30/2006 23:01'!ticks: ticks offset: utcOffset	"ticks is {julianDayNumber. secondCount. nanoSeconds}"	self normalize: 3 ticks: ticks base: NanosInSecond.	self normalize: 2 ticks: ticks base: SecondsInDay.	jdn	_ ticks at: 1.	seconds	_ ticks at: 2.	nanos := ticks at: 3.	offset := utcOffset! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'gk 8/31/2006 00:49'!clockPrecision	"One nanosecond precision"	^ Duration seconds: 0 nanoSeconds: 1! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'KLC 5/9/2008 20:13'!now 	| nanoTicks msm |	nanoTicks := (msm := self milliSecondsSinceMidnight) * 1000000.	(LastTick < nanoTicks) ifTrue: [		LastTick := nanoTicks.		^ self todayAtMilliSeconds: msm].	LastTickSemaphore critical: [	 	 		LastTick _  LastTick + 1.		^ self todayAtNanoSeconds: LastTick]" [ 10000 timesRepeat: [ self now. ] ] timeToRun / 10000.0 . If calls to DateAndTime-c-#now are within a single millisecond the semaphore code to ensure that (self now <= self now) slows things down considerably by a factor of about 20.The actual speed of a single call to DateAndTime-now in milliseconds is demonstrated by the unguarded method below.[ 100000 timesRepeat: [ self todayAtMilliSeconds: (self milliSecondsSinceMidnight) ] ] timeToRun / 100000.0 .  0.00494 0.00481 0.00492 0.00495  "! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'gk 8/31/2006 01:00'!fromSeconds: seconds	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"	^ self basicNew ticks: (Array with: SqueakEpoch with: seconds with: 0) offset: self localOffset! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'kph 12/11/2006 21:13'!millisecondClockValue	^ self clock millisecondClockValue! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'kph 12/9/2006 23:46'!totalSeconds	^ self clock totalSeconds! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'kph 6/13/2007 23:07'!localOffset	"Answer the duration we are offset from UTC"	^ LocalOffset ifNil:[ LocalOffset := self localTimeZone offset ]! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'kph 10/20/2007 13:56'!milliSecondsSinceMidnight		| msm msClock |		msClock := self millisecondClockValue. 	(msClock < LastMilliSeconds) 		ifTrue:[ "rolled over" MilliSecondOffset := MilliSecondOffset + (SmallInteger maxVal // 2) + 1 ].	LastMilliSeconds := msClock.	[ msm := msClock + MilliSecondOffset. 	 (msm >= 86400000) ] 		whileTrue: [ "next day" 			LastTick := -1.			DaysSinceEpoch := DaysSinceEpoch + 1. 			MilliSecondOffset := MilliSecondOffset - 86400000 ].	"day rolled over sanity check"	((LastTick = -1) and: [		(Duration days: SqueakEpoch hours: 0 minutes: 0 seconds: 								(self clock totalSeconds)) days ~= DaysSinceEpoch ]) 		ifTrue: 				[  self initializeOffsets. 				 ^ self milliSecondsSinceMidnight ].		^msm! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'kph 12/19/2006 15:01'!readFrom: aStream	| bc year month day hour minute second nanos offset buffer ch |	aStream peek = $- ifTrue: [ aStream next. bc _ -1] ifFalse: [bc _ 1].	year _ (aStream upTo: $-) asInteger * bc.	month _ (aStream upTo: $-) asInteger.	day _ (aStream upTo: $T) asInteger.	hour _ (aStream upTo: $:) asInteger. 	buffer _ '00:' copy. ch _ nil.	minute _ WriteStream on: buffer.	[ aStream atEnd | (ch = $:) | (ch = $+) | (ch = $-) ]		whileFalse: [ ch _ minute nextPut: aStream next. ].	(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $: ].	minute _ ((ReadStream on: buffer) upTo: ch) asInteger.	buffer _ '00.' copy.	second _ WriteStream on: buffer.	[ aStream atEnd | (ch = $.) | (ch = $+) | (ch = $-) ]		whileFalse: [ ch _ second nextPut: aStream next. ].	(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $. ].	second _ ((ReadStream on: buffer) upTo: ch) asInteger.	buffer _ '000000000+' copy.	(ch = $.) ifTrue: [ 		nanos _ WriteStream on: buffer.		[ aStream atEnd | ((ch := aStream next) = $+) | (ch = $-) ]			whileFalse: [ nanos nextPut: ch. ].		(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $+ ].	].	nanos _ buffer asInteger.	aStream atEnd		ifTrue: [ offset _ self localOffset ]		ifFalse:		 	[offset _ Duration fromString: (ch asString, '0:', aStream upToEnd).			(offset = self localOffset) ifTrue: [ offset _ self localOffset ]].	^ self		year: year		month: month		day: day		hour: hour		minute: minute		second: second		nanoSecond:  nanos		offset: offset.	"	'-1199-01-05T20:33:14.321-05:00' asDateAndTime		' 2002-05-16T17:20:45.1+01:01' asDateAndTime		' 2002-05-16T17:20:45.02+01:01' asDateAndTime		' 2002-05-16T17:20:45.003+01:01' asDateAndTime		' 2002-05-16T17:20:45.0004+01:01' asDateAndTime  		' 2002-05-16T17:20:45.00005' asDateAndTime		' 2002-05-16T17:20:45.000006+01:01' asDateAndTime		' 2002-05-16T17:20:45.0000007+01:01' asDateAndTime		' 2002-05-16T17:20:45.00000008-01:01' asDateAndTime   		' 2002-05-16T17:20:45.000000009+01:01' asDateAndTime  		' 2002-05-16T17:20:45.0000000001+01:01' asDateAndTime   		' 2002-05-16T17:20' asDateAndTime		' 2002-05-16T17:20:45' asDateAndTime		' 2002-05-16T17:20:45+01:57' asDateAndTime 		' 2002-05-16T17:20:45-02:34' asDateAndTime 		' 2002-05-16T17:20:45+00:00' asDateAndTime		' 1997-04-26T01:02:03+01:02:3' asDateAndTime  	"! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'kph 12/11/2006 20:36'!todayAtMilliSeconds: milliSecondsSinceMidnight	 ^ self basicNew			setJdn: DaysSinceEpoch 			seconds: (milliSecondsSinceMidnight // 1000) 			nano: (milliSecondsSinceMidnight  \\ 1000 * 1000000  ) 			offset: self localOffset	 " [ 100000 timesRepeat: [ self fromMilliSeconds: self milliSecondsSinceMidnight. ] ] timeToRun.    "! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'kph 12/11/2006 20:36'!todayAtNanoSeconds: nanoSecondsSinceMidnight	^ self basicNew			setJdn: DaysSinceEpoch 			seconds: (nanoSecondsSinceMidnight // 1000000000) 			nano: (nanoSecondsSinceMidnight  \\ 1000000000  ) 			offset: self localOffset ! !!DateAndTime class methodsFor: 'clock provider' stamp: 'kph 12/11/2006 20:14'!clock 	 "the provider of real time seconds/milliseconds."	^ ClockProvider ! !!DateAndTime class methodsFor: 'initialize-release' stamp: 'kph 6/13/2007 23:05'!initialize	super initialize.	ClockProvider := Time.	LastTickSemaphore := Semaphore forMutualExclusion.	LastMilliSeconds := 0.	LocalOffset := nil.	LastTick := 0.	Smalltalk addToStartUpList: self.	self startUp: true.! !!DateAndTime class methodsFor: 'initialize-release' stamp: 'kph 6/16/2007 06:51'!initializeOffsets 	| epochianSeconds secondsSinceMidnight nowSecs  |	  	LastTick := 0.  	nowSecs :=  self clock secondsWhenClockTicks.	LastMilliSeconds := self millisecondClockValue. 	epochianSeconds := Duration days: SqueakEpoch hours: 0 minutes: 0 seconds: nowSecs.	DaysSinceEpoch := epochianSeconds days.	secondsSinceMidnight := (epochianSeconds - (Duration days: DaysSinceEpoch hours: 0 minutes: 0 seconds: 0)) asSeconds.  	MilliSecondOffset := (secondsSinceMidnight * 1000 - LastMilliSeconds).	LocalOffset := nil.  ! !!DateAndTime class methodsFor: 'initialize-release' stamp: 'kph 12/13/2006 21:46'!startUp: resuming 	resuming ifFalse: [^ self].		[ self initializeOffsets ] fork.	! !!DateAndTime class methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:39'!localTimeZone	"Answer the local time zone"	^ LocalTimeZone ifNil: [ LocalTimeZone _ TimeZone default ]! !!Time class methodsFor: 'clock' stamp: 'kph 12/14/2006 01:43'!secondsWhenClockTicks	"waits for the moment when a new second begins"	| lastSecond delay |	delay :=  Delay forMilliseconds: 1.	lastSecond _ self primSecondsClock.	[ lastSecond = self primSecondsClock ] whileTrue: [ delay wait ]. 	^ lastSecond + 1! !DateAndTime initialize!Magnitude subclass: #DateAndTime	instanceVariableNames: 'seconds offset jdn nanos'	classVariableNames: 'ClockProvider DaysSinceEpoch LastMilliSeconds LastTick LastTickSemaphore LocalOffset LocalTimeZone MilliSecondOffset'	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!