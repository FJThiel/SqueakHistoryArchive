'From Squeak3.6beta of 4 July 2003 [latest update: #5395] on 24 August 2003 at 10:11:57 am'!"Change Set:		Chronology-j-ConvertTDateTTimeTTimestamp2of2Date:			24 August 2003Author:			Brent PinkneyThis is the second of two change sets to migrate the system to the new Date, Time, and TimeStamp classes."!!Date class methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 00:00'!dateAndTimeNow
	"Answer an Array whose with Date today and Time now."

	^ Time dateAndTimeNow! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 23:56'!asDate
	^ Date starting: self
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 00:00'!asTime
	^ Time seconds: seconds nanoSeconds: nanos! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 00:02'!asTimeStamp	^ self as: TimeStamp! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 00:00'!millisecondClockValue	^ Time millisecondClockValue! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 00:01'!totalSeconds	^ Time totalSeconds! !!String methodsFor: 'converting' stamp: 'brp 8/23/2003 23:58'!asDate	"Many allowed forms, see Date>>#readFrom:"	^ Date fromString: self! !!String methodsFor: 'converting' stamp: 'brp 8/24/2003 00:01'!asTime	"Many allowed forms, see Time>>readFrom:"	^ Time fromString: self.! !!String methodsFor: 'converting' stamp: 'brp 8/24/2003 00:02'!asTimeStamp	"Convert from obsolete TimeStamp format"
	^ TimeStamp fromString: self! !!Time methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 23:58'!asDate	^ Date today! !!Time class methodsFor: 'benchmarks' stamp: 'brp 8/24/2003 00:06'!benchmarkMillisecondClock		"Time benchmarkMillisecondClock"
	"Benchmark the time spent in a call to Time>>millisecondClockValue.
	On the VM level this tests the efficiency of calls to ioMSecs()."
	"PII/400 Windows 98: 0.725 microseconds per call"
	| temp1 temp2 temp3 delayTime nLoops time |
	delayTime _ 5000. "Time to run benchmark is approx. 2*delayTime"

	"Don't run the benchmark if we have an active delay since
	we will measure the additional penalty in the primitive dispatch
	mechanism (see #benchmarkPrimitiveResponseDelay)."
	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	"Flush the cache for this benchmark so we will have
	a clear cache hit for each send to #millisecondClockValue below"
	Object flushCache.
	temp1 _ 0.
	temp2 _ self. "e.g., temp1 == Time"
	temp3 _ self millisecondClockValue + delayTime.

	"Now check how often we can run the following loop in the given time"
	[temp2 millisecondClockValue < temp3]
		whileTrue:[temp1 _ temp1 + 1].

	nLoops _ temp1. "Remember the loops we have run during delayTime"

	"Setup the second loop"
	temp1 _ 0.
	temp3 _ nLoops.

	"Now measure how much time we spend without sending #millisecondClockValue"
	time _ Time millisecondClockValue.
	[temp1 < temp3]
		whileTrue:[temp1 _ temp1 + 1].
	time _ Time millisecondClockValue - time.

	"And compute the number of microseconds spent per call to #millisecondClockValue"
	^((delayTime - time * 1000.0 / nLoops) truncateTo: 0.001) printString,
		' microseconds per call to Time>>millisecondClockValue'! !!Time class methodsFor: 'general inquiries' stamp: 'brp 8/23/2003 23:59'!humanWordsForSecondsAgo: secs
	| date today |
	"Return natural language for this date and time in the past."

	secs <= 1 ifTrue: [^ 'a second ago'].
	secs < 45 ifTrue: [^ secs printString, ' seconds ago'].
	secs < 90 ifTrue: [^ 'a minute ago'].
	secs < "45*60" 2700 ifTrue: [^ (secs//60) printString, ' minutes ago'].
	secs < "90*60" 5400 ifTrue: [^ 'an hour ago'].
	secs < "18*60*60" 64800 ifTrue: [^ (secs//3600) printString, ' hours ago'].
	date _ Date fromSeconds: self totalSeconds - secs.		"now work with dates"
	today _ Date today.
	date > (today subtractDays: 2) ifTrue: [^ 'yesterday'].
	date > (today subtractDays: 8) ifTrue: [^ 'last ', date dayOfWeekName].
	date > (today subtractDays: 13) ifTrue: [^ 'a week ago'].
	date > (today subtractDays: 28) ifTrue: [
		^ ((today subtractDate: date)//7) printString, ' weeks ago'].
	date > (today subtractDays: 45) ifTrue: [^ 'a month ago'].
	date > (today subtractDays: 300) ifTrue: [^ 'last ', date monthName].
	^ date monthName, ', ', date year printString

"Example
#(0.5 30 62 130 4000 10000 60000 90000 345600 864000 1728000 3456000 17280000 34560000 345600000) 		collect: [:ss | Time humanWordsForSecondsAgo: ss].
"! !!Time class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 23:59'!dateAndTimeFromSeconds: secondCount

	^ Array
		with: (Date fromSeconds: secondCount)
		with: (Time fromSeconds: secondCount \\ 86400)
! !!TimeStamp class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 23:59'!readFrom: aStream 	"Answer a new instance from the read stream.  	TimeStamp readFrom: '1-10-2000 11:55:00 am' readStream."	^ self		date: (Date readFrom: (aStream upTo: Character space) readStream)		time: (Time readFrom: (aStream upToEnd) readStream)! !!TimeStamp class methodsFor: 'deprecated' stamp: 'brp 8/24/2003 00:01'!midnightOn: aDate	"Answer a new instance that represents aDate at midnight."	^ self 		deprecatedExplanation: 'Deprecated';		date: aDate time: Time midnight! !!TimeStamp class methodsFor: 'deprecated' stamp: 'brp 8/24/2003 00:01'!noonOn: aDate	"Answer a new instance that represents aDate at noon."	^ self 		deprecatedExplanation: 'Deprecated';		date: aDate time: Time noon! !Object removeSelector: #chronologyMigrationClassPrefix!Object removeSelector: #dateClass!Object removeSelector: #timeClass!Object removeSelector: #timeStampClass!"Postscript:Remove the tempory migration category"Object removeCategory: '*kernel-chronology'.!