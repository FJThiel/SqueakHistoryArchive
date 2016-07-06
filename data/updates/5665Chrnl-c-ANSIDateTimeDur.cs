'From Squeak3.7alpha of 11 September 2003 [latest update: #5623] on 16 January 2004 at 2:31:53 pm'!"Change Set:		Chronology-c-ANSIDateAndTimeDuration
Date:			24 August 2003
Author:			Brent Pinkney

This change set introduces the ANSI compliant DateAndTime and Duration classes.
It also introduces the new Timespan,Week, Month, Year, and Schedule classes."!SharedPool subclass: #ChronologyConstants	instanceVariableNames: 'seconds offset jdn nanos'	classVariableNames: 'DayNames DaysInMonth MonthNames NanosInMillisecond NanosInSecond SecondsInDay SecondsInHour SecondsInMinute SqueakEpoch'	poolDictionaries: ''	category: 'Kernel-Chronology'!Magnitude subclass: #DateAndTime	instanceVariableNames: 'seconds offset jdn nanos'	classVariableNames: 'FirstMilliSecondValue LastTick LastTickSemaphore LocalTimeZone'	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!DateAndTime commentStamp: 'brp 5/13/2003 08:07' prior: 0!I represent a point in UTC time as defined by ISO 8601. I have zero duration.


My implementation uses three SmallIntegers
 and a Duration:
jdn		- julian day number.
seconds	- number of seconds since midnight.
nanos	- the number of nanoseconds since the second.

offset	- duration from UTC.

The nanosecond attribute is almost always zero but it defined for full ISO compliance and is suitable for timestamping.
!Magnitude subclass: #Duration	instanceVariableNames: 'nanos seconds'	classVariableNames: ''	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!Duration commentStamp: '<historical>' prior: 0!I represent a duration of time. I have nanosecond precision
!Object subclass: #TimeZone	instanceVariableNames: 'offset abbreviation name'	classVariableNames: ''	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!TimeZone commentStamp: 'brp 9/4/2003 06:32' prior: 0!TimeZone is a simple class to colect the information identifying a UTC time zone.

offset			-	Duration	- the time zone's offset from UTC
abbreviation	-	String		- the abbreviated name for the time zone.
name			-	String		- the name of the time zone.

TimeZone class >> #timeZones returns an array of the known time zones
TimeZone class >> #default returns the default time zone (Grenwich Mean Time)!!BlockContext methodsFor: 'evaluating' stamp: 'brp 9/25/2003 13:49'!durationToRun
	"Answer the duration taken to execute this block."

	^ Duration milliSeconds: self timeToRun

! !!ChronologyConstants class methodsFor: 'as yet unclassified' stamp: 'brp 9/25/2003 10:49'!initialize
	"ChronologyConstants initialize" 	SqueakEpoch _ 2415386. 		"Julian day number of 1 Jan 1901" 
	SecondsInDay _ 86400.
	SecondsInHour _ 3600.
	SecondsInMinute _ 60.
	NanosInSecond _ 10 raisedTo: 9.
	NanosInMillisecond _ 10 raisedTo: 6.
	DayNames _ #(Sunday Monday Tuesday Wednesday Thursday Friday Saturday).
		
	MonthNames _ #(January February March April May June July
 			August September October November December).
	DaysInMonth _ #(31 28 31 30 31 30 31 31 30 31 30 31).
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 7/9/2005 08:45'!+ operand
	"operand conforms to protocol Duration"
	| ticks | 	ticks _ self ticks + (operand asDuration ticks) .
	^ self class basicNew
		ticks: ticks
		offset: self offset; 
		yourself.
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 1/9/2004 05:39'!- operand
	"operand conforms to protocol DateAndTime or protocol Duration"

	^ (operand respondsTo: #asDateAndTime)
		ifTrue: 
			[ | lticks rticks |
			lticks _ self asLocal ticks.	
		rticks _ operand asDateAndTime asLocal ticks.
			Duration
 				seconds: (SecondsInDay *(lticks first - rticks first)) + 
							(lticks second - rticks second)
 				nanoSeconds: (lticks third - rticks third) ]	
	ifFalse:
		
 	[ self + (operand negated) ].
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 15:49'!< comparand
	"comparand conforms to protocol DataAndTime"

	| lticks rticks |

	lticks _ self asUTC ticks.
	rticks _ comparand asDateAndTime asUTC ticks. 	^ (lticks first < rticks first)
		ifTrue: [ true ]
	
		ifFalse:
			[ (lticks first > rticks first)
				ifTrue: [ false ]
				ifFalse:
					[ (lticks second < rticks second
)
						ifTrue: [ true ]
						ifFalse: 
							[ (lticks second > rticks second)
								ifTrue: [ false ]
								ifFalse:
									[ lticks third < rticks third ]]]]
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 15:49'!= comparand
	"comparand conforms to protocol DateAndTime"

	^ self == comparand
	
		ifTrue: [true]
		ifFalse:
 [ [self asUTC ticks = comparand asDateAndTime asUTC ticks ]
					on: MessageNotUnderstood do: [false] ].! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 13:11'!asLocal
	

	^ (self offset = self class localOffset)

		ifTrue: [self]
		ifFalse: [self utcOffset: self class localOffset]
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 13:12'!asUTC


	^ self utcOffset: 0! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 21:03'!dayOfMonth
	"Answer which day of the month is represented by the receiver."

	^ self
		dayMonthYearDo: [ :d :m :y | d ]! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/24/2003 12:25'!dayOfWeek

	"Sunday=1, ... , Saturday=7"

	^ (jdn + 1 rem: 7) + 1! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 10:34'!dayOfWeekAbbreviation

	^ self dayOfWeekName copyFrom: 1 to: 3! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:28'!dayOfWeekName

	^ Week nameOfDay: self dayOfWeek
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:29'!dayOfYear


	^ jdn - (Year year: self year) start julianDayNumber + 1
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 15:49'!hash

	^ self asUTC ticks hash
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:29'!hour

	^ self hour24
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 11:21'!hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver." 	| h |
	h _ (self hour24 abs + 1).
	^ h > 12 ifTrue: [h - 12] ifFalse: [h].
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:29'!hour24


	^ (Duration seconds: seconds) hours
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:29'!isLeapYear


	^ Year isLeapYear: self year.
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/24/2003 11:03'!meridianAbbreviation

	^ self asTime meridianAbbreviation! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:30'!minute


	^ (Duration seconds: seconds) minutes
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 21:05'!month

	^ self 
		dayMonthYearDo: [ :d :m :y | m ].! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:30'!monthAbbreviation


	^ self monthName copyFrom: 1 to: 3
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:30'!monthName


	^ Month nameOfMonth: self month
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:30'!offset

	^ offset
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 21:09'!offset: anOffset

	"Answer a <DateAndTime> equivalent to the receiver but with its local time 
	being offset from UTC by offset."

	^ self class basicNew 
		ticks: self ticks offset: anOffset asDuration;
		yourself
		! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:31'!second


	^ (Duration seconds: seconds) seconds
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 9/4/2003 06:42'!timeZoneAbbreviation

	^ self class localTimeZone abbreviation
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 9/4/2003 06:42'!timeZoneName

	^ self class localTimeZone name
! !!DateAndTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 21:05'!year
	^ self
		dayMonthYearDo: [ :d :m :y | y ]! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 23:56'!asDate
	^ Date starting: self
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:46'!asDateAndTime

	^ self
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:47'!asDuration

	"Answer the duration since midnight"

	^ Duration seconds: seconds nanoSeconds: nanos
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:47'!asMonth

	^ Month starting: self
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:45'!asNanoSeconds
	"Answer the number of nanoseconds since midnight"

	^ self asDuration asNanoSeconds
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 00:00'!asTime
	^ Time seconds: seconds nanoSeconds: nanos! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 00:02'!asTimeStamp	^ self as: TimeStamp! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:47'!asWeek

	^ Week starting: self 
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:47'!asYear

	^ Year starting: self
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:47'!dayMonthYearDo: aBlock
	"Evaluation the block with three arguments: day month, year."

	| l n i j dd mm yyyy |
	l := jdn + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	dd := l - (2447 * j // 80).
	l := j // 11.
	mm := j + 2 - (12 * l).
	yyyy := 100 * (n - 49) + i + l.

	^ aBlock
		value: dd
		value: mm
		value: yyyy.! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:49'!duration

	^ Duration zero
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:49'!julianDayNumber


	^ jdn
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:49'!middleOf: aDuration
	"Return a Timespan where the receiver is the middle of the Duration"

	| duration |
	duration _ aDuration asDuration.

	^ Timespan starting: (self - (duration / 2)) duration: duration.
		! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:48'!midnight
	"Answer a DateAndTime starting at midnight local time"

	^ self
		dayMonthYearDo: [ :d :m :y | self class year: y month: m day: d ]! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:50'!nanoSecond


	^ nanos
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:49'!noon
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: 
		[ :d :m :y | self class year: y month: m day: d hour: 12 minute: 0 second: 0 ]! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:23'!printOn: aStream 
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1. 
	-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z "
	| year month day |
	self dayMonthYearDo: [ :d :m :y | year _ y. month _ m. day _ d ].
	aStream
		nextPut: (year negative ifTrue: [$-] ifFalse: [ Character space ]);
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPut: $-;
		nextPutAll: (month asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (day asString padded: #left to: 2 with: $0);
		nextPut: $T;
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: $:;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: $:;
		nextPutAll: (self second asString padded: #left to: 2 with: $0).
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps _ self nanoSecond printString padded: #left to: 9 with: $0.
		z _ ps findLast: [ :c | c asciiValue > $0 asciiValue ].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aStream
		nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
		nextPutAll: (offset hours abs asString padded: #left to: 2 with: $0);
		nextPut: $:;
		nextPutAll: (offset minutes abs asString padded: #left to: 2 with: $0).
	offset seconds = 0 ifFalse: 
		[ aStream 
			nextPut: $:;
			nextPutAll: (offset seconds abs truncated asString) ].! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:50'!to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ Timespan starting: self ending: (anEnd asDateAndTime).
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 15:57'!to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (Schedule starting: self ending: (anEnd asDateAndTime))
		schedule: (Array with: aDuration asDuration);
		yourself.
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 16:01'!to: anEnd by: aDuration do: aBlock
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (self to: anEnd by: aDuration) scheduleDo: aBlock
! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:37'!utcOffset: anOffset

	"Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset"

	| equiv |
	equiv _ self + (anOffset asDuration - self offset).
	^ equiv ticks: (equiv ticks) offset: anOffset asDuration; yourself
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 21:03'!asSeconds
	"Return the number of seconds since the Squeak epoch"

	^ (self - (self class epoch)) asSeconds
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 17:53'!day

	^ self dayOfYear! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 07:48'!daysInMonth
	"Answer the number of days in the month represented by the receiver."


	^ self asMonth daysInMonth
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 07:48'!daysInYear

	"Answer the number of days in the year represented by the receiver."

	^ self asYear daysInYear
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 15:44'!daysLeftInYear
	"Answer the number of days in the year after the date of the receiver."

	^ self daysInYear - self dayOfYear
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 15:44'!firstDayOfMonth

	^ self asMonth start day! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 18:30'!hours

	^ self hour! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 1/7/2004 15:45'!minutes

	^ self minute! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 07:50'!monthIndex


	^ self month
! !!DateAndTime methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 18:31'!seconds

	^ self second! !!DateAndTime methodsFor: 'private' stamp: 'brp 8/23/2003 15:45'!ticks
	"Private - answer an array with our instance variables. Assumed to be UTC "

	^ Array with: jdn with: seconds with: nanos
.! !!DateAndTime methodsFor: 'private' stamp: 'brp 1/14/2004 09:08'!ticks: ticks offset: utcOffset
	"ticks is {julianDayNumber. secondCount. nanoSeconds}"	| normalize |	normalize _ [ :i :base | | tick quo rem |		tick _ ticks at: i.		quo _ tick abs // base.		rem _ tick abs \\ base.		(tick negative and: [rem ~= 0]) ifTrue: [ quo _ (quo+1) negated. rem _ base - rem ].		ticks at: (i-1) put: ((ticks at: i-1) + quo).		ticks at: i put: rem ].	
	normalize value: 3 value: NanosInSecond.
	normalize value: 2 value: SecondsInDay.	
	jdn	_ ticks first.
	seconds	_ ticks second.
	nanos _ ticks third.
	offset _ utcOffset.

! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:32'!clockPrecision
	"One nanosecond precision"

	^ Duration nanoSeconds: 1
! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 9/25/2003 10:59'!now
	| ticks millis |
	LastTickSemaphore critical:
		[millis _ self millisecondClockValue - FirstMilliSecondValue.
		ticks _ (self totalSeconds * NanosInSecond) + (millis * NanosInMillisecond).
		LastTick _ (LastTick >= ticks ifTrue: [LastTick + 1] ifFalse: [ticks]).
		^ self basicNew 
			ticks: (Duration 
					days: SqueakEpoch 
					hours: 0 
					minutes: 0 
					seconds: 0 
					nanoSeconds: LastTick) ticks
			offset: self localOffset;
			yourself]
! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 7/27/2003 15:25'!year: year day: dayOfYear hour: hour minute: minute second: second

	^ self
		year: year
		day: dayOfYear
		hour: hour
		minute: minute
		second: second
		offset: self localOffset.
! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 7/27/2003 15:28'!year: year day: dayOfYear hour: hour minute: minute second: second offset: offset 
	"Return a DataAndTime"

	| y d |
	y _ self
		year: year
		month: 1
		day: 1
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset.

	d _ Duration days: (dayOfYear - 1).

	^ y + d! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 21:00'!year: year month: month day: day hour: hour minute: minute second: second
	"Return a DateAndTime"

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		offset: self localOffset
! !!DateAndTime class methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:36'!year: year month: month day: day hour: hour minute: minute second: second offset: offset

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:36'!current


	^ self now
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 16:12'!date: aDate time: aTime

	^ self 
		year: aDate year 
		day: aDate dayOfYear 
		hour: aTime hour 
		minute: aTime minute 
		second: aTime second
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp` 8/24/2003 19:11'!epoch
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self julianDayNumber: SqueakEpoch
	! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 07:36'!fromString: aString


	^ self readFrom: (ReadStream on: aString)
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 21:08'!julianDayNumber: aJulianDayNumber

	^ self basicNew
		ticks: aJulianDayNumber days ticks offset: self localOffset;
		yourself
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 9/4/2003 06:40'!localOffset
	"Answer the duration we are offset from UTC"

	^ self localTimeZone offset
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 9/4/2003 06:39'!localTimeZone
	"Answer the local time zone"

	^ LocalTimeZone ifNil: [ LocalTimeZone _ TimeZone default ]

! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 9/4/2003 06:40'!localTimeZone: aTimeZone
	"Set the local time zone"

	LocalTimeZone _ aTimeZone

! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:09'!midnight

	^ self now midnight
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:57'!new
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self epoch
	! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:09'!noon

	^ self now noon! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:58'!readFrom: aStream
	| bc year month day hour minute second nanos offset buffer ch |


	aStream peek = $- ifTrue: [ aStream next. bc _ -1] ifFalse: [bc _ 1].
	year _ (aStream upTo: $-) asInteger * bc.
	month _ (aStream upTo: $-) asInteger.
	day _ (aStream upTo: $T) asInteger.
	hour _ (aStream upTo: $:) asInteger.
 	buffer _ '00:'. ch _ nil.
	minute _ WriteStream on: buffer.
	[ aStream atEnd | (ch = $:) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch _ minute nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $: ].
	minute _ ((ReadStream on: buffer) upTo: ch) asInteger.
	buffer _ '00.'.
	second _ WriteStream on: buffer.
	[ aStream atEnd | (ch = $.) | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch _ second nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $. ].
	second _ ((ReadStream on: buffer) upTo: ch) asInteger.
	buffer _ '00000000+'.
	nanos _ WriteStream on: buffer.
	[ aStream atEnd | (ch = $+) | (ch = $-) ]
		whileFalse: [ ch _ nanos nextPut: aStream next. ].
	(ch isNil or: [ch isDigit]) ifTrue: [ ch _ $+ ].
	nanos _ ((ReadStream on: buffer) upTo: ch) asInteger.
	aStream atEnd
		ifTrue: [ offset _ self localOffset ]
	
	ifFalse:
		 	[offset _ Duration fromString: (ch asString, '0:', aStream upToEnd).
	
		(offset = self localOffset) ifTrue: [ offset _ self localOffset ]].
	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute

		second: second
		nanoSecond:  nanos

		offset: offset.


	"	'-1199-01-05T20:33:14.321-05:00' asDateAndTime
		' 2002-05-16T17:20:45.00000001+01:01' asDateAndTime
  		' 2002-05-16T17:20:45.00000001' asDateAndTime
 		' 2002-05-16T17:20' asDateAndTime
		' 2002-05-16T17:20:45' asDateAndTime
		' 2002-05-16T17:20:45+01:57' asDateAndTime
 		' 2002-05-16T17:20:45-02:34' asDateAndTime
 		' 2002-05-16T17:20:45+00:00' asDateAndTime
		' 1997-04-26T01:02:03+01:02:3' asDateAndTime 
 	"
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:09'!today

	^ self midnight
! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 12:19'!tomorrow

	^ self today asDate next asDateAndTime! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:53'!year: year day: dayOfYear
	"Return a DateAndTime"

	^ self
		year: year
		day: dayOfYear
		hour: 0
		minute: 0
		second: 0! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:54'!year: year month: month day: day
	"Return a DateAndTime, midnight local time" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: 0
		minute: 0! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:54'!year: year month: month day: day hour: hour minute: minute

	"Return a DateAndTime" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 1/7/2004 15:39'!year: year month: month day: day hour: hour minute: minute second: second nanoSecond: nanoCount offset: offset
	"Return a DateAndTime"

	| monthIndex p q r s julianDayNumber since |

	monthIndex _ month isInteger ifTrue: [month] ifFalse: [Month indexOfMonth: month].
	p _ (monthIndex - 14) quo: 12.
	q _ year + 4800 + p.
	r _ monthIndex - 2 - (12 * p).
	s _ (year + 4900 + p) quo: 100.

	julianDayNumber _
 		( (1461 * q) quo: 4 ) +
			( (367 * r) quo: 12 ) -
 				( (3 * s) quo: 4 ) +
 					( day - 32075 ).

	since _ Duration days: julianDayNumber hours: hour 				minutes: minute seconds: second nanoSeconds: nanoCount.

	^ self basicNew
 		ticks: since ticks offset: offset;
		yourself.! !!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 8/24/2003 12:19'!yesterday

	^ self today asDate previous asDateAndTime
! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'brp` 8/24/2003 19:09'!fromSeconds: seconds
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| since |
	since _ Duration days: SqueakEpoch hours: 0 minutes: 0 seconds: seconds.
	^ self basicNew
		ticks: since ticks offset: self localOffset;
		yourself.
! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 00:00'!millisecondClockValue	^ Time millisecondClockValue! !!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 00:01'!totalSeconds	^ Time totalSeconds! !!DateAndTime class methodsFor: 'initialize-release' stamp: 'brp 9/25/2003 10:59'!initialize

	super initialize.
	LastTickSemaphore _ Semaphore forMutualExclusion.
	FirstMilliSecondValue _ DateAndTime millisecondClockValue.
	LastTick _ 0.
	Smalltalk addToStartUpList: self.! !!DateAndTime class methodsFor: 'initialize-release' stamp: 'brp 9/25/2003 10:28'!startUp
	"This message is sent to registered classes when the system is coming up."
	super startUp.
	self initialize.! !!Delay class methodsFor: 'instance creation' stamp: 'brp 9/25/2003 13:43'!forDuration: aDuration

	^ self forMilliseconds: aDuration asMilliSeconds
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:59'!* operand
	"operand is a Number" 	^ self class nanoSeconds: ( (self asNanoSeconds * operand) asInteger).
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:59'!+ operand

	"operand is a Duration" 	^ self class nanoSeconds: (self asNanoSeconds + operand asNanoSeconds)
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:59'!- operand
	"operand is a Duration" 	^ self + operand negated
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:00'!/ operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self asNanoSeconds / operand) asInteger ]
		ifFalse: [ self asNanoSeconds / operand asDuration asNanoSeconds ]
.
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:00'!< comparand

	^ self asNanoSeconds < comparand asNanoSeconds
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 1/9/2004 06:25'!= comparand 
	"Answer whether the argument is a <Duration> representing the same 
	period of time as the receiver."

	^ self == comparand
		ifTrue: [true]
		ifFalse: 
			[self species = comparand species 
				ifTrue: [self asNanoSeconds = comparand asNanoSeconds]
				ifFalse: [false] ]! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!abs

	^ self class seconds: seconds abs nanoSeconds: nanos abs
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!asDuration

	^ self
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!asSeconds


	^ seconds
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 1/7/2004 16:20'!days

	"Answer the number of days the receiver represents."

	^ seconds quo: SecondsInDay
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!hash 	^seconds bitXor: nanos
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!hours
	"Answer the number of hours the receiver represents."


	^ (seconds rem: SecondsInDay) quo: SecondsInHour
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!minutes

	"Answer the number of minutes the receiver represents."


	^ (seconds rem: SecondsInHour) quo: SecondsInMinute
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!negated

	^ self class seconds: seconds negated nanoSeconds: nanos negated
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!negative


	^ self positive not
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!positive


	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]
! !!Duration methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 10:03'!seconds
	"Answer the number of seconds the receiver represents."

	^ (seconds rem: SecondsInMinute) + (nanos / NanosInSecond)! !!Duration methodsFor: 'private' stamp: 'brp 7/27/2003 15:08'!seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Duration class"

	seconds _ secondCount.
	nanos _ nanoCount! !!Duration methodsFor: 'private' stamp: 'brp 9/25/2003 14:42'!storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self className;
		nextPutAll: ' seconds: ';
		print: seconds;
		nextPutAll: ' nanoSeconds: ';
		print: nanos;
		nextPut: $).
! !!Duration methodsFor: 'private' stamp: 'brp 8/23/2003 20:31'!ticks
	"Answer an array {days. seconds. nanoSeconds}. Used by DateAndTime and Time"

	^ Array 
		with: self days
		with: (self hours * 3600) + (self minutes * 60 ) + (self seconds truncated)
		with: self nanoSeconds! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 14:29'!// operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self asNanoSeconds // operand) asInteger ]
		ifFalse: [ self asNanoSeconds // operand asDuration asNanoSeconds ]
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 15:07'!\\ operand

	"modulo. Remainder defined in terms of //. Answer a Duration with the 
	same sign as aDuration. operand is a Duration or a Number."

	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self asNanoSeconds \\ operand) ]
		ifFalse: [ self - (operand * (self // operand)) ]
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:42'!asDelay

	^ Delay forDuration: self! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:03'!asMilliSeconds


	^ ((seconds * NanosInSecond) + nanos) // (10 raisedToInteger: 6)
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:03'!asNanoSeconds

	^ (seconds * NanosInSecond) + nanos
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:03'!nanoSeconds


	^ nanos
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:22'!printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]" 	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h < 10 ifTrue: [ aStream nextPut: $0. ].
	h printOn: aStream. aStream nextPut: $:.
	m < 10 ifTrue: [ aStream nextPut: $0. ].
	m printOn: aStream. aStream nextPut: $:.
	s < 10 ifTrue: [ aStream nextPut: $0. ].
	s printOn: aStream.
	n = 0 ifFalse:
		[ | z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c asciiValue > $0 asciiValue ].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 15:42'!roundTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."

	^ self class nanoSeconds: (self asNanoSeconds roundTo: aDuration asNanoSeconds)

! !!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 15:38'!truncateTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes."

	^ self class
		nanoSeconds: (self asNanoSeconds truncateTo: aDuration asNanoSeconds)

! !!Duration class methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:55'!days: days hours: hours minutes: minutes seconds: seconds

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: 0.! !!Duration class methodsFor: 'ansi protocol' stamp: 'brp 7/27/2003 15:02'!seconds: aNumber

	^ self days: 0 hours: 0 minutes: 0 seconds: aNumber nanoSeconds: 0! !!Duration class methodsFor: 'ansi protocol' stamp: 'brp 7/27/2003 15:03'!zero

	^ self days: 0 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:00'!days: aNumber

	^ self days: aNumber hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 1/7/2004 15:38'!days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos 	^ self nanoSeconds: 
			( ( (days * SecondsInDay) 
				+ (hours * SecondsInHour)
					+ (minutes * SecondsInMinute) 
						+ seconds ) * NanosInSecond )
							+ nanos.
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 5/16/2003 11:29'!fromString: aString


	^ self readFrom: (ReadStream on: aString)
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:00'!hours: aNumber


	^ self days: 0 hours: aNumber minutes: 0 seconds: 0 nanoSeconds: 0! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:04'!milliSeconds: milliCount


	^ self days: 0 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 
			(milliCount * (10 raisedToInteger: 6))
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:01'!minutes: aNumber

	^ self days: 0 hours: 0 minutes: aNumber seconds: 0 nanoSeconds: 0! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 1/9/2004 17:20'!month: aMonth	"aMonth is an Integer or a String"		^ (Month month: aMonth year: Year current year) duration! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 5/21/2003 08:27'!nanoSeconds: nanos

	^ self new
		seconds: (nanos quo: NanosInSecond) 
		nanoSeconds: (nanos rem: NanosInSecond) rounded;
		yourself.
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 12:47'!readFrom: aStream
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	To assiste DateAndTime>>#readFrom: SS may be unpadded or absent."

	| sign days hours minutes seconds nanos ws ch |
	sign _ (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].

	days _ (aStream upTo: $:) asInteger sign: sign.
	hours _ (aStream upTo: $:) asInteger sign: sign.
	minutes _ (aStream upTo: $:) asInteger sign: sign.

	aStream atEnd 
		ifTrue: [seconds _ 0. nanos _ 0]
		ifFalse: 
			[ ws _ String new writeStream.
			[ch _ aStream next. (ch isNil) | (ch = $.)]
				whileFalse: [ ws nextPut: ch ].
			seconds _ ws contents asInteger sign: sign.
			ws reset.
			9 timesRepeat: 
				[ ch _ aStream next. 
				ws nextPut: (ch ifNil: [$0] ifNotNil: [ch]) ].
			nanos _ ws contents asInteger sign: sign].

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos.

	"	'0:00:00:00' asDuration
		'0:00:00:00.000000001' asDuration
		'0:00:00:00.999999999' asDuration
		'0:00:00:00.100000000' asDuration
		'0:00:00:00.10' asDuration
		'0:00:00:00.1' asDuration
		'0:00:00:01' asDuration
		'0:12:45:45' asDuration
		'1:00:00:00' asDuration
		'365:00:00:00' asDuration
		'-7:09:12:06.10' asDuration
		'+0:01:02' asDuration
		'+0:01:02:3' asDuration
 	"
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 15:01'!seconds: seconds nanoSeconds: nanos

	^ self days: 0 hours: 0 minutes: 0 seconds: seconds nanoSeconds: nanos
! !!Duration class methodsFor: 'squeak protocol' stamp: 'brp 8/6/2003 18:54'!weeks: aNumber

	^ self days: (aNumber * 7) hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0
! !!Number methodsFor: 'converting' stamp: 'brp 5/13/2003 10:13'!asDuration

	^ Duration nanoSeconds: self asInteger
! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:12'!day

	^ self sign days! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 07:56'!days

	^ Duration days: self! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:28'!hour

	^ self sign hours
! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 07:56'!hours

	^ Duration hours: self! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:26'!milliSecond

	^ self sign milliSeconds
! !!Number methodsFor: 'converting' stamp: 'brp 9/25/2003 13:16'!milliSeconds

	^ Duration milliSeconds: self
! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:16'!minute

	^ self sign minutes
! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 07:56'!minutes

	^ Duration minutes: self! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:27'!nanoSecond

	^ self sign nanoSeconds
! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 08:52'!nanoSeconds

	^ Duration nanoSeconds: self.! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:17'!second

	^ self sign seconds
! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 07:57'!seconds

	^ Duration seconds: self! !!Number methodsFor: 'converting' stamp: 'brp 5/21/2003 08:20'!sign: aNumber
	"Return a Number with the same sign as aNumber"

	^ aNumber positive ifTrue: [self abs] ifFalse: [self abs negated].! !!Number methodsFor: 'converting' stamp: 'brp 1/9/2004 06:19'!week

	^ self sign weeks
! !!Number methodsFor: 'converting' stamp: 'brp 5/16/2003 07:57'!weeks

	^ Duration weeks: self! !!String methodsFor: 'converting' stamp: 'brp 7/27/2003 17:28'!asDateAndTime

	"Convert from UTC format" 	^ DateAndTime fromString: self! !!String methodsFor: 'converting' stamp: 'brp 5/16/2003 11:59'!asDuration
	"convert from [nnnd]hh:mm:ss[.nanos] format. [] implies optional elements"

	^ Duration fromString: self
! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:28'!abbreviation

	^ abbreviation
! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:28'!abbreviation: aString

	abbreviation _ aString
! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:29'!name

	^ name
! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:28'!name: aString

	name _ aString
! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:28'!offset

	^ offset! !!TimeZone methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:28'!offset: aDuration

	offset _ aDuration! !!TimeZone methodsFor: 'private' stamp: 'brp 9/4/2003 06:37'!printOn: aStream

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self abbreviation;
		nextPut: $).! !!TimeZone class methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:38'!default
	"Answer the default time zone - GMT"

	^ self timeZones detect: [ :tz | tz offset = Duration zero ]
! !!TimeZone class methodsFor: 'accessing' stamp: 'brp 9/4/2003 06:35'!timeZones

	^ {
		self offset:  0 hours name: 'Grenwich Mean Time' abbreviation: 'GMT'.
		self offset:  0 hours name: 'British Summer Time' abbreviation: 'BST'.
		self offset:  2 hours name: 'South African Standard Time' abbreviation: 'SAST'.
	  }
! !!TimeZone class methodsFor: 'instance creation' stamp: 'brp 9/4/2003 06:33'!offset: aDuration name: aName abbreviation: anAbbreviation

	^ self new
		offset: aDuration;
		name: aName;
		abbreviation: anAbbreviation;
		yourself! !DateAndTime initialize!!DateAndTime class reorganize!('ansi protocol' clockPrecision now year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)('squeak protocol' current date:time: epoch fromString: julianDayNumber: localOffset localTimeZone localTimeZone: midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)('smalltalk-80' fromSeconds: millisecondClockValue totalSeconds)('initialize-release' initialize startUp)!ChronologyConstants initialize!