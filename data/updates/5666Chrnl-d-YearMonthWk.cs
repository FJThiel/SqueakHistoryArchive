'From Squeak3.7alpha of 11 September 2003 [latest update: #5623] on 16 January 2004 at 2:31:58 pm'!"Change Set:		Chronology-d-YearMonthWeekRefactoringDate:			24 August 2003Author:			Brent PinkneyThis change set refactors the existing 3.6 classes Week and Month. It also introduces the new Timespan, Year, and Schedule classes."!!Week class methodsFor: 'deprecated' stamp: 'brp 8/5/2003 19:09'!startMonday	^ true! !!Week class methodsFor: 'deprecated' stamp: 'brp 8/5/2003 19:11'!toggleStartMonday! !Object subclass: #Stopwatch	instanceVariableNames: 'timespans state'	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Chronology'!Magnitude subclass: #Timespan	instanceVariableNames: 'start duration'	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Chronology'!!Timespan commentStamp: 'brp 5/13/2003 08:07' prior: 0!I represent a duration starting on a specific DateAndTime.
!Timespan subclass: #Month	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!Month commentStamp: 'brp 5/13/2003 09:48' prior: 0!I represent a month.!Timespan subclass: #Schedule	instanceVariableNames: 'schedule'	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Chronology'!!Schedule commentStamp: 'brp 5/13/2003 09:48' prior: 0!I represent a powerful class for implementing recurring schedules.!Timespan subclass: #Week	instanceVariableNames: ''	classVariableNames: 'StartDay '	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!!Week commentStamp: 'brp 5/13/2003 09:48' prior: 0!I represent a week.!Timespan subclass: #Year	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Chronology'!!Year commentStamp: '<historical>' prior: 0!I represent a year.
!!Integer methodsFor: 'converting' stamp: 'brp 5/13/2003 10:12'!asYear
	^ Year year: self 
! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:12'!activate	self isSuspended ifTrue:		[self timespans add: 			(Timespan starting: DateAndTime now duration: Duration zero).		self state: #active]! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:45'!duration	| ts last |	self isSuspended 		ifTrue:			[ (ts _ self timespans) isEmpty ifTrue: 				[ ts _ { Timespan starting: DateAndTime now duration: Duration zero } ] ]		ifFalse:			[ last _ self timespans last.			ts _ self timespans allButLast				add: (last duration: (DateAndTime now - last start); yourself);				yourself ].			^ (ts collect: [ :t | t duration ]) sum! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 11:21'!end	^ self timespans last next! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:48'!isActive	^ self state = #active! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:48'!isSuspended	^ self state = #suspended! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:25'!printOn: aStream	super printOn: aStream.	aStream		nextPut: $(;		nextPutAll: self state;		nextPut: $:;		print: self duration;		nextPut: $).! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 12:03'!reActivate	self 		suspend;		activate.! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 11:54'!reset	self suspend.	timespans _ nil.! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:18'!start	^ self timespans first start! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:47'!state	^ state ifNil: [ state _ #suspended ]! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:46'!state: aSymbol	state _ aSymbol! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 23:13'!suspend	| ts |	self isActive ifTrue:		[ ts _ self timespans last.		ts duration: (DateAndTime now - ts start).		self state: #suspended]! !!Stopwatch methodsFor: 'squeak protocol' stamp: 'brp 9/24/2003 22:44'!timespans	^ timespans ifNil: [ timespans _ OrderedCollection new ]! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 9/15/2003 14:05'!+ operand	"operand conforms to protocol Duration"
		^ self class starting: (self start + operand) duration: self duration! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 9/15/2003 14:07'!- operand	"operand conforms to protocol DateAndTime or protocol Duration"	^ (operand respondsTo: #asDateAndTime)
	 	ifTrue: [ self start - operand ]	
	ifFalse: [ self + (operand negated) ].
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:43'!< comparand	^ self start < comparand	
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:43'!= comparand	^ (self start = comparand start) and: [self duration = comparand duration]
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 7/27/2003 17:49'!dayOfMonth	"Answer the day of the month represented by the receiver."	^ start dayOfMonth! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 8/6/2003 18:42'!dayOfWeek	"Answer the day of the week represented by the receiver."	^ start dayOfWeek! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 8/6/2003 18:42'!dayOfWeekName	"Answer the day of the week represented by the receiver."	^ start dayOfWeekName! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 8/24/2003 11:50'!dayOfYear	"Answer the day of the year represented by the receiver."	^ start dayOfYear! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:44'!hash	^ start hash + duration hash
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:44'!isLeapYear	^ start isLeapYear
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:44'!month	^ start month
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 1/7/2004 16:25'!monthAbbreviation
	^ start monthAbbreviation
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:44'!monthName
	^ start monthName
! !!Timespan methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:44'!year
	^ start year
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:44'!asDate
	^ start asDate
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:44'!asDateAndTime	^ start
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/30/2003 00:10'!asDuration	^ self duration! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:45'!asMonth
	^ start asMonth
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:45'!asTime	^ start asTime! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:25'!asTimeStamp	^ start asTimeStamp! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:45'!asWeek	^ start asWeek
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:45'!asYear
	^ start asYear! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:45'!duration
	"Answer the Duration of this timespan"	^ duration
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:45'!end
	^ self next start - DateAndTime clockPrecision 
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 1/7/2004 16:05'!includes: aDateAndTime
	^ (aDateAndTime isKindOf: Timespan)			ifTrue: [ (self includes: aDateAndTime start)						and: [ self includes: aDateAndTime end ] ]			ifFalse: [ aDateAndTime asDateAndTime between: start and: self end ]! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:54'!includesAllOf: aCollection 	"Answer whether all the elements of aCollection are in the receiver."	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].	^ true! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 1/7/2004 15:59'!includesAnyOf: aCollection 	"Answer whether any element of aCollection is included in the receiver"	aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].	^false! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:47'!intersection: aTimespan
	 "Return the Timespan both have in common, or nil"	 | aBegin anEnd |
	 aBegin _ self start max: aTimespan start.
	 anEnd _ self end min: aTimespan end.
	 anEnd < aBegin ifTrue: [^nil].
	 ^ self class starting: aBegin ending: anEnd.
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:47'!julianDayNumber
	^ start julianDayNumber
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 09:17'!printOn: aStream
	super printOn: aStream.	aStream 		nextPut: $(;		print: start;		nextPut: $D;		print: duration;		nextPut: $).! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:48'!start
	"Answer the start DateAndTime of this timespan"	^ start
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:48'!start: aDateAndTime	"Store the start DateAndTime of this timespan"	start _ aDateAndTime asDateAndTime
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:49'!to: anEnd	"Answer an Timespan. anEnd must be aDateAndTime or a Timespan"
	^ Timespan starting: (self start) ending: (anEnd asDateAndTime).
! !!Timespan methodsFor: 'squeak protocol' stamp: 'brp 1/9/2004 16:46'!union: aTimespan
	 "Return the Timespan spanned by both"	| aBegin anEnd |
	aBegin _ self start min: aTimespan start.	anEnd _ self end max: aTimespan end.	^ Timespan starting: aBegin ending: (anEnd + DateAndTime clockPrecision).
! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:49'!dates
	| dates |
	dates _ OrderedCollection new.
	self datesDo: [ :m | dates add: m ].	^ dates asArray.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:49'!datesDo: aBlock
	self do: aBlock with: start asDate.
! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:50'!every: aDuration do: aBlock	| element end |
	element _ self start.
	end _ self end.
	[ element <= end ] whileTrue:	
	[ aBlock value: element.
		element _ element + aDuration. ]
! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:50'!months
	| months |
	months _ OrderedCollection new: 12.	self monthsDo: [ :m | months add: m ].
	^ months asArray.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:50'!monthsDo: aBlock
	self do: aBlock with: start asMonth.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:51'!weeks
	| weeks |
	weeks _ OrderedCollection new.	self weeksDo: [ :m | weeks add: m ].
	^ weeks asArray.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:51'!weeksDo: aBlock	self do: aBlock with: self asWeek.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:51'!workDatesDo: aBlock
	"Exclude Saturday and Sunday"	self do: aBlock with: start asDate when: [ :d | d dayOfWeek < 6 ].
! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:51'!years
	| years |
	years _ OrderedCollection new.
	self yearsDo: [ :m | years add: m ].
	^ years asArray.! !!Timespan methodsFor: 'enumerating' stamp: 'brp 5/13/2003 08:58'!yearsDo: aBlock	self do: aBlock with: start asYear.! !!Timespan methodsFor: 'private' stamp: 'brp 5/13/2003 08:58'!do: aBlock with: aFirstElement
	self do: aBlock with: aFirstElement when: [ :t | true ].
! !!Timespan methodsFor: 'private' stamp: 'brp 5/13/2003 08:59'!do: aBlock with: aFirstElement when: aConditionBlock	| element end |	element _ aFirstElement.	end _ self end.
	[ element start <= end ] whileTrue:	
	[(aConditionBlock value: element)			ifTrue: [ aBlock value: element ].		element _ element next. ]! !!Timespan methodsFor: 'private' stamp: 'brp 5/13/2003 08:59'!duration: aDuration	"Set the Duration of this timespan"
	duration _ aDuration
! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 14:09'!day	"Answer the day of the year represented by the receiver."	^ self dayOfYear! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 08:45'!daysInMonth
	^ start daysInMonth
! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 08:45'!daysInYear
	"Answer the number of days in the month represented by the receiver."	^ start daysInYear
! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 17:50'!daysLeftInYear	^ start daysLeftInYear! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 17:55'!firstDayOfMonth	^ start firstDayOfMonth! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 08:47'!monthIndex	^ self month
! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 08:47'!next	^ self class starting: (start + duration) duration: duration
! !!Timespan methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 08:48'!previous
	^ self class starting: (start - duration) duration: duration
! !!Timespan methodsFor: 'deprecated' stamp: 'brp 8/5/2003 22:18'!firstDate	self deprecated: 'Use #start'.	^ self start asDate! !!Timespan methodsFor: 'deprecated' stamp: 'brp 8/5/2003 22:19'!lastDate 	self deprecated: 'Use #end'.	^ self end asDate! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:04'!asMonth	^ self
! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:05'!daysInMonth	^ self duration days.! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:05'!index	^ self monthIndex
! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:05'!name
	^ self monthName
! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:05'!previous
	^ self class starting: (self start - 1)
! !!Month methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:05'!printOn: aStream
	aStream nextPutAll: self monthName, ' ', self year printString.! !!Month methodsFor: 'deprecated' stamp: 'brp 8/5/2003 22:08'!eachWeekDo: aBlock	self deprecated: 'Use #weeksDo:'.	self weeksDo: aBlock! !!Schedule methodsFor: 'enumerating' stamp: 'brp 5/13/2003 09:50'!between: aStart and: anEnd do: aBlock
	| element end i |	end _ self end min: anEnd.	element _ self start.
		i _ 1.	[ element < aStart ] whileTrue:	
	[ element _ element + (schedule at: i).
		i _ i + 1. (i > schedule size) ifTrue: [i _ 1]].
	i _ 1.
	[ element <= end ] whileTrue:	
	[ aBlock value: element.		element _ element + (schedule at: i).
		i _ i + 1.		(i > schedule size) ifTrue: [i _ 1]]
.! !!Schedule methodsFor: 'enumerating' stamp: 'brp 5/13/2003 09:50'!dateAndTimes	| dateAndTimes |	dateAndTimes _ OrderedCollection new.	self scheduleDo: [ :e | dateAndTimes add: e ].	^ dateAndTimes asArray.! !!Schedule methodsFor: 'enumerating' stamp: 'brp 5/13/2003 09:50'!schedule	^ schedule
! !!Schedule methodsFor: 'enumerating' stamp: 'brp 5/13/2003 09:50'!schedule: anArrayOfDurations	schedule _ anArrayOfDurations
! !!Schedule methodsFor: 'enumerating' stamp: 'brp 5/13/2003 09:51'!scheduleDo: aBlock	self between: (self start) and: (self end) do: aBlock.
! !!Schedule methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:38'!includes: aDateAndTime
	| dt |
	dt _ aDateAndTime asDateAndTime.
	self scheduleDo: [ :e | e = dt ifTrue: [^true] ].
	^ false.
! !!Timespan class methodsFor: 'squeak protocol' stamp: 'brp 5/21/2003 08:35'!current
	^ self starting: DateAndTime now
! !!Timespan class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 18:49'!new	"Answer a Timespan starting on the Squeak epoch: 1 January 1901"	^ self starting: DateAndTime new! !!Timespan class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:42'!starting: aDateAndTime
	^ self starting: aDateAndTime duration: Duration zero
! !!Timespan class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 18:48'!starting: aDateAndTime duration: aDuration	^ self basicNew
 		start: aDateAndTime asDateAndTime;
		duration: aDuration;		yourself.! !!Timespan class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:16'!starting: startDateAndTime ending: endDateAndTime	^ self 		starting: startDateAndTime 		duration: (endDateAndTime asDateAndTime - startDateAndTime).
! !!Timespan class methodsFor: 'deprecated' stamp: 'brp 8/5/2003 22:21'!fromDate: aDate	^ self		deprecated: 'Use #starting: ';		starting: aDate! !!Month class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:22'!month: month year: year	"Create a Month for the given <year> and <month>.	<month> may be a number or a String with the	name of the month. <year> should be with 4 digits."	^ self starting: (DateAndTime year: year month: month day: 1)! !!Month class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:21'!readFrom: aStream	| m y c |
	m _ (ReadWriteStream with: '') reset.
	[(c _ aStream next) isSeparator] whileFalse: [m nextPut: c].
	[(c _ aStream next) isSeparator] whileTrue.
	y _ (ReadWriteStream with: '') reset.
	y nextPut: c.
	[aStream atEnd] whileFalse: [y nextPut: aStream next].
	^ self 		month: m contents		year: y contents"Month readFrom: 'July 1998' readStream"! !!Month class methodsFor: 'squeak protocol' stamp: 'brp 7/1/2003 13:59'!starting: aDateAndTime duration: aDuration 	"Override - a each month has a defined duration"	| start adjusted days |	start _ aDateAndTime asDateAndTime.	adjusted _ DateAndTime				year: start year				month: start month				day: 1.	days _ self daysInMonth: adjusted month forYear: adjusted year.	^ super		starting: adjusted		duration: (Duration days: days)! !!Month class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:27'!daysInMonth: indexOrName forYear: yearInteger 	| index |	index _ indexOrName isInteger 				ifTrue: [indexOrName]				ifFalse: [self indexOfMonth: indexOrName].	^ (DaysInMonth at: index)			+ ((index = 2					and: [Year isLeapYear: yearInteger])						ifTrue: [1] ifFalse: [0])! !!Month class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 09:29'!indexOfMonth: aMonthName
	1 to: 12 do: [ :i |  (aMonthName, '*' match: (MonthNames at: i)) ifTrue: [^i] ].
	self error: aMonthName , ' is not a recognized month name'.! !!Month class methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 09:02'!nameOfMonth: anIndex	^ MonthNames at: anIndex.! !!Week methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:36'!asWeek	^ self
! !!Week methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:37'!printOn: aStream	aStream nextPutAll: 'a Week starting: '.	self start printOn: aStream.
! !!Week methodsFor: 'deprecated' stamp: 'brp 8/5/2003 22:17'!do: aBlock	self deprecated: 'Use #datesDo:'.	self datesDo: aBlock! !!Week methodsFor: 'deprecated' stamp: 'brp 8/6/2003 18:39'!index	self deprecated: 'obsolete'.	^ self indexInMonth: self asMonth ! !!Week methodsFor: 'deprecated' stamp: 'brp 8/6/2003 18:42'!indexInMonth: aMonth	"1=first week, 2=second week, etc."	self deprecated: 'obsolete'.	^ (Date dayOfWeek: aMonth dayOfWeekName) + self dayOfMonth - 2  // 7 + 1! !!Week class methodsFor: 'deprecated' stamp: 'brp 8/5/2003 19:09'!startMonday	self deprecated: 'Use #startDay'.	^ self startDay = #Monday! !!Week class methodsFor: 'deprecated' stamp: 'brp 8/5/2003 19:11'!toggleStartMonday	self deprecated: 'Use #startDay:'.	(self startDay = #Monday)		ifTrue: [ self startDay: #Sunday ]		ifFalse: [ self startDay: #Monday ]! !!Week class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:34'!dayNames	^ DayNames
! !!Week class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:34'!indexOfDay: aSymbol	^ DayNames indexOf: aSymbol
! !!Week class methodsFor: 'squeak protocol' stamp: 'brp` 8/24/2003 19:38'!startDay	^ StartDay
ifNil: [ StartDay
 _ DayNames first ]! !!Week class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 09:30'!startDay: aSymbol	(DayNames includes: aSymbol)		ifTrue: [ StartDay _ aSymbol ]		ifFalse: [ self error: aSymbol, ' is not a recognised day name' ]
! !!Week class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:42'!starting: aDateAndTime
 duration: aDuration	"Override - the duration is always one week.	 Week will start from the Week class>>startDay"	| midnight delta adjusted |	midnight _ aDateAndTime asDateAndTime midnight.
	delta _ ((midnight dayOfWeek + 7 - (DayNames indexOf: StartDay)) rem: 7) abs.
	adjusted _ midnight - (Duration days: delta hours: 0 minutes: 0 seconds: 0).	^ super starting: adjusted duration: (Duration weeks: 1).! !!Week class methodsFor: 'smalltalk-80' stamp: 'brp 5/13/2003 09:34'!nameOfDay: anIndex	^ DayNames at: anIndex
! !!Year methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:01'!asYear
	^ self
! !!Year methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:01'!daysInMonth
	self shouldNotImplement 
! !!Year methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:01'!daysInYear	^ self duration days.! !!Year methodsFor: 'squeak protocol' stamp: 'brp 5/21/2003 08:38'!printOn: aStream	aStream nextPutAll: 'a Year ('.
	self start year printOn: aStream.
	aStream nextPutAll: ')'.! !!Year class methodsFor: 'squeak protocol' stamp: 'brp 9/11/2003 14:05'!current 	^ self year: (DateAndTime now year)! !!Year class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:00'!isLeapYear: aYearInteger
	| adjustedYear |
	adjustedYear _ aYearInteger > 0		ifTrue: [aYearInteger]		ifFalse: [(aYearInteger + 1) negated].
	"There was no year 0"
	^ ((adjustedYear \\ 4 ~= 0) or: [(adjustedYear \\ 100 = 0) and: [adjustedYear \\ 400 ~= 0]]) not.! !!Year class methodsFor: 'squeak protocol' stamp: 'brp 7/1/2003 13:53'!starting: aDateAndTime duration: aDuration 	"Override - start from midnight"	| midnight |	midnight _ aDateAndTime asDateAndTime midnight.	^ super		starting: midnight		duration: (Duration days: (self daysInYear: midnight year)).! !!Year class methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 09:00'!year: aYear	^ self starting: (DateAndTime year: aYear month: 1 day: 1).! !!Year class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:52'!daysInYear: yearInteger	^ 365 + ((self isLeapYear: yearInteger) ifTrue: [1] ifFalse: [0]).! !!Year class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:55'!leapYear: yearInteger 	^ (self isLeapYear: yearInteger)		ifTrue: [1]		ifFalse: [0]! !Week class removeSelector: #fromDate:!Week removeSelector: #asDate!Week removeSelector: #duration!Week removeSelector: #firstDate!Week removeSelector: #lastDate!Week removeSelector: #next!Week removeSelector: #previous!Timespan subclass: #Week	instanceVariableNames: ''	classVariableNames: 'StartDay'	poolDictionaries: 'ChronologyConstants'	category: 'Kernel-Chronology'!Month class removeSelector: #fromDate:!Month removeSelector: #asDate!Month removeSelector: #duration!Month removeSelector: #firstDate!Month removeSelector: #lastDate!Month removeSelector: #next!!Stopwatch reorganize!('squeak protocol' activate duration end isActive isSuspended printOn: reActivate reset start state state: suspend timespans)!