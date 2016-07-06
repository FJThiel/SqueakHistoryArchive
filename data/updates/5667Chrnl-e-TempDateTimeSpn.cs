'From Squeak3.6beta of ''4 July 2003'' [latest update: #5395] on 24 August 2003 at 7:35:04 pm'!"Change Set:		Chronology-e-TemporaryDateTimeTimeSpanDate:			24 August 2003Author:			Brent PinkneyThis changeset introduces temporary TDate, TTime, TTimeStamp classes which will eventually replace the existing 3.6 classes.This allows the new system to be developed without affecting the existing system"!Timespan subclass: #TDate	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: 'ChronologyConstants '	category: 'Kernel-Chronology'!Magnitude subclass: #TTime	instanceVariableNames: 'seconds nanos '	classVariableNames: ''	poolDictionaries: 'ChronologyConstants '	category: 'Kernel-Chronology'!DateAndTime subclass: #TTimeStamp	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Chronology'!!Object methodsFor: '*kernel-chronology' stamp: 'brp 8/23/2003 23:45'!chronologyMigrationClassPrefix	^ Smalltalk at: #ChronologyMigrationInProgress ifAbsent: [''].! !!Object methodsFor: '*kernel-chronology' stamp: 'brp 8/23/2003 23:41'!dateClass	^ self class environment at: 		(self chronologyMigrationClassPrefix, #Date) asSymbol.! !!Object methodsFor: '*kernel-chronology' stamp: 'brp 8/23/2003 23:41'!timeClass	^ self class environment at: 		(self chronologyMigrationClassPrefix, #Time) asSymbol.! !!Object methodsFor: '*kernel-chronology' stamp: 'brp 8/23/2003 23:41'!timeStampClass	^ self class environment at: 		(self chronologyMigrationClassPrefix, #TimeStamp) asSymbol.! !!DateAndTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 22:37'!asTime
	^ self timeClass seconds: seconds nanoSeconds: nanos! !!String methodsFor: 'converting' stamp: 'brp 8/23/2003 22:03'!asDate	"Many allowed forms, see Date>>#readFrom:"	^ self dateClass fromString: self! !!String methodsFor: 'converting' stamp: 'brp 8/23/2003 22:23'!asTime	"Many allowed forms, see Time>>readFrom:"	^ self timeClass fromString: self.! !!TDate methodsFor: 'printing' stamp: 'brp 7/27/2003 16:07'!mmddyyyy
	"Answer the receiver rendered in standard U.S.A format mm/dd/yyyy.	Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, 	so that for example February 1 1996 is 2/1/96"


	^ self printFormat: #(2 1 3 $/ 1 1)! !!TDate methodsFor: 'printing' stamp: 'brp 7/27/2003 16:06'!printFormat: formatArray 
	"Answer a String describing the receiver using the argument formatArray."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream format: formatArray.
	^ aStream contents! !!TDate methodsFor: 'printing' stamp: 'BP 3/23/2001 12:27'!printOn: aStream

	self printOn: aStream format: #(1 2 3 $  3 1 )! !!TDate methodsFor: 'printing' stamp: 'brp 7/27/2003 16:05'!printOn: aStream format: formatArray 	"Print a description of the receiver on aStream using the format 	denoted the argument, formatArray: 	
		#(item item item sep monthfmt yearfmt twoDigits) 	
		items: 1=day 2=month 3=year will appear in the order given, 	
		separated by sep which is eaither an ascii code or character. 	
		monthFmt: 1=09 2=Sep 3=September 	
		yearFmt: 1=1996 2=96 	
		digits: (missing or)1=9 2=09. 	
	See the examples in printOn: and mmddyy"	| gregorian twoDigits element monthFormat |	gregorian _ self dayMonthYearDo: [ :d :m :y | {d. m. y} ].	twoDigits _ formatArray size > 6 and: [(formatArray at: 7) > 1].	1 to: 3 do: 		[ :i | 			element := formatArray at: i.			element = 1				ifTrue: [twoDigits						ifTrue: [aStream								nextPutAll: (gregorian first asString										padded: #left										to: 2										with: $0)]						ifFalse: [gregorian first printOn: aStream]].			element = 2				ifTrue: [monthFormat := formatArray at: 5.					monthFormat = 1						ifTrue: [twoDigits								ifTrue: [aStream										nextPutAll: (gregorian middle asString												padded: #left												to: 2												with: $0)]								ifFalse: [gregorian middle printOn: aStream]].					monthFormat = 2						ifTrue: [aStream								nextPutAll: ((Month nameOfMonth: gregorian middle)										copyFrom: 1										to: 3)].					monthFormat = 3						ifTrue: [aStream								nextPutAll: (Month nameOfMonth: gregorian middle)]].			element = 3				ifTrue: [(formatArray at: 6)							= 1						ifTrue: [gregorian last printOn: aStream]						ifFalse: [aStream								nextPutAll: ((gregorian last \\ 100) asString										padded: #left										to: 2										with: $0)]].			i < 3				ifTrue: [(formatArray at: 4)							~= 0						ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]]! !!TDate methodsFor: 'printing' stamp: 'BP 3/23/2001 12:27'!storeOn: aStream

	aStream print: self printString; nextPutAll: ' asDate'! !!TDate methodsFor: 'printing' stamp: 'brp 7/27/2003 16:04'!yyyymmdd
	"Format the date in ISO 8601 standard like '2002-10-22'."

	^ self printFormat: #(3 2 1 $- 1 1 2)! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 22:09'!addDays: dayCount 	^ (self asDateAndTime + (dayCount days)) asDate! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:08'!asSeconds
	"Answer the seconds since the Squeak epoch: 1 January 1901"

	^ start asSeconds! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:08'!leap	"Answer whether the receiver's year is a leap year."	^ start isLeapYear ifTrue: [1] ifFalse: [0].! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 1/16/2004 14:30'!previous: dayName 	"Answer the previous date whose weekday name is dayName."	| days |	days _ 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.	days = 0 ifTrue: [ days _ 7 ].	^ self subtractDays: days! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:09'!subtractDate: aDate 	"Answer the number of days between self and aDate"	^ (self start - aDate asDateAndTime) days! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 22:05'!subtractDays: dayCount 	^ (self asDateAndTime - (dayCount days)) asDate! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 12:04'!weekday	"Answer the name of the day of the week on which the receiver falls."	^ self dayOfWeekName! !!TDate methodsFor: 'smalltalk-80' stamp: 'brp 8/24/2003 12:04'!weekdayIndex	"Sunday=1, ... , Saturday=7"	^ self dayOfWeek! !!TDate methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:10'!asDate

	^ self! !!TDate methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:10'!dayMonthYearDo: aBlock 	"Supply integers for day, month and year to aBlock and return the result"	^ start dayMonthYearDo: aBlock! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:36'!asGregorian	"Return an array of integers #(dd mm yyyy)"	^ self		deprecated: 'Use #dayMonthYearDo:';		dayMonthYearDo: [ :d :m :y | { d. m. y } ] ! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:32'!asJulianDayNumber	^ self 		deprecated: 'Use #julianDayNumber';		julianDayNumber! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:36'!day: dayInteger year: yearInteger	^ self		deprecated: 'Obsolete'! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:39'!daylightSavingsInEffect	"Return true if DST is observed at or after 2am on this day"	self deprecated: 'Deprecated'.	self dayMonthYearDo: 		[ :day :month :year |		(month < 4 or: [month > 10]) ifTrue: [^ false].  "False November through March"		(month > 4 and: [month < 10]) ifTrue: [^ true].  "True May through September"		month = 4		ifTrue:	["It's April -- true on first Sunday or later"				day >= 7 ifTrue: [^ true].  "Must be after"				^ day > (self weekdayIndex \\ 7)]		ifFalse: ["It's October -- false on last Sunday or later".				day <= 24 ifTrue: [^ true].  "Must be before"				^ day <= (24 + (self weekdayIndex \\ 7))]]! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:39'!daylightSavingsInEffectAtStandardHour: hour	"Return true if DST is observed at this very hour (standard time)"	"Note: this *should* be the kernel method, and daylightSavingsInEffect		should simply be self daylightSavingsInEffectAtHour: 3"	self deprecated: 'Deprecated'.	self daylightSavingsInEffect		ifTrue: [^ (self addDays: -1) daylightSavingsInEffect or: [hour >= 2]]		ifFalse: [^ (self addDays: -1) daylightSavingsInEffect and: [hour < 1]]! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:37'!firstDayOfMonthIndex: monthIndex 	^ self		deprecated: 'Obsolete'! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:41'!julianDayNumber: anInteger	"Set the number of days elapsed since midnight GMT on January 1st, 4713 B.C."	self deprecated: 'Obsolete'.! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:34'!mmddyy	"Please use mmddyyyy instead, so dates in 2000 will be unambiguous"	^ self 		deprecated: 'Use #mmddyyyy';		printFormat: #(2 1 3 $/ 1 2)! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:46'!uniqueDateStringBetween: aStart and: anEnd	"Return a String, with just enough information to distinguish it from other dates in the range."	"later, be more sophisticated"	self deprecated: 'Deprecated'.	aStart year + 1 >= anEnd year ifFalse: [^ self printFormat: #(1 2 3 $  3 1)].	"full"	aStart week next >= anEnd week ifFalse: [^ self printFormat: #(2 1 9 $  3 1)]. "May 6"	^ self weekday! !!TDate methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:31'!week	^ self 		deprecated: 'Use #asWeek';		asWeek! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 21:48'!dateAndTimeNow
	"Answer an Array whose with Date today and Time now."

	^ self timeClass dateAndTimeNow! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:35'!dayOfWeek: dayName 	^ Week indexOfDay: dayName! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:59'!daysInMonth: monthName forYear: yearInteger 	^ Month daysInMonth: monthName forYear: yearInteger.! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:53'!daysInYear: yearInteger 	^ Year daysInYear: yearInteger.! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 1/16/2004 14:35'!firstWeekdayOfMonth: month year: year	"Answer the weekday index of the first day in <month> in the <year>."	^ (self newDay: 1 month: month year: year) weekdayIndex! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:01'!fromDays: dayCount 	"Days since 1 January 1901"	^ self julianDayNumber: dayCount + SqueakEpoch! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:02'!fromSeconds: seconds
	"Answer an instance of me which is 'seconds' seconds after January 1, 1901."

	^ self fromDays: ((Duration seconds: seconds) days)! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:39'!indexOfMonth: aMonthName 	^ Month indexOfMonth: aMonthName.! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:56'!leapYear: yearInteger 	^ Year leapYear: yearInteger! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:37'!nameOfDay: dayIndex 	^ Week nameOfDay: dayIndex ! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:40'!nameOfMonth: anIndex 	^ Month nameOfMonth: anIndex.! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:02'!newDay: day month: month year: year 

	^ self year: year month: month day: day! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:01'!newDay: dayCount year: yearInteger	^ self year: yearInteger day: dayCount! !!TDate class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:01'!today

	^ self current! !!TDate class methodsFor: 'deprecated' stamp: 'brp 8/4/2003 22:13'!absoluteDaysToYear: gregorianYear	self deprecated: 'Deprecated'! !!TDate class methodsFor: 'deprecated' stamp: 'brp 8/4/2003 22:14'!fromJulianDayNumber: aJulianDayNumber	self 		deprecated: 'Deprecated';		julianDayNumber: aJulianDayNumber! !!TDate class methodsFor: 'deprecated' stamp: 'brp 8/4/2003 22:15'!yearAndDaysFromDays: days into: aTwoArgBlock	self deprecated: 'Deprecated'! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 16:03'!fromString: aString
	"Answer an instance of created from a string with format dd.mm.yyyy."

	^ self readFrom: aString readStream.
! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 18:25'!julianDayNumber: aJulianDayNumber	^ self starting: (DateAndTime julianDayNumber: aJulianDayNumber)! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/1/2003 09:21'!readFrom: aStream 	"Read a Date from the stream in any of the forms:  	
		<day> <monthName> <year>		(5 April 1982; 5-APR-82)  	
		<monthName> <day> <year>		(April 5, 1982)  	
		<monthNumber> <day> <year>		(4/5/82) 			<day><monthName><year>			(5APR82)"	| day month year |	aStream peek isDigit		ifTrue: [day := Integer readFrom: aStream].	[aStream peek isAlphaNumeric]		whileFalse: [aStream skip: 1].	aStream peek isLetter		ifTrue: ["number/name... or name..."			month := WriteStream						on: (String new: 10).			[aStream peek isLetter]				whileTrue: [month nextPut: aStream next].			month := month contents.			day isNil				ifTrue: ["name/number..."					[aStream peek isAlphaNumeric]						whileFalse: [aStream skip: 1].					day := Integer readFrom: aStream]]		ifFalse: ["number/number..."			month := Month nameOfMonth: day.			day := Integer readFrom: aStream].	[aStream peek isAlphaNumeric]		whileFalse: [aStream skip: 1].	year := Integer readFrom: aStream.	year < 10 ifTrue: [year := 2000 + year] 		ifFalse: [ year < 1900 ifTrue: [ year := 1900 + year]].	^ self		year: year		month: month		day: day! !!TDate class methodsFor: 'squeak protocol' stamp: 'BP 3/23/2001 12:36'!starting: aDateAndTime

	^super starting: (aDateAndTime midnight) duration: (Duration days: 1)
! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/1/2003 18:09'!tomorrow	^ self today next! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 22:03'!year: year day: dayOfYear	^ self starting: (DateAndTime year: year day: dayOfYear)! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 22:02'!year: year month: month day: day

	^ self starting: (DateAndTime year: year month: month day: day)
! !!TDate class methodsFor: 'squeak protocol' stamp: 'brp 7/1/2003 18:09'!yesterday	^ self today previous! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 15:54'!< aTime	^ self asDuration < aTime asDuration! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:11'!= aTime	^ [ self ticks = aTime ticks ]		on: MessageNotUnderstood do: [false]! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:32'!duration	^ Duration zero
! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:11'!hash	^ self ticks hash
! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:10'!hour	^ self hour24
! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:11'!hour12	"Answer an <integer> between 1 and 12, inclusive, representing the hour 	of the day in the 12-hour clock of the local time of the receiver."	| h |	h _ (self hour24 abs + 1).	^ h > 12 ifTrue: [h - 12] ifFalse: [h].! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:17'!hour24
	^ self asDuration hours
! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:41'!meridianAbbreviation	^ self hour < 12 ifTrue: ['AM'] ifFalse: ['PM'].
! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 22:08'!minute	^ self asDuration minutes! !!TTime methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 19:19'!second
	^ self asDuration seconds! !!TTime methodsFor: 'deprecated' stamp: 'brp 8/23/2003 19:03'!hours: anInteger	self 		deprecated: 'Deprecated';		hours: anInteger minutes: 0 seconds: 0.! !!TTime methodsFor: 'deprecated' stamp: 'brp` 8/24/2003 19:27'!hours: hourInteger minutes: minInteger seconds: secInteger	self 		deprecated: 'Deprecated';		setSeconds: (hourInteger * SecondsInHour) + (minInteger * SecondsInMinute) + secInteger.		! !!TTime methodsFor: 'deprecated' stamp: 'brp` 8/24/2003 19:28'!setSeconds: secondCount	self 		deprecated: 'Deprecated'.	self ticks: { 0. secondCount. 0 }! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 15:25'!hhmm24
	"Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm), of exactly 4 digits"

	^(String streamContents: 
		[ :aStream | self print24: true showSeconds: false on: aStream ])
			copyWithout: $:! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 15:25'!print24
	"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"

	^String streamContents:
		[ :aStream | self print24: true on: aStream ]

! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 15:25'!print24: hr24 on: aStream 
	"Format is 'hh:mm:ss' or 'h:mm:ss am' "

	self print24: hr24 showSeconds: true on: aStream 
! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 16:11'!print24: hr24 showSeconds: showSeconds on: aStream 
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h _ self hour. m _ self minute. s _ self second.
	hr24	
	ifTrue: 			[ h < 10 ifTrue: [ aStream nextPutAll: '0' ].	
		h printOn: aStream ]	
	ifFalse:			[ h > 12		
		ifTrue: [h - 12 printOn: aStream]		
		ifFalse: 			
		[h < 1		
				ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ]]].

	aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
	m printOn: aStream.

	showSeconds ifTrue:	
	[ aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
		s printOn: aStream ].

	hr24 ifFalse:	
	[ aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ].
! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 15:25'!printMinutes
	"Return as string 'hh:mm pm'  "

	^String streamContents:
		[ :aStream | self print24: false showSeconds: false on: aStream ]
! !!TTime methodsFor: 'printing' stamp: 'brp 8/23/2003 19:14'!printOn: aStream 
	self print24: false showSeconds: (self seconds ~= 0) on: aStream! !!TTime methodsFor: 'printing' stamp: 'BP 3/30/2001 15:25'!storeOn: aStream

	aStream print: self printString; nextPutAll: ' asTime'! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 19:02'!addSeconds: nSeconds 	"Answer a Time that is nSeconds after the receiver."	^ self class seconds: self asSeconds + nSeconds! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 19:02'!addTime: timeAmount
	"Answer a Time that is timeInterval after the receiver. timeInterval is an 
	instance of Date or Time."

	^ self class seconds: self asSeconds + timeAmount asSeconds! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 15:55'!asSeconds
	"Answer the number of seconds since midnight of the receiver."

	^ seconds! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 7/1/2003 13:29'!hours	^ self hour! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 19:24'!intervalString	"Treat the time as a difference.  Give it in hours and minutes with two digits of accuracy."	| d |	d _ self asDuration.	^ String streamContents: [ :s |		d hours > 0 ifTrue: [s print: d hours; nextPutAll: ' hours'].		d minutes > 0 ifTrue: [s space; print: d minutes; nextPutAll: ' minutes'].		d seconds > 0 ifTrue: [s space; print: d seconds; nextPutAll: ' seconds'] ].! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 22:07'!minutes	^ self asDuration minutes! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 18:18'!seconds	^ self second! !!TTime methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 19:03'!subtractTime: timeAmount 	"Answer a Time that is timeInterval before the receiver. timeInterval is  	an instance of Date or Time."	^ self class seconds: self asSeconds - timeAmount asSeconds! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 21:46'!asDate	^ self dateClass today! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:26'!asDateAndTime	^ DateAndTime today + self! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:01'!asDuration
	"Answer the duration since midnight"	^ Duration seconds: seconds nanoSeconds: nanos
! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:29'!asMonth	^ self asDateAndTime asMonth! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:29'!asNanoSeconds	"Answer the number of nanoseconds since midnight"	^ self asDuration asNanoSeconds
! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:08'!asTime	^ self! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:27'!asTimeStamp	^ self asDateAndTime asTimeStamp! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:28'!asWeek	^ self asDateAndTime asWeek! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:43'!asYear	^ self asDateAndTime asYear! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:33'!nanoSecond
	^ nanos
! !!TTime methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:35'!to: anEnd	"Answer a Timespan. anEnd must respond to #asDateAndTime"	^ self asDateAndTime to: anEnd! !!TTime methodsFor: 'private' stamp: 'brp 8/23/2003 22:38'!ticks	"Answer an Array: { seconds. nanoSeconds }"	^ Array with: 0 with: seconds with: nanos.! !!TTime methodsFor: 'private' stamp: 'brp 8/23/2003 20:44'!ticks: anArray	"ticks is an Array: { days. seconds. nanoSeconds }"	seconds _ anArray second.	nanos _ anArray third.! !!TTime class methodsFor: 'benchmarks' stamp: 'brp 8/23/2003 21:49'!benchmarkMillisecondClock		"Time benchmarkMillisecondClock"
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
	time _ self timeClass millisecondClockValue.
	[temp1 < temp3]
		whileTrue:[temp1 _ temp1 + 1].
	time _ self timeClass millisecondClockValue - time.

	"And compute the number of microseconds spent per call to #millisecondClockValue"
	^((delayTime - time * 1000.0 / nLoops) truncateTo: 0.001) printString,
		' microseconds per call to Time>>millisecondClockValue'! !!TTime class methodsFor: 'benchmarks' stamp: 'BP 3/30/2001 15:25'!benchmarkPrimitiveResponseDelay	"Time benchmarkPrimitiveResponseDelay"
	"Benchmark the overhead for primitive dispatches with an active Delay.
	On the VM level, this tests the efficiency of ioLowResMSecs."

	"PII/400 Windows98: 0.128 microseconds per prim"

	"ar 9/6/1999: This value is *extremely* important for stuff like sockets etc.
	I had a bad surprise when Michael pointed this particular problem out:
	Using the hardcoded clock() call for ioLowResMSecs on Win32 resulted in an overhead
	of 157.4 microseconds per primitive call - meaning you can't get no more than
	approx. 6000 primitives per second on my 400Mhz PII system with an active delay!!
	BTW, it finally explains why Squeak seemed soooo slow when running PWS or 
	other socket stuff. The new version (not using clock() but some Windows function) 
	looks a lot better (see above; approx. 8,000,000 prims per sec with an active delay)."

	| nLoops bb index baseTime actualTime delayTime |
	delayTime _ 5000. "Time to run this test is approx. 3*delayTime"

	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	bb _ Array new: 1. "The object we send the prim message to"

	"Compute the # of loops we'll run in a decent amount of time"
	[(Delay forMilliseconds: delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	nLoops _ 0.
	[Delay anyActive] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		nLoops _ nLoops + 1.
	].

	"Flush the cache and make sure #basicSize is in there"
	Object flushCache.
	bb basicSize.

	"Now run the loop without any active delay
	for getting an idea about its actual speed."
	baseTime _ self millisecondClockValue.
	index _ nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index _ index - 1.
	].
	baseTime _ self millisecondClockValue - baseTime.

	"Setup the active delay but try to never make it active"
	[(Delay forMilliseconds: delayTime + delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	"And run the loop"
	actualTime _ self millisecondClockValue.
	index _ nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index _ index - 1.
	].
	actualTime _ self millisecondClockValue - actualTime.

	"And get us some result"
	^((actualTime - baseTime) * 1000 asFloat / (nLoops * 10) truncateTo: 0.001) printString,
		' microseconds overhead per primitive call'! !!TTime class methodsFor: 'general inquiries' stamp: 'BP 3/30/2001 15:25'!condenseBunches: aCollectionOfSeconds
	| secArray pause now out prev bunchEnd ago |
	"Identify the major intervals in a bunch of numbers.  	Each number is a seconds since 1901 that represents a date and time.	We want the last event in a bunch.  Return array of seconds for:	
	Every event in the last half hour.
		Every bunch separated by 30 min in the last 24 hours.	
	Every bunch separated by two hours before that."
	"Time condenseBunches: 
		(#(20 400 401  20000 20200 20300 40000 45000  200000 201000 202000) 			collect: [ :tt | self totalSeconds - tt])
"
	secArray _ aCollectionOfSeconds asSortedCollection.
	pause _ 1.
	now _ self totalSeconds.
	out _ OrderedCollection new.
	prev _ 0.
	bunchEnd _ nil.
	secArray reverseDo: [:secs | "descending"
		ago _ now - secs.
		ago > (60*30) ifTrue: [pause _ "60*30" 1800].
		ago > (60*60*24) ifTrue: [pause _ "60*120" 7200].
		ago - prev >= pause ifTrue: [out add: bunchEnd.  bunchEnd _ secs].
		prev _ ago].
	out add: bunchEnd.
	out removeFirst.
	^ out! !!TTime class methodsFor: 'general inquiries' stamp: 'brp 8/23/2003 21:47'!humanWordsForSecondsAgo: secs
	| date today |
	"Return natural language for this date and time in the past."

	secs <= 1 ifTrue: [^ 'a second ago'].
	secs < 45 ifTrue: [^ secs printString, ' seconds ago'].
	secs < 90 ifTrue: [^ 'a minute ago'].
	secs < "45*60" 2700 ifTrue: [^ (secs//60) printString, ' minutes ago'].
	secs < "90*60" 5400 ifTrue: [^ 'an hour ago'].
	secs < "18*60*60" 64800 ifTrue: [^ (secs//3600) printString, ' hours ago'].
	date _ self dateClass fromSeconds: self totalSeconds - secs.		"now work with dates"
	today _ self dateClass today.
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
"! !!TTime class methodsFor: 'general inquiries' stamp: 'BP 3/30/2001 15:25'!namesForTimes: arrayOfSeconds
	| simpleEnglish prev final prevPair myPair |
	"Return English descriptions of the times in the array.  They are each seconds since 1901.  If two names are the same, append the date and time to distinguish them."

	simpleEnglish _ arrayOfSeconds collect: [:secsAgo |
		self humanWordsForSecondsAgo: self totalSeconds - secsAgo].
	prev _ ''.
	final _ simpleEnglish copy.
	simpleEnglish withIndexDo: [:eng :ind | 
		eng = prev ifFalse: [eng]
			ifTrue: ["both say 'a month ago'"
				prevPair _ self dateAndTimeFromSeconds: 
						(arrayOfSeconds at: ind-1).
				myPair _ self dateAndTimeFromSeconds: 
						(arrayOfSeconds at: ind).
				(final at: ind-1) = prev ifTrue: ["only has 'a month ago'"
					final at: ind-1 put: 
							(final at: ind-1), ', ', prevPair first mmddyyyy].
				final at: ind put: 
							(final at: ind), ', ', myPair first mmddyyyy.
				prevPair first = myPair first 
					ifTrue: [
						(final at: ind-1) last == $m ifFalse: ["date but no time"
							final at: ind-1 put: 
								(final at: ind-1), ', ', prevPair second printMinutes].
						final at: ind put: 
							(final at: ind), ', ', myPair second printMinutes]].
		prev _ eng].
	^ final! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 21:47'!dateAndTimeFromSeconds: secondCount

	^ Array
		with: (self dateClass fromSeconds: secondCount)
		with: (self timeClass fromSeconds: secondCount \\ 86400)
! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 19:50'!dateAndTimeNow
	"Answer a two-element Array of (Date today, Time now)."

	^ self dateAndTimeFromSeconds: self totalSeconds! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:11'!fromSeconds: secondCount 
	"Answer an instance of me that is secondCount number of seconds since midnight."

	^ self seconds: secondCount
! !!TTime class methodsFor: 'smalltalk-80' stamp: 'BP 3/30/2001 15:25'!millisecondClockValue
	"Answer the number of milliseconds since the millisecond clock was last 
	reset or rolled over."

	^ self primMillisecondClock! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:11'!millisecondsToRun: timedBlock 
	"Answer the number of milliseconds timedBlock takes to return its value."

	| initialMilliseconds |
	initialMilliseconds _ self millisecondClockValue.
	timedBlock value.
	^ self millisecondClockValue - initialMilliseconds! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 20:01'!new	"Answer a Time representing midnight"	^ self midnight! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 22:01'!primMillisecondClock	"Primitive. Answer the number of milliseconds since the millisecond clock	 was last reset or rolled over. Answer zero if the primitive fails.	 Optional. See Object documentation whatIsAPrimitive."	<primitive: 135>	^ 0! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 22:01'!primSecondsClock	"Answer the number of seconds since 00:00 on the morning of	 January 1, 1901 (a 32-bit unsigned number).	 Essential. See Object documentation whatIsAPrimitive. "	<primitive: 137>	self primitiveFailed! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 8/23/2003 20:07'!readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm |
	hour _ Integer readFrom: aStream.
	minute _ 0.
	second _ 0.
	(aStream peekFor: $:) ifTrue:	
	[ minute _ Integer readFrom: aStream.
		(aStream peekFor: $:) ifTrue: [ second _ Integer readFrom: aStream ]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: 		[ampm _ aStream next asLowercase.	
	(ampm = $p and: [hour < 12]) ifTrue: [hour _ hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour _ 0].	
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self hour: hour minute: minute second: second

	"Time readFrom: (ReadStream on: '2:23:09 pm')"
! !!TTime class methodsFor: 'smalltalk-80' stamp: 'brp 7/27/2003 16:12'!totalSeconds
	"Answer the total seconds since the Squeck epoch: 1 January 1901."

	^ self primSecondsClock! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:46'!current 	^ self now! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 22:34'!fromString: aString
	^ self readFrom: aString readStream
! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:05'!hour: hour minute: minute second: second	"Answer a Time"

	^ self hour: hour minute: minute second: second nanoSecond: 0! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp` 8/24/2003 19:26'!hour: hour minute: minute second: second  nanoSecond: nanoCount	"Answer a Time - only second precision for now"

	^ self 		seconds: (hour * SecondsInHour) + (minute * SecondsInMinute) + second 		nanoSeconds: nanoCount! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:01'!midnight	^ self seconds: 0! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 18:58'!milliseconds: currentTime since: lastTime
	"Answer the elapsed time since last recorded in milliseconds.
	Compensate for rollover."

	| delta |
	delta _ currentTime - lastTime.
	^ delta < 0
		ifTrue: [SmallInteger maxVal + delta]
		ifFalse: [delta]
! !!TTime class methodsFor: 'squeak protocol' stamp: 'BP 3/30/2001 15:25'!millisecondsSince: lastTime
	"Answer the elapsed time since last recorded in milliseconds.
	Compensate for rollover."

	^self milliseconds: self millisecondClockValue since: lastTime! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp` 8/24/2003 19:26'!noon	^ self seconds: (SecondsInDay / 2)! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:47'!seconds: seconds
	"Answer a Time from midnight"
	^ self seconds: seconds nanoSeconds: 0! !!TTime class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 20:46'!seconds: seconds nanoSeconds: nanoCount
	"Answer a Time from midnight"
	^ self basicNew		ticks: (Duration seconds: seconds nanoSeconds: nanoCount) ticks;		yourself
! !!TTime class methodsFor: 'ansi protocol' stamp: 'brp 8/23/2003 18:56'!now	"Answer a Time representing the time right now - this is a 24 hour clock."	^ self seconds: self totalSeconds \\ 86400.! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:13'!asTimeStamp	"Answer the receiver as an instance of TimeStamp."	^ self! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:14'!date	"Answer the date of the receiver."	^ self asDate! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!dateAndTime	"Answer a two element Array containing the receiver's date and time."	^ Array with: self date with: self time! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!minusDays: anInteger	"Answer a TimeStamp which is anInteger days before the receiver."	^ self - (anInteger days)! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!minusSeconds: anInteger	"Answer a TimeStamp which is anInteger number of seconds before the receiver."	^ self - (anInteger seconds)! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:18'!plusDays: anInteger	"Answer a TimeStamp which is anInteger days after the receiver."	^ self + (anInteger days)! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!plusSeconds: anInteger	"Answer a TimeStamp which is anInteger number of seconds after the receiver."	^ self + (anInteger seconds)! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!printOn: aStream 	"Print receiver's date and time on aStream."	aStream 		nextPutAll: self date printString;		space;		nextPutAll: self time printString.! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!storeOn: aStream 	aStream 		print: self printString;		nextPutAll: ' asTimeStamp'! !!TTimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:15'!time	"Answer the time of the receiver."	^ self asTime! !!TTimeStamp methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:19'!date: aDate	self deprecated: 'Deprecated'! !!TTimeStamp methodsFor: 'deprecated' stamp: 'brp 8/5/2003 18:19'!time: aTime	self deprecated: 'Deprecated'! !!TTimeStamp class methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 21:47'!readFrom: aStream 	"Answer a new instance from the read stream.  	TimeStamp readFrom: '1-10-2000 11:55:00 am' readStream."	^ self		date: (self dateClass readFrom: (aStream upTo: Character space) readStream)		time: (self timeClass readFrom: (aStream upToEnd) readStream)! !!TTimeStamp class methodsFor: 'deprecated' stamp: 'brp 8/23/2003 21:49'!midnightOn: aDate	"Answer a new instance that represents aDate at midnight."	^ self 		deprecated: 'Deprecated';		date: aDate time: self timeClass midnight! !!TTimeStamp class methodsFor: 'deprecated' stamp: 'brp 8/23/2003 21:49'!noonOn: aDate	"Answer a new instance that represents aDate at noon."	^ self 		deprecated: 'Deprecated';		date: aDate time: self timeClass noon! !!String methodsFor: 'squeak protocol' stamp: 'brp 8/23/2003 19:27'!asTimeStamp	^ self timeStampClass fromString: self! !!TTime reorganize!('ansi protocol' < = duration hash hour hour12 hour24 meridianAbbreviation minute second)('deprecated' hours: hours:minutes:seconds: setSeconds:)('printing' hhmm24 print24 print24:on: print24:showSeconds:on: printMinutes printOn: storeOn:)('smalltalk-80' addSeconds: addTime: asSeconds hours intervalString minutes seconds subtractTime:)('squeak protocol' asDate asDateAndTime asDuration asMonth asNanoSeconds asTime asTimeStamp asWeek asYear nanoSecond to:)('private' ticks ticks:)!TDate class removeSelector: #initialize!!TDate class reorganize!('smalltalk-80' dateAndTimeNow dayOfWeek: daysInMonth:forYear: daysInYear: firstWeekdayOfMonth:year: fromDays: fromSeconds: indexOfMonth: leapYear: nameOfDay: nameOfMonth: newDay:month:year: newDay:year: today)('deprecated' absoluteDaysToYear: fromJulianDayNumber: yearAndDaysFromDays:into:)('squeak protocol' fromString: julianDayNumber: readFrom: starting: tomorrow year:day: year:month:day: yesterday)!"Postscript:Add a global to indicate that the Chronology migration is in progress"Smalltalk at: #ChronologyMigrationInProgress put: #T.!