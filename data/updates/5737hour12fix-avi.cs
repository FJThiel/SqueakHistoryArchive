'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 20 February 2004 at 6:49:58 pm'!"Change Set:		hour12fix-aviDate:			21 February 2004Author:			Avi Bryant#hour12 in Time and DateAndTime was using a strange algorithm that gave incorrect results - for example, (Time fromString: '3:00 pm') hour12 gave 4 instead of 3.  This fixes the problem."!!DateAndTime methodsFor: 'ansi protocol' stamp: 'avi 2/21/2004 18:46'!hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."	^ self hour24 - 1 \\ 12 + 1! !!Time methodsFor: 'ansi protocol' stamp: 'avi 2/21/2004 18:45'!hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."	^ self hour24 - 1 \\ 12 + 1! !