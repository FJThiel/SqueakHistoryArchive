'From Squeak3.3alpha of 15 February 2002 [latest update: #4951] on 8 August 2002 at 9:35:47 am'!"Change Set:		DateReadString-tkDate:			8 August 2002Author:			Ted KaehlerPublished to 3.3a as 4953DateReadString-tk.cs.Strings with a space, separator, or other odd character at the front get an error when sent asDate.  This change makes (' 5 Apr 2002' asDate) 5 April 2002 work correctly."!!Date class methodsFor: 'instance creation' stamp: 'tk 5/24/2002 14:18'!readFrom: aStream	"Read a Date from the stream in any of the forms:		<day> <monthName> <year>		(5 April 1982; 5-APR-82)		<monthName> <day> <year>		(April 5, 1982)		<monthNumber> <day> <year>	(4/5/82)"	| day month |	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].	"leading blanks"	aStream peek isDigit ifTrue: [day _ Integer readFrom: aStream].	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].	aStream peek isLetter		ifTrue:		"number/name... or name..."			[month _ WriteStream on: (String new: 10).			[aStream peek isLetter] whileTrue: [month nextPut: aStream next].			month _ month contents.			day isNil ifTrue:		"name/number..."				[[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].				day _ Integer readFrom: aStream]]		ifFalse:		"number/number..."			[month _ Date nameOfMonth: day.			day _ Integer readFrom: aStream].	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].	^self newDay: day month: month year: (Integer readFrom: aStream)	"Date readFrom: (ReadStream on: '5APR82')"! !