"Printing.st
A few modified printing methods: Time without seconds, Dates with leading 0's (e.g., 1995.04.08).

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    October 3, 1992"!

!Time methodsFor: 'printing'!

printOnWithoutSecs: aStream 
	"Append to the argument aStream a sequence of characters that 
	identifies  the receiver. Format is h:mm am/pm."

	| nrmHrs nrmMins |
	nrmMins := (seconds // 60 + minutes) \\ 60.
	nrmHrs := (seconds // 3600 + (minutes // 60) + hours) \\ 24.
	nrmHrs > 12
				ifTrue: [nrmHrs - 12 printOn: aStream]
				ifFalse: [nrmHrs < 1
						ifTrue: [12 printOn: aStream]
						ifFalse: [nrmHrs printOn: aStream]].
	aStream nextPutAll: (nrmMins < 10
			ifTrue: [':0']
			ifFalse: [':']).
	nrmMins printOn: aStream.
	aStream nextPutAll: (nrmHrs < 12
			ifTrue: [' am']
			ifFalse: [' pm'])! !


!Date methodsFor: 'printing'!

printOn: aStream format: formatArray 
	"Print a description of the receiver on aStream.  The argument
	formatArray is the print format, where
	1-3		positions to print day, month, year respectively 
	4		character separator 
	5		month format (1 month #, 2 first 3 chars, 3 entire name) 
	6		year format (1 year #, 2 year # \\ 100)

	See also the comment for printFormat:"
	"Changed by stp to print a leading $0 if either the day or month are < 10 
	and numerical format is requested"

	| monthIndex element monthFormat d |
	monthIndex := self monthIndex.
	1 to: 3 do: 
		[:elementIndex | 
		element := formatArray at: elementIndex.
		element = 1
			ifTrue: [d := day - self firstDayOfMonth + 1.
				d < 10 ifTrue: [aStream nextPut: $0].
				d printOn: aStream].
		element = 2
			ifTrue: 
				[monthFormat := formatArray at: 5.
				monthFormat = 1 
					ifTrue: [monthIndex < 10 ifTrue: [aStream nextPut: $0].
						monthIndex printOn: aStream].
				monthFormat = 2
					ifTrue: [aStream nextPutAll: ((MonthNames at: monthIndex)
													copyFrom: 1 to: 3)].
				monthFormat = 3 
					ifTrue: [aStream nextPutAll: (MonthNames at: monthIndex)]].
		element = 3 
			ifTrue: 
				[(formatArray at: 6) = 1
					ifTrue: [year printOn: aStream]
					ifFalse: [(year \\ 100) printOn: aStream]].
		elementIndex < 3 
			ifTrue: 
				[(formatArray at: 4) ~= 0 
					ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]]! !

