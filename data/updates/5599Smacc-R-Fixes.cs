'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5595] on 4 December 2003 at 1:34:37 pm'!"Change Set:		Smacc-R-FixesDate:			4 December 2003Author:			Marcus DenkerSome fixes for SmaCC-Runtime from Anthony's Closure CompilerAfter applying the fix, all the tests from SmaCC-Development keepon running (And I checked with my own parsers. too)"!!SmaCCScanner methodsFor: 'initialize-release' stamp: 'ajh 3/17/2003 12:05'!on: aStream 
	stream := aStream.
	start := stream position + 1.! !!SmaCCScanner methodsFor: 'private' stamp: 'ajh 3/17/2003 12:04'!resetScanner
	start := stream position + 1.
	outputStream reset.
	lastOutputStreamMatchPosition := 0! !!SmaCCScanner methodsFor: 'private'!scannerError
	(stream atEnd and: [start == (stream position + 1)]) 
		ifTrue: 
			[returnMatchBlock value: (SmaCCToken 
						value: ''
						start: start
						id: (Array with: self emptySymbolTokenId))].
	stream position: start - 1.
	returnMatchBlock value: (SmaCCToken 
				value: (String with: stream next)
				start: start
				id: #(0))! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!startPosition
	^ start! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!stopPosition
	^ start + value size - 1! !