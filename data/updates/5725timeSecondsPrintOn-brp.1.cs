'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 16 February 2004 at 9:13 am'!"Change Set:		timeSecondsPrintOn-brpDate:			16 February 2004Author:			Brent PinkneyTime printString should only show second precision."!!Time methodsFor: 'printing' stamp: 'brp 2/16/2004 09:10'!print24: hr24 showSeconds: showSeconds on: aStream 
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
		s asInteger printOn: aStream ].

	hr24 ifFalse:	
	[ aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ].
! !