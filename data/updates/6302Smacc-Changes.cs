'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6301] on 6 October 2004 at 5:48:43 pm'!"Change Set:		Smacc-ChangesDate:			6 October 2004Author:			ajhSome accessors for SmaCCToken for Anthony's closureCompiler"!!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 2/27/2003 17:55'!length	^ value size! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/11/2003 23:05'!sourceInterval	^ self start to: self stop! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!start	^ start! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!startPosition	^ start! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!stop	^ start ifNotNil: [start + value size - 1]! !!SmaCCToken methodsFor: 'accessing' stamp: 'ajh 3/17/2003 12:03'!stopPosition	^ start + value size - 1! !!SmaCCToken methodsFor: 'initialize-release' stamp: 'ajh 3/13/2003 14:04'!start: startPositionInteger	start _ startPositionInteger! !!SmaCCToken methodsFor: 'initialize-release' stamp: 'ajh 3/13/2003 15:25'!value: anObject	value _ anObject! !!SmaCCToken class methodsFor: 'instance creation' stamp: 'ajh 3/13/2003 14:04'!value: aString	^ self value: aString start: nil id: nil! !!SmaCCToken class methodsFor: 'instance creation' stamp: 'ajh 3/11/2003 23:28'!value: aString start: anInteger	^ self value: aString start: anInteger id: nil! !