'From Squeak 2.1 of June 30, 1998 on 14 September 1998 at 9:56:39 pm'!"Change Set:		stringEquals2-lsDate:			14 September 1998Author:			Lex SpoonTwo Strings aren't equal unless they actually are both strings."!!String methodsFor: 'comparing' stamp: 'ls 9/14/1998 21:54'!= aString 	"Answer whether the receiver sorts equally as aString.	The collation order is simple ascii (with case differences)."	self species == aString species ifFalse: [^false].	^ (self compare: self with: aString collated: AsciiOrder) = 2! !