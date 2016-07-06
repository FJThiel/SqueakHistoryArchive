'From Squeak 2.2 of Sept 23, 1998 on 15 October 1998 at 11:26:40 pm'!

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:15'!
differenceFromString: aString 
	^ self decrementFrom: aString!
]style[(22 7 25 7)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:15'!
productFromString: aString 
	^ self scale: aString!
]style[(19 7 17 7)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:16'!
quotientFromString: aString 
	^ self ratioFrom: aString!
]style[(20 7 4 24)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:17'!
sumFromString: aString 
	^ self increment: aString! !


!Number methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:10'!
differenceFromString: aString 
	^ aString asNumeric - self!
]style[(22 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:11'!
productFromString: aString 
	^ aString asNumeric * self!
]style[(19 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:10'!
quotientFromString: aString 
	^ aString asNumeric / self!
]style[(20 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:10'!
sumFromString: aString 
	^ aString asNumeric + self!
]style[(15 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !


!Point methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:09'!
differenceFromString: aString 
	^ aString asNumeric - self!
]style[(22 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:09'!
productFromString: aString 
	^ aString asNumeric * self!
]style[(19 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:09'!
quotientFromString: aString 
	^ aString asNumeric / self!
]style[(20 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/15/1998 23:09'!
sumFromString: aString 
	^ aString asNumeric + self!
]style[(15 7 5 7 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !


!String reorganize!
('accessing' at: at:put: atPin: atWrap: endsWithDigit findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findInsensitive:allUpper: findString:startingAt: findTokens: findTokens:includes: findTokens:keep: includesSubString: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:ifAbsent: indexOfAnyOf:startingAt: indexOfAnyOf:startingAt:ifAbsent: lineCorrespondingToIndex: lineCount lineNumber: linesDo: size skipAnySubStr:startingAt: skipDelimiters:startingAt: startsWithDigit)
('comparing' < <= = > >= alike: beginsWith: caseSensitiveLessOrEqual: charactersExactlyMatching: compare: crc16 endsWith: hash hashMappedBy: match: sameAs:)
('copying' copyReplaceTokens:with: copyUpTo: deepCopy padded:to:with:)
('converting' asByteArray asDate asDisplayText asFileName asHtml asIRCLowercase asLegalSelector asLowercase asNumber asPacked asParagraph asString asSymbol asText asTime asUnHtml asUppercase asUrl asUrlRelativeTo: askIfAddStyle:req: capitalized compressWithTable: contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: encodeForHTTP initialInteger keywords sansPeriodSuffix splitInteger stemAndNumericSuffix substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateWith: truncateTo: truncateWithElipsisTo: withBlanksTrimmed withFirstCharacterDownshifted withSeparatorsCompacted)
('displaying' displayAt: displayOn: displayOn:at: displayProgressAt:from:to:during: newTileMorphRepresentative)
('printing' basicType isLiteral printOn: storeOn: stringRepresentation)
('private' correctAgainstEnumerator:continuedFrom: replaceFrom:to:with:startingAt: stringhash)
('system primitives' compare:with:collated: numArgs)
('as numeric' * + - / asNumeric differenceFromCollection: differenceFromFloat: differenceFromFraction: differenceFromInteger: differenceFromPoint: differenceFromSequenceableCollection: differenceFromString: productFromCollection: productFromFloat: productFromFraction: productFromInteger: productFromPoint: productFromSequenceableCollection: productFromString: quotientFromCollection: quotientFromFloat: quotientFromFraction: quotientFromInteger: quotientFromPoint: quotientFromSequenceableCollection: quotientFromString: sumFromCollection: sumFromFloat: sumFromFraction: sumFromInteger: sumFromPoint: sumFromSequenceableCollection: sumFromString:)
('Celeste' includesSubstring:caseSensitive: withCRs)
('internet' replaceHtmlCharRefs unescapePercents withInternetLineEndings withSqueakLineEndings withoutQuoting)
('testing' isAllSeparators)
!


!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:47'!
* aNumeric 
	^ aNumeric productFromString: self!
]style[(2 8 5 8 24)f1,f1cgreen;,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:47'!
+ aNumeric 
	^ aNumeric sumFromString: self!
]style[(2 8 5 8 20)f1,f1cgreen;,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:47'!
- aNumeric 
	^ aNumeric differenceFromString: self!
]style[(2 8 5 8 27)f1,f1cgreen;,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:47'!
/ aNumeric 
	^ aNumeric quotientFromString: self!
]style[(2 8 5 8 25)f1,f1cgreen;,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:50'!
asNumeric
	"The plan is to eventually implement this as a more stringent numeric 
	conversion than the default asNumber. #asNumber will for example, 
	return 0 when converting the string 'abc'. IMO, this is not such a good 
	facility since it my hide errors when treating strings like numeric 
	representations. "
	^ self asNumber!
]style[(9 2 302 17)f1b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:18'!
differenceFromCollection: aCollection 
	^ aCollection decrement: self!
]style[(26 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:51'!
differenceFromFloat: aFloat 
	^ aFloat - self asNumeric!
]style[(21 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:52'!
differenceFromFraction: aFraction 
	^ aFraction - self asNumeric!
]style[(24 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:52'!
differenceFromInteger: anInteger 
	^ anInteger - self asNumeric!
]style[(23 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:53'!
differenceFromPoint: aPoint 
	^ aPoint - self asNumeric!
]style[(21 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:19'!
differenceFromSequenceableCollection: aSequence 
	^ aSequence decrement: self!
]style[(38 9 5 9 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:51'!
differenceFromString: aString 
	^ (aString asNumeric - self asNumeric) printString!
]style[(22 7 6 7 40)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:21'!
productFromCollection: aCollection 
	^ aCollection scale: self!
]style[(23 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:57'!
productFromFloat: aFloat 
	^ aFloat * self asNumeric!
]style[(18 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:01'!
productFromFraction: aFraction 
	^ aFraction * self asNumeric!
]style[(21 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:01'!
productFromInteger: anInteger 
	^ anInteger * self asNumeric!
]style[(20 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:05'!
productFromPoint: aPoint 
	^ aPoint * self asNumeric!
]style[(18 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:20'!
productFromSequenceableCollection: aSequence 
	^ aSequence scale: self!
]style[(35 9 5 9 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:48'!
productFromString: aString 
	^ (aString asNumeric * self asNumeric) printString!
]style[(19 7 6 7 40)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:21'!
quotientFromCollection: aCollection 
	^ aCollection ratio: self!
]style[(24 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:57'!
quotientFromFloat: aFloat 
	^ aFloat / self asNumeric!
]style[(19 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:01'!
quotientFromFraction: aFraction 
	^ aFraction / self asNumeric!
]style[(22 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:58'!
quotientFromInteger: anInteger 
	^ anInteger / self asNumeric!
]style[(21 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:02'!
quotientFromPoint: aPoint 
	^ aPoint / self asNumeric!
]style[(19 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:20'!
quotientFromSequenceableCollection: aSequence 
	^ aSequence ratio: self!
]style[(36 9 5 9 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:48'!
quotientFromString: aString 
	^ (aString asNumeric / self asNumeric) printString!
]style[(20 7 6 7 40)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:21'!
sumFromCollection: aCollection 
	^ aCollection increment: self!
]style[(19 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:57'!
sumFromFloat: aFloat 
	^ aFloat + self asNumeric!
]style[(14 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:02'!
sumFromFraction: aFraction 
	^ aFraction + self asNumeric!
]style[(17 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:58'!
sumFromInteger: anInteger 
	^ anInteger + self asNumeric!
]style[(16 9 5 9 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:02'!
sumFromPoint: aPoint 
	^ aPoint + self asNumeric!
]style[(14 6 5 6 17)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 23:19'!
sumFromSequenceableCollection: aSequence 
	^ aSequence increment: self!
]style[(31 9 5 9 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!String methodsFor: 'as numeric' stamp: 'TAG 10/15/1998 22:48'!
sumFromString: aString 
	^ (aString asNumber + self asNumber) printString!
]style[(15 7 6 7 38)f1b,f1cgreen;b,f1,f1cblue;,f1! !


String removeSelector: #differentFromString:!
String removeSelector: #differentFromNumeric:!
String removeSelector: #differentFromFloat:!

