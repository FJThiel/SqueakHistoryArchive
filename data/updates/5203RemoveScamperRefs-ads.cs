'From Squeak3.4 of 1 March 2003 [latest update: #5170] on 1 April 2003 at 7:27:32 pm'!!FreeTranslation class methodsFor: 'scamper' stamp: 'ads 4/1/2003 19:24'!openScamperOn: currentSelection	"Submit the string to the translation server at www.freetranslation.com.  Ask it to translate from (Preferences parameterAt: #languageTranslateFrom) to (Preferences parameterAt: #languageTranslateTo).  Display the results in a Scamper window, reusing the previous one if possible."	| inputs scamperWindow from to | 	currentSelection size >= 10000 ifTrue: [^ self inform: 'Text selection is too long.'].	from _ Preferences parameterAt: #languageTranslateFrom ifAbsentPut: ['English'].	to _ Preferences parameterAt: #languageTranslateTo ifAbsentPut: ['German'].	from = to ifTrue:			[^ self inform: 'You asked to translate from ', from, ' to ', to, '.\' withCRs,				'Use "choose language" to set these.'].  	inputs _ Dictionary new.	inputs at: 'SrcText' put: (Array with: currentSelection).	inputs at: 'Sequence' put: #('core').	inputs at: 'Mode' put: #('html').	inputs at: 'template' put: #('TextResult2.htm').	inputs at: 'Language' put: (Array with: from, '/', to).	scamperWindow _ (WebBrowser default ifNil: [^self]) newOrExistingOn: 'http://ets.freetranslation.com'.	scamperWindow model submitFormWithInputs: inputs 		url: 'http://ets.freetranslation.com:5081' asUrl		method: 'post'.	scamperWindow activate.! !!MethodFinder methodsFor: 'initialize' stamp: 'ads 3/29/2003 17:12'!initialize2	"The methods we are allowed to use.  (MethodFinder new initialize) ""Set"	#("in class" sizeFor:"testing" "adding" "removing" "enumerating""private" array findElementOrNil: "accessing" someElement) do: [:sel | Approved add: sel]."Dictionary, IdentityDictionary, IdentitySet"	#("accessing" associationAt: associationAt:ifAbsent: at:ifPresent: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys"testing" includesKey: ) do: [:sel | Approved add: sel].	#(removeKey: removeKey:ifAbsent:) do: [:sel | AddAndRemove add: sel]."LinkedList, Interval, MappedCollection"	#("in class"  from:to: from:to:by:"accessing" contents) do: [:sel | Approved add: sel].	#("adding" addFirst: addLast:) do: [:sel | AddAndRemove add: sel]."OrderedCollection, SortedCollection"	#("accessing" after: before:"copying" copyEmpty"adding"  growSize"removing" "enumerating" "private" "accessing" sortBlock) do: [:sel | Approved add: sel].	#("adding" add:after: add:afterIndex: add:before: addAllFirst: addAllLast: addFirst: addLast:"removing" removeAt: removeFirst removeLast"accessing" sortBlock:) do: [:sel | AddAndRemove add: sel]."Character"	#("in class, instance creation" allCharacters digitValue: new separators	"accessing untypeable characters" backspace cr enter lf linefeed nbsp newPage space tab	"constants" alphabet characterTable"accessing" asciiValue digitValue"comparing""testing" isAlphaNumeric isDigit isLetter isLowercase isSafeForHTTP isSeparator isSpecial isUppercase isVowel tokenish"copying""converting" asIRCLowercase asLowercase asUppercase	) do: [:sel | Approved add: sel]."String"	#("in class, instance creation" crlf fromPacked:	"primitives" findFirstInString:inSet:startingAt: indexOfAscii:inString:startingAt: 	"internet" valueOfHtmlEntity:"accessing" byteAt: endsWithDigit findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:includes: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf:startingAt: indexOfAnyOf: indexOfAnyOf:ifAbsent: indexOfAnyOf:startingAt: indexOfAnyOf:startingAt:ifAbsent: lineCorrespondingToIndex: lineCount lineNumber: skipAnySubStr:startingAt: skipDelimiters:startingAt: startsWithDigit"comparing" alike: beginsWith: caseSensitiveLessOrEqual: charactersExactlyMatching: compare: crc16 endsWith: endsWithAnyOf: sameAs: startingAt:match:startingAt:"copying" copyReplaceTokens:with: padded:to:with:"converting" asByteArray asDate asDisplayText asFileName asHtml asLegalSelector asPacked asParagraph asText asTime asUnHtml asUrl asUrlRelativeTo: capitalized compressWithTable: contractTo: correctAgainst: encodeForHTTP initialIntegerOrNil keywords quoted sansPeriodSuffix splitInteger stemAndNumericSuffix substrings surroundedBySingleQuotes truncateWithElipsisTo: withBlanksTrimmed withFirstCharacterDownshifted withNoLineLongerThan: withSeparatorsCompacted withoutLeadingDigits withoutTrailingBlanks"displaying" "printing""system primitives" compare:with:collated: "Celeste" withCRs"internet" decodeMimeHeader decodeQuotedPrintable unescapePercents withInternetLineEndings withSqueakLineEndings withoutQuoting"testing" isAllSeparators lastSpacePosition"paragraph support" indentationIfBlank:"arithmetic" ) do: [:sel | Approved add: sel].	#(byteAt:put: translateToLowercase match:) do: [:sel | AddAndRemove add: sel]."Symbol"	#("in class, private" hasInterned:ifTrue:	"access" morePossibleSelectorsFor: possibleSelectorsFor: selectorsContaining: thatStarts:skipping:"accessing" "comparing" "copying" "converting" "printing" "testing" isInfix isKeyword isPvtSelector isUnary) do: [:sel | Approved add: sel]."Array"	#("comparing" "converting" evalStrings "printing" "private" hasLiteralSuchThat:) do: [:sel | Approved add: sel]."Array2D"	#("access" at:at: atCol: atCol:put: atRow: extent extent:fromArray: height width width:height:type:) do: [:sel | Approved add: sel].	#(at:at:add: at:at:put: atRow:put: ) do: [:sel | AddAndRemove add: sel]."ByteArray"	#("accessing" doubleWordAt: wordAt: "platform independent access" longAt:bigEndian: shortAt:bigEndian: unsignedLongAt:bigEndian: unsignedShortAt:bigEndian: "converting") do: [:sel | Approved add: sel].	#(doubleWordAt:put: wordAt:put: longAt:put:bigEndian: shortAt:put:bigEndian: unsignedLongAt:put:bigEndian: unsignedShortAt:put:bigEndian:	) do: [:sel | AddAndRemove add: sel]."FloatArray"		"Dont know what happens when prims not here"	false ifTrue: [#("accessing" "arithmetic" *= += -= /="comparing""primitives-plugin" primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:"primitives-translated" primAddArray:withArray:from:to: primMulArray:withArray:from:to: primSubArray:withArray:from:to:"converting" "private" "user interface") do: [:sel | Approved add: sel].	]."IntegerArray, WordArray""RunArray"	#("in class, instance creation" runs:values: scanFrom:"accessing" runLengthAt: "adding" "copying""private" runs values) do: [:sel | Approved add: sel].	#(coalesce addLast:times: repeatLast:ifEmpty: repeatLastIfEmpty:		) do: [:sel | AddAndRemove add: sel]."Stream  -- many operations change its state"	#("testing" atEnd) do: [:sel | Approved add: sel].	#("accessing" next: nextMatchAll: nextMatchFor: upToEndnext:put: nextPut: nextPutAll: "printing" print: printHtml:	) do: [:sel | AddAndRemove add: sel]."PositionableStream"	#("accessing" contentsOfEntireFile originalContents peek peekFor: "testing""positioning" position ) do: [:sel | Approved add: sel].	#(nextDelimited: nextLine upTo: position: reset resetContents setToEnd skip: skipTo: upToAll: ) do: [:sel | AddAndRemove add: sel].	"Because it is so difficult to test the result of an operation on a Stream (you have to supply another Stream in the same state), we don't support Streams beyond the basics.  We want to find the messages that convert Streams to other things.""ReadWriteStream"	#("file status" closed) do: [:sel | Approved add: sel].	#("accessing" next: on: ) do: [:sel | AddAndRemove add: sel]."WriteStream"	#("in class, instance creation" on:from:to: with: with:from:to:		) do: [:sel | Approved add: sel].	#("positioning" resetToStart"character writing" crtab crtab:) do: [:sel | AddAndRemove add: sel]."LookupKey, Association, Link"	#("accessing" key nextLink) do: [:sel | Approved add: sel].	#(key: key:value: nextLink:) do: [:sel | AddAndRemove add: sel]."Point"	#("in class, instance creation" r:degrees: x:y:"accessing" x y "comparing" "arithmetic" "truncation and round off""polar coordinates" degrees r theta"point functions" bearingToPoint: crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors grid: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized octantOf: onLineFrom:to: onLineFrom:to:within: quadrantOf: rotateBy:centerAt: transposed unitVector"converting" asFloatPoint asIntegerPoint corner: extent: rect:"transforming" adhereTo: rotateBy:about: scaleBy: scaleFrom:to: translateBy: "copying""interpolating" interpolateTo:at:) do: [:sel | Approved add: sel]."Rectangle"	#("in class, instance creation" center:extent: encompassing: left:right:top:bottom: 	merging: origin:corner: origin:extent: "accessing" area bottom bottomCenter bottomLeft bottomRight boundingBox center corner corners innerCorners left leftCenter origin right rightCenter top topCenter topLeft topRight"comparing""rectangle functions" adjustTo:along: amountToTranslateWithin: areasOutside: bordersOn:along: encompass: expandBy: extendBy: forPoint:closestSideDistLen: insetBy: insetOriginBy:cornerBy: intersect: merge: pointNearestTo: quickMerge: rectanglesAt:height: sideNearestTo: translatedToBeWithin: withBottom: withHeight: withLeft: withRight: withSide:setTo: withTop: withWidth:"testing" containsPoint: containsRect: hasPositiveExtent intersects: isTall isWide"truncation and round off""transforming" align:with: centeredBeneath: newRectFrom: squishedWithin: "copying"	) do: [:sel | Approved add: sel]."Color"	#("in class, instance creation" colorFrom: colorFromPixelValue:depth: fromRgbTriplet: gray: h:s:v: r:g:b: r:g:b:alpha: r:g:b:range:	"named colors" black blue brown cyan darkGray gray green lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightRed lightYellow magenta orange red transparent veryDarkGray veryLightGray veryVeryDarkGray veryVeryLightGray white yellow	"other" colorNames indexedColors pixelScreenForDepth: quickHighLight:"access" alpha blue brightness green hue luminance red saturation"equality""queries" isBitmapFill isBlack isGray isSolidFill isTranslucent isTranslucentColor"transformations" alpha: dansDarker darker lighter mixed:with: muchLighter slightlyDarker slightlyLighter veryMuchLighter alphaMixed:with:"groups of shades" darkShades: lightShades: mix:shades: wheel:"printing" shortPrintString"other" colorForInsets rgbTriplet"conversions" asB3DColor asColor balancedPatternForDepth: bitPatternForDepth: closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor halfTonePattern1 halfTonePattern2 indexInMap: pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth: scaledPixelValue32"private" privateAlpha privateBlue privateGreen privateRGB privateRed "copying"	) do: [:sel | Approved add: sel]."	For each selector that requires a block argument, add (selector argNum) 		to the set Blocks.""ourClasses _ #(Object Boolean True False UndefinedObject Behavior ClassDescription Class Metaclass MethodContext BlockContext Message Magnitude Date Time Number Integer SmallInteger LargeNegativeInteger LargePositiveInteger Float Fraction Random Collection SequenceableCollection ArrayedCollection Bag Set Dictionary IdentityDictionary IdentitySet LinkedList Interval MappedCollection OrderedCollection SortedCollection Character String Symbol Array Array2D ByteArray FloatArray IntegerArray WordArray RunArray Stream PositionableStream ReadWriteStream WriteStream LookupKey Association Link Point Rectangle Color).ourClasses do: [:clsName | cls _ Smalltalk at: clsName.	(cls selectors) do: [:aSel |		((Approved includes: aSel) or: [AddAndRemove includes: aSel]) ifTrue: [			(cls formalParametersAt: aSel) withIndexDo: [:tName :ind |				(tName endsWith: 'Block') ifTrue: [					Blocks add: (Array with: aSel with: ind)]]]]]."#((timesRepeat: 1 ) (indexOf:ifAbsent: 2 ) (pairsCollect: 1 ) (mergeSortFrom:to:by: 3 ) (ifNotNil:ifNil: 1 ) (ifNotNil:ifNil: 2 ) (ifNil: 1 ) (at:ifAbsent: 2 ) (ifNil:ifNotNil: 1 ) (ifNil:ifNotNil: 2 ) (ifNotNil: 1 ) (at:modify: 2 ) (identityIndexOf:ifAbsent: 2 ) (sort: 1 ) (sortBlock: 1 ) (detectMax: 1 ) (repeatLastIfEmpty: 1 ) (allSubclassesWithLevelDo:startingLevel: 1 ) (keyAtValue:ifAbsent: 2 ) (in: 1 ) (ifTrue: 1 ) (or: 1 ) (select: 1 ) (inject:into: 2 ) (ifKindOf:thenDo: 2 ) (forPoint:closestSideDistLen: 2 ) (value:ifError: 2 ) (selectorsDo: 1 ) (removeAllSuchThat: 1 ) (keyAtIdentityValue:ifAbsent: 2 ) (detectMin: 1 ) (detect:ifNone: 1 ) (ifTrue:ifFalse: 1 ) (ifTrue:ifFalse: 2 ) (detect:ifNone: 2 ) (hasLiteralSuchThat: 1 ) (indexOfAnyOf:ifAbsent: 2 ) (reject: 1 ) (newRectFrom: 1 ) (removeKey:ifAbsent: 2 ) (at:ifPresent: 2 ) (associationAt:ifAbsent: 2 ) (withIndexCollect: 1 ) (repeatLast:ifEmpty: 2 ) (findLast: 1 ) (indexOf:startingAt:ifAbsent: 3 ) (remove:ifAbsent: 2 ) (ifFalse:ifTrue: 1 ) (ifFalse:ifTrue: 2 ) (caseOf:otherwise: 2 ) (count: 1 ) (collect: 1 ) (sortBy: 1 ) (and: 1 ) (asSortedCollection: 1 ) (with:collect: 2 ) (sourceCodeAt:ifAbsent: 2 ) (detect: 1 ) (scopeHas:ifTrue: 2 ) (collectWithIndex: 1 ) (compiledMethodAt:ifAbsent: 2 ) (bindWithTemp: 1 ) (detectSum: 1 ) (indexOfSubCollection:startingAt:ifAbsent: 3 ) (findFirst: 1 ) (sourceMethodAt:ifAbsent: 2 ) (collect:thenSelect: 1 ) (collect:thenSelect: 2 ) (select:thenCollect: 1 ) (select:thenCollect: 2 ) (ifFalse: 1 ) (indexOfAnyOf:startingAt:ifAbsent: 3 ) (indentationIfBlank: 1 ) ) do: [:anArray |	Blocks add: anArray].self initialize3."MethodFinder new initialize.MethodFinder new organizationFiltered: TranslucentColor class ""Do not forget class messages for each of these classes"! !!TextURL methodsFor: 'as yet unclassified' stamp: 'ads 4/1/2003 19:25'!actOnClickFor: anObject	"Do what you can with this URL.  Later a web browser."	| response m |	(url beginsWith: 'sqPr://') ifTrue: [		ProjectLoading thumbnailFromUrl: (url copyFrom: 8 to: url size).		^self		"should not get here, but what the heck"	].	"if it's a web browser, tell it to jump"	anObject isWebBrowser		ifTrue: [anObject jumpToUrl: url. ^ true]		ifFalse: [((anObject respondsTo: #model) and: [anObject model isWebBrowser])				ifTrue: [anObject model jumpToUrl: url. ^ true]].		"if it's a morph, see if it is contained in a web browser"		(anObject isKindOf: Morph) ifTrue: [			m _ anObject.			[ m ~= nil ] whileTrue: [				(m isWebBrowser) ifTrue: [					m  jumpToUrl: url.					^true ].				(m hasProperty: #webBrowserView) ifTrue: [					m model jumpToUrl: url.					^true ].				m _ m owner. ]		].	"no browser in sight.  ask if we should start a new browser"	((self confirm: 'open a browser to view this URL?') and: [WebBrowser default notNil]) ifTrue: [		WebBrowser default openOnUrl: url.		^ true ].	"couldn't display in a browser.  Offer to put up just the source"	response _ (PopUpMenu labels: 'View web page as source\Cancel' withCRs)		startUpWithCaption: 'Couldn''t find a web browser.  Viewpage as source?'.	response = 1 ifTrue: [HTTPSocket httpShowPage: url].	^ true! !!WordNet class methodsFor: 'miscellaneous' stamp: 'ads 4/1/2003 19:25'!openScamperOn: aWord	| aUrl scamperWindow |	"Open a Scamper web browser on the WordNet entry for this word.  If Scamper is already pointing at WordNet, use the same browser."	aUrl _ 'http://www.cogsci.princeton.edu/cgi-bin/webwn/', 		'?stage=1&word=', aWord.	scamperWindow _ (WebBrowser default ifNil: [^self]) newOrExistingOn: aUrl.	scamperWindow model jumpToUrl: aUrl asUrl.	scamperWindow activate.! !!PortugueseLexiconServer class methodsFor: 'as yet unclassified' stamp: 'ads 4/1/2003 19:25'!openScamperOn: aWord	| aUrl scamperWindow |	"Open a Scamper web browser on the web dictionary entry for this word.  If Scamper is already pointing at it, use the same browser.  Special code for this server."	aUrl _ 'http://www.priberam.pt/scripts/dlpouniv.dll', 		'?search_value=', (self decodeAccents: aWord).	scamperWindow _ (WebBrowser default ifNil: [^self]) newOrExistingOn: aUrl.	scamperWindow model jumpToUrl: aUrl asUrl.	scamperWindow activate.! !