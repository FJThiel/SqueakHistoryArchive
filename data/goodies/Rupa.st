"      NAME	Rupa Investigator
       AUTHOR	nishis@urban.ne.jp (NISHIHARA Satoshi)
       URL	(none)
       FUNCTION	WMC (Weighted Methods per Class)
       KEYWORDS	Metrics WMC CKMetrics
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.0
       DATE	14-Nov-98

SUMMARY

Rupa InvestigatorWMC(Weighted Methods per Class) Goodies for Squeak.This goodies is a modification for Squeak,originaly written for Smalltalk Workbook by Mr. AOKI.Rupa 001 (February 7, 1996)Copyright (C) 1996 by AOKI AtsushiWith helps, advices and encouragements by Smalltalkers Giants as Dan Ingalls, John Maloney, OHSHIMA Yoshiki and of course AOKI Atsushi.And Squeak mailing-list, SML (Smalltalkers'' Salon -> Smalltalkers mailing-list in Japan).see below http://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Program01.texthttp://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Chapter05.html

				NISHIHARA Satoshi
"!

'14 November 1998 2:52:36 pm'!

'
#
# This file is encoded in (so called) SHIFT-JIS,
# line delimitor is CR.
#
Rupa Investigator
WMC(Weighted Methods per Class) Goodies for Squeak.

This goodies is a modification for Squeak,
originaly written for Smalltalk Workbook by Mr. AOKI.
Rupa 001 (February 7, 1996)
Copyright (C) 1996 by AOKI Atsushi

With helps, advices and encouragements by Smalltalkers Giants as 
Dan Ingalls, John Maloney, OHSHIMA Yoshiki and of course AOKI Atsushi.
And Squeak mailing-list, SML (Smalltalkers'' Salon -> Smalltalkers mailing-list in Japan).

see below 
http://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Program01.text
http://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Chapter05.html

----------------------------------------------------------
¼Œ´‘Žm (NISHIHARA Satoshi)
e-mail:	nishis@urban.ne.jp, tcc00164@nifty.ne.jp
URL:	http://www.urban.ne.jp/home/nishis/
----------------------------------------------------------

'!

Object subclass: #RupaInvestigator
	instanceVariableNames: 'classes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Rupa-Tools'!


Object subclass: #RupaInvestigator
	instanceVariableNames: 'classes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Rupa-Tools'!
!RupaInvestigator commentStamp: 'nishis 11/14/1998 14:52' prior: 0!
#
# This file is encoded in (so called) SHIFT-JIS,
# line delimitor is CR.
#
Rupa Investigator
WMC(Weighted Methods per Class) Goodies for Squeak.

This goodies is a modification for Squeak,
originaly written for Smalltalk Workbook by Mr. AOKI.
Rupa 001 (February 7, 1996)
Copyright (C) 1996 by AOKI Atsushi

With helps, advices and encouragements by Smalltalkers Giants as 
Dan Ingalls, John Maloney, OHSHIMA Yoshiki and of course AOKI Atsushi.
And Squeak mailing-list, SML (Smalltalkers' Salon -> Smalltalkers mailing-list in Japan).

see below 
http://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Program01.text
http://www.sra.co.jp/people/aoki/SmalltalkWorkbookJ/Chapter05.html

----------------------------------------------------------
¼Œ´‘Žm (NISHIHARA Satoshi)
e-mail:	nishis@urban.ne.jp, tcc00164@nifty.ne.jp
URL:	http://www.urban.ne.jp/home/nishis/
----------------------------------------------------------
!


!RupaInvestigator methodsFor: 'accessing'!
behaviors
	^self classes , (self classes collect: [:each | each class])! !

!RupaInvestigator methodsFor: 'accessing'!
classes
	classes isNil ifTrue: [classes := self class allClasses].
	^classes! !

!RupaInvestigator methodsFor: 'accessing'!
classes: classCollection 
	| collection |
	collection := OrderedCollection new: classCollection size.
	classCollection do: [:aClass | aClass isMeta ifFalse: [collection add: aClass]].
	classes := collection! !


!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 11/12/1998 21:03'!
byteSizeOfObject: anObject 
	"RupaInvestigator new byteSizeOfObject: (OrderedCollection new: 10)."
	"RupaInvestigator new byteSizeOfObject: (Array new: 10)."
	"RupaInvestigator new byteSizeOfObject: 123.4e."
	"RupaInvestigator new byteSizeOfObject: 123.4."
	"RupaInvestigator new byteSizeOfObject: 123."
	"RupaInvestigator new byteSizeOfObject: $C."
	"RupaInvestigator new byteSizeOfObject: #Squeak."
	"RupaInvestigator new byteSizeOfObject: []."
	"RupaInvestigator new byteSizeOfObject: (Point compiledMethodAt: #rounded)."

	"ex) see below
	Browser fullOnClass: SystemDictionary selector: #printSpaceAnalysis:on:.
	"

	| aClass baseHeaderSize objectHeadersSize bytesInOOP instVarFieldSize indexableFieldSize byteSize |

	"see Purple or Blue Book, pp.115, 441 (565 at Blue Book)"
	"
	SmallIntegers are immediate.  Characters are not, but there are only 256
	of them which are shared, so their space is effectively 0.
 	"
	"
	Small integers are immediate. Characters are shared
	references to one of a set of 256 Character objects.
	String elements are, of course, a single byte, which
	is used as an index into the 256 Character table.
	"
	aClass := anObject class.
	((aClass == SmallInteger) or: [aClass == Character]) ifTrue: [^0].
	"Object headers can be 1, 2, or 3 words (4, 8, or 12 bytes)."
	baseHeaderSize := self baseHeaderSize.
	objectHeadersSize := aClass indexIfCompact > 0 
				ifTrue: [baseHeaderSize] 
				ifFalse: 
					[((self sizeInWordsOf: anObject) + 1 ) > 16r3F
						ifTrue: [baseHeaderSize * 3]
						ifFalse: [baseHeaderSize * 2]].
	bytesInOOP := aClass isBytes ifTrue: [1] ifFalse: [4].

	"see Smalltalk-Idioms, pp. 84-92, Purple or Blue Book, pp.280-281"
	aClass isPointers
		ifTrue: 
			[instVarFieldSize := aClass instSize * bytesInOOP.
			aClass isVariable
				ifTrue: 
					[indexableFieldSize := anObject basicSize * bytesInOOP]
				ifFalse: 
					[indexableFieldSize := 0]]
		ifFalse: 
			[instVarFieldSize := 0.
			aClass isVariable
				ifTrue: 
					["Browser fullOnClass: RupaInvestigator selector: #weightForMethod:."
					indexableFieldSize := (anObject basicSize + 3) bitAnd: 16r1FFFFFFC]
				ifFalse: 
					["never fall down here"
					indexableFieldSize := 0]].
	"Squeak doesn't have an object table, so no space for that "
	byteSize := objectHeadersSize + instVarFieldSize + indexableFieldSize.
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing'!
byteSizeOfObject: anObject and: aCollection 
	"RupaInvestigator new byteSizeOfObject: 0 @ 1.23 and: #('x' 'y')."

	| visitedObjects byteSize index size object |
	visitedObjects := IdentitySet new.
	visitedObjects add: anObject.
	byteSize := self byteSizeOfObject: anObject.
	aCollection
		do: 
			[:instVarName | 
			index := self instVarIndex: anObject instVarName: instVarName.
			object := anObject instVarAt: index.
			((visitedObjects includes: object) not and: [self shouldCountObject: object])
				ifTrue: 
					[size := self byteSizeOfObject: object.
					visitedObjects add: object]
				ifFalse: [size := 0].
			byteSize := byteSize + size].
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing'!
byteSizeOfObjectsIn: anObject 
	"RupaInvestigator new byteSizeOfObjectsIn: 0 @ 1.23."

	| aClass visitedObjects byteSize object |
	aClass := anObject class.
	visitedObjects := IdentitySet new.
	visitedObjects add: anObject.
	byteSize := 0.
	1 to: aClass instSize
		do: 
			[:index | 
			object := anObject instVarAt: index.
			((visitedObjects includes: object) not and: [self shouldCountObject: object])
				ifTrue: 
					[byteSize := byteSize + (self byteSizeOfObject: object).
					visitedObjects add: object]].
	aClass isVariable ifTrue: [1 to: anObject basicSize
			do: 
				[:index | 
				object := anObject basicAt: index.
				((visitedObjects includes: object) not and: [self shouldCountObject: object])
					ifTrue: 
						[byteSize := byteSize + (self byteSizeOfObject: object).
						visitedObjects add: object]]].
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing'!
weightForClass: aClass 
	"RupaInvestigator new weightForClass: Point."
	"RupaInvestigator new weightForClass: RupaInvestigator."

	| theClass byteSize index |
	aClass isMeta
		ifTrue: [theClass := aClass soleInstance]
		ifFalse: [theClass := aClass].
	byteSize := self byteSizeOfObject: theClass and: #('subclasses' 'name' 'sharedPools').
	index := self instVarIndex: theClass instVarName: 'methodDict'.
	byteSize := byteSize + (self weightForMethodDictionary: (theClass instVarAt: index)).
	index := self instVarIndex: theClass instVarName: 'instanceVariables'.
	byteSize := byteSize + (self weightForObject: (theClass instVarAt: index)).
	index := self instVarIndex: theClass instVarName: 'organization'.
	byteSize := byteSize + (self weightForObject: (theClass instVarAt: index)).
	index := self instVarIndex: theClass instVarName: 'classPool'.
	byteSize := byteSize + (self weightForObject: (theClass instVarAt: index)).
	theClass := theClass class.
	byteSize := byteSize + (self byteSizeOfObject: theClass and: #('subclasses')).
	index := self instVarIndex: theClass instVarName: 'methodDict'.
	byteSize := byteSize + (self weightForMethodDictionary: (theClass instVarAt: index)).
	index := self instVarIndex: theClass instVarName: 'instanceVariables'.
	byteSize := byteSize + (self weightForObject: (theClass instVarAt: index)).
	index := self instVarIndex: theClass instVarName: 'organization'.
	byteSize := byteSize + (self weightForObject: (theClass instVarAt: index)).
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 11/12/1998 17:08'!
weightForLiteral: aLiteral 
	| literalClass |
	literalClass := aLiteral class.
	literalClass == Array ifTrue: [^self weightForLiteralArray: aLiteral].
	(literalClass == Symbol or: [(literalClass == SmallInteger)])
		ifFalse: [^self byteSizeOfObject: aLiteral].
	^0! !

!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 11/5/1998 00:57'!
weightForLiteralArray: anArray 
	| byteSize |
	byteSize := self byteSizeOfObject: anArray.
	1 to: anArray size do: [:index | byteSize := byteSize + (self weightForLiteral: (anArray at: index))].
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 11/14/1998 14:49'!
weightForMethod: aMethod 
	"RupaInvestigator new weightForMethod: (Point compiledMethodAt: #x)."
	"RupaInvestigator new weightForMethod: (Point compiledMethodAt: #rounded)."
	"RupaInvestigator new weightForMethod: (RupaInvestigator compiledMethodAt: #weightForMethod:)."
	"RupaInvestigator new weightForMethod: (RupaInvestigator compiledMethodAt: #behaviors)."
	"RupaInvestigator new weightForMethod: (Integer compiledMethodAt: #digitDiv:neg:)."
	"RupaInvestigator new weightForMethod2: (RupaInvestigator compiledMethodAt: #test)."

	"ex) see below
	Browser fullOnClass: ClassDescription selector: #spaceUsed.

    The response to size is the method size in bytes, including the method header, the
    literal frame, the bytecodes and the 4-byte sourcecode pointer at the end.
	"

	| byteSize objectHeadersSize size |
	objectHeadersSize := 4.
	"
	ex) see below
	inc := (size + 3) bitAnd: 16r1FFFFFFC.
	from
	Browser fullOnClass: ObjectMemory selector: #instantiateClass:indexableSize:.
	"
	byteSize := (aMethod size + 3) bitAnd: 16r1FFFFFFC.
	byteSize := byteSize + objectHeadersSize.
	aMethod
		literals do: 
			[:literal | 
			size := self weightForLiteral: literal.
			byteSize := byteSize + size].
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 10/3/1998 04:21'!
weightForMethodDictionary: aMethodDictionary 
	"RupaInvestigator new weightForMethodDictionary: Point copyOfMethodDictionary."
	"RupaInvestigator new weightForMethodDictionary: RupaInvestigator copyOfMethodDictionary."

	| byteSize size |
	byteSize := self byteSizeOfObject: aMethodDictionary and: #('array').
	aMethodDictionary
		do: 
			[:method | 
			size := self weightForMethod: method.
			byteSize := byteSize + size].
	^byteSize! !

!RupaInvestigator methodsFor: 'weighing' stamp: 'nishis 10/2/1998 23:33'!
weightForObject: anObject 
	"RupaInvestigator new weightForObject: 0 @ 0."

	(self shouldCountObject: anObject)
		ifFalse: [^0].
	^(self byteSizeOfObject: anObject)
		+ (self byteSizeOfObjectsIn: anObject)! !


!RupaInvestigator methodsFor: 'metrics' stamp: 'nishis 10/2/1998 01:37'!
allClassWeights
	"RupaInvestigator new allClassWeights."

	| targetClasses aDictionary saveCursor byteSize |
	targetClasses := self classes.
	aDictionary := IdentityDictionary new: targetClasses size.
	saveCursor := Cursor currentCursor.
	targetClasses
		do: 
			[:aClass | 
			byteSize := self weightForClass: aClass.
			aDictionary at: aClass put: byteSize.
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	^aDictionary! !

!RupaInvestigator methodsFor: 'metrics'!
allMethodComplexities
	"RupaInvestigator new allMethodComplexities."

	| aDictionary weight complexity |
	aDictionary := self allMethodWeights.
	aDictionary do: [:collection | collection
			do: 
				[:assoc | 
				weight := assoc value.
				complexity := (weight / self class constantForComplexity) asFloat.
				assoc value: complexity]].
	^aDictionary! !

!RupaInvestigator methodsFor: 'metrics' stamp: 'nishis 10/2/1998 01:38'!
allMethodWeights
	"RupaInvestigator new allMethodWeights."

	| targetClasses aDictionary saveCursor messageSelectors aCollection byteSize |
	targetClasses := self behaviors.
	aDictionary := IdentityDictionary new: targetClasses size.
	saveCursor := Cursor currentCursor.
	targetClasses
		do: 
			[:aClass | 
			messageSelectors := aClass selectors.
			aCollection := OrderedCollection new: messageSelectors size.
			messageSelectors
				do: 
					[:aSelector | 
					byteSize := self weightForMethod: (aClass compiledMethodAt: aSelector).
					aCollection add: aSelector -> byteSize].
			aDictionary at: aClass put: aCollection.
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	^aDictionary! !

!RupaInvestigator methodsFor: 'metrics' stamp: 'nishis 10/3/1998 05:37'!
allObjectCounts
	"RupaInvestigator new allObjectCounts."

	| targetClasses aDictionary saveCursor instanceCount aClass |
	targetClasses := self behaviors.
	aDictionary := IdentityDictionary new: targetClasses size.
	targetClasses do: [:theClass | aDictionary at: theClass put: 0].
	Smalltalk garbageCollectMost.
	saveCursor := Cursor currentCursor.
	instanceCount := 0.
	self
		allObjectsDo: 
			[:anObject | 
			aClass := anObject class.
			(aDictionary includesKey: aClass)
				ifTrue: 
					[aDictionary at: aClass put: 1 + (aDictionary at: aClass).
					instanceCount := instanceCount + 1].
			instanceCount \\ 1000 = 0 ifTrue: [Cursor currentCursor = Cursor execute
					ifTrue: [Cursor normal show]
					ifFalse: [Cursor execute show]]].
	saveCursor show.
	^Array with: aDictionary with: instanceCount! !

!RupaInvestigator methodsFor: 'metrics' stamp: 'nishis 10/3/1998 05:42'!
allObjectWeights
	"RupaInvestigator new allObjectWeights."

	| targetClasses aDictionary saveCursor instanceCount byteSize aClass |
	targetClasses := self classes.
	aDictionary := IdentityDictionary new: targetClasses size.
	targetClasses
		do: 
			[:theClass | 
			byteSize := self byteSizeOfObject: theClass.
			byteSize := byteSize + (self byteSizeOfObject: theClass class).
			aDictionary at: theClass put: byteSize].
	Smalltalk garbageCollectMost.
	saveCursor := Cursor currentCursor.
	instanceCount := 0.
	self
		allObjectsDo: 
			[:anObject | 
			aClass := anObject class.
			(aDictionary includesKey: aClass)
				ifTrue: 
					[byteSize := self byteSizeOfObject: anObject.
					byteSize := byteSize + (aDictionary at: aClass).
					aDictionary at: aClass put: byteSize.
					instanceCount := instanceCount + 1].
			instanceCount \\ 1000 = 0 ifTrue: [Cursor currentCursor = Cursor execute
					ifTrue: [Cursor normal show]
					ifFalse: [Cursor execute show]]].
	saveCursor show.
	^Array with: aDictionary with: instanceCount! !

!RupaInvestigator methodsFor: 'metrics'!
allWMCs
	"RupaInvestigator new allWMCs."

	| methodComplexities aDictionary wmc key weight |
	methodComplexities := self allMethodComplexities.
	aDictionary := IdentityDictionary new: (methodComplexities size / 0.6) asInteger.
	methodComplexities
		associationsDo: 
			[:pair | 
			wmc := 0.
			pair value
				do: 
					[:assoc | 
					weight := assoc value.
					wmc := wmc + weight].
			key := pair key.
			key isMeta ifTrue: [key := key soleInstance].
			(aDictionary includesKey: key)
				ifTrue: [aDictionary at: key put: wmc + (aDictionary at: key)]
				ifFalse: [aDictionary at: key put: wmc]].
	^aDictionary! !


!RupaInvestigator methodsFor: 'debug' stamp: 'nishis 11/14/1998 14:50'!
debug: aBlock 
	"aBlock value"! !


!RupaInvestigator methodsFor: 'private' stamp: 'nishis 10/6/1998 02:36'!
allObjectsDo: aBlock 
	| number allObjects |
	number := 0.
	Smalltalk allObjectsDo: [:anObject | number := number + 1].
	allObjects := OrderedCollection new: number.
	Smalltalk allObjectsDo: [:anObject | allObjects add: anObject].
	allObjects do: aBlock! !

!RupaInvestigator methodsFor: 'private' stamp: 'nishis 11/5/1998 03:09'!
baseHeaderSize
	^(ObjectMemory classVarNames includes: #BaseHeaderSize) 
		ifTrue: [ObjectMemory classPool at: #BaseHeaderSize] 
		ifFalse: [4]! !

!RupaInvestigator methodsFor: 'private' stamp: 'nishis 10/2/1998 01:34'!
instVarIndex: anObject instVarName: instVarName 
	"RupaInvestigator new instVarIndex: 0 @ 0 instVarName: #y."

	| aClass instVarNames instVarIndex |
	aClass := anObject class.
	instVarNames := aClass allInstVarNames.
	instVarIndex := instVarNames findFirst: [:each | each asString = instVarName asString].
	^instVarIndex! !

!RupaInvestigator methodsFor: 'private'!
shouldCountObject: anObject 
	^(anObject == nil or: [anObject == true or: [anObject == false]]) not! !

!RupaInvestigator methodsFor: 'private' stamp: 'nishis 11/14/1998 14:51'!
sizeInWordsOf: anObject 
	"copied from SystemTracer>>sizeInWordsOf:."

"NOTE: This is the new length of the object in LONG WORDS.
		Does not include the class (header) word."
	| class |
	class _ anObject class.
	class isBytes ifTrue: [^ anObject basicSize+3 // 4].
	class isBits ifTrue: [^ anObject basicSize].	"in two byte chunks"
	class isVariable ifTrue: [^ class instSize + anObject basicSize].
	^ class instSize! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

RupaInvestigator class
	instanceVariableNames: ''!

!RupaInvestigator class methodsFor: 'copyleft'!
copyright
	^'Copyright (C) 1996 by AOKI Atsushi' copy! !

!RupaInvestigator class methodsFor: 'copyleft'!
version
	^'Rupa 001 (February 7, 1996)' copy! !


!RupaInvestigator class methodsFor: 'instance creation'!
class: aClass 
	^(super new) classes: (Array with: aClass); yourself! !

!RupaInvestigator class methodsFor: 'instance creation'!
classes: classCollection 
	^(super new) classes: classCollection; yourself! !


!RupaInvestigator class methodsFor: 'accessing'!
allBehaviors
	"RupaInvestigator allBehaviors."

	| collection |
	collection := OrderedCollection new: 2048.
	self allBehaviorsDo: [:each | collection add: each].
	^collection! !

!RupaInvestigator class methodsFor: 'accessing'!
allClasses
	"RupaInvestigator allClasses."

	| collection |
	collection := OrderedCollection new: 1024.
	self allClassesDo: [:each | collection add: each].
	^collection! !

!RupaInvestigator class methodsFor: 'accessing'!
constantForComplexity
	"128 is the average byte size of method weights."

	^128! !

!RupaInvestigator class methodsFor: 'accessing' stamp: 'nishis 10/2/1998 01:26'!
implementedMessages
	"RupaInvestigator implementedMessages."

	| implementedMessages saveCursor |
	implementedMessages := IdentitySet new: Symbol allInstances size.
	saveCursor := Cursor currentCursor.
	self 
		allBehaviorsDo: 
			[:aClass | 
			aClass selectors do: [:selector | implementedMessages add: selector].
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	^implementedMessages! !

!RupaInvestigator class methodsFor: 'accessing'!
investigateMessages
	"RupaInvestigator investigateMessages."

	| implementedMessages sentMessages sentAndImplementedMessages notSentMessages notImplementedMessages |
	implementedMessages := self implementedMessages.
	sentMessages := self sentMessages.
	sentAndImplementedMessages := IdentitySet new: sentMessages size.
	notSentMessages := IdentitySet new.
	notImplementedMessages := IdentitySet new.
	implementedMessages do: [:message | (sentMessages includes: message)
			ifTrue: [sentAndImplementedMessages add: message]
			ifFalse: [notSentMessages add: message]].
	sentMessages do: [:message | (implementedMessages includes: message)
			ifFalse: [notImplementedMessages add: message]].
	^Array
		with: sentAndImplementedMessages
		with: notSentMessages
		with: notImplementedMessages! !

!RupaInvestigator class methodsFor: 'accessing' stamp: 'nishis 10/7/1998 00:37'!
sentMessages
	"RupaInvestigator sentMessages."

	| sentMessages saveCursor method literals |
	sentMessages := IdentitySet new: Symbol allInstances size.
	saveCursor := Cursor currentCursor.
	self
		allBehaviorsDo: 
			[:aClass | 
			aClass selectors
				do: 
					[:selector | 
					method := aClass compiledMethodAt: selector.
					literals := method messages.
					literals do: [:symbol | sentMessages add: symbol]].
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	^sentMessages! !


!RupaInvestigator class methodsFor: 'enumerating' stamp: 'nishis 10/2/1998 01:17'!
allBehaviorsDo: aBlock 
	"
	| collection | 
	collection := SortedCollection new. 
	collection sortBlock: [:x :y | x name < y name]. 
	RupaInvestigator allBehaviorsDo: [:each | collection add: each]. 
	^collection
	"

	self rootsOfTheWorld
		do: 
			[:aClass | 
			aBlock value: aClass.
			aClass allSubclassesDo: aBlock]! !

!RupaInvestigator class methodsFor: 'enumerating' stamp: 'nishis 10/2/1998 01:17'!
allClassesDo: aBlock 
	"
	| collection | 
	collection := SortedCollection new. 
	collection sortBlock: [:x :y | x name < y name]. 
	RupaInvestigator allClassesDo: [:each | collection add: each]. 
	^collection 
	"

	self rootsOfTheWorld do: [:aClass | aClass isMeta not
			ifTrue: 
				[aBlock value: aClass.
				aClass allSubclassesDo: [:each | each isMeta not ifTrue: [aBlock value: each]]]]! !


!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:28'!
reportClassWeightsFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportClassWeightsFor: classes on: stream.
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator aDictionary aCollection totalWeight writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	aDictionary := anInvestigator allClassWeights.
	aCollection := SortedCollection new: aDictionary size.
	aCollection sortBlock: [:a1 :a2 | a1 value = a2 value
			ifTrue: [a1 key name < a2 key name]
			ifFalse: [a1 value > a2 value]].
	totalWeight := 0.
	aDictionary
		associationsDo: 
			[:assoc | 
			aCollection add: assoc.
			totalWeight := totalWeight + assoc value].
	writeBlock := 
			[aStream nextPutAll: '*** Class Weight ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:assoc | 
					aStream nextPutAll: assoc value printString.
					aStream tab.
					aStream nextPutAll: assoc key printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: totalWeight printString.
			aStream tab.
			aStream nextPutAll: '*** Total of Class Weight ***'.
			aStream cr.
			aStream nextPutAll: aCollection size printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Classes ***'.
			aStream cr.
			aStream nextPutAll: ((totalWeight / aCollection size) asFloat roundTo: 0.01) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of Classe Weights ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^aDictionary! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:29'!
reportInvestigationOfMessagesOn: aStream 
	"
	| stream model | 
	stream := WriteStream on: String new. 
	RupaInvestigator reportInvestigationOfMessagesOn: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anArray sentAndImplementedMessages notSentMessages notImplementedMessages outputBlock writeBlock |
	anArray := RupaInvestigator investigateMessages.
	sentAndImplementedMessages := anArray at: 1.
	notSentMessages := anArray at: 2.
	notImplementedMessages := anArray at: 3.
	outputBlock := [:set :stream | set asSortedCollection
				do: 
					[:symbol | 
					stream tab.
					stream nextPutAll: symbol.
					stream cr]].
	writeBlock := 
			[aStream nextPutAll: 'Sent and Implemented Messages'.
			aStream nextPutAll: ' (' , sentAndImplementedMessages size printString , ')'.
			aStream cr.
			outputBlock value: Set new value: aStream.
			aStream cr.
			aStream nextPutAll: 'Not Sent Messages'.
			aStream nextPutAll: ' (' , notSentMessages size printString , ')'.
			aStream cr.
			outputBlock value: Set new value: aStream.
			aStream cr.
			aStream nextPutAll: 'Not Implemented Messages'.
			aStream nextPutAll: ' (' , notImplementedMessages size printString , ')'.
			aStream cr.
			outputBlock value: notImplementedMessages value: aStream].
	Cursor write showWhile: [writeBlock value].
	^anArray! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:38'!
reportMethodComplexitiesFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportMethodComplexitiesFor: classes on: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator aDictionary aCollection totalComplexity saveCursor writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	aDictionary := anInvestigator allMethodComplexities.
	aCollection := SortedCollection new: aDictionary size * 20.
	aCollection sortBlock: [:a1 :a2 | (a1 at: 3)
			= (a2 at: 3)
			ifTrue: [(a1 at: 2)
					= (a2 at: 2)
					ifTrue: [(a1 at: 1) name < (a2 at: 1) name]
					ifFalse: [(a1 at: 2)
							< (a2 at: 2)]]
			ifFalse: [(a1 at: 3)
					> (a2 at: 3)]].
	totalComplexity := 0.
	saveCursor := Cursor currentCursor.
	aDictionary
		associationsDo: 
			[:assoc | 
			assoc value
				do: 
					[:each | 
					aCollection add: (Array
							with: assoc key
							with: each key
							with: (each value roundTo: 0.001)).
					totalComplexity := totalComplexity + each value].
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	writeBlock := 
			[aStream nextPutAll: '*** Method Complexity ***'.
			aStream tab.
			aStream nextPutAll: '*** Method ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:array | 
					aStream nextPutAll: (array at: 3) printString.
					aStream tab.
					aStream nextPutAll: (array at: 2).
					aStream tab.
					aStream nextPutAll: (array at: 1) printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: (totalComplexity roundTo: 0.001) printString.
			aStream tab.
			aStream nextPutAll: '*** Total of Method Complexities ***'.
			aStream cr.
			aStream nextPutAll: aCollection size printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Methods ***'.
			aStream cr.
			aStream nextPutAll: ((totalComplexity / aCollection size) asFloat roundTo: 0.001) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of Method Complexities ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^aDictionary! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:39'!
reportMethodWeightsFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportMethodWeightsFor: classes on: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator aDictionary aCollection totalWeight saveCursor writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	aDictionary := anInvestigator allMethodWeights.
	aCollection := SortedCollection new: aDictionary size * 20.
	aCollection sortBlock: [:a1 :a2 | (a1 at: 3)
			= (a2 at: 3)
			ifTrue: [(a1 at: 2)
					= (a2 at: 2)
					ifTrue: [(a1 at: 1) name < (a2 at: 1) name]
					ifFalse: [(a1 at: 2)
							< (a2 at: 2)]]
			ifFalse: [(a1 at: 3)
					> (a2 at: 3)]].
	totalWeight := 0.
	saveCursor := Cursor currentCursor.
	aDictionary
		associationsDo: 
			[:assoc | 
			assoc value
				do: 
					[:each | 
					aCollection add: (Array
							with: assoc key
							with: each key
							with: each value).
					totalWeight := totalWeight + each value].
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	writeBlock := 
			[aStream nextPutAll: '*** Method Weight ***'.
			aStream tab.
			aStream nextPutAll: '*** Method ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:array | 
					aStream nextPutAll: (array at: 3) printString.
					aStream tab.
					aStream nextPutAll: (array at: 2).
					aStream tab.
					aStream nextPutAll: (array at: 1) printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: totalWeight printString.
			aStream tab.
			aStream nextPutAll: '*** Total of Method Weights ***'.
			aStream cr.
			aStream nextPutAll: aCollection size printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Methods ***'.
			aStream cr.
			aStream nextPutAll: ((totalWeight / aCollection size) asFloat roundTo: 0.01) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of Method Weights ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^aDictionary! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:39'!
reportObjectCountsFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportObjectCountsFor: classes on: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator anArray aDictionary instanceCount aCollection saveCursor writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	anArray := anInvestigator allObjectCounts.
	aDictionary := anArray at: 1.
	instanceCount := anArray at: 2.
	aCollection := SortedCollection new: aDictionary size.
	aCollection sortBlock: [:a1 :a2 | a1 value = a2 value
			ifTrue: [a1 key name < a2 key name]
			ifFalse: [a1 value > a2 value]].
	saveCursor := Cursor currentCursor.
	aDictionary
		associationsDo: 
			[:assoc | 
			aCollection add: assoc.
			Cursor currentCursor = Cursor execute
				ifTrue: [Cursor normal show]
				ifFalse: [Cursor execute show]].
	saveCursor show.
	writeBlock := 
			[aStream nextPutAll: '*** Instance Count ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:assoc | 
					aStream nextPutAll: assoc value printString.
					aStream tab.
					aStream nextPutAll: assoc key printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: instanceCount printString.
			aStream tab.
			aStream nextPutAll: '*** Total of Instance Counts ***'.
			aStream cr.
			aStream nextPutAll: aCollection size printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Classes ***'.
			aStream cr.
			aStream nextPutAll: ((instanceCount / aCollection size) asFloat roundTo: 0.01) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of Instance Counts ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^anArray! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:30'!
reportObjectWeightsFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportObjectWeightsFor: classes on: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator anArray aDictionary instanceCount aCollection totalWeight writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	anArray := anInvestigator allObjectWeights.
	aDictionary := anArray at: 1.
	instanceCount := anArray at: 2.
	aCollection := SortedCollection new: aDictionary size.
	aCollection sortBlock: [:a1 :a2 | a1 value = a2 value
			ifTrue: [a1 key name < a2 key name]
			ifFalse: [a1 value > a2 value]].
	totalWeight := 0.
	aDictionary
		associationsDo: 
			[:assoc | 
			aCollection add: assoc.
			totalWeight := totalWeight + assoc value].
	writeBlock := 
			[aStream nextPutAll: '*** Instance Weight ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:assoc | 
					aStream nextPutAll: assoc value printString.
					aStream tab.
					aStream nextPutAll: assoc key printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: totalWeight printString.
			aStream tab.
			aStream nextPutAll: '*** Total of Instance Weights ***'.
			aStream cr.
			aStream nextPutAll: instanceCount printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Instances ***'.
			aStream cr.
			aStream nextPutAll: ((totalWeight / instanceCount) asFloat roundTo: 0.01) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of Instance Wieghts ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^anArray! !

!RupaInvestigator class methodsFor: 'reporting' stamp: 'nishis 10/2/1998 01:30'!
reportWMCsFor: classCollection on: aStream 
	"
	| classes stream model | 
	classes := Array with: RupaInvestigator. 
	stream := WriteStream on: String new. 
	RupaInvestigator reportWMCsFor: classes on: stream. 
	model := (StringHolder new) contents: stream contents. 
	StringHolderView open: model label: 'Report'. 
	"

	| anInvestigator aDictionary aCollection totalWMC writeBlock |
	anInvestigator := RupaInvestigator classes: classCollection.
	aDictionary := anInvestigator allWMCs.
	aCollection := SortedCollection new: aDictionary size.
	aCollection sortBlock: [:a1 :a2 | a1 value = a2 value
			ifTrue: [a1 key name < a2 key name]
			ifFalse: [a1 value > a2 value]].
	totalWMC := 0.
	aDictionary
		associationsDo: 
			[:assoc | 
			aCollection add: assoc.
			totalWMC := totalWMC + assoc value].
	writeBlock := 
			[aStream nextPutAll: '*** WMC ***'.
			aStream tab.
			aStream nextPutAll: '*** Class ***'.
			aStream cr.
			aStream cr.
			aCollection
				do: 
					[:assoc | 
					aStream nextPutAll: (assoc value asFloat roundTo: 0.001) printString.
					aStream tab.
					aStream nextPutAll: assoc key printString.
					aStream cr].
			aStream cr.
			aStream nextPutAll: (totalWMC roundTo: 0.001) printString.
			aStream tab.
			aStream nextPutAll: '*** Total of WMCs ***'.
			aStream cr.
			aStream nextPutAll: aCollection size printString.
			aStream tab.
			aStream nextPutAll: '*** Number of Classes ***'.
			aStream cr.
			aStream nextPutAll: ((totalWMC / aCollection size) asFloat roundTo: 0.001) printString.
			aStream tab.
			aStream nextPutAll: '*** Average of WMCs ***'.
			aStream cr].
	Cursor write showWhile: [writeBlock value].
	^aDictionary! !


!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:31'!
example1
	"RupaInvestigator example1."

	| string stream result |
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportInvestigationOfMessagesOn: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:33'!
example2
	"RupaInvestigator example2."

	| classes string stream result |
	classes := RupaInvestigator allClasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportObjectCountsFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:33'!
example3
	"RupaInvestigator example3."

	| classes string stream result |
	classes := RupaInvestigator allClasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportObjectWeightsFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:33'!
example4
	"RupaInvestigator example4."

	| classes string stream result |
	classes := RupaInvestigator allClasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportClassWeightsFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 11/12/1998 22:09'!
example5
	"RupaInvestigator example5."

	| classes string stream result |
	classes := Magnitude withAllSubclasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportMethodWeightsFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:33'!
example6
	"RupaInvestigator example6."

	| classes string stream result |
	classes := Magnitude withAllSubclasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportMethodComplexitiesFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !

!RupaInvestigator class methodsFor: 'examples' stamp: 'nishis 10/2/1998 01:33'!
example7
	"RupaInvestigator example7."

	| classes string stream result |
	classes := Magnitude withAllSubclasses.
	string := 'Report.txt'.
	stream := FileStream newFileNamed: string.
	result := RupaInvestigator reportWMCsFor: classes on: stream.
	stream close.
	(FileStream oldFileNamed: string) edit.
	^result! !


!RupaInvestigator class methodsFor: 'saving' stamp: 'nishis 11/12/1998 19:01'!
save
	"RupaInvestigator save."

	| fileName classCollection aStream timeStamp |
	fileName := 'Rupa.st'.
	classCollection := self saveClasses.
	aStream := FileStream newFileNamed: fileName.
	aStream reset.
	Cursor write
		showWhile: 
			[timeStamp := Date today printString, ' ' , Time now printString.
			aStream cr.
			aStream nextChunkPut: timeStamp printString.
			aStream cr; cr.
			(self comment isNil or: [self comment isEmpty])
				ifFalse: 
					[aStream nextChunkPut: (String cr, 
							self comment asString, String cr) printString.
					aStream cr; cr].
			classCollection
				do: 
					[:aClass | 
					aStream nextChunkPut: aClass definition.
					aStream cr; cr].
			classCollection
				do: 
					[:aClass | 
					aStream nextPut: Character newPage.
					aStream cr.
					aClass fileOutOn: aStream moveSource: false toFile: 0.
					aStream cr.
					aStream cr]].
	aStream close.
	^classCollection! !

!RupaInvestigator class methodsFor: 'saving' stamp: 'nishis 10/6/1998 02:30'!
saveClasses
	"RupaInvestigator saveClasses."

	| patternCollection classCollection string something |
	patternCollection := #('*RupaInvestigator*').
	classCollection := (SystemOrganization superclassOrder: self category).
	classCollection := classCollection
				select: 
					[:aClass | 
					string := aClass name asString.
					something := patternCollection detect: [:it | it match: string]
								ifNone: [nil].
					something notNil].
	^classCollection! !


!RupaInvestigator class methodsFor: 'private' stamp: 'nishis 10/2/1998 01:17'!
rootsOfTheWorld
	^(Smalltalk select: 
		[:each | (each isKindOf: Behavior) and: [each superclass == nil]]) asOrderedCollection! !



