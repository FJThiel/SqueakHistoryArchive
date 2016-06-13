"      NAME	CaseStatement
       AUTHOR	cgreuter@calum.csclub.uwaterloo.ca (Chris Reuter)
       URL	(none)
       FUNCTION	Control structure similar to C's switch/case construct.
       KEYWORDS	case switch
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.0.1
       DATE	03-Sep-98

SUMMARY

This is my trivial Squeak port of Paul Bauman'scase statements as published in the July/August1997 issue of The Smalltalk Report.It seems to work correctly, but your mileagemay vary.  If you do find and fix a bug, pleasesend me a copy of the changes.See the class comment for documentation.

				Chris Reuter
"!
'From Squeak 2.0 of May 22, 1998 on 17 July 1998 at 7:34:18 pm'!
Object subclass: #Case
	instanceVariableNames: 'satisfied response criterion '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Case Statement'!

!Object methodsFor: 'case statement'!
switch
	"Answer a new instance of Case with the receiver as the
	 criterion to test against."

	^Case for: self! !


!Case commentStamp: 'cgr 7/17/1998 19:34' prior: 0!
Case class add a robust case statement implementation to Smalltalk.  The implemenation does NOT require any
compiler changes.  The code is from the article, "Smalltalk Case Statements" by Paul Baumann in the July/August
1997 issue of The Smalltalk Report.  Baumann's article illustrates the ease of providing support for a case
statement, gives examples of their use,  and explains why the example case statement is appropiate.  Comments
taken from Baumann's article on the factors of when using a case statement is good.  These factors are:
	(a) tests must be performed in sequence;
	(b) the conditional test is complex;
	(c) the test conditions are not consistent;
	(d) the criterion isn't easily represented by the application classes;
	(e) the test conditions depend on state of the criterion rather than the class or behavior of the criterion;
	(f) ease of maintenance is more important than performance;
	(g) there are more than three possible results.

Various class examples illustrate some factor or factors as stated by Baumann.

InstanceVariables:
	satisfied <Boolean | nil > 
	response <Object> result of  execBlock value where execBlock is from one of the following:
		caseIs: testObject then: execBlock
		caseIsAny: testCollection then: execBlock
		isFalse: testBlock then: execBlock
		isTrue: testBlock then: execBlock
		default: execBlock
	criterion <Object> for which an instance of Case was made.  Case class >> for: anObject
					which is sent by Object >> switch

ClassVariables: NONE

!

!Case methodsFor: 'public'!
case: oneArgTestBlock process: execBlock
	"Idential to #case:then: except that the receiver
	 will not be satisfied, and processing will fall
	 through to the next statement."

	self isSatisfied ifFalse: [
		(oneArgTestBlock value: criterion) ifTrue: [
			self response: execBlock value.
			satisfied := false.
		].
	].
	^self response! !

!Case methodsFor: 'public'!
case: oneArgTestBlock then: execBlock
	"oneArgTestBlock must return a boolean value
	 when passed the criterion of the receiver."

	self isSatisfied ifFalse: [
		(oneArgTestBlock value: criterion) ifTrue: [
			self response: execBlock value.
			satisfied := true.
		].
	].
	^self response! !

!Case methodsFor: 'public'!
caseIs: testObject process: execBlock
	"Idential to #caseIs:then: except that the receiver
	 will not be satisfied, and processing will fall
	 through to the next statement."

	^self 
		case: [:caseCriterion| caseCriterion = testObject ]
		process: execBlock! !

!Case methodsFor: 'public'!
caseIs: testObject then: execBlock
	"If testObject equals the criterion of the
	 receiver, then satisfy the receiver and 
	 answer the value of execBlock."

	^self 
		case: [:caseCriterion| caseCriterion = testObject ]
		then: execBlock! !

!Case methodsFor: 'public'!
caseIsAny: testCollection process: execBlock
	"Idential to #caseIsAny:then: except that the receiver
	 will not be satisfied, and processing will fall
	 through to the next statement."

	^self 
		case: [:caseCriterion| testCollection includes: caseCriterion ]
		process: execBlock! !

!Case methodsFor: 'public'!
caseIsAny: testCollection then: execBlock
	"If testCollection includes the criterion of the
	 receiver, then satisfy the receiver and 
	 answer the value of execBlock."

	^self 
		case: [:caseCriterion| testCollection includes: caseCriterion ]
		then: execBlock! !

!Case methodsFor: 'public'!
default: execBlock
	"This method sets the response without signaling
	 the receiver has been satisfied. Note that the value
	 of satisfied can be nil, true, or false; and that only
	 a value of nil indicates that no case was run."
	
	satisfied isNil ifTrue: [self response: execBlock value].
	^self response! !

!Case methodsFor: 'public'!
isFalse: testBlock process: execBlock
	"Idential to #isFalse:then: except that the receiver
	 will not be satisfied, and processing will fall
	 through to the next statement."

	^self 
		case: [:caseCriterion| testBlock value not ]
		process: execBlock! !

!Case methodsFor: 'public'!
isFalse: testBlock then: execBlock
	"testBlock is a zero argument block that when
	 evaluates false, execBlock will be executed.
	 The criterion of the receiver is ignored."

	^self 
		case: [:caseCriterion| testBlock value not ]
		then: execBlock! !

!Case methodsFor: 'public'!
isTrue: testBlock process: execBlock
	"Idential to #isTrue:then: except that the receiver
	 will not be satisfied, and processing will fall
	 through to the next statement."

	^self 
		case: [:caseCriterion| testBlock value ]
		process: execBlock! !

!Case methodsFor: 'public'!
isTrue: testBlock then: execBlock
	"testBlock is a zero argument block that when
	 evaluates true, execBlock will be executed.
	 The criterion of the receiver is ignored."

	^self 
		case: [:caseCriterion| testBlock value ]
		then: execBlock! !

!Case methodsFor: 'private'!
criterion: anObject
	"Set the standard of judging a true case."

	criterion := anObject! !

!Case methodsFor: 'private'!
isSatisfied
	"satisfied may be nil, false, or true."

	^satisfied == true! !

!Case methodsFor: 'private'!
response
	"Answer the last response for a processed 
	 selection, or nil if no selections were processed."

	^response! !

!Case methodsFor: 'private'!
response: newResponse
	"Set the value that will be returned for
	 the receiver."

	response := newResponse! !


!Case class methodsFor: 'documentation'!
aaClassComment
"Class comment located here so it does not get lost when filed in to Digitalk Smalltalk.
Case class add a robust case statement implementation to Smalltalk.  The implemenation does NOT require any complier changes.  The code is from the article, 'Smalltalk Case Statements' by Paul Baumann in the July/August 1997 issue of The Smalltalk Report.  Baumann's article illustrates the ease of providing support for a case statement, gives examples of their use,  and explains why the example case statement is appropiate.  Comments taken from Baumann's article on the factors of when using a case statement is good.  These factors are:
(a) tests must be performed in sequence;
(b) the conditional test is complex;
(c) the test conditions are not consistent;
(d) the criterioan isn't easily represented by the application classes;
(e) the test conditions depend on state of the criterion reather than the class or behavior of the criterion;
(f) ease of maintenance is more important than performance;
(g) there are more than three possible results.

Various class examples illustrate some factor or factors as stated by Baumann.

InstanceVariables:
satisfied <Boolean | nil > 
response <Object> result of  execBlock value where execBlock is from one of the following:
	caseIs: testObject then: execBlock
	caseIsAny: testCollection then: execBlock
	isFalse: testBlock then: execBlock
	isTrue: testBlock then: execBlock
	default: execBlock
criterion <Object> for which an instance of Case was made using
	Case class >> for: anObject which is send by Object >> switch

ClassVariables: NONE
"! !

!Case class methodsFor: 'documentation'!
programersGuide
"
A simple example is given in #vendLargestAvailableCurrency: unVendedCurrenyValue
The first part of the method after comments is:
	| hasHundred hasFifty hasTwenty hasTen |
	hasHundred := true. 	hasFifty := true. hasTwenty := true. hasTen := true.
	^unVendedCurrenyValue switch
			. . . cascaded case keyword messages  . . .
The activation of the Case statements is via the unary message #switch.  #switch creates an instance of Case with the receiver (unVendedCurrenyValue) stored in the instance variable, criterion.  The cascaded case keyword messages are then send to the instance of Case.  The method is exited when the first #case: block that is 'satisfied' after processing the #then: block.  If no case: blocks were satisfied then the default: block is executed.

See Case instance protocol 'public' for the available types of Case keyword messages.  The meanings of Case keyword and arguments are as follows:
default: aBlock - where aBlock is a zero argument block which is executed if none of the preceeding case statements criterion tests were satisified.
A then: keyword means exit the case cascade after evaluating execBlock.
A process: keyword means evaluate the execBlock and continue with the next case statement.
oneArgTestBlock - a one argument block which will be sent the value of criterion when evaluated.
	Block MUST answer a boolean indicating the result of the test.
execBlock - a zero argument block to evaluate when the criterion test result is true (satisfied).
testObject - test the testObject equal (=) the criterion.
testCollection - A collection of obects which the criterion must be equal(=) to one of them.
testBlock - a one argument block that must answer a boolean.  The criterion is send to testBlock.

Case Statements that will test against the criterion.
	case: oneArgTestBlock then: execBlock
	case: oneArgTestBlock process: execBlock

	caseIs: testObject then: execBlock
	caseIs: testObject process: execBlock
	caseIsAny: testCollection then: execBlock
	caseIsAny: testCollection process: execBlock
For #caseIs: testObject,  testObject is tested to be equal the criterion.
For #caseIsAny: testCollection,  testCollection includes: criterion


Case Statements that will NOT test against the criterion.
isFalse: testBlock then: execBlock
isFalse: testBlock process: execBlock
isTrue: testBlock then: execBlock
isTrue: testBlock process: execBlock

Argument testBlock is a zero argument block that must evaluate to a boolean.
"! !

!Case class methodsFor: 'examples'!
enumerationTest: anInteger
	"Answer true if anInteger is included in collection else answer false.
	This can be combined with other caseIsAny:then: and case:then: statements to test aValue to be one of a combination of discrete values or passed any of several range tests."
	"Case enumerationTest: 6."
	"Case enumerationTest: 10."

	^anInteger switch
		caseIsAny: #( 1 3 6 12 16) then: [ true ];
		default: [ false ].! !

!Case class methodsFor: 'examples'!
vendLargestAvailableCurrency: unVendedCurrenyValue
	"Similiar to Baumann's MoneyVendor>>vendLargestAvailableCurrency but modified to not require 
	additional classes or new methods. "
	"Case vendLargestAvailableCurrency:   5"
	"Case vendLargestAvailableCurrency:  50"
	"Case vendLargestAvailableCurrency: 500"

	| hasHundred hasFifty hasTwenty hasTen |
	hasHundred := true. 	hasFifty := true. hasTwenty := true. hasTen := true.
	^unVendedCurrenyValue switch
		case: [:value | value >= 100 and: [hasHundred]]
			then: ['vend hundred']; 
		case: [:value | value >= 50 and: [hasFifty]]
			then: ['vend fifty']; 
		case: [:value | value >= 20 and: [hasTwenty]]
			then: ['vend twenty']; 
		case: [:value | value >= 10 and: [hasTen]]
			then: ['vend ten']; 
		default: ['raise out of cash signal']! !

!Case class methodsFor: 'instance creation'!
for: anObject

	^self new criterion: anObject! !



