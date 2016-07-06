Delay subclass: #SUnitDelay
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit-Preload'!

Object subclass: #SUnitNameResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit-Preload'!

SUnitNameResolver class
	instanceVariableNames: ''!
	
Exception subclass: #TestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit-Preload'!
!TestFailure commentStamp: '<historical>' prior: 0!
Signaled in case of a failed test (failure). The test framework distinguishes between failures and errors. A failure is anticipated and checked for with assertions. Errors are unanticipated problems like a division by 0 or an index out of bounds ...!

TestFailure subclass: #ResumableTestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit-Preload'!
!ResumableTestFailure commentStamp: '<historical>' prior: 0!
A ResumableTestFailure triggers a TestFailure, but lets execution of the TestCase continue. this is useful when iterating through collections, and #assert: ing on each element. in combination with methods like testcase>>#assert:description:, this lets you run through a whole collection and note which tests pass.

here''s an example:

	

	(1 to: 30) do: [ :each |
		self assert: each odd description: each printString, ' is even' resumable: true]

for each element where #odd returns <false>, the element will be printed to the Transcript. !

!Object methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:58'!
sunitAddDependent: anObject
 
        self addDependent: anObject! !

!Object methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:58'!
sunitChanged: anAspect
 
        self changed: anAspect! !

!Object methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:58'!
sunitRemoveDependent: anObject
 
        self removeDependent: anObject! !

!Behavior methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:46'!
sunitAllSelectors 

        ^self allSelectors asSortedCollection asOrderedCollection! !

!Behavior methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:46'!
sunitSelectors
 
        ^self selectors asSortedCollection asOrderedCollection! !

!BlockContext methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:56'!
sunitEnsure: aBlock
 
        ^self ensure: aBlock! !

!BlockContext methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:57'!
sunitOn: anException do: aHandlerBlock
 
        ^self on: anException do: aHandlerBlock! !

!Class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 09:57'!
sunitName
 
        ^self name! !

!Exception methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:03'!
sunitExitWith: aValue
 
        self return: aValue! !

!Exception class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:04'!
sunitSignalWith: aString
  
        ^self signal: aString! !

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'SSS 7/3/2000
11:11'!
classNamed: aSymbol

        ^Smalltalk
                at: aSymbol
                ifAbsent: [nil].! !

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'!
defaultLogDevice
	^ Transcript! !

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'!
errorObject
	^Error! !

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'!
mnuExceptionObject
	^MessageNotUnderstood new! !

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 13:56'!
notificationObject
	^Notification new! !
	
!String methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:04'!
sunitAsSymbol
 
        ^self asSymbol! !

!String methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:04'!
sunitMatch: aString
 
        ^self match: aString! !

!String methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:04'!
sunitSubStrings
 
        ^self substrings! !


!Symbol methodsFor: 'Camp Smalltalk' stamp: 'jp 3/17/2003 10:05'!
sunitAsClass
 
        ^SUnitNameResolver classNamed: self! !
 
!TestFailure methodsFor: 'Camp Smalltalk' stamp: 'Sames 4/11/2000 18:07'!
defaultAction

        Debugger
                openContext: initialContext
                label: messageText
                contents: initialContext shortStack! !

!ResumableTestFailure methodsFor: 'Camp Smalltalk'!
isResumable
	"Of course a ResumableTestFailure is resumable ;-)"

	^true! !

!ResumableTestFailure methodsFor: 'Camp Smalltalk'!
sunitExitWith: aValue
	self resume: aValue! !
