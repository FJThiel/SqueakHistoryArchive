"
	Automatically generated using the Rosetta2Cs stylesheet.
	
	Author: Joseph Pelrine
	Copyright: 2002 MetaProg GmbH
"

	Object subclass:  #TestCase
	instanceVariableNames: 'testSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
	
TestCase class 
	instanceVariableNames: ''!
	
	TestCase comment: 'A TestCase is a Command representing the future running of a test case. Create one with the class method #selector: aSymbol, passing the name of the method to be run when the test case runs.

When you discover a new fixture, subclass TestCase, declare instance variables for the objects in the fixture, override #setUp to initialize the variables, and possibly override# tearDown to deallocate any external resources allocated in #setUp.

When you are writing a test case method, send #assert: aBoolean when you want to check for an expected value. For example, you might say "self assert: socket isOpen" to test whether or not a socket is open at a point in a test.'!
	Object subclass:  #TestResource
	instanceVariableNames: 'name description'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
	
TestResource class 
	instanceVariableNames: 'current'!
	
	Object subclass:  #TestResult
	instanceVariableNames: 'failures errors passed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
	
TestResult class 
	instanceVariableNames: ''!
	
	TestResult comment: 'This is a Collecting Parameter for the running of a bunch of tests. TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. Kent has seen TestResults that recorded coverage information and that sent email when they were done.'!
	Object subclass:  #TestSuite
	instanceVariableNames: 'tests resources name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
	
TestSuite class 
	instanceVariableNames: ''!
	
	TestSuite comment: 'This is a Composite of Tests, either TestCases or other TestSuites. The common protocol is #run: aTestResult and the dependencies protocol'!
	
!TestCase methodsFor: 'Dependencies' !
addDependentToHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			! !

!TestCase methodsFor: 'Accessing' !
assert: aBoolean

	aBoolean ifFalse: [self signalFailure: 'Assertion failed']
			! !

!TestCase methodsFor: 'Accessing' !
assert: aBoolean description: aString
	aBoolean ifFalse: [
		self logFailure: aString.
		TestResult failure sunitSignalWith: aString]
			! !

!TestCase methodsFor: 'Accessing' !
assert: aBoolean description: aString resumable: resumableBoolean 
	| exception |
	aBoolean
		ifFalse: 
			[self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]
			! !

!TestCase methodsFor: 'Running' !
debug
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[(self class selector: testSelector) runCase] 
		sunitEnsure: [self resources do: [:each | each reset]]
			! !

!TestCase methodsFor: 'Running' !
debugAsFailure
	| semaphore |
	semaphore := Semaphore new.
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[semaphore wait. self resources do: [:each | each reset]] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore.

			! !

!TestCase methodsFor: 'Accessing' !
deny: aBoolean

	self assert: aBoolean not
			! !

!TestCase methodsFor: 'Accessing' !
deny: aBoolean description: aString
	self assert: aBoolean not description: aString
			! !

!TestCase methodsFor: 'Accessing' !
deny: aBoolean description: aString resumable: resumableBoolean 
	self
		assert: aBoolean not
		description: aString
		resumable: resumableBoolean
			! !

!TestCase methodsFor: 'Private' !
executeShould: aBlock inScopeOf: anExceptionalEvent 
	^[aBlock value.
 	false] sunitOn: anExceptionalEvent
		do: [:ex | ex sunitExitWith: true]
			! !

!TestCase methodsFor: 'Running' !
failureLog	
	^SUnitNameResolver defaultLogDevice

			! !

!TestCase methodsFor: 'Running' !
isLogging
	"By default, we're not logging failures. If you override this in 
	a subclass, make sure that you override #failureLog"
	^false
			! !

!TestCase methodsFor: 'Running' !
logFailure: aString
	self isLogging ifTrue: [
		self failureLog 
			cr; 
			nextPutAll: aString; 
			flush]
			! !

!TestCase methodsFor: 'Running' !
openDebuggerOnFailingTestMethod
	"SUnit has halted one step in front of the failing test method. Step over the 'self halt' and 
	 send into 'self perform: testSelector' to see the failure from the beginning"

	self
		halt;
		performTest
			! !

!TestCase methodsFor: 'Private' !
performTest

	self perform: testSelector sunitAsSymbol
			! !

!TestCase methodsFor: 'Printing' !
printOn: aStream

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: testSelector
			! !

!TestCase methodsFor: 'Dependencies' !
removeDependentFromHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			! !

!TestCase methodsFor: 'Accessing' !
resources
	| allResources resourceQueue |
	allResources := Set new.
	resourceQueue := OrderedCollection new.
	resourceQueue addAll: self class resources.
	[resourceQueue isEmpty] whileFalse: [
		| next |
		next := resourceQueue removeFirst.
		allResources add: next.
		resourceQueue addAll: next resources].
	^allResources
			! !

!TestCase methodsFor: 'Running' !
run
	| result |
	result := TestResult new.
	self run: result.
	^result
			! !

!TestCase methodsFor: 'Running' !
run: aResult
	aResult runCase: self
			! !

!TestCase methodsFor: 'Running' !
runCase

	[self setUp.
	self performTest] sunitEnsure: [self tearDown]
			! !

!TestCase methodsFor: 'Running' !
runCaseAsFailure: aSemaphore
	[self setUp.
	self openDebuggerOnFailingTestMethod] sunitEnsure: [
		self tearDown.
		aSemaphore signal]
			! !

!TestCase methodsFor: 'Accessing' !
selector
	^testSelector
			! !

!TestCase methodsFor: 'Private' !
setTestSelector: aSymbol
	testSelector := aSymbol
			! !

!TestCase methodsFor: 'Running' !
setUp
			! !

!TestCase methodsFor: 'Accessing' !
should: aBlock
	self assert: aBlock value
			! !

!TestCase methodsFor: 'Accessing' !
should: aBlock description: aString
	self assert: aBlock value description: aString
			! !

!TestCase methodsFor: 'Accessing' !
should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
			! !

!TestCase methodsFor: 'Accessing' !
should: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString
			! !

!TestCase methodsFor: 'Accessing' !
shouldnt: aBlock
	self deny: aBlock value
			! !

!TestCase methodsFor: 'Accessing' !
shouldnt: aBlock description: aString
	self deny: aBlock value description: aString
			! !

!TestCase methodsFor: 'Accessing' !
shouldnt: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
			! !

!TestCase methodsFor: 'Accessing' !
shouldnt: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not 		description: aString
			! !

!TestCase methodsFor: 'Accessing' !
signalFailure: aString
	TestResult failure sunitSignalWith: aString! !

!TestCase methodsFor: 'Running' !
tearDown
			! !

!TestResource methodsFor: 'Accessing' !
description

	description isNil
		ifTrue: [^''].

	^description
			! !

!TestResource methodsFor: 'Accessing' !
description: aString

	description := aString
			! !

!TestResource methodsFor: 'Init / Release' !
initialize
	self setUp

			! !

!TestResource methodsFor: 'Testing' !
isAvailable
	"override to provide information on the
	readiness of the resource"
	
	^true
			! !

!TestResource methodsFor: 'Testing' !
isUnavailable
	"override to provide information on the
	readiness of the resource"
	
	^self isAvailable not
			! !

!TestResource methodsFor: 'Accessing' !
name

	name isNil
		ifTrue: [^self printString].

	^name
			! !

!TestResource methodsFor: 'Accessing' !
name: aString

	name := aString
			! !

!TestResource methodsFor: 'Printing' !
printOn: aStream

	aStream nextPutAll: self class printString
			! !

!TestResource methodsFor: 'Accessing' !
resources
	^self class resources
			! !

!TestResource methodsFor: 'Running' !
setUp
	"Does nothing. Subclasses should override this
	to initialize their resource"
			! !

!TestResource methodsFor: 'Running' !
signalInitializationError
	^self class signalInitializationError
			! !

!TestResource methodsFor: 'Running' !
tearDown
	"Does nothing. Subclasses should override this
	to tear down their resource"
			! !

!TestResult methodsFor: 'Accessing' !
correctCount
	"depreciated - use #passedCount"

	^self passedCount
			! !

!TestResult methodsFor: 'Accessing' !
defects
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself
			! !

!TestResult methodsFor: 'Accessing' !
errorCount

	^self errors size
			! !

!TestResult methodsFor: 'Accessing' !
errors

	errors isNil
		ifTrue: [errors := OrderedCollection new].
	^errors
			! !

!TestResult methodsFor: 'Accessing' !
failureCount

	^self failures size
			! !

!TestResult methodsFor: 'Accessing' !
failures
	failures isNil
		ifTrue: [failures := Set new].
	^failures
			! !

!TestResult methodsFor: 'Testing' !
hasErrors

	^self errors size > 0
			! !

!TestResult methodsFor: 'Testing' !
hasFailures

	^self failures size > 0
			! !

!TestResult methodsFor: 'Testing' !
hasPassed

	^self hasErrors not and: [self hasFailures not]
			! !

!TestResult methodsFor: 'Init / Release' !
initialize
			! !

!TestResult methodsFor: 'Testing' !
isError: aTestCase

	^self errors includes: aTestCase
			! !

!TestResult methodsFor: 'Testing' !
isFailure: aTestCase
	^self failures includes: aTestCase
			! !

!TestResult methodsFor: 'Testing' !
isPassed: aTestCase

	^self passed includes: aTestCase
			! !

!TestResult methodsFor: 'Accessing' !
passed

	passed isNil
		ifTrue: [passed := OrderedCollection new].

	^passed
			! !

!TestResult methodsFor: 'Accessing' !
passedCount

	^self passed size
			! !

!TestResult methodsFor: 'Printing' !
printOn: aStream

	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self correctCount printString;
		nextPutAll: ' passed, ';
		nextPutAll: self failureCount printString;
		nextPutAll: ' failed, ';
		nextPutAll: self errorCount printString;
		nextPutAll: ' error'.

	self errorCount ~= 1
		ifTrue: [aStream nextPut: $s]
			! !

!TestResult methodsFor: 'Running' !
runCase: aTestCase

	| testCasePassed |

	testCasePassed :=
		[
			[
				aTestCase runCase.
				true]
					sunitOn: self class failure
					do: [:signal |
						self failures add: aTestCase.
						signal sunitExitWith: false]]
							sunitOn: self class error
							do: [:signal |
								self errors add: aTestCase.
								signal sunitExitWith: false].

	testCasePassed
		ifTrue: [self passed add: aTestCase]
			! !

!TestResult methodsFor: 'Accessing' !
runCount

	^self passedCount + self failureCount + self errorCount
			! !

!TestResult methodsFor: 'Accessing' !
tests

	^(OrderedCollection new: self runCount)
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself
			! !

!TestSuite methodsFor: 'Dependencies' !
addDependentToHierachy: anObject
	self sunitAddDependent: anObject.
	self tests do: [ :each | each addDependentToHierachy: anObject]
			! !

!TestSuite methodsFor: 'Accessing' !
addTest: aTest
	self tests add: aTest
			! !

!TestSuite methodsFor: 'Accessing' !
addTests: aCollection 
	aCollection do: [:eachTest | self addTest: eachTest]
			! !

!TestSuite methodsFor: 'Accessing' !
defaultResources
	^self tests 
		inject: Set new
		into: [:coll :testCase | 
			coll
				addAll: testCase resources;
				yourself]
			! !

!TestSuite methodsFor: 'Accessing' !
name

	^name
			! !

!TestSuite methodsFor: 'Accessing' !
name: aString

	name := aString
			! !

!TestSuite methodsFor: 'Dependencies' !
removeDependentFromHierachy: anObject
	self sunitRemoveDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]
			! !

!TestSuite methodsFor: 'Accessing' !
resources
	resources isNil ifTrue: [resources := self defaultResources].
	^resources
			! !

!TestSuite methodsFor: 'Accessing' !
resources: anObject
	resources := anObject
			! !

!TestSuite methodsFor: 'Running' !
run
	| result |
 	result := TestResult new.
	self resources do: [ :res |
		res isAvailable ifFalse: [^res signalInitializationError]].
	[self run: result] sunitEnsure: [self resources do: [:each | each reset]].
	^result
			! !

!TestSuite methodsFor: 'Running' !
run: aResult 
	self tests do: [:each | 
		self sunitChanged: each.
		each run: aResult]
			! !

!TestSuite methodsFor: 'Accessing' !
tests
	tests isNil ifTrue: [tests := OrderedCollection new].
	^tests
			! !

!TestCase class methodsFor: 'Accessing' !
allTestSelectors

	^self sunitAllSelectors select: [:each | 'test*' sunitMatch: each]
			! !

!TestCase class methodsFor: 'Building Suites' !
buildSuite
	| suite |
	^self isAbstract
		ifTrue: 
			[suite := self suiteClass named: self name asString.
			self allSubclasses 
				do: [:each | each isAbstract ifFalse: [suite addTest: each buildSuiteFromSelectors]].
			suite]
		ifFalse: [self buildSuiteFromSelectors]
			! !

!TestCase class methodsFor: 'Building Suites' !
buildSuiteFromAllSelectors

	^self buildSuiteFromMethods: self allTestSelectors
			! !

!TestCase class methodsFor: 'Building Suites' !
buildSuiteFromLocalSelectors

	^self buildSuiteFromMethods: self testSelectors
			! !

!TestCase class methodsFor: 'Building Suites' !
buildSuiteFromMethods: testMethods

	^testMethods
		inject: (self suiteClass named: self name asString)
		into: [:suite :selector |
			suite
				addTest: (self selector: selector);
				yourself]
			! !

!TestCase class methodsFor: 'Building Suites' !
buildSuiteFromSelectors

	^self shouldInheritSelectors
		ifTrue: [self buildSuiteFromAllSelectors]
		ifFalse: [self buildSuiteFromLocalSelectors]
			! !

!TestCase class methodsFor: 'Instance Creation' !
debug: aSymbol

	^(self selector: aSymbol) debug
			! !

!TestCase class methodsFor: 'Testing' !
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TestCase
			! !

!TestCase class methodsFor: 'Accessing' !
resources

	^#()
			! !

!TestCase class methodsFor: 'Instance Creation' !
run: aSymbol

	^(self selector: aSymbol) run
			! !

!TestCase class methodsFor: 'Instance Creation' !
selector: aSymbol

	^self new setTestSelector: aSymbol
			! !

!TestCase class methodsFor: 'Testing' !
shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."

	^self superclass isAbstract
		or: [self testSelectors isEmpty]

"$QA Ignore:Sends system method(superclass)$"
			! !

!TestCase class methodsFor: 'Instance Creation' !
suite

	^self buildSuite
			! !

!TestCase class methodsFor: 'Building Suites' !
suiteClass
	^TestSuite
			! !

!TestCase class methodsFor: 'Accessing' !
testSelectors

	^self sunitSelectors select: [:each | 'test*' sunitMatch: each]
			! !

!TestCase class methodsFor: 'Accessing' !
sunitVersion
	^'3.1'
			! !

!TestResource class methodsFor: 'Accessing' !
current

	current isNil
		ifTrue: [current := self new].

	^current
			! !

!TestResource class methodsFor: 'Accessing' !
current: aTestResource

	current := aTestResource
			! !

!TestResource class methodsFor: 'Testing' !
isAbstract
	"Override to true if a TestResource subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TestResource
			! !

!TestResource class methodsFor: 'Testing' !
isAvailable
	^self current notNil and: [self current isAvailable]
			! !

!TestResource class methodsFor: 'Testing' !
isUnavailable

	^self isAvailable not
			! !

!TestResource class methodsFor: 'Creation' !
new

	^super new initialize
			! !

!TestResource class methodsFor: 'Creation' !
reset

	current notNil ifTrue: [
		[current tearDown] ensure: [
			current := nil]]
			! !

!TestResource class methodsFor: 'Accessing' !
resources
	^#()
			! !

!TestResource class methodsFor: 'Creation' !
signalInitializationError
	^TestResult signalErrorWith: 'Resource ' , self name , ' could not be initialized'
			! !

!TestResult class methodsFor: 'Exceptions' !
error
	^self exError
			! !

!TestResult class methodsFor: 'Exceptions' !
exError
	^SUnitNameResolver errorObject
			! !

!TestResult class methodsFor: 'Exceptions' !
failure
	^TestFailure
			! !

!TestResult class methodsFor: 'Init / Release' !
new
	^super new initialize
			! !

!TestResult class methodsFor: 'Exceptions' !
resumableFailure
	^ResumableTestFailure
			! !

!TestResult class methodsFor: 'Exceptions' !
signalErrorWith: aString 
	self error sunitSignalWith: aString
			! !

!TestResult class methodsFor: 'Exceptions' !
signalFailureWith: aString 
	self failure sunitSignalWith: aString
			! !

!TestSuite class methodsFor: 'Creation' !
named: aString

	^self new
		name: aString;
		yourself
			! !
