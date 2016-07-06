'From Squeak 1.2 of June 29, 1997 on 8 August 1997 at 10:01:50 pm'!Object subclass: #Exception
	instanceVariableNames: 'description '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions'!
Exception subclass: #Error
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions'!
Error subclass: #DumbError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions Testing'!
Object subclass: #ExceptionClassList
	instanceVariableNames: 'list '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions'!
Object subclass: #ExceptionHandler
	instanceVariableNames: 'normalBlock exceptionBlock exceptionClassList isHandlingException '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions'!
Object subclass: #ExceptionTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions Testing'!
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList errorHandler exceptionHandlers '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!
Error subclass: #StupidError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Exceptions Testing'!

!BlockContext methodsFor: 'vs exceptions' stamp: 'taj 8/6/97 01:06'!
ensure: aBlock
	self ifCurtailed: aBlock.
	^aBlock value! !

!BlockContext methodsFor: 'vs exceptions' stamp: 'taj 8/4/97 04:52'!
ifCurtailed: aBlock
	^self on: Error do: [ :anException| aBlock value. anException pass ].! !

!BlockContext methodsFor: 'vs exceptions' stamp: 'taj 8/5/97 23:23'!
on: exceptionClassOrExceptionClassList do: aBlockContext
	^(ExceptionHandler
		normalBlock: self
		exceptionClassList: exceptionClassOrExceptionClassList asExceptionClassList
		exceptionBlock: aBlockContext) value ! !


!Exception methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:45'!
description
	^description! !

!Exception methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:45'!
description: aString
	description := aString! !

!Exception methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 22:19'!
pass
	Processor activeProcess handleException: self! !


!Exception class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 04:56'!
, anExceptionClass
	^ExceptionClassList new, self, anExceptionClass ! !

!Exception class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 05:17'!
asExceptionClassList
	^ExceptionClassList new,self! !

!Exception class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:47'!
signal
	self signal: self printString! !

!Exception class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:46'!
signal: aString
	self new description: aString; pass! !


!ExceptionClassList methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:58'!
, anExceptionClass
	list add: anExceptionClass.
	^self
! !

!ExceptionClassList methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 05:15'!
asExceptionClassList
	^self! !

!ExceptionClassList methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 05:28'!
includes: aClass
	list do: [:i| (i == aClass or: [aClass inheritsFrom: i]) ifTrue: [^true]].
	^false ! !

!ExceptionClassList methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 04:56'!
initialize
	list := OrderedCollection new.! !


!ExceptionClassList class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 03:55'!
new
	^super new initialize! !


!ExceptionHandler methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 22:45'!
canHandle: anException
	^exceptionClassList includes: anException class! !

!ExceptionHandler methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 23:34'!
handle: anException
	thisContext swapSender: normalBlock sender sender.
	^exceptionBlock numArgs = 0
				ifTrue: [exceptionBlock value]
				ifFalse: [exceptionBlock value: anException]
! !

!ExceptionHandler methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 22:51'!
normalBlock: aBlock exceptionClassList: anExceptionClassList exceptionBlock: anotherBlock
	normalBlock := aBlock.
	exceptionClassList := anExceptionClassList.
	exceptionBlock := anotherBlock.
! !

!ExceptionHandler methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 22:50'!
value
	|value|
	Processor activeProcess pushExceptionHandler: self.
	value := normalBlock value.
	Processor activeProcess popExceptionHandler.
	^value! !


!ExceptionHandler class methodsFor: 'as yet unclassified' stamp: 'taj 8/5/97 23:07'!
normalBlock: normalBlock exceptionClassList: anExceptionClassList exceptionBlock: exceptionBlock
	^self new 
		normalBlock: normalBlock 
		exceptionClassList: anExceptionClassList 
		exceptionBlock: exceptionBlock ! !


!ExceptionTester class methodsFor: 'as yet unclassified' stamp: 'taj 8/4/97 05:33'!
signalDumbError
	DumbError signal.! !

!ExceptionTester class methodsFor: 'as yet unclassified' stamp: 'taj 8/6/97 01:00'!
test1 
	"simple use of ensure:"
	[Transcript show: 'do this'] ensure: [Transcript show: ' then this';cr].

	"simple use of ifCurtailed:"
	[Transcript show: 'do this'; cr] ifCurtailed: [Transcript show: 'but not this'; cr].

	"ifCurtailed: within an on:do:"
	[[Transcript show: 'do this'. self signalDumbError. Transcript show: 'not this']
		ifCurtailed: [Transcript show: ' and this']] 
			on: Error do: [Transcript show: ' then this'; cr].

	"handling more than one type of exception"
	[ self signalDumbError ] 
		on: DumbError,StupidError 
		do: [Transcript show: 'and finally this';cr].
  
	"nested exception handlers"
	[[self signalDumbError ] on: StupidError do: [Transcript show: 'not this']] 
		on: DumbError do: [ Transcript show: 'and this too';cr].! !


!Process methodsFor: 'vs exceptions' stamp: 'taj 8/5/97 23:28'!
exceptionHandlerFor: anException
	|exceptionHandler|
	[true] whileTrue: 
		[self exceptionHandlers isEmpty ifTrue: [self halt: anException description].
		exceptionHandler := self popExceptionHandler.
		(exceptionHandler canHandle: anException) ifTrue: [^exceptionHandler]].! !

!Process methodsFor: 'vs exceptions' stamp: 'taj 8/5/97 22:01'!
exceptionHandlers
	exceptionHandlers isNil 
		ifTrue: [ exceptionHandlers := OrderedCollection new].
	^exceptionHandlers! !

!Process methodsFor: 'vs exceptions' stamp: 'taj 8/6/97 01:03'!
handleException: anException
	"called only by Exception>>pass"
	(self exceptionHandlerFor: anException) handle: anException 
	! !

!Process methodsFor: 'vs exceptions' stamp: 'taj 8/5/97 23:28'!
popExceptionHandler
	^self exceptionHandlers removeFirst! !

!Process methodsFor: 'vs exceptions' stamp: 'taj 8/5/97 23:25'!
pushExceptionHandler: anExceptionHandler
	self exceptionHandlers addFirst: anExceptionHandler! !


