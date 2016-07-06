"Kernel-Exceptions.st
Curt's simple exception handling from GNU Smalltalk.

    AUTHOR         Curt McKelvey (ported by Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    06/30/97"!

Object subclass: #ExceptionEvent
	instanceVariableNames: 'exceptionName parent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Exceptions'!
ExceptionEvent comment:
'I am used to raise an exception and define an exception block.  My instances represent an exception.

	NAME		Exeptions
        AUTHOR          mckelvey@eso.mc.xerox.com
        FUNCTION        Ability to raise exceptions and define handlers for them
        ST-VERSIONS     GNU-1.1.1
	VERSION	ID	1.0
	VERSION DATE	26 Apr 95
	SUMMARY

	This directory contains the Smalltalk source Exceptions.st and
        documentation in Exceptions.txt which describes the usage.
        The classes in Exceptions.st provide a mechanism to create an
        exception object, called an ExceptionEvent.  Raising an exception
        is done by send the raise or raiseWith: message to an ExceptionEvent
        instance.  A raised exception may be handled in an exception block,
        which is created by the following expression:

        ExceptionEvent 
            on: exceptionEvent1 handle: handler1 
            { on: exceptionEvent2 handle: handler2 }
            { on: exceptionEvent3 handle: handler3 } 
            { on: exceptionEvent4 handle: handler4 }
            { onAnyOther: otherHandler }
            do: aBlock

        where the selectors enclosed in { } are optional.  First, aBlock
        is invoked with no arguments.  If it sends raise or raiseWith: to
        the ExceptionEvent exceptionEvent1, the block handler1 is invoked
        with one argument, an ExceptionValue.  If exceptionEvent2 is raised,
        handler2 is invoked, and so on.  If an ExceptionEvent is raised 
        other than those listed in an on:handle: pair, the handler 
        otherHandler is invoked, passing the ExceptionValue argument.
        For more details, see the documentation in Exceptions.txt.  
      
						Curt McKelvey - 4-26-95
'!

!ExceptionEvent methodsFor: 'raising an exception'!
raise

	^self raiseWith: nil! !

!ExceptionEvent methodsFor: 'raising an exception'!
raiseWith: someData

	^(ExceptionValue event: self value: someData) raise! !


!ExceptionEvent methodsFor: 'defining an exception block'!
handle: aHandlerBlock do: aBlock
	^ExceptionEvent on: self handle: aHandlerBlock do: aBlock! !


!ExceptionEvent methodsFor: 'accessing'!
exceptionName

	^exceptionName! !


!ExceptionEvent methodsFor: 'private'!
exceptionValue

	^self exceptionValue: nil! !

!ExceptionEvent methodsFor: 'private'!
exceptionValue: exceptionData

	^ExceptionValue event: self value: exceptionData! !

!ExceptionEvent methodsFor: 'private'!
init: name

	exceptionName := name! !

!ExceptionEvent methodsFor: 'private' stamp: 'stp 07/01/97 12:22'!
name: name parent: par
	"Set the receiver's name and parent."

	exceptionName := name.
	parent := par! !

!ExceptionEvent methodsFor: 'private'!
printOn: stream

	'EE' printOn: stream. 
	(self exceptionName isNil)
		ifFalse: [' named ' printOn: stream.  
			self exceptionName printOn: stream]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ExceptionEvent class
	instanceVariableNames: ''!

!ExceptionEvent class methodsFor: 'instance creation' stamp: 'stp 07/01/97 12:22'!
name: nam parent: par

	^super new name: nam parent: par! !

!ExceptionEvent class methodsFor: 'instance creation' stamp: 'stp 07/01/97 12:23'!
new
	"All instances are children of error signal."

	^(super new) name: nil parent: Object errorSignal! !

!ExceptionEvent class methodsFor: 'instance creation' stamp: 'stp 07/01/97 12:24'!
new: nam

	^(super new) name: nam parent: Object errorSignal! !


!ExceptionEvent class methodsFor: 'defining an exception block'!
exceptionHandlers: aDictionary anyOther: aHandlerBlock do: aBlock

	^(aBlock newExceptionProcess: aDictionary anyOther: aHandlerBlock) doIt! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
exceptionHandlers: aDictionary do: aBlock

	^(aBlock newExceptionProcess: aDictionary) doIt! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 do: aBlock

	^self 
		exceptionHandlers: (Dictionary
			with: (Association key: exceptionEvent1 value: handler1))
		do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 on: exceptionEvent2 handle: handler2 do: aBlock

	^self exceptionHandlers: (Dictionary
			with: (Association key: exceptionEvent1 value: handler1)
			with: (Association key: exceptionEvent2 value: handler2))
		do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 
on: exceptionEvent2 handle: handler2 
on: exceptionEvent3 handle: handler3 
do: aBlock

	^self 
	exceptionHandlers: (Dictionary
		with: (Association key: exceptionEvent1 value: handler1)
		with: (Association key: exceptionEvent2 value: handler2)
		with: (Association key: exceptionEvent3 value: handler3)
		)
	do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 
on: exceptionEvent2 handle: handler2 
on: exceptionEvent3 handle: handler3 
on: exceptionEvent4 handle: handler4 
do: aBlock

	^self exceptionHandlers: (Dictionary
			with: (Association key: exceptionEvent1 value: handler1)
			with: (Association key: exceptionEvent2 value: handler2)
			with: (Association key: exceptionEvent3 value: handler3)
			with: (Association key: exceptionEvent4 value: handler4))
		do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 
on: exceptionEvent2 handle: handler2 
on: exceptionEvent3 handle: handler3 
on: exceptionEvent4 handle: handler4 
onAnyOther: otherHandler
do: aBlock

	^self 
	exceptionHandlers: (Dictionary
		with: (Association key: exceptionEvent1 value: handler1)
		with: (Association key: exceptionEvent2 value: handler2)
		with: (Association key: exceptionEvent3 value: handler3)
		with: (Association key: exceptionEvent4 value: handler4)
		)
	anyOther: otherHandler
	do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 
on: exceptionEvent2 handle: handler2 
on: exceptionEvent3 handle: handler3 
onAnyOther: otherHandler
do: aBlock

	^self 
	exceptionHandlers: (Dictionary
		with: (Association key: exceptionEvent1 value: handler1)
		with: (Association key: exceptionEvent2 value: handler2)
		with: (Association key: exceptionEvent3 value: handler3)
		)
	anyOther: otherHandler
	do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1 
on: exceptionEvent2 handle: handler2 
onAnyOther: otherHandler
do: aBlock

	^self 
	exceptionHandlers: (Dictionary
		with: (Association key: exceptionEvent1 value: handler1)
		with: (Association key: exceptionEvent2 value: handler2)
		)
	anyOther: otherHandler
	do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
on: exceptionEvent1 handle: handler1
onAnyOther: otherHandler
do: aBlock

	^self 
	exceptionHandlers: (Dictionary
		with: (Association key: exceptionEvent1 value: handler1)
		)
	anyOther: otherHandler
	do: aBlock! !

!ExceptionEvent class methodsFor: 'defining an exception block'!
onAny: aHandlerBlock do: aBlock
	"These simply create an ExceptionProcess, and execute it"

	^self exceptionHandlers: (Dictionary new: 1) anyOther: aHandlerBlock do: aBlock! !


!ExceptionEvent class methodsFor: 'examples'!
example1
	"ExceptionEvent example1"

	| exc "blk proc" |
	exc := ExceptionEvent new: 'randomExc'.
	Transcript cr.
	exc handle: 
			[ :ex | 
			Transcript show: 'Handle: ', ex printString; cr.
			ex resume]
		do: [Transcript show: 'before'; cr.
			exc raiseWith: 21.
			Transcript show: 'after'; cr]
! !

!ExceptionEvent class methodsFor: 'examples'!
example2
	"ExceptionEvent example2"

	| exc "blk proc" |
	exc := ExceptionEvent new: 'randomException'.
	exc raiseWith: 21.
! !


Process subclass: #ExceptionProcess
	instanceVariableNames: 'handlers otherHandler uncaughtException invoker execution '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Exceptions'!
ExceptionProcess comment:
'WARNING: This entire interface is private.  It is used by the ExceptionEvent interface.  I am a process that is forked to catch exceptions.'!

!ExceptionProcess methodsFor: 'execution control'!
beginExecution
	"before beginning, wait for the other block to signal"

	execution wait! !

!ExceptionProcess methodsFor: 'execution control'!
doIt

	| raisedException  |
	" start the exception block from aBlock.  This will block, 
		waiting on the execution semaphore"
	self resume." start the exception block "		
	Processor yield.   " let it run, just to wait on execution "
	" now we really start the aBlock.  It may return an uncaught
		exception raised in the block.  If so, the block hasn't completed."
	raisedException := self monitorException.
	(Processor activeProcess isMemberOf: ExceptionProcess)
	ifTrue: [  
	"then I am the outer exception block.  See if I can catch it"
	[raisedException notNil] whileTrue: [
		raisedException raise.
		raisedException := self monitorException]]
	ifFalse: [
	"we are at the outer most block; raised exception won't be caught"
	(raisedException notNil) ifTrue: [
		self terminate]].
	^raisedException! !

!ExceptionProcess methodsFor: 'execution control'!
endExecution
	"signal before ending"

	execution signal! !

!ExceptionProcess methodsFor: 'execution control'!
monitorException

	self toggleExecution.		"change execution..."
	^self uncaughtException   "...when we come back, report the result"
! !

!ExceptionProcess methodsFor: 'execution control'!
toggleAndDie

	self endExecution.
	self terminate! !

!ExceptionProcess methodsFor: 'execution control'!
toggleExecution

	self endExecution.
	self beginExecution! !


!ExceptionProcess methodsFor: 'accessing'!
handlerFor: anExceptionEvent

	^handlers at: anExceptionEvent ifAbsent: [otherHandler]! !

!ExceptionProcess methodsFor: 'accessing'!
invoker

	^invoker! !

!ExceptionProcess methodsFor: 'accessing'!
uncaughtException

	^uncaughtException! !

!ExceptionProcess methodsFor: 'accessing'!
uncaughtException: anExceptionValue

	uncaughtException := anExceptionValue! !


!ExceptionProcess methodsFor: 'private'!
handlers: aHandlerDictionary anyOther: aHandlerBlock

	handlers := aHandlerDictionary.
	otherHandler := aHandlerBlock.
	invoker := Processor activeProcess.
	execution := Semaphore new! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ExceptionProcess class
	instanceVariableNames: ''!

!ExceptionProcess class methodsFor: 'instance creation'!
do: aBlock handlerDictionary: aHandlerDictionary

	^self do: aBlock handlerDictionary: aHandlerDictionary anyOther: nil
! !

!ExceptionProcess class methodsFor: 'instance creation'!
do: aBlock handlerDictionary: aHandlerDictionary anyOther: aHandlerBlock

	^(super forContext: aBlock priority: Processor activePriority)
	handlers: aHandlerDictionary anyOther: aHandlerBlock! !


Object subclass: #ExceptionValue
	instanceVariableNames: 'event data '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Exceptions'!
ExceptionValue comment:
'I am passed to a client if an ExceptionEvent is raised.  I contain the ExceptionEvent and its value.  An instance cannot be used outside its own exception block.  Generally, you should not create one of me; use ExceptionEvent to raise me.'!

!ExceptionValue methodsFor: 'accessing'!
data

	^data! !

!ExceptionValue methodsFor: 'accessing'!
event

	^event! !


!ExceptionValue methodsFor: 'catching the exception'!
resume

	| activeProcess |
	activeProcess := Processor activeProcess.
	(activeProcess isMemberOf: ExceptionProcess) 
	ifTrue: [ " just mark the exception has caught and move on."
	activeProcess uncaughtException: nil]! !

!ExceptionValue methodsFor: 'catching the exception'!
terminate

	| activeProcess |
	activeProcess := Processor activeProcess.
	(activeProcess isMemberOf: ExceptionProcess) 
	ifTrue: [" die instantly, but indicate to caller exception was caught"
	activeProcess uncaughtException: nil.
	activeProcess toggleAndDie]! !


!ExceptionValue methodsFor: 'private - raising the exception' stamp: 'stp 07/01/97 12:56'!
raise

	| activeProcess exceptionHandler |
	activeProcess := Processor activeProcess.
	" indicate an exception was raised "
	(activeProcess isMemberOf: ExceptionProcess)
		ifTrue: [activeProcess uncaughtException: self.
			exceptionHandler := activeProcess handlerFor: self event.
			exceptionHandler notNil
				ifTrue: [exceptionHandler value: self.
					(activeProcess uncaughtException notNil)
						ifTrue: [self propogateUp: activeProcess]]
				ifFalse: [self propogateUp: activeProcess]]! !


!ExceptionValue methodsFor: 'private'!
event: anExceptionEvent value: exceptionData
	event := anExceptionEvent.
	data := exceptionData! !

!ExceptionValue methodsFor: 'private'!
propogateUp: activeProcess
	activeProcess toggleExecution.  " propogate exception to outer block"
	"if we come back, we must have been caught & resumed"
	activeProcess uncaughtException: nil! !


!ExceptionValue methodsFor: 'printing'!
printOn: stream
	'EV of ' printOn: stream.
	self event printOn: stream.
	'(' printOn: stream.
	self data printOn: stream.
	')' printOn: stream! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ExceptionValue class
	instanceVariableNames: ''!

!ExceptionValue class methodsFor: 'private - instance creation'!
event: anExceptionEvent value: exceptionData

	^super new event: anExceptionEvent value: exceptionData! !
