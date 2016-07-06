"ExceptionJunta.st
The *real* use of exception handling--only partially complete.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    06/30/97"!

Object subclass: #Collection
	instanceVariableNames: ''
	classVariableNames: 'AccessError IndexError NonIntegerIndexError NotFoundError OutOfBoundsError RandomForPicking '
	poolDictionaries: ''
	category: 'Collections-Abstract'!

!Object methodsFor: 'error handling'!
caseError
	"Report an error from an in-line or explicit case statement."

"	Object caseError raise."
	self error: 'Case not found, and no otherwise clause'! !

!Object methodsFor: 'error handling'!
doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but the receiver does not understand this message (typically sent from the machine when a message is sent to the receiver and no method is defined for that selector)."
	"Unless the receiver has an error handler defined for the active process (this mechanism appears vestigal so it is probably defunct), report to the user that the receiver does not understand the argument, aMessage, as a message."
	"Testing: (3 activeProcess)"

	| thisProcess errorString |
"	Object messageNotUnderstoodError raise."
	(Preferences autoAccessors and: [self tryToDefineVariableAccess: aMessage])
		ifTrue: [^ aMessage sentTo: self].
	errorString _ 'Message not understood: ', aMessage selector.
	(thisProcess _ Processor activeProcess) isErrorHandled
		ifTrue: [thisProcess errorHandler value: errorString value: self]
		ifFalse: [DebuggerView openContext: thisContext
					label: errorString
					contents: thisContext shortStack].
	^ aMessage sentTo: self! !
!Object methodsFor: 'error handling'!
error: aString 
	"The default behavior for error: is the same as halt:. The code is 
	replicated in order to avoid showing an extra level of message sending 
	in the Debugger. This additional message is the one a subclass should 
	override in order to change the error handling behavior."

"	Object errorSignal raise."
	^self realError: aString

	"nil error: 'error message'."! !
!Object methodsFor: 'error handling'!
error: labelString with: contentsString

"	Object errorSignal raise."
	^self realError: labelString with: contentsString! !
!Object methodsFor: 'error handling'!
halt
	"This is the typical message to use for inserting breakpoints during 
	debugging. It behaves like halt:, but does not call on halt: in order to 
	avoid putting this message on the stack. Halt is especially useful when 
	the breakpoint message is an arbitrary one."

	self halt: 'Halt encountered.'

	"nil halt."! !
!Object methodsFor: 'error handling'!
halt: aString 
	"This is the typical message to use for inserting breakpoints during 
	debugging. It creates and schedules a Notifier with the argument, 
	aString, as the label."

"	Object errorSignal raise."
	^self realError: aString

	"nil halt: 'Test of halt:.'."! !
!Object methodsFor: 'error handling'!
notify: aString at: location
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed. Subclasses can
	override this and insert an error message at location within aString."

	self notify: aString

	"nil notify: 'confirmation message' at: 12"! !
!Object methodsFor: 'error handling'!
primitiveFailed
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

"	Object primitiveErrorSignal raise."
	self error: 'a primitive has failed'! !
!Object methodsFor: 'error handling'!
realError: aString 
	"The default behavior for error: is the same as halt:. The code is 
	replicated in order to avoid showing an extra level of message sending 
	in the Debugger. This additional message is the one a subclass should 
	override in order to change the error handling behavior."
	"Do not raise the error signal."

	| currentProcesss currentProcess |
	(currentProcess _ ScheduledControllers activeControllerProcess) isErrorHandled
        ifTrue:
            [currentProcess errorHandler value: aString value: self]
        ifFalse:
            [DebuggerView
			openContext: thisContext
			label: aString
			contents: thisContext shortStack]

	"nil error: 'error message'."! !
!Object methodsFor: 'error handling'!
realError: labelString with: contentsString
	"Do not raise the error signal."

	DebuggerView
		openContext: thisContext
		label: labelString
		contents: contentsString! !
!Object methodsFor: 'error handling'!
shouldNotImplement
	"Announce that, although the receiver inherits this message, it should 
	not implement it."

"	Object shouldNotImplementSignal raise."
	self error: 'This message is not appropriate for this object'! !
!Object methodsFor: 'error handling'!
subclassResponsibility
	"This message sets up a framework for the behavior of the class' 
	subclasses. Announce that the subclass should have implemented this 
	message."

"	Object subclassResponsibilitySignal raise."
	self error: 'My subclass should have overridden one of my messages.'! !
!Object methodsFor: 'private'!
errorImproperStore
	"Create an error notification that an improper store was attempted."

	Collection accessError raise.
	self error: 'Improper store into indexable object'! !
!Object methodsFor: 'private'!
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	Collection nonIntegerIndexError raise.
	self error: 'only integers should be used as indices'! !
!Object methodsFor: 'private'!
errorSubscriptBounds: index 
	"Create an error notification that an improper integer was used as an index."

	Collection outOfBoundsError raise.
	self error: 'subscript is out of bounds: ' , index printString! !
!Object methodsFor: 'private'!
mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the
	interpreter."

"	Object booleanError raise."
	self error: 'NonBoolean receiver--proceed for truth.'.
	^true! !
!Interval methodsFor: 'accessing'!
at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	Collection accessError raise.
	self error: 'you can not store into an interval'! !
!Interval methodsFor: 'removing'!
remove: newObject 
	"Removing from an Interval is not allowed."

	Collection accessError raise.
	self error: 'elements cannot be removed from an Interval'! !
!Object class methodsFor: 'exceptions'!
error
	"Answer the exception used for general errors."

	^ErrorSignal! !
!Object class methodsFor: 'exceptions'!
errorSignal
	"Answer the exception used for general errors."

	"Object errorSignal handle: [ :ex | Transcript crtab; show: 'Handler'; cr]
		do: [nil halt: 'error message']"
	"Object errorSignal handle: [ :ex | Transcript crtab; show: 'Handler'; cr.
			ex resume]
		do: [nil halt: 'error message']"

	^ErrorSignal! !
!Object class methodsFor: 'exceptions'!
initializeExceptions
	"Initialize the exception used for general errors."
	"Object initializeExceptions"

	ErrorSignal := ExceptionEvent name: 'Object Error' parent: nil! !
!Collection class methodsFor: 'exceptions'!
accessError
	"Answer the exception used for general accessing errors."

	^AccessError! !
!Collection class methodsFor: 'exceptions'!
indexError
	"Answer the exception used for general index errors."

	^IndexError! !
!Collection class methodsFor: 'exceptions'!
initializeExceptions
	"Initialize the exception used for collection errors."
	"Collection initializeExceptions"

	AccessError := ExceptionEvent name: 'Collection Access Error' 
				parent: Object errorSignal.
	IndexError := ExceptionEvent name: 'Collection Index Error' 
				parent: AccessError.
	NonIntegerIndexError := ExceptionEvent name: 'NonInt Index Error' 
				parent: IndexError.
	OutOfBoundsError := ExceptionEvent name: 'Bounds Error' 
				parent: AccessError.
	NotFoundError := ExceptionEvent name: 'Not Found Error' 
				parent: AccessError.
! !
!Collection class methodsFor: 'exceptions'!
nonIntegerIndexError
	"Answer the exception used for non-integer index errors."

	^NonIntegerIndexError! !
!Collection class methodsFor: 'exceptions'!
notFoundError 
	"Answer the exception used for item-not-found errors."
"OutOfBoundsError"
	^NotFoundError! !
!Collection class methodsFor: 'exceptions'!
outOfBoundsError 
	"Answer the exception used for index bounds errors."

	^OutOfBoundsError! !
!Set methodsFor: 'private'!
findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."
	| index |
	index _ self scanFor: anObject.
	index > 0 ifTrue: [ ^ index ].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	Collection accessError raise.
	self error: 'There is no free space in this set!!'.! !
!Dictionary methodsFor: 'private'!
errorKeyNotFound

	Collection notFoundError raise.
	self error: 'key not found'! !
!Dictionary methodsFor: 'private'!
errorValueNotFound

	Collection notFoundError raise.
	self error: 'value not found'! !
!String methodsFor: 'accessing'!
string
	
	^self! !

Object initializeExceptions!

Collection initializeExceptions!
