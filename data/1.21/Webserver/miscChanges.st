"miscChanges.st
Some changes, fixes and additions. I recommend viewing this file as a changes list.


	TITLE			miscChanges.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			1.0
	IMAGE VERSION	1.19d, 1.2
	PREREQUISITES	none
	DATE			April 21, 1997"!

"...Error handling... 
Minor cleanup of the error handling methods already present in the image.
The methods ContextPart>failureCatcher: and BlockContext>ifFail: are inoperable and therefore removed. BlockContext>ifError: and BlockContext>value:ifError: have been commented, ifError: has been changed so both methods send two parameters to the error block. Also a misleading statement in the comment of Object>doesNotUnderstand is removed."!

!BlockContext methodsFor: 'evaluating'!
value: arg1 ifError: aBlock
	"Evaluate the block represented by the receiver. If an error occurs aBlock is evaluated
	 with the error message and the receiver as parameters. The receiver should not contain
	 an explicit return statement as this would leave an obsolete error handler hanging around."

	| errorBlock lastHandler val activeControllerProcess  |
	activeControllerProcess _ ScheduledControllers activeControllerProcess.
	lastHandler _ activeControllerProcess errorHandler.
	errorBlock _ [:aString :aReceiver |
		activeControllerProcess errorHandler: lastHandler.
		^ aBlock value: aString value: aReceiver
	].
	activeControllerProcess errorHandler: errorBlock.
	val _ self value: arg1.
	activeControllerProcess errorHandler: lastHandler.
	^ val! !
!BlockContext methodsFor: 'evaluating'!
ifError: aBlock
	"Evaluate the block represented by the receiver. If an error occurs aBlock is evaluated
	 with the error message and the receiver as parameters. The receiver should not contain
	 an explicit return statement as this would leave an obsolete error handler hanging around."

	| errorBlock lastHandler val activeControllerProcess  |
	activeControllerProcess _ ScheduledControllers activeControllerProcess.
	lastHandler _ activeControllerProcess errorHandler.
	errorBlock _ [:aString :aReceiver |
		activeControllerProcess errorHandler: lastHandler.
		^ aBlock value: aString value: aReceiver
	].
	activeControllerProcess errorHandler: errorBlock.
	val _ self value.
	activeControllerProcess errorHandler: lastHandler.
	^ val! !
ContextPart removeSelector: #failureCatcher:!
BlockContext removeSelector: #ifFail:!

!Object methodsFor: 'error handling'!
doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but the receiver does not understand this message (typically sent from the machine when a message is sent to the receiver and no method is defined for that selector)."
	"Unless the receiver has an error handler defined for the active process, report to the user that the receiver does not understand the argument, aMessage, as a message."
	"Testing: (3 activeProcess)"
	| thisProcess errorString |
	(Preferences autoAccessors and: [self tryToDefineVariableAccess: aMessage])
		ifTrue: [^ aMessage sentTo: self].
	errorString _ 'Message not understood: ', aMessage selector.
	(thisProcess _ Processor activeProcess) isErrorHandled
		ifTrue: [thisProcess errorHandler value: errorString value: self]
		ifFalse: [DebuggerView openContext: thisContext
					label: errorString
					contents: thisContext shortStack].
	^ aMessage sentTo: self! !
	
"...Symbol... 
remove a redundant nil check in symbol lookup to gain a bit of speed."!

!Symbol class methodsFor: 'instance creation'!
intern: aString 
	"Answer a unique Symbol whose characters are those of aString."

	| ascii table mainTable index sym numArgs |

	ascii _ (aString at: 1) asciiValue.
	aString size = 1 ifTrue: [ascii < 128 ifTrue: 
		[^ SingleCharSymbols at: ascii + 1]].

	table _ ((ascii >= "$a asciiValue" 97) and:
		[(ascii <= "$z asciiValue" 122) and:
		[(numArgs _ aString numArgs) >= 0]])
			ifTrue: [ (mainTable _ SelectorTables
									at: (numArgs + 1 min: SelectorTables size))
						at: (index _ ascii - "($a asciiValue - 1)" 96) ]
			ifFalse: [ (mainTable _ OtherTable)
						at: (index _ aString stringhash \\ OtherTable size + 1)].

	1 to: table size do: [:i |
		aString size = (table at: i) size ifTrue: [aString = (table at: i) ifTrue: [^ table at: i]]
	].

	sym _ (aString isMemberOf: Symbol)
		ifTrue: [aString]	 "putting old symbol in new table"
		ifFalse: [(Symbol new: aString size) string: aString]. "create a new one"

	mainTable at: index put: (table copyWith: sym).
	^sym
! !
!Symbol class methodsFor: 'private'!
hasInterned: aString ifTrue: symBlock 
	"Answer with false if aString hasnt been interned (into a Symbol), 
	otherwise supply the symbol to symBlock and return true."

	| table ascii numArgs |

	ascii _ (aString at: 1) asciiValue.
	aString size = 1 ifTrue: [ascii < 128 ifTrue: 
		[symBlock value: (SingleCharSymbols at: ascii + 1).
		^true]].

	table _ ((ascii >= "$a asciiValue" 97) and:
		[(ascii <= "$z asciiValue" 122) and:
		[(numArgs _ aString numArgs) >= 0]])
			ifTrue: [ (SelectorTables at: (numArgs + 1 min: SelectorTables size))
						at: ascii - "($a asciiValue - 1)" 96 ]
			ifFalse: [ OtherTable at: aString stringhash \\ OtherTable size + 1].

	1 to: table size do: [:i | 
		aString size = (table at: i) size ifTrue: [aString = (table at: i) ifTrue: [
			symBlock value: (table at: i).
			^true]]
	].
	^false
! !

"...String... 
Needed for NewsAgent.
We can reduce code bloat a wee bit (one method and one message selector) by defining
'asString' for Object and removing 'stringRepresentation'.
Also add some utilitiy methods."!
 
!Object methodsFor: 'converting'!
asString
	"Answer a string that represents the receiver."

	^ self printString ! !
Object removeSelector: #stringRepresentation!
String removeSelector: #stringRepresentation!
!String methodsFor: 'converting'!
substrings
	"Answer an array of the substrings that compose the receiver."
	| result aStream char |
	result _ WriteStream on: (Array new: 10).
	aStream _ WriteStream on: (String new: 16).
	1 to: self size do: [:i |
		((char _ self at: i) isSeparator)
		 ifTrue: [ aStream isEmpty ifFalse: [ result nextPut: aStream contents. aStream reset ]]
		 ifFalse: [ aStream nextPut: char ]
	].
	aStream isEmpty ifFalse: [result nextPut: aStream contents].
	^ result contents! !
!String methodsFor: 'comparing'!
crc16

	| crc |
	crc := 0.
	self do: [:c |
		crc := (crc bitXor: (c asciiValue bitShift: 8)) bitAnd: 16rFFFF.
		1 to: 8 do: [ :dmy | "due to compiler optimization this is a bit faster than timesRepeat:"
			crc := (crc bitAnd: 16r8000) ~= 0
			 ifTrue: [ (crc bitShift: 1) bitXor: 16r1021 ]
			 ifFalse: [ crc bitShift: 1 ]
		].
	].
	^crc bitAnd: 16rFFFF! !

"...Socket... 
Needed for NewsAgent.
A utility method for reading data."!

!Socket methodsFor: 'sending-receiving'!
readInto: aStringOrByteArray startingAt: aNumber
	"Read data into the given buffer starting at the given index and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	(self waitForDataUntil: Socket standardDeadline) ifFalse: [ self error: 'receive timeout'].
	^ self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size - aNumber + 1
! !

