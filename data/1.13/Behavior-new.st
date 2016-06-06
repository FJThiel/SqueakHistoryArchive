'From Squeak 1.13 of October 17, 1996 on 6 December 1996 at 6:48:47 pm'!

!Behavior methodsFor: 'instance creation'!
basicNew
	"Answer a new instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."
	"Essential Primitive. See Object documentation whatIsAPrimitive.
	NOTE:  This method should not be overridden in any subclasses."

	<primitive: 70>
	self isVariable ifTrue: [ ^ self basicNew: 0 ].
	"space must be low"
	Smalltalk signalLowSpace.
	^self basicNew  "retry if user proceeds"
!
basicNew: anInteger 
	"Answer a new instance of the receiver (which is a class) with the number of indexable variables specified by the argument, anInteger. Fail if the class is not indexable or if the argument is not a positive Integer."
	"Essential Primitive. See Object documentation whatIsAPrimitive.
	NOTE:  This method should not be overridden in any subclasses."

	<primitive: 71>
	(anInteger isInteger and: [anInteger >= 0]) ifTrue: [
		"arg okay; space must be low"
		Smalltalk signalLowSpace.
		^ self basicNew: anInteger  "retry if user proceeds"
	].
	self primitiveFailed!
new
	"Answer a new instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."
	"Essential Primitive. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	self isVariable ifTrue: [^self basicNew: 0].
	"space must be low"
	Smalltalk signalLowSpace.
	^self basicNew  "retry if user proceeds"
!
new: anInteger 
	"Answer a new instance of the receiver (which is a class) with the number of indexable variables specified by the argument, anInteger. Fail if the class is not indexable or if the argument is not a positive Integer."
	"Essential Primitive. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	(anInteger isInteger and: [anInteger >= 0]) ifTrue: [
		"arg okay; space must be low"
		Smalltalk signalLowSpace.
		"retry if user proceeds"
		^self basicNew: anInteger].
	self primitiveFailed! !

