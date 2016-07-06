"Kernel-Values.st
This is a minimal value framework.
It has useful items such as indirect values (AKA PluggableAdaptors), lazy-block values, range values, and index values.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   Object-testing.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 20, 1996"!

Model subclass: #Value
	instanceVariableNames: 'subject '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Values'!
Value comment:
'Instances of the subclasses of the abstract class Value are various kinds of indirect or deferred values.

Instance Variables
	subject <object>	The underlying model-thing of the value'!

!Value methodsFor: 'accessing'!
asValue
	"Answer the receiver."

	^self!
subject
	"Answer the receiver's 'subject'."

	^subject!
subject: anObject
	"Set the receiver's instance variable 'subject' to be anObject."

	subject := anObject! !

!Value methodsFor: 'evaluation'!
value
	"Answer the 'value' of the receiver."

	^subject!
value: anObject
	"Set the 'value' of the receiver."

	subject := anObject! !

!Value methodsFor: 'updating'!
update: anAspect with: aValue
	"Pass all updates on to the subject."

	subject update: anAspect with: aValue! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Value class
	instanceVariableNames: ''!

!Value class methodsFor: 'instance creation'!
on: theSubject
	"Answefr an instance of the receiver class with the given subject."

	^self new subject: theSubject! !

Value subclass: #ComputedValue
	instanceVariableNames: 'arguments '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Values'!
ComputedValue comment:
'Instances of ComputedValue are used with blicks that are lazily evaluated whenever called for.

Instance Variables:
	argValues <nil | Object | Array of Object>	The values of the arguments'!

!ComputedValue methodsFor: 'accessing'!
arguments: anObject
	"Set the receiver's instance variable 'arguments' to be anObject."

	arguments := anObject! !

!ComputedValue methodsFor: 'evaluation'!
value
	"Answer the result of executing the receiver's block."

	^arguments isNil
		ifTrue: [subject value]
		ifFalse: [arguments hasItems
			ifFalse: [subject value: arguments]
			ifTrue: [subject valueWithArguments: arguments]]!
value: ignored
	"Set the 'value' of the receiver."

	self shouldNotImplement! !

Value subclass: #IndexedValue
	instanceVariableNames: 'index '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Values'!
IndexedValue comment:
'An instance of IndexedValue holds onto a SequenceableCollection (the subject) and an index, which is used for accessing.

Instance Variables:
	index		<Integer>	The index into the subject'!

!IndexedValue methodsFor: 'accessing'!
index
	"Answer the receiver's 'index'."

	^index!
index: anObject
	"Set the receiver's instance variable 'index' to be anObject."

	index := anObject! !

!IndexedValue methodsFor: 'evaluation'!
value
	"Answer the 'value' of the receiver."

	^subject at: index!
value: anObject
	"Set the 'value' of the receiver."

	subject at: index put: anObject.
	self changed! !

Value subclass: #IndirectValue
	instanceVariableNames: 'getter setter updater aspect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Values'!
IndirectValue comment:
'An instance of IndirectValue uses its getter and setter to access the subject. These can be symbols (which are performed) or blocks (which are evaluated).

Instance Variables:
	getter 	<Symbol | BlockContext>		how to get the subject''s value
	setter 	<Symbol | BlockContext>		how to set the subhect''s value
	updater 	<Symbol | BlockContext | nil>	how to update on change messages
	aspect		<Symbol | nil>			The aspect to pass along on updates'!

!IndirectValue methodsFor: 'accessing'!
aspect
	"Answer the receiver's 'aspect'."

	^aspect!
aspect: anObject
	"Set the receiver's instance variable 'aspect' to be anObject."

	aspect := anObject!
getter
	"Answer the receiver's 'getter'."

	^getter!
getter: anObject
	"Set the receiver's instance variable 'getter' to be anObject."

	getter := anObject!
setter
	"Answer the receiver's 'setter'."

	^setter!
setter: anObject
	"Set the receiver's instance variable 'setter' to be anObject."

	setter := anObject!
updater
	"Answer the receiver's 'updater'."

	^updater!
updater: anObject
	"Set the receiver's instance variable 'updater' to be anObject."

	updater := anObject! !

!IndirectValue methodsFor: 'evaluation'!
update: anAspect with: anObject
	"Pass the update on."

	^updater isNil
		ifFalse: [updater isSymbol
			ifTrue: [subject perform: updater with: anObject]
			ifFalse: [updater value: subject value: anObject]]!
value
	"Answer the 'value' of the receiver."

	^getter isSymbol
		ifTrue: [subject perform: getter]
		ifFalse: [getter value: subject]!
value: anObject
	"Set the 'value' of the receiver."

	setter isNil
		ifFalse: [setter isSymbol
			ifTrue: [subject perform: setter with: anObject]
			ifFalse: [setter value: subject value: anObject].
		self changed]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

IndirectValue class
	instanceVariableNames: ''!

!IndirectValue class methodsFor: 'as yet unclassified'!
example
	"IndirectValue example"

	| value |
	value := (IndirectValue on: Smalltalk) getter: #size.
	value value! !

Value subclass: #RangeValue
	instanceVariableNames: 'start stop step '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Values'!
RangeValue comment:
'An instance of RangeValue maps an arbitrary numerical range onto the range from 0.0 to 1.0.

Instance Variables:
	start	<Number>	the base of the range
	stop		<Number>	the stop value of the range
	step		<Number | nil>	The step size for rounding'!

!RangeValue methodsFor: 'accessing'!
start
	"Answer the receiver's 'start'."

	^start!
start: anObject
	"Set the receiver's instance variable 'start' to be anObject."

	start := anObject!
step
	"Answer the receiver's 'step'."

	^step!
step: anObject
	"Set the receiver's instance variable 'step' to be anObject."

	step := anObject!
stop
	"Answer the receiver's 'stop'."

	^stop!
stop: anObject
	"Set the receiver's instance variable 'stop' to be anObject."

	stop := anObject! !

!RangeValue methodsFor: 'evaluation'!
value
	"Answer the 'value' of the receiver."

	^subject value - start / (stop - start)!
value: anObject
	"Set the 'value' of the receiver."

	| tempValue |
	tempValue := (anObject * (stop - start) + start).
	step isNil
		ifTrue: [subject value: tempValue]
		ifFalse: [subject value: (tempValue roundTo: step)].
	self changed! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

RangeValue class
	instanceVariableNames: ''!

!RangeValue class methodsFor: 'examples'!
example
	"RangeValue example"

	| range |
	range := (RangeValue on: 21 asValue) start: 15; stop: 30; step: 5.
	range value! !

