"Object-dependency.st
This extends the changed/update messages for passing along new values.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 14, 1996"!


!Object methodsFor: 'updating'!

changed
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

	self changed: nil with: nil!

changed: aParameter 
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter with: nil]!

changed: aParameter with: aValue
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter with: aValue]!

update: aParameter with: aValue
	"Receive a change notice from an object of whom the receiver is a 
	dependent. The default behavior is to do nothing; a subclass might want 
	to change itself in some way."

	^self update: aParameter! !

