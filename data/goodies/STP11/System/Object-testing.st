"Object-testing.st
These are a few random testing methods (e.g., isString isSymbol) plus value coercion (asValue).

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    June 4, 1988"!


!Object methodsFor: 'testing'!
hasItems
	"Overridden to return true in Collection-like classes."
	^ false! !

!Collection methodsFor: 'testing'!
hasItems
	"Answer whether or not the receiver has items or components (true)."

	^true! !

!Object methodsFor: 'testing'!
isString
	"Overridden to return true in String."

	^ false! !

!String methodsFor: 'testing'!
isString
	"Answer true."

	^true! !

!Text methodsFor: 'testing'!
isString
	"Answer true (i.e., lie about it)."

	^true! !

!Object methodsFor: 'testing'!
isSymbol
	"Overridden to return true in Symbol."

	^ false! !

!Symbol methodsFor: 'testing'!
isSymbol
	"Answer true."

	^true! !

!Object methodsFor: 'converting'!
value
	"Answer the value of the receiver."

	^self!

asValue
	"Answer the receiver as a value."

	^Value on: self! !

