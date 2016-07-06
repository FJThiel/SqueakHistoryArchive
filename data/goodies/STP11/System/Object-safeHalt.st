"Object-safeHalt.st
This makes a left-shift hack out of halt (i.e., it is different than break:)

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 10, 1996"!


!Object methodsFor: 'error handling'!
safeHalt
	"Halt only if the shift key is held down."

	Sensor leftShiftDown ifTrue: [self halt]! !
