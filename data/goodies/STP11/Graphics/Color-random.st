"Color-random.st
This is the random instance creation method for colors.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    July 8, 1997"!

!Color class methodsFor: 'instance creation' stamp: 'stp 07/01/97 13:25'!
random: rand
	"Answer a ColorValue with random RGB intensity (between 0 and 1)."

	^self r: rand next g: rand next b: rand next! !
