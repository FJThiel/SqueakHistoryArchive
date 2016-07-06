"StandardSystemController-open.st
This is for compatibility...

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    December 5, 1996"!


!StandardSystemController methodsFor: 'scheduling'!
open
	"Create an area on the screen in which the receiver's scheduled view can 
	be displayed. Make it the active view."

	Preferences userPlacement
		ifTrue: [view resize]
		ifFalse: [view resizeInitially].
	status _ #open.
	ScheduledControllers scheduleActive: self! !