"BrowserFix.st
Check for empty class list.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    06/15/96"!


!Browser methodsFor: 'class list'!
selectedClassName
	"Answer the name of the current class. Answer nil if no selection exists."

	classListIndex = 0 ifTrue: [^nil].
	self classList isEmpty ifTrue: [^nil].
	^self classList at: classListIndex! !
