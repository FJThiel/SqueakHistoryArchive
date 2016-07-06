"changesForOutlines.st
These are the simple changes necessary to make the outline browser application work.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 25, 1996"!


Workspace rename: #Environment!

!Environment class methodsFor: 'instance creation'!
contents: aText
	"Answer an instance of the receiver with the given attributes."

	| inst |
	inst := self new.
	inst contents: aText.
	^inst! !
