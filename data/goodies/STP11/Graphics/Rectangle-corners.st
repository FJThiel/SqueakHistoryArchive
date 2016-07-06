"Rectangle-corners.st
This is what it looks like--answers an array of 4 points.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 14, 1996"!

!Rectangle methodsFor: 'accessing'!
corners
	"Return an array of corner points in the order of a quadrilateral spec for WarpBlt"
	^ Array with: self topLeft with: self bottomLeft with: self bottomRight with: self topRight! !