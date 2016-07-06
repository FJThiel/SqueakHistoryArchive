"View-addSubViewinborderWidth.st
This is for compatibility...

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    December 5, 1996"!


!View methodsFor: 'subView inserting'!
addSubView: aView in: aRectangle borderWidth: borderInteger
	"Add the given view in t4he given relative rectangle and with the given border 
	width (in pixels), checking for cyclic views."

        | theViewPort theWindow |
        self addSubView: aView ifCyclic: [self error: 'cycle in view structure.'].
        aView borderWidth: borderInteger.
        theViewPort _ self window extent * aRectangle origin
                        corner: self window extent * aRectangle corner.
        theWindow _ 0 @ 0 extent: (self window extent * aRectangle extent).
        aView window: theWindow viewport: theViewPort! !
