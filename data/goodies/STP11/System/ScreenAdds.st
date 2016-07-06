"ScreenAdds.st
Adds a few utility methods to class DisplayScreen for determining the current screen size and getting default rectangles for view opening. This is very useful for applications that are supposed to be portable between desk- and lap-top machines.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    April 28, 1995"!

!DisplayScreen methodsFor: 'defaults'!

aspectRatio
	"Answer the bounding rectangle of the full screen."
	"Display aspectRatio"

	| ext |
	ext := self boundingBox extent.
	^ext x asFloat / ext y!

inset10percent
	"Answer the rectangle for the full screen inset by 10%."
	"Display inset10percent"

	| rect ext |
	rect := self boundingBox.
	ext := rect extent.
	^(ext / 10) rounded extent: (ext * 0.8) rounded!

inset20percent
	"Answer the rectangle for the full screen inset by 20%."
	"Display inset20percent"

	| rect ext |
	rect := self boundingBox.
	ext := rect extent.
	^(ext / 5) rounded extent: (ext * 0.6) rounded!

insetRectangle
	"Answer the rectangle for the full screen inset by 10%."
	"Display insetRectangle"

	| rect ext |
	rect := self boundingBox.
	ext := rect extent.
	^(ext / 10) rounded extent: (ext * 0.8) rounded!

isLarge
	"Answer whether the screen is 'large' (more than 800,000 pixels)."
	"Display isLarge"

	^self boundingBox area > 800000!

isSmall
	"Answer whether the screen is 'small' (less than 320,000 pixels, so that 640 * 480 is small)."
	"Display isSmall"

	^self boundingBox area < 320000! !
