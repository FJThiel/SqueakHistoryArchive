'From Squeak 2.3 of January 14, 1999 on 17 March 1999 at 1:36:26 am'!"Change Set:		SizedScrollBarFixDate:			17 March 1999Author:			Doug WayFixes a small bug, so that if a 100% full scrollbar is clicked on, thecontents will always completely scroll into view."!!ScrollBar methodsFor: 'other events' stamp: 'dew 3/17/1999 01:32'!mouseDownInSlider: event	"this makes sure the entire scrollable area is in fact visible ifthe interval is 1.0"		interval = 1.0 ifTrue: [self setValue: 0].	super mouseDownInSlider: event! !