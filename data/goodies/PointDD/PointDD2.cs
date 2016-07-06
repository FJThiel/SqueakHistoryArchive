'From Squeak 1.3 of Jan 16, 1998 on 29 January 1998 at 12:22:38 am'!

!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
// scale 
	"reimplemented for upwards scalability"

	^(self / scale) floor! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
quo: scale 
	"implemented as such for upwards scalability"

	^(self / scale) truncated! !



