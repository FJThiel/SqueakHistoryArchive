'From Squeak2.9alpha of 13 June 2000 [latest update: #2761] on 28 September 2000 at 2:48:26 pm'!"Change Set:		HeadingTweak-arDate:			28 September 2000Author:			Andreas RaabA little tweak that prevents flexing of sketches immediately after being created."!!Player methodsFor: 'slot getters/setters' stamp: 'ar 9/28/2000 14:47'!setHeading: newHeading	| aCostume |	aCostume _ self costume.	aCostume heading: newHeading.	aCostume _ self costume. "in case we just got flexed for no apparent reason"	(aCostume isFlexMorph and:[aCostume hasNoScaleOrRotation]) 		ifTrue:[aCostume removeFlexShell].! !