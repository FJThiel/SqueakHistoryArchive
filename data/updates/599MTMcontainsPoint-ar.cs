'From Squeak 2.3 of January 14, 1999 on 15 January 1999 at 4:39:40 pm'!"Change Set:		MTMcontainsPoint-arDate:			15 January 1999Author:			Andreas RaabFixes the picking behavior of MatrixTransformMorph."!!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 1/15/1999 16:34'!containsPoint: aPoint	self visible ifFalse:[^false].	(bounds containsPoint: aPoint) ifFalse: [^ false].	self hasSubmorphs		ifTrue: [self submorphsDo: 					[:m | (m fullContainsPoint: (self transform globalPointToLocal: aPoint))							ifTrue: [^ true]].				^ false]		ifFalse: [^ true]! !