'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 28 February 2001 update 3952] on 25 April 2001 at 11:13:33 am'!"Change Set:		InvalidationFix-arDate:			25 April 2001Author:			Andreas RaabFixes an invalidation problem"!!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 4/25/2001 11:12'!invalidRect: rect from: aMorph	^super invalidRect: (self transform localBoundsToGlobal: rect) from: aMorph! !