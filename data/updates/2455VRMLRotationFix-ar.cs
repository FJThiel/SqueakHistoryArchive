'From Squeak2.9alpha of 12 June 2000 [latest update: #2454] on 14 July 2000 at 10:14:40 am'!"Change Set:		VRMLRotationFix-arDate:			14 July 2000Author:			Andreas RaabA small fix for rotations in VRML"!!VRMLWonderlandBuilder methodsFor: 'converting' stamp: 'ar 7/14/2000 10:14'!rotationFrom: attr	"Fix up coord systems.	FIXME: attr should always be a B3DRotation but for now	the defaults are arrays. How ugly..."	attr class == Array		ifTrue:[^B3DRotation radiansAngle: (attr at: 4) negated axis: 			(B3DVector3 x: (attr at: 1) y: (attr at: 2) z: (attr at: 3))]		ifFalse:[^attr negated]! !