'From Squeak2.8alpha of 12 January 2000 [latest update: #2139] on 16 May 2000 at 10:18:54 pm'!"Change Set:		FloatPointR-DegreesDate:			16 May 2000Author:			Dan IngallsMakes Point class>>r:degrees: return a point with Float coordinates.This is essential for any geometry with, eg, unit vectors.Any existing senders that were depending on the old integer resultcan be easily repaired by applying asIntegerPoint to the new result."!!Point methodsFor: 'private' stamp: 'di 5/16/2000 22:05'!setR: rho degrees: theta 	| radians |	radians _ theta asFloat degreesToRadians.	x _ rho asFloat * radians cos.	y _ rho asFloat * radians sin.! !