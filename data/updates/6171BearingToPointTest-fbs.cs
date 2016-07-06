'From Squeak3.7gamma of ''17 July 2004'' [latest update: #5985] on 10 August 2004 at 1:33:28 pm'!"Change Set:		BearingToPointTest-fbsDate:			5 January 2004Author:			Frank Sheararmd: changed category of test.This replaces a constant (the conversion factor for radians to degrees - 180 / pi) in #bearingToPoint: with the message #radiansToDegrees.And that's not all!!!! You also get some test cases for #bearingToPoint:."!!Point methodsFor: 'point functions' stamp: 'FBS 1/5/2004 13:08'!bearingToPoint: anotherPoint    "Return the bearing, in degrees, from the receiver to anotherPoint.     Adapted from Playground, where the ultimate provenance of the algorithm was a wild earlier method of Jay Fenton's which I never checked carefully, but the thing has always seemed to work"    | deltaX deltaY  |    deltaX := anotherPoint x -  x.    deltaY := anotherPoint y - y.    deltaX abs < 0.001        ifTrue:            [^ deltaY > 0 ifTrue: [180] ifFalse: [0]].    ^ ((deltaX >= 0 ifTrue: [90] ifFalse: [270])            - ((deltaY / deltaX) arcTan negated radiansToDegrees)) rounded! !!PointTest methodsFor: 'testing - testing' stamp: 'FBS 1/5/2004 13:08'!testBearingToPoint	self assert: (0@0 bearingToPoint: 0@0) = 0.	self assert: (0@0 bearingToPoint: 0@-1) = 0.	self assert: (0@0 bearingToPoint: 1@0) = 90.	self assert: (0@0 bearingToPoint: 0@1) = 180.	self assert: (0@0 bearingToPoint: -1@0) = 270.	self assert: (0@0 bearingToPoint: 1@1) = 135.	self assert: (0@0 bearingToPoint: 0.01@0) = 90.	self assert: (0@0 bearingToPoint: -2@-3) = 326.	self assert: (0@0 bearingToPoint: -0@0) = 0.		self assert: (-2@-3 bearingToPoint: 0@0) = 146.! !