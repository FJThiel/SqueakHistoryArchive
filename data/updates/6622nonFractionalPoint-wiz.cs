'From Squeak3.9alpha of ''2 November 2004'' [latest update: #6461] on 25 November 2004 at 1:17:16 pm'!"Change Set:		NonFractionalPoint-wizDate:			25 November 2004Author:			Wiz (Jerome Peace)Fractional points give warpbit the heebee geebees.Ned wrote a to morph>>position that guarded against this but caused a truncation error that sends rotating morphs wobbling to the top left.This replaces the guard with a simple insurance against fractions in point coords.The true fix would be to have number and fraction have a asNonFraction methods But this should suffice as a patch."!!Morph methodsFor: 'geometry' stamp: 'wiz 11/25/2004 12:54'!position: aPoint 	"Change the position of this morph and and all of its	submorphs. "	| delta box |	delta := aPoint asNonFractionalPoint - bounds topLeft.	(delta x = 0			and: [delta y = 0])		ifTrue: [^ self].	"Null change"	box := self fullBounds.	(delta dotProduct: delta)			> 100		ifTrue: ["e.g., more than 10 pixels moved"			self invalidRect: box.			self				invalidRect: (box translateBy: delta)]		ifFalse: [self				invalidRect: (box						merge: (box translateBy: delta))].	self privateFullMoveBy: delta.	owner		ifNotNil: [owner layoutChanged]! !!Point methodsFor: 'converting' stamp: 'wiz 11/25/2004 12:48'!asNonFractionalPoint(x isFraction or: [y isFraction])	ifTrue:[^ x asFloat @ y asFloat]! !