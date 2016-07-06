"ZPoint.st
3-D points, i.e., one can say 3@4@7.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    February 5, 1985"!
	
Point subclass: #ZPoint
	instanceVariableNames: 'z '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

ZPoint comment:
'ZPoints define a third instance variable (z) in addition to Point''s x and y to hold 3-dimensional point values.'!

!ZPoint methodsFor: 'accessing'!
z
	^z!

z: aNumber
	z := aNumber! !

!ZPoint methodsFor: 'arithmetic'!
* aZPointOrNumber 
	"only works if the argument is a ZPoint or Number"

	| scalePoint |
	scalePoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x * scalePoint x
		y: y * scalePoint y
		z: z * scalePoint z!

+ aZPointOrNumber 
	"Answer a new ZPoint that is the sum of the receiver and delta 
	(which is a ZPoint  
	or Number)."

	| deltaPoint |
	deltaPoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x + deltaPoint x
		y: y + deltaPoint y
		z: z + deltaPoint z!

- aZPointOrNumber 
	"Answer a new ZPoint that is the sum of the receiver and delta 
	(which is a ZPoint  
	or Number)."

	| deltaPoint |
	deltaPoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x - deltaPoint x
		y: y - deltaPoint y
		z: z - deltaPoint z!

/ aZPointOrNumber 
	"only works if the argument is a ZPoint or Number"

	| scalePoint |
	scalePoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x / scalePoint x
		y: y / scalePoint y
		z: z / scalePoint z!

rounded
	^self class basicNew
		setX: x rounded
		setY: y rounded
		setZ: z rounded! !

!ZPoint methodsFor: 'converting'!
asPoint
	^x@y! !

!ZPoint methodsFor: 'point functions'!
dotProduct: aZPoint
	| temp |
	temp := self * aZPoint.
	^temp x abs + temp y abs + temp z abs! !

!ZPoint methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: '@'.
	z printOn: aStream!

storeOn: aStream
	"copied & modified from class Point"

	aStream nextPut: $(;
	nextPutAll: self species name;
	nextPutAll: ' x: ';
	store: x;
	nextPutAll: ' y: ';
	store: y;
	nextPutAll: ' z: ';
	store: z;
	nextPut: $).! !

!ZPoint methodsFor: 'private'!
convertToFloats
	x := x asFloat.
	y := y asFloat.
	z := z asFloat!

setX: xValue setY: yValue setZ: zValue
	x := xValue.
	y := yValue.
	z := zValue! !

!ZPoint methodsFor: 'testing'!
= aPoint
	^super = aPoint and: [z = aPoint z]!

hash
	^super hash bitXor: z hash!

isZero
	^super isZero and: [z isZero]! !

ZPoint class
	instanceVariableNames: ''!

!ZPoint class methodsFor: 'instance creation'!
x: xValue y: yValue z: zValue
	^self basicNew setX: xValue setY: yValue setZ: zValue! !


!Point methodsFor: 'coercing'!
@ aZValue
	"Answer a ZPoint with the receiver and the argument as coordinates."

	^ZPoint x: x y: y z: aZValue! !


!Point methodsFor: 'truncation and round off'!
float
	"Answer a new Point that is the receiver's x and y as floating-point numbers."

	^x asFloat @ y asFloat! !
