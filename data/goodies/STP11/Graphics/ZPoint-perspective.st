"ZPoint-perspective.st
These are a few additional ZPoint methods that are needed by the 3D graphics or fractal surface code.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   ZPoint.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 14, 1996"!

!ZPoint methodsFor: 'perspective view'!
perspective
	"Apply a depth-perspective distortion to the receiver."

	| fact |
	fact := 500.0 / (z + 500.0).
	^ZPoint basicNew
		setX: x * fact
		setY: 0.0 - y * fact
		setZ: fact*7! !

!Point methodsFor: 'point functions'!
unitVector
	"Answer a unit vector with the same direction as the receiver"

	^self / self r! !

!ZPoint methodsFor: 'point functions'!
middle: otherPoint displaced: range using: rand
	"Answer a point in the middle, randomly displaced.
	Keep coordinates integral for speed only."
	
	| newX newY newZ |
	newX _ (x + otherPoint x) // 2.
	newY _ (y + otherPoint y) // 2.
	newZ _ (z + otherPoint z) // 2.
	newZ _ newZ + (rand next - 0.5 * range) truncated.
	^ZPoint x: newX y: newY z: newZ! !

