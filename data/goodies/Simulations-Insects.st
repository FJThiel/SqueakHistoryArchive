       NAME     Insect Simulation
       AUTHOR   rlpa80@email.mot.com
       URL      http://
       FUNCTION 
       ST-VERSIONS      Squeak
       PREREQUISITES    (none)
       CONFLICTS        (none known)
       DISTRIBUTION     world
       VERSION  Sq1.0a
       DATE     06-Feb-99

SUMMARY

This is a modified version of the program Gnats.st
which I found on the UIUC archive. The original
author is TPH@cs.man.ac.uk  . All of my changes
are minor; only the ones needed to get Gnats
flying in Squeak

                                John-Reed Maffeo
"!
Model subclass: #Gnat
	instanceVariableNames: 'x y z dx dy dz '
	classVariableNames: 'MaxDX MaxDY MaxDZ MaxX MaxY MaxZ RandomGenerator '
	poolDictionaries: ''
	category: 'Simulations-Insects'!
!Gnat commentStamp: '<historical>' prior: 0!
I represent a class of noxious flying insects, often found in gardens during
summer evenings, and also in LaTeX manuals.

I keep a known location in 3-space, using x, y and z instance variables,
together with a current velocity (dx, dy, dz).  Occasionally, I change my
velocity extensively.!


!Gnat methodsFor: 'initialize-release'!
initialize
	"Initialize the receiver."

	x _ (RandomGenerator next * MaxX) truncated.
	y _ (RandomGenerator next * MaxY) truncated.
	z _ (RandomGenerator next * MaxZ) truncated.
	dx _ (RandomGenerator next * MaxDX * 2) truncated - MaxDX.
	dy _ (RandomGenerator next * MaxDY * 2) truncated - MaxDY.
	dz _ (RandomGenerator next * MaxDZ * 2) truncated - MaxDZ.! !


!Gnat methodsFor: 'accessing'!
asPoint
	"Answer with a 2D point representing the x and y coordinates
	 of the receiver."

	^x@y! !

!Gnat methodsFor: 'accessing'!
dx: aNumber
	"Set the x speed of the receiver."

	dx _ aNumber! !

!Gnat methodsFor: 'accessing'!
dy: aNumber
	"Set the y speed of the receiver."

	dy _ aNumber! !

!Gnat methodsFor: 'accessing'!
dz: aNumber
	"Set the z speed of the receiver."

	dz _ aNumber! !

!Gnat methodsFor: 'accessing'!
x: aNumber
	"Set the x coordinate of receiver."

	x _ aNumber! !

!Gnat methodsFor: 'accessing'!
y: aNumber
	"Set the y coordinate of receiver."

	y _ aNumber! !

!Gnat methodsFor: 'accessing'!
z: aNumber
	"Set the z coordinate of receiver."

	z _ aNumber! !


!Gnat methodsFor: 'moving'!
change
	"Only change speed and direction with low probability."

	(RandomGenerator next > 0.9) ifTrue: [self changeDirection].! !

!Gnat methodsFor: 'moving'!
changeDirection
	"Change the current deltas."

	dx _ (RandomGenerator next * 2 * MaxDX) truncated - MaxDX.
	dy _ (RandomGenerator next * 2 * MaxDY) truncated - MaxDY.
	dz _ (RandomGenerator next * 2 * MaxDZ) truncated - MaxDZ.! !

!Gnat methodsFor: 'moving'!
move
	"Move the receiver to a new location as given by the current
	 deltas.  Simulate a closed universe."

	x _ x + dx \\ MaxX.
	y _ y + dy \\ MaxY.
	z _ z + dz \\ MaxZ.! !


!Gnat methodsFor: 'printing'!
printOn: aStream

	aStream nextPutAll: self class printString, ' at ('.
	x printOn: aStream.
	aStream nextPut: Character space.
	y printOn: aStream.
	aStream nextPut: Character space.
	z printOn: aStream.
	aStream nextPut: $).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Gnat class
	instanceVariableNames: ''!

!Gnat class methodsFor: 'instance creation'!
new
	"Create a new initialized instance of the receiver."

	^super new initialize! !


!Gnat class methodsFor: 'class initialization'!
initialize
	"Gnat initialize."

	MaxX _ MaxY _ MaxZ _ 1000.
	MaxDX _ MaxDY _ MaxDZ _ 50.
	RandomGenerator _ Random new.! !


Gnat initialize!


MouseMenuController subclass: #GnatController
	instanceVariableNames: ''
	classVariableNames: 'GnatYellowButtonMenu GnatYellowButtonMessages '
	poolDictionaries: ''
	category: 'Simulations-Insects'!
!GnatController commentStamp: '<historical>' prior: 0!
I represent a controller for interaction with a world full of flying insects.
I support a yellow button menu which allows new Gnats to be added
to the world, or an (arbitrary) Gnat removed.  I also permit the form,
used by the View to display the Gnats, to be edited.!


!GnatController methodsFor: 'initialize-release'!
initialize
	"Initialize the yellow button menu."

	super initialize.
	self
		yellowButtonMenu: GnatYellowButtonMenu
		yellowButtonMessages: GnatYellowButtonMessages! !


!GnatController methodsFor: 'menu messages'!
addGnat
	"Add a Gnat to the model."

	model add: Gnat new! !

!GnatController methodsFor: 'menu messages'!
editForm
	"Edit the form used to display the Gnat."

	BitEditor openOnForm: view form scale: 32@32! !

!GnatController methodsFor: 'menu messages'!
removeGnat
	"Remove an (arbitrary) Gnat from the model."

	model remove! !


!GnatController methodsFor: 'control defaults'!
controlActivity
	"Modify the model."

	self model move.
	super controlActivity! !

!GnatController methodsFor: 'control defaults'!
isControlActive
	^(view containsPoint: sensor cursorPoint) & sensor blueButtonPressed not! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GnatController class
	instanceVariableNames: ''!

!GnatController class methodsFor: 'class initialization'!
initialize
	"GnatController initialize."

	GnatYellowButtonMenu _ PopUpMenu
		labels: 'add gnat\remove gnat\edit gnat image' withCRs
		lines: #(2).
	GnatYellowButtonMessages _ #(addGnat removeGnat editForm).! !


GnatController initialize!


View subclass: #GnatView
	instanceVariableNames: 'gnatform '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Simulations-Insects'!
!GnatView commentStamp: '<historical>' prior: 0!
I represent a View on a world full of flying insects.  I keep a form
which is used to display each gnat.!


!GnatView methodsFor: 'initialize-release' stamp: 'jrm 2/5/1999 22:05'!
initialize
	"Initialize the receiver.  Create a suitable form for display purposes."

	super initialize.
	gnatform _ (Form
	dotOfSize: 8)! !


!GnatView methodsFor: 'accessing'!
form
	"Answer with the form used to display the Gnats."

	^gnatform! !


!GnatView methodsFor: 'displaying' stamp: 'jrm 2/5/1999 22:07'!
displayView
	"Re-display the entire model."

	model asPoints do: [ :eachPoint |
		gnatform
			displayOn: Display
			at: (self displayTransform: eachPoint)
			clippingBox: self insetDisplayBox
			rule: Form paint
			fillColor: Color black]! !


!GnatView methodsFor: 'controller access'!
defaultControllerClass
	^GnatController! !


!GnatView methodsFor: 'updating'!
update: aParameter
	"Ignore aParameter, and update the view."

	self display! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GnatView class
	instanceVariableNames: ''!

!GnatView class methodsFor: 'instance creation'!
open
	"Open a view on an empty GnatWorld."
	"GnatView open."

	self openOn: GnatWorld new! !

!GnatView class methodsFor: 'instance creation' stamp: 'jrm 2/5/1999 22:06'!
openOn: aGnatWorld 
	"GnatView openOn: (GnatWorld new: 10)."
	| topView gnatView |
	topView _ StandardSystemView new
				model: nil;
				label: aGnatWorld class printString;
				minimumSize: 300 @ 300.
	gnatView _ self new borderWidth: 1.
	gnatView model: aGnatWorld.
	gnatView window: (0 @ 0 corner: 1000 @ 1000).
	gnatView insideColor:  Color white.
	topView addSubView: gnatView.
	topView controller open! !


Model subclass: #GnatWorld
	instanceVariableNames: 'gnats '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Simulations-Insects'!
!GnatWorld commentStamp: '<historical>' prior: 0!
I represent a world full of flying insects.  I keep an OrderedCollection
of insects.  Gnats can be added or removed.!


!GnatWorld methodsFor: 'initialize-release'!
initialize

	gnats _ OrderedCollection new.! !


!GnatWorld methodsFor: 'accessing'!
add: aGnat
	"insert aGnat into the world."

	gnats add: aGnat! !

!GnatWorld methodsFor: 'accessing'!
asPoints
	"Answer with an OrderedCollection of (2D) points representing the
	 receiver."

	^gnats collect: [ :eachGnat | eachGnat asPoint]! !

!GnatWorld methodsFor: 'accessing'!
remove
	"Remove an arbitrary Gnat from the world."

	gnats isEmpty ifFalse: [gnats removeLast]! !


!GnatWorld methodsFor: 'moving'!
move
	"Move all the Gnats in the world."

	gnats do: [ :eachGnat | eachGnat move.  eachGnat change].
	self changed! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GnatWorld class
	instanceVariableNames: ''!

!GnatWorld class methodsFor: 'instance creation'!
new
	"Create a new instance of the receiver."

	^super new initialize! !

!GnatWorld class methodsFor: 'instance creation'!
new: aNumber
	"Create a new instance of the receiver, containing aNumber gnats."

	| newWorld |
	newWorld _ self new.
	aNumber timesRepeat: [newWorld add: Gnat new].
	^newWorld! !
