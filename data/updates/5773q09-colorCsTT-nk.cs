'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:53 pm'!
"Change Set:		q09-colorCsTT-nk
Date:			9 July 2003
Author:			Ned Konz

Adopted for Squeak 3.7a on 3/4/04 (sw), from Squeakland update 0162colorCsTurtleTrails-nk.cs.  There were three collisions with MCP changes but in each case Ned's Squeakland version subsumed the MCP change, so all the code in this update is identical to the Squeakland version.

* Makes turtle trails appear above the background sketch if any
* Makes #color:sees: and #touchesColor: tests work right with display depths > 8.
* Makes #color:sees: and #touchesColor: work right in the World
* Makes the grid visible to #color:sees:, #colorUnder and #touchesColor:

It may break some scripts that use #color:sees: or #touchesColor: on playfields that have background grids, if the color being sought is exactly the same color as the color of the grid. To fix these scripts, change the grid color slightly.

It also will use more memory for the turtle trails form when your display depth is > 8. However, this memory will be discarded before saving the image.

This change set also removes an unnecessary ColorPatchCanvas copy that was in #patchAt:without:andNothingAbove:

v2: made sure that backgroundMorph gets nil'd out when it's unlocked and then deleted."!


!FormCanvas class methodsFor: 'instance creation' stamp: 'nk 7/4/2003 10:11'!
extent: extent depth: depth origin: aPoint clipRect: aRectangle

	^ self new
		setForm: (Form extent: extent depth: depth);
		setOrigin: aPoint clipRect: aRectangle;
		yourself! !


!Morph methodsFor: 'geometry eToy' stamp: 'nk 7/7/2003 17:18'!
color: sensitiveColor sees: soughtColor 
	"Return true if any of my pixels of sensitiveColor intersect with pixels of soughtColor."

	"Make a mask with black where sensitiveColor is, white elsewhere"

	| myImage sensitivePixelMask map patchBelowMe tfm morphAsFlexed i1 pasteUp |
	pasteUp _ self world ifNil: [ ^false ].
	tfm := self transformFrom: pasteUp.
	morphAsFlexed := tfm isIdentity 
				ifTrue: [self]
				ifFalse: [TransformationMorph new flexing: self clone byTransformation: tfm].
	myImage := morphAsFlexed imageForm offset: 0 @ 0.
	sensitivePixelMask := Form extent: myImage extent depth: 1.
	"ensure at most a 16-bit map"
	map := Bitmap new: (1 bitShift: (myImage depth - 1 min: 15)).
	map at: (i1 := sensitiveColor indexInMap: map) put: 1.
	sensitivePixelMask 
		copyBits: sensitivePixelMask boundingBox
		from: myImage form
		at: 0 @ 0
		colorMap: map.

	"get an image of the world below me"
	patchBelowMe := pasteUp 
				patchAt: morphAsFlexed fullBounds
				without: self
				andNothingAbove: false.
	"
sensitivePixelMask displayAt: 0@0.
patchBelowMe displayAt: 100@0.
"
	"intersect world pixels of the color we're looking for with the sensitive pixels"
	map at: i1 put: 0.	"clear map and reuse it"
	map at: (soughtColor indexInMap: map) put: 1.
	sensitivePixelMask 
		copyBits: patchBelowMe boundingBox
		from: patchBelowMe
		at: 0 @ 0
		clippingBox: patchBelowMe boundingBox
		rule: Form and
		fillColor: nil
		map: map.
	"
sensitivePixelMask displayAt: 200@0.
"
	^(sensitivePixelMask tallyPixelValues second) > 0! !

!Morph methodsFor: 'geometry eToy' stamp: 'nk 7/7/2003 17:19'!
touchesColor: soughtColor 
	"Return true if any of my pixels overlap pixels of soughtColor."

	"Make a shadow mask with black in my shape, white elsewhere"

	| map patchBelowMe shadowForm tfm morphAsFlexed pasteUp |
	pasteUp := self world ifNil: [ ^false ].

	tfm := self transformFrom: pasteUp.
	morphAsFlexed := tfm isIdentity 
				ifTrue: [self]
				ifFalse: [TransformationMorph new flexing: self clone byTransformation: tfm].
	shadowForm := morphAsFlexed shadowForm offset: 0 @ 0.

	"get an image of the world below me"
	patchBelowMe := (pasteUp 
				patchAt: morphAsFlexed fullBounds
				without: self
				andNothingAbove: false) offset: 0 @ 0.
	"
shadowForm displayAt: 0@0.
patchBelowMe displayAt: 100@0.
"
	"intersect world pixels of the color we're looking for with our shape."
	"ensure a maximum 16-bit map"
	map := Bitmap new: (1 bitShift: (patchBelowMe depth - 1 min: 15)).
	map at: (soughtColor indexInMap: map) put: 1.
	shadowForm 
		copyBits: patchBelowMe boundingBox
		from: patchBelowMe
		at: 0 @ 0
		clippingBox: patchBelowMe boundingBox
		rule: Form and
		fillColor: nil
		map: map.
	"
shadowForm displayAt: 200@0.
"
	^(shadowForm tallyPixelValues second) > 0! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'nk 7/4/2003 16:07'!
drawOn: aCanvas 
	"Draw in order:
	- background color
	- grid, if any
	- background sketch, if any
	- Update and draw the turtleTrails form. See the comment in updateTrailsForm.
	- cursor box if any

	Later (in drawSubmorphsOn:) I will skip drawing the background sketch."

	"draw background fill"
	super drawOn: aCanvas.

	"draw grid"
	(self griddingOn and: [self gridVisible]) 
		ifTrue: 
			[aCanvas fillRectangle: self bounds
				fillStyle: (self 
						gridFormOrigin: self gridOrigin
						grid: self gridModulus
						background: nil
						line: Color lightGray)].

	"draw background sketch."
	backgroundMorph ifNotNil: [
		self clipSubmorphs ifTrue: [
			aCanvas clipBy: self clippingBounds
				during: [ :canvas | canvas fullDrawMorph: backgroundMorph ]]
			ifFalse: [ aCanvas fullDrawMorph: backgroundMorph ]].

	"draw turtle trails"
	self updateTrailsForm.
	turtleTrailsForm 
		ifNotNil: [aCanvas paintImage: turtleTrailsForm at: self position].

	"draw cursor"
	(submorphs notEmpty and: [self indicateCursor]) 
		ifTrue: 
			[aCanvas 
				frameRectangle: self selectedRect
				width: 2
				color: Color black]! !

!PasteUpMorph methodsFor: 'painting' stamp: 'nk 7/4/2003 15:59'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front, but skip my background sketch."

	| drawBlock |
	submorphs isEmpty ifTrue: [^self].
	drawBlock := [:canvas | submorphs reverseDo: [:m | m ~~ backgroundMorph ifTrue: [ canvas fullDrawMorph: m ]]].
	self clipSubmorphs 
		ifTrue: [aCanvas clipBy: self clippingBounds during: drawBlock]
		ifFalse: [drawBlock value: aCanvas]! !

!PasteUpMorph methodsFor: 'pen' stamp: 'nk 7/7/2003 11:17'!
createOrResizeTrailsForm
	"If necessary, create a new turtleTrailsForm or resize the existing one to fill my bounds.
	On return, turtleTrailsForm exists and is the correct size.
	Use the Display depth so that color comparisons (#color:sees: and #touchesColor:) will work right."

	| newForm |
	(turtleTrailsForm isNil or: [ turtleTrailsForm extent ~= self extent ]) ifTrue:
		["resize TrailsForm if my size has changed"
		newForm _ Form extent: self extent depth: Display depth.
		turtleTrailsForm ifNotNil: [
			newForm copy: self bounds from: turtleTrailsForm
					to: 0@0 rule: Form paint ].
		turtleTrailsForm _ newForm.
		turtlePen _ nil].

	"Recreate Pen for this form"
	turtlePen ifNil: [turtlePen _ Pen newOnForm: turtleTrailsForm].! !

!PasteUpMorph methodsFor: 'project state' stamp: 'nk 7/4/2003 16:47'!
handsDo: aBlock

	^ worldState ifNotNil: [ worldState handsDo: aBlock ]! !

!PasteUpMorph methodsFor: 'project state' stamp: 'nk 7/4/2003 16:46'!
handsReverseDo: aBlock

	^ worldState ifNotNil: [ worldState handsReverseDo: aBlock ]! !

!PasteUpMorph methodsFor: 'submorphs-accessing' stamp: 'nk 7/4/2003 16:49'!
morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock
	"Include hands if the receiver is the World"
	self handsDo:[:m|
		m == someMorph ifTrue:["Try getting out quickly"
			owner ifNil:[^self].
			^owner morphsInFrontOf: self overlapping: aRectangle do: aBlock].
		"The hand only overlaps if it's not the hardware cursor"
		m needsToBeDrawn ifTrue:[
			(m fullBoundsInWorld intersects: aRectangle)
				ifTrue:[aBlock value: m]]].
	^super morphsInFrontOf: someMorph overlapping: aRectangle do: aBlock! !

!PasteUpMorph methodsFor: 'world state' stamp: 'nk 7/7/2003 11:15'!
patchAt: patchRect without: stopMorph andNothingAbove: stopThere
	"Return a complete rendering of this patch of the display screen
	without stopMorph, and possibly without anything above it."

	| c |
	c _ ColorPatchCanvas
		extent: patchRect extent
		depth: Display depth
		origin: patchRect topLeft negated
		clipRect: (0@0 extent: patchRect extent).
	c stopMorph: stopMorph.
	c doStop: stopThere.

	(self bounds containsRect: patchRect) ifFalse:
		["Need to fill area outside bounds with black."
		c form fillColor: Color black].
	(self bounds intersects: patchRect) ifFalse:
		["Nothing within bounds to show."
		^ c form].
	self fullDrawOn: c.
	stopThere ifFalse: [ self world handsReverseDo: [:h | h drawSubmorphsOn: c]].
	^c form
! !

!PasteUpMorph methodsFor: 'private' stamp: 'nk 7/8/2003 09:18'!
privateRemoveMorph: aMorph
	backgroundMorph == aMorph ifTrue: [ backgroundMorph _ nil ].
	^super privateRemoveMorph: aMorph.
! !

