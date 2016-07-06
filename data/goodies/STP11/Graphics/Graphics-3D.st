"Graphics-3D.st
This is Hans-Martin Mosner's 3-D graphics example. It creates wire-frame models and can display and rotate them. It also serves as a cool screen saver. It requires the 3-D point class ZPoint and the two additional Zpoint methods in this directory.

    AUTHOR          Hans-Martin Mosner, ported by Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.02
    PREREQUISITES   ZPoint.st, ZPoint-perspective.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    December 5, 1996"!

Object subclass: #FullTransformation
	instanceVariableNames: 'xx xy xz xt yx yy yz yt zx zy zz zt '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-3D'!
FullTransformation comment:
'FullTransformation does 3-d transformation with a (virtual) 4x4 matrix.
The instance variables represent the following positions in the matrix:
	xx	yx	zx	0
	xy	yy	zy	0
	xz	yz	zz	0
	xt	yt	zt	1'!

!FullTransformation methodsFor: 'transforming'!
moveBy: aZPoint
	xt := xt + aZPoint x.
	yt := yt + aZPoint y.
	zt := zt + aZPoint z! !

!FullTransformation methodsFor: 'transforming'!
scaleBy: aNumber
	xx := xx * aNumber.
	xy := xy * aNumber.
	xz := xz * aNumber.
	yx := yx * aNumber.
	yy := yy * aNumber.
	yz := yz * aNumber.
	zx := zx * aNumber.
	zy := zy * aNumber.
	zz := zz * aNumber! !

!FullTransformation methodsFor: 'transforming'!
turnDegreesX: degrees
	| rad sin cos y z |
	rad := degrees degreesToRadians.
	sin := rad sin.
	cos := rad cos.
	y := xy * cos + (xz * sin).
	z := xz * cos + (xy * sin negated).
	xy := y.
	xz := z.
	y := yy * cos + (yz * sin).
	z := yz * cos + (yy * sin negated).
	yy := y.
	yz := z.
	y := zy * cos + (zz * sin).
	z := zz * cos + (zy * sin negated).
	zy := y.
	zz := z! !

!FullTransformation methodsFor: 'transforming'!
turnDegreesY: degrees
	| rad sin cos x z |
	rad := degrees degreesToRadians.
	sin := rad sin.
	cos := rad cos.
	x := xx * cos + (xz * sin).
	z := xz * cos + (xx * sin negated).
	xx := x.
	xz := z.
	x := yx * cos + (yz * sin).
	z := yz * cos + (yx * sin negated).
	yx := x.
	yz := z.
	x := zx * cos + (zz * sin).
	z := zz * cos + (zx * sin negated).
	zx := x.
	zz := z! !

!FullTransformation methodsFor: 'transforming'!
turnDegreesZ: degrees
	| rad sin cos x y |
	rad := degrees degreesToRadians.
	sin := rad sin.
	cos := rad cos.
	x := xx * cos + (xy * sin).
	y := xy * cos + (xx * sin negated).
	xx := x.
	xy := y.
	x := yx * cos + (yy * sin).
	y := yy * cos + (yx * sin negated).
	yx := x.
	yy := y.
	x := zx * cos + (zy * sin).
	y := zy * cos + (zx * sin negated).
	zx := x.
	zy := y! !


!FullTransformation methodsFor: 'applying transform'!
applyTo: aZPoint
	| x y z |
	x := aZPoint x.
	y := aZPoint y.
	z := aZPoint z.
	^ZPoint basicNew
		setX: xx * x + (xy * y) + (xz * z) + xt
		setY: yx * x + (yy * y) + (yz * z) + yt
		setZ: zx * x + (zy * y) + (zz * z) + zt! !


!FullTransformation methodsFor: 'private'!
identity
	"establish identity transformation"
	xx := yy := zz := 1.0.
	xy := xz := xt := 0.0.
	yx := yz := yt := 0.0.
	zx := zy := zt := 0.0! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

FullTransformation class
	instanceVariableNames: ''!

!FullTransformation class methodsFor: 'instance creation'!
identity
	^self new identity! !


DisplayObject subclass: #WireModel
	instanceVariableNames: 'points wires colors transformedPoints '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-3D'!
WireModel comment:
'WireModel is a simple class that represents 3-dimensional models built out of straight lines (wires).  The functionality is not way complete, but it is a good example (and benchmark and screen saver).

Instance Variables:
	points			<Array of ZPoint> the endpoints of wires and positions of balls.
	wires			<Array of Array of SmallInteger> indices into points
	colors			<Array of Form> colors for wires
	transformedPoints	<Array of ZPoint> perspectively transformed points (rounded)

Written by Hans-Martin Mosner many moons ago, extended and ported by Stephen Travis Pope over the years'!

!WireModel methodsFor: 'initialize-release'!
fixUp
	"Convert the collections to Arrays for slightly faster access. For Squeak,
	this probably doesn't make a difference, but it was noticeable on the
	2-Processor Smalltalk on which I wrote his code :-)
	- HMM"
	wires := wires asArray.
	points := points asArray.
	colors := colors asArray.
! !

!WireModel methodsFor: 'initialize-release'!
initialize
	wires := OrderedCollection new.
	colors := OrderedCollection new.
	points := OrderedCollection new.
! !


!WireModel methodsFor: 'accessing'!
addWire: aCollection
	wires add: (aCollection collect: [:each | self pointIndexFor: each]) asArray.
	colors add: Color black! !

!WireModel methodsFor: 'accessing'!
addWire: aCollection color: aForm 
	wires add: (aCollection collect: [:each | self pointIndexFor: each]) asArray.
	colors add: aForm! !

!WireModel methodsFor: 'accessing'!
pointIndexFor: aPoint 
	| index |
	index := points indexOf: aPoint.
	index = 0
		ifTrue:  [points add: aPoint convertToFloats.
			index := points size].
	^index! !

!WireModel methodsFor: 'accessing'!
setPoints: aCollection
	transformedPoints := aCollection! !

!WireModel methodsFor: 'accessing'!
transformPointsBy: trans
	| result |
	result := Array new: points size.
	1 to: points size do: 
		[:i | 
		result at: i put: (trans applyTo: (points at: i)) perspective truncated].
	^result! !


!WireModel methodsFor: 'displaying'!
displayOn: aGraphicsContext at: aPoint
	"I only have to do it this way because I can't figure out how to get color blt to work..."

	self displayOn: aGraphicsContext at: aPoint inColor: Color black! !

!WireModel methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint inColor: color
	"Display the receiver model on the given DisplayMedium at the given point."
	"The constant color is because I can't find the bug that makes it not show up when I try to use the model's real colors--see the commented-outline below."

	| wire blt pt1 pt2 |
	blt := BitBlt
		destForm: aDisplayMedium
		sourceForm: nil
		fillColor: nil		"should be nil"
		combinationRule: Form paint
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: Display boundingBox.
	1 to: wires size do: 
		[ :i | 
		wire := wires at: i.
		blt fillColor: (colors at: i).
		pt1 := (transformedPoints at: wire first) asPoint + aPoint.
		2 to: wire size do:
			[ :index | 
			pt2 := (transformedPoints at: (wire at: index)) asPoint + aPoint.
			blt drawFrom: pt1 to: pt2.
			pt1 := pt2]]! !

!WireModel methodsFor: 'displaying'!
floatIn: displayGC while: aBlock
	"Animate the receiver on the given display medium while the given block is true."
	"This is great for making screen savers."
	"WireModel screenSaver"

	| trans origin nextPoint rand step numberSteps list dx dy dz  pixmapGC |
	displayGC fillBlack.
	trans := FullTransformation identity.
	trans scaleBy: 5.
	pixmapGC := Form extent: 400 @ 400 depth: Display depth.
	rand := Random new.
	origin := (displayGC width * rand next) @ (displayGC height * rand next).
	numberSteps := 0.
	list := self transformPointsBy: trans.
	dx := dy := dz := 0.0.
	[aBlock value]
		whileTrue: [numberSteps = 0
				ifTrue: [nextPoint :=  (displayGC width * rand next) 
							@ (displayGC height * rand next).
					step := (nextPoint - origin) unitVector * 2.0.
					numberSteps := ((origin dist: nextPoint) / 2.0) rounded].
			self setPoints: list.
			pixmapGC fillBlack.
			self displayOn: pixmapGC at: 200 @ 200 inColor: Color white.
			pixmapGC displayOn: displayGC at: origin rounded - (200@200).
			origin := origin + step.
			numberSteps := numberSteps - 1.
			dx := (dx + rand next - 0.5 min: 7) max: -7.
			dy := (dy + rand next - 0.5 min: 7) max: -7.
			dz := (dz + rand next - 0.5 min: 7) max: -7.
			trans turnDegreesX: dx.
			trans turnDegreesY: dy.
			trans turnDegreesZ: dz.
			list := self transformPointsBy: trans]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WireModel class
	instanceVariableNames: ''!

!WireModel class methodsFor: 'instance creation'!
new
	^super new initialize! !

!WireModel class methodsFor: 'instance creation'!
readFrom: aStream 
	| newModel wire x y z color colorName |
	newModel := self new.
	color := Color black.
	[aStream atEnd]
		whileFalse: 
			[wire := WriteStream on: (Array new: 5).
			[aStream atEnd or: [aStream peek isLetter]]
				whileFalse: 
					[x := Number readFrom: aStream.
					aStream skipSeparators.
					y := Number readFrom: aStream.
					aStream skipSeparators.
					z := Number readFrom: aStream.
					aStream skipSeparators.
					wire nextPut: (ZPoint
							x: x asFloat
							y: y asFloat
							z: z asFloat)].
			wire contents size > 0 ifTrue: [newModel addWire: wire contents color: color].
			colorName := (aStream upTo: Character cr) asSymbol.
			color := Color perform: colorName].
	^newModel! !


!WireModel class methodsFor: 'screen saving'!
example: aSymbol
	
	^self example: aSymbol direct: false! !

!WireModel class methodsFor: 'screen saving'!
example: aSymbol direct: direct 
	"Show a model in the upper left corner of the screen. The model can 
	be moved by moving the cursor. When direct is false, the model is 
	displayed in a shadow form to reduce flicker (but it's slower this 
	way on some machines)"
	"WireModel example: #eightCubes direct: false"
	"WireModel example: #eightCubes direct: true"
	"WireModel example: #lines direct: false"
	"WireModel example: #chair direct: false"

	| model trans points lastPoint newPoint output |
	model := WireModel perform: aSymbol.
	model fixUp.
	lastPoint := nil.
	direct ifFalse: [output := Form extent: Display extent depth: Display depth]
		ifTrue: [output := Display].
	[Sensor noButtonPressed]
		whileTrue: 
			[newPoint := Sensor cursorPoint.
			lastPoint = newPoint
				ifFalse: [output fillWhite.
					lastPoint := newPoint.
					trans := FullTransformation identity.
					trans scaleBy: 5.
					trans turnDegreesY: newPoint x - 400 / 2.0.
					trans turnDegreesZ: newPoint y - 400 / 2.0.
					points := model transformPointsBy: trans.
					model setPoints: points.
					model displayOn: output at: 250 @ 250.
					direct ifFalse: [output displayOn: Display]]].
	ScheduledControllers restore.! !

!WireModel class methodsFor: 'screen saving'!
exampleMenu
	"Run WireModel demos from a Menu."
	"WireModel exampleMenu."

	| coll stream menu choice |
	coll := WireModel class organization listAtCategoryNamed: #examples.
	stream := WriteStream on: (String new: 64).
	coll do: [ :i |
		stream nextPutAll: i asString, '\'].
	stream skip: -1.
	menu := PopUpMenu labels: stream contents withCRs.
	choice := menu startUp.
	choice = 0 ifTrue: [^nil].
	WireModel perform: #example: with: (coll at: choice).! !

!WireModel class methodsFor: 'screen saving'!
screenSaver
	"Float a wire model around on the screen for a while; change it every 
	so often; stop when the user presses qa mouse button or keyboard key."
	"WireModel screenSaver"

	| variants dict stop rnd model message time cached last screenWindow sensor |
	variants := self class organization listAtCategoryNamed: #examples.
	cached := "Set new" variants "#(#chair #eightCubes #globe #globe2 #wheel )" .
	dict := Dictionary new.
	stop := false.
	rnd := Random new.
	last := nil.
	Cursor blank showWhile: 
	[[stop]
		whileFalse: 
			[[message := variants at: (rnd next * variants size) truncated + 1.
			last == message] whileTrue.
			last := message.
			model := (cached includes: message)
				ifFalse: [(self perform: message) fixUp]
				ifTrue: [dict at: message 
					ifAbsent: [dict at: message put: (self perform: message) fixUp]].
			time := Time totalSeconds.
			model
				floatIn: Display while: 
					[Sensor anyButtonPressed ifTrue: [stop := true].
					Sensor keyboardPressed
						ifTrue: [Sensor keyboard.
							stop := true].
					Time totalSeconds - time < 30 and: [stop not]]]].
	ScheduledControllers restore! !


!WireModel class methodsFor: 'examples'!
chair
	"WireModel example: #chair"

	^WireModel readFrom: (ReadStream on: 'red
-10 -20 10
-10 20 10
10 20 10
10 -20 10
red
-10 -20 -10
-10 0 -10
red
10 -20 -10
10 0 -10
10 0 10
-10 0 10
-10 0 -10
10 0 -10
gray
8 0 -10
8 0 10
gray
6 0 -10
6 0 10
gray
4 0 -10
4 0 10
gray
2 0 -10
2 0 10
gray
0 0 -10
0 0 10
gray
-8 0 -10
-8 0 10
gray
-6 0 -10
-6 0 10
gray
-4 0 -10
-4 0 10
gray
-2 0 -10
-2 0 10
red
-5 0 10
-5 20 10
red
0 0 10
0 20 10
red
5 0 10
5 20 10
gray
')! !

!WireModel class methodsFor: 'examples'!
chair2
	"WireModel example: #chair2"

	^WireModel readFrom: (ReadStream on: 'white
-10 -20 10
-10 20 10
10 20 10
10 -20 10
white
-10 -20 -10
-10 0 -10
white
10 -20 -10
10 0 -10
10 0 10
-10 0 10
-10 0 -10
10 0 -10
white
8 0 -10
8 0 10
white
6 0 -10
6 0 10
white
4 0 -10
4 0 10
white
2 0 -10
2 0 10
white
0 0 -10
0 0 10
white
-8 0 -10
-8 0 10
white
-6 0 -10
-6 0 10
white
-4 0 -10
-4 0 10
white
-2 0 -10
-2 0 10
white
-5 0 10
-5 20 10
white
0 0 10
0 20 10
white
5 0 10
5 20 10
white
')! !

!WireModel class methodsFor: 'examples'!
eightCubes
	"WireModel example: #eightCubes"

	| model block |
	model := WireModel new.
	block := [:cx :cy :cz | #(#(#(-5 -5 -5 ) #(-5 5 -5 ) #(-5 5 5 ) #(-5 -5 5 ) #(-5 -5 -5 ) #(5 -5 -5 ) #(5 5 -5 ) #(5 5 5 ) #(5 -5 5 ) #(5 -5 -5 ) ) #(#(-5 5 -5 ) #(5 5 -5 ) ) #(#(-5 5 5 ) #(5 5 5 ) ) #(#(-5 -5 5 ) #(5 -5 5 ) ) ) do: [:array | model addWire: (array collect: [:triple | ZPoint
							x: triple first + cx
							y: (triple at: 2)
									+ cy
							z: triple last + cz])
					color: Color gray]].
	#(-10 10 ) do: [:x | #(-10 10 ) do: [:y | #(-10 10 ) do: [:z | block
					value: x
					value: y
					value: z]]].
	^model! !

!WireModel class methodsFor: 'examples'!
globe
	"WireModel example: #globe"

	| model scale points lat lon cos sin wire last each |
	model := self new.
	scale := 20.0.
	points := OrderedCollection new.
	points add: (ZPoint x: scale y: 0.0 z: 0.0).
	lat := (90 - 63.5) degreesToRadians.
	0 to: 360 - 72 by: 72 do: 
			[:deg | 
			lon := deg degreesToRadians.
			cos := lon cos.
			sin := lon sin.
			points add: (ZPoint
					x: lat sin * scale
					y: cos * lat cos * scale
					z: sin * lat cos * scale)].
	lat := (90 - 63.5) negated degreesToRadians.
	36 to: 360 - 36 by: 72 do: 
			[:deg | 
			lon := deg degreesToRadians.
			cos := lon cos.
			sin := lon sin.
			points add: (ZPoint
					x: lat sin * scale
					y: cos * lat cos * scale
					z: sin * lat cos * scale)].
	points add: (ZPoint x: scale negated y: 0 z: 0).
	#(#(2 3 4 5 6 2 1 3 8 4 9 5 10 6 11 2 7 12 8 9 10 11 7 8 ) #(7 3 ) #(1 4 ) #(1 5 ) #(1 6 ) #(12 9 ) #(12 10 ) #(12 11 ) ) do: 
		[:collection | 
		wire := OrderedCollection new.
		last := nil.
		collection do: 
			[:index | 
			each := points at: index.
			last == nil ifFalse: [wire add: (last + each) unitVector * scale].
			last := each.
			wire add: each].
		model addWire: wire color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
globe2
	"WireModel example: #globe2"
	| cos sin model rad z f s c  |
	model := self new.
	cos := OrderedCollection new.
	sin := OrderedCollection new.
	0 to: 360 by: 15 do: 
			[:degree | 
			rad := degree degreesToRadians.
			cos add: rad cos.
			sin add: rad sin].
	2 to: cos size // 2 do: 
		[:j | 
		z := 20 * (cos at: j).
		f := 20 * (sin at: j).
		model addWire: ((1 to: sin size) , #(1 ) collect: [:i | ZPoint
					x: (sin at: i) * f
					y: (cos at: i) * f
					z: z])
			color: Color random].
	1 to: sin size // 2 do: 
		[:j | 
		s := 20 * (sin at: j).
		c := 20 * (cos at: j).
		model addWire: ((1 to: sin size) , #(1 )
				collect: 
					[:i | 
					f := sin at: i.
					ZPoint x: f * s
						y: f * c
						z: 20 * (cos at: i)])
			color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
globe4
	| cos sin model rad z f s c |
	model := self new.
	cos := OrderedCollection new.
	sin := OrderedCollection new.
	0 to: 360 by: 30 do: 
			[:degree | 
			rad := degree degreesToRadians.
			cos add: rad cos.
			sin add: rad sin].
	2 to: cos size // 2 do: 
		[:j | 
		z := 20 * (cos at: j).
		f := 20 * (sin at: j).
		model addWire: ((1 to: sin size) , #(1 ) collect: [:i | ZPoint
					x: (sin at: i) * f
					y: (cos at: i) * f
					z: z])
			color: Color random].
	1 to: sin size // 2 do: 
		[:j | 
		s := 20 * (sin at: j).
		c := 20 * (cos at: j).
		model addWire: ((1 to: sin size) , #(1 ) collect: 
					[:i | 
					f := sin at: i.
					ZPoint
						x: f * s
						y: f * c
						z: 20 * (cos at: i)])
			color: Color random].
"	set := Set new.
	#(#(#(-20 -20 -20 ) #(-20 20 -20 ) #(-20 20 20 ) #(-20 -20 20 ) #(-20 -20 -20 ) #(20 -20 -20 ) #(20 20 -20 ) #(20 20 20 ) #(20 -20 20 ) #(20 -20 -20 ) ) #(#(-20 20 -20 ) #(20 20 -20 ) ) #(#(-20 20 20 ) #(20 20 20 ) ) #(#(-20 -20 20 ) #(20 -20 20 ) ) ) 
		do: [:array | model addWire: 
			(array collect: [:triple | 
				set add: (ZPoint x: triple first y: (triple at: 2) z: triple last)])
			color: Color random].
	set do: [:point | model addBallAt: point].
"	^model! !

!WireModel class methodsFor: 'examples'!
lines
	"WireModel example: #lines"

	| model rnd  rndBlock |
	model := self new.
	rnd := Random new.
	rndBlock := [(ZPoint
				x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) * 60].
	20 timesRepeat: [model addWire: (Array
				with: rndBlock value
				with: rndBlock value)
			color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
randomCubes
	"WireModel example: #randomCubes direct: false"

	| model block rnd side |
	model := WireModel new.
	side := 40.
	rnd := Random new.
	block := [:cx :cy :cz :color |
			#(#(#(-2 -2 -2 ) #(-2 2 -2 ) #(-2 2 2 ) #(-2 -2 2 ) #(-2 -2 -2 ) #(2 -2 -2 ) #(2 2 -2 ) #(2 2 2 ) #(2 -2 2 ) #(2 -2 -2 ) ) #(#(-2 2 -2 ) #(2 2 -2 ) ) #(#(-2 2 2 ) #(2 2 2 ) ) #(#(-2 -2 2 ) #(2 -2 2 ) ) ) do: [:array | model addWire: (array collect: [:triple | "trans applyTo:"
						ZPoint
							x: triple first + cx
							y: (triple at: 2)
									+ cy
							z: triple last + cz])
					color: color]].
	16 timesRepeat: [block valueWithArguments: 
			(Array with: (rnd next * 60 - 30) rounded
					with: (rnd next * 60 - 30) rounded
					with: (rnd next * 60 - 30) rounded
					with: Color random)].
	(Array
		with: (Array with: (ZPoint
					x: 0
					y: 0
					z: side)
				with: (ZPoint
						x: 0
						y: 0
						z: side negated))
		with: (Array with: (ZPoint
					x: 0
					y: side
					z: 0)
				with: (ZPoint
						x: 0
						y: side negated
						z: 0))
		with: (Array with: (ZPoint
					x: side
					y: 0
					z: 0)
				with: (ZPoint
						x: side negated
						y: 0
						z: 0)))
		do: 
			[:array | 
			model addWire: array color: Color white].
	^model! !

!WireModel class methodsFor: 'examples'!
randomCubes2
	"WireModel example: #randomCubes2 direct: false"

	| model block rnd |
	model := WireModel new.
	rnd := Random new.
	block := [ :cx :cy :cz :color |
			#(#(#(-2 -2 -2 ) #(-2 2 -2 ) #(-2 2 2 ) #(-2 -2 2 ) #(-2 -2 -2 ) #(2 -2 -2 ) #(2 2 -2 ) #(2 2 2 ) #(2 -2 2 ) #(2 -2 -2 ) ) #(#(-2 2 -2 ) #(2 2 -2 ) ) #(#(-2 2 2 ) #(2 2 2 ) ) #(#(-2 -2 2 ) #(2 -2 2 ) ) ) do: 
		[ :array | model addWire: (array collect: 
			[ :triple | 
			ZPoint x: triple first + cx
					y: (triple at: 2) + cy
					z: triple last + cz])
				color: color]].
	24 timesRepeat: [block valueWithArguments: 
			(Array with: (rnd next * 60 - 30) rounded
					with: (rnd next * 60 - 30) rounded
					with: (rnd next * 60 - 30) rounded
					with: Color random)].
	^model! !

!WireModel class methodsFor: 'examples'!
rays
	| model rnd rndBlock point |
	model := self new.
	rnd := Random new.
	rndBlock := [(ZPoint
				x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) unitVector * 20].
	32 timesRepeat: 
			[point := rndBlock value.
			model addWire: (Array with: point with: point * 0.2)
				color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
rays2
	| model rnd rndBlock point |
	model := self new.
	rnd := Random new.
	rndBlock := [(ZPoint
				x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) unitVector * 40].
	32 timesRepeat: 
			[point := rndBlock value.
			model addWire: (Array with: point with: point * 0.2)
				color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
spikes
	"WireModel example: #spikes"

	| model rnd center rndBlock |
	model := self new.
	rnd := Random new.
	center := ZPoint x: 0 y: 0 z: 0.
	rndBlock := [(ZPoint
				x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) * 40].
	24 timesRepeat: [model addWire: (Array
				with: rndBlock value
				with: center
				with: rndBlock value)
			color: Color random].
	^model! !

!WireModel class methodsFor: 'examples'!
string
	"WireModel example: #string"

	| model rnd point list trans |
	model := self new.
	rnd := Random new.
	point := (ZPoint x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) unitVector * 30.
	list := OrderedCollection new.
	trans := FullTransformation identity.
	80 timesRepeat: 
			[list add: point.
			trans turnDegreesX: rnd next - 0.5 * 10;
				 turnDegreesY: rnd next - 0.5 * 10;
				 turnDegreesZ: rnd next - 0.5 * 10.
			point := trans applyTo: point].
	model addWire: list color: Color white.
	^model! !

!WireModel class methodsFor: 'examples'!
string2
	| model rnd point list trans |
	model := self new.
	rnd := Random new.
	point := (ZPoint
				x: rnd next - 0.5
				y: rnd next - 0.5
				z: rnd next - 0.5) unitVector * 30.
	list := OrderedCollection new.
	trans := FullTransformation identity.
	40 timesRepeat: 
			[list add: point.
			trans turnDegreesX: rnd next - 0.5 * 30;
				 turnDegreesY: rnd next - 0.5 * 30;
				 turnDegreesZ: rnd next - 0.5 * 30.
			point := trans applyTo: point].
	model addWire: list color: Color white.
	^model! !

!WireModel class methodsFor: 'examples'!
wheel
	"WireModel example: #wheel"

	| cos sin model rad center |
	model := self new.
	cos := OrderedCollection new.
	sin := OrderedCollection new.
	0 to: 360 - 15 by: 15 do: 
			[:degree | 
			rad := degree degreesToRadians.
			cos add: rad cos * 20.
			sin add: rad sin * 20].
	center := ZPoint x: 0 y: 0 z: 0.
	sin with: cos do: 
		[:x :y | 
		model addWire: (Array with: center with: (ZPoint
					x: x
					y: y
					z: 0))
			color: Color gray.
		model addWire: (Array with: (ZPoint x: x y: y z: 5)
				with: (ZPoint x: x y: y z: -5))
			color: Color gray].
	model addWire: ((1 to: sin size)
			, #(1 ) collect: [:i | ZPoint
				x: (sin at: i)
				y: (cos at: i)
				z: 5])
		color: Color gray.
	model addWire: ((1 to: sin size)
			, #(1 ) collect: [:i | ZPoint
				x: (sin at: i)
				y: (cos at: i)
				z: -5])
		color: Color gray.
	^model! !
