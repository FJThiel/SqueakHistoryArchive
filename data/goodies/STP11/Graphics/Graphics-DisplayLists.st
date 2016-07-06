"Graphics-DisplayLists.st
This is the Squeak Siren DisplayList Framework. It is written to support the MODE's music editing tools, and is not the world's most comprehensive and elegant structured graphics hierarchy. It is, however, one of the simplest and most easily extensible.
	One could easily add support for regular polygons, better arcs, more kinds of paths, etc.
	Note also that the display item hierarchy is separate from the display list editing framework.

The class DisplayList has loads of example methods; try,
	DisplayList testAll
for a good sampler

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.2
    PREREQUISITES   STP's system changes
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    July 8, 1997"!

DisplayObject subclass: #DisplayModel
	instanceVariableNames: 'container model '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayModel comment:
'An instance of a subclass of the abstract class DisplayModel is a DisplayObject that has some other thing as its "domain model."
Instance Variables
	container		<DisplayModel> the "outer" display item
	model		<Object> the "domain" model
The class DisplayList has loads of example methods; try
	DisplayList testAll
For more notes, see the documentation class protocol in this class'!

!DisplayModel methodsFor: 'initialize-release'!
initialize
	"Set up the default state of the receiver"
	^self! !


!DisplayModel methodsFor: 'accessing'!
container
	"Answer the receiver's 'container'."

	^container! !

!DisplayModel methodsFor: 'accessing'!
container: anObject
	"Set the receiver's instance variable 'container' to be anObject."

	container := anObject! !

!DisplayModel methodsFor: 'accessing'!
model
	"Answer the receiver's 'model'."

	^model! !

!DisplayModel methodsFor: 'accessing'!
model: anObject
	"Set the receiver's instance variable 'model' to be anObject."

	model := anObject! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayModel class
	instanceVariableNames: ''!

!DisplayModel class methodsFor: 'documentation'!
KnownBugs
	"This version is the first release of the MODE on Squeak. 
	Many features were left out in the interest of simplicity. If you want a really sophisticated system, port HotDraw to Squeak.
	Arcs and Ellipses are rather broken because of the weakness of the Squeak (Smalltalk-80) Paths. 
	Splines are black-and-white because I use Form dotOfSize: to draw with.
	There are also some problems with color BitBlt that I haven't fixed yet. (Any help would be most appreciated.)"
	self flag: #Documentation! !

!DisplayModel class methodsFor: 'documentation'!
README
	"This is the Squeak MODE DisplayList Framework. It is written to support the MODE's music editing tools, and is not the world's most comprehensive and elegant structured graphics hierarchy. It is, however, one of the simplest and most easily extensible.
	One could easily add support for regular polygons, better arcs, more kinds of paths, etc.
	Note also that the display item hierarchy is separate from the display list editing framework.
The class DisplayList has loads of example methods; try,
	DisplayList testAll
for a good sampler"
	self flag: #Documentation! !

!DisplayModel class methodsFor: 'documentation'!
VersionHistory
	"This implementation is MODE 3.0_alpha for Squeak.
	Version History: 
		PCS/Cadmus, Munich,  Spring, 1985 (DoubleTalk);
		CCRMA/PARC, Palo Alto, Late 1987 (HyperScore ToolKit);
		STEIM, Amsterdam, May/June 1990 (MODE 1); 
		Lagoon, Palo Alto, July, 1991-May, 1992 (MODE 1.1);
		SICS, Stockholm, Summer, 1993 (MODE 2);
		CREATE, Santa Barbara, November, 1996 (MODE 3).
The entirety of this software is Copyright (c) 1985-96, Stephen Travis Pope. 
Distribution rights granted according to the Squeak license."
	self flag: #Documentation! !


DisplayModel subclass: #DisplayItem
	instanceVariableNames: 'origin corner color '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayItem comment:
'Instances of the subclasses of the abstract class DisplayItem are used as the items in display lists. They can generally display themselves on display media.
Instance Variables:
	origin	<Point> the object''s origin relative to its container (display list)
	corner	<Point> the object''s corner
	color	<Color> the object''s display color (or nil)
The subclasses add special display-related state and behavior such as an extent point and display method, or a visual item such as a string or image to display.
They generally implement displayOn: aGraphicsContext and bounds accessing methods.
'!

!DisplayItem methodsFor: 'initialize-release'!
initialize
	"Initialize the instance variables of the receiver."
	origin := 0@0! !


!DisplayItem methodsFor: 'accessing'!
bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y! !

!DisplayItem methodsFor: 'accessing'!
bottomLeft: aPoint
	"Set the point at the left edge of the bottom horizontal line of the receiver."
	origin x: aPoint x.
	corner y: aPoint y! !

!DisplayItem methodsFor: 'accessing'!
color
	"Answer the receiver's display color."
	^color! !

!DisplayItem methodsFor: 'accessing'!
color: aVal
	"Set the receiver's display color to the argument."
	color := ((aVal isMemberOf: BlockContext)
		ifTrue: [aVal value]
		ifFalse: [aVal])! !

!DisplayItem methodsFor: 'accessing'!
corner
	"Answer the receiver's display corner."
	^corner! !

!DisplayItem methodsFor: 'accessing'!
corner: aVal
	"Set the receiver's display corner to the argument."
	corner := ((aVal isMemberOf: BlockContext)
		ifTrue: [aVal value]
		ifFalse: [aVal])! !

!DisplayItem methodsFor: 'accessing'!
extent
	"Answer the receiver's extent."
	^corner - origin! !

!DisplayItem methodsFor: 'accessing'!
extent: aPoint
	"Set the extent of the receiver."
	corner := origin + aPoint! !

!DisplayItem methodsFor: 'accessing'!
offset: aPoint
	"Set the origin of the receiver."
	origin := aPoint! !

!DisplayItem methodsFor: 'accessing'!
operatorForCorner: aPoint
	"Answer the message to send to change the given corner point."
	self subclassResponsibility! !

!DisplayItem methodsFor: 'accessing'!
origin
	"Answer the receiver's display origin."
	^origin! !

!DisplayItem methodsFor: 'accessing'!
origin: aPoint
	"Set the origin of the receiver."
	origin := aPoint! !

!DisplayItem methodsFor: 'accessing'!
selectionHandles
	"Answer the points at which to display selection handles when the receiver is selected."
	^self boundingBox corners! !

!DisplayItem methodsFor: 'accessing'!
topRight
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y! !

!DisplayItem methodsFor: 'accessing'!
topRight: aPoint
	"Set the point at the top right corner of the receiver's top horizontal line."
	corner x: aPoint x.
	origin y: aPoint y! !


!DisplayItem methodsFor: 'bounds accessing'!
boundingBox
	"Answer the receiver's actual bounding rectangle."
	| x1 y1 x2 y2 |
	x1 := origin x.
	y1 := origin y.
	x2 := corner x.
	y2 := corner y.
	^Rectangle origin: ((x1 min: x2)@(y1 min: y2)) corner: ((x1 max: x2)@(y1 max: y2))! !


!DisplayItem methodsFor: 'copying'!
copy
	"Answer a shallow copy of the receiver."
	^self shallowCopy! !


!DisplayItem methodsFor: 'testing'!
isDisplayItem
	"Answer whether the receiver is a kind of DisplayItem"
	^true! !

!DisplayItem methodsFor: 'testing'!
isHit: aPoint within: distance
	"Answer whether the given point is a hit on the receiver to within the given distance."
	^(self boundingBox expandBy: distance) containsPoint: aPoint! !


!DisplayItem methodsFor: 'transforming'!
scaledBy: aPoint
	"Scale the receiver's offset and extent by the argument; answer a copy."
	| new |
	new := self copy.
	new origin: self origin * aPoint.
	self corner == nil
		ifFalse: [new corner: self corner * aPoint].
	^new! !

!DisplayItem methodsFor: 'transforming'!
translateBy: aPoint
	"Translate the receiver's origin and corner by the argument."
	self origin: origin + aPoint; corner: corner + aPoint! !


!DisplayItem methodsFor: 'displaying'!
displayOn: aDisplayMedium
	"Display the receiver on the argument."
	self displayOn: aDisplayMedium at: 0@0! !

!DisplayItem methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver on the given medium with the given offset."
	self subclassResponsibility! !


!DisplayItem methodsFor: 'drawing' stamp: 'stp 07/02/97 10:58'!
drawOn: aCanvas
	"Display the receiver on the given canvas."

	self drawOn: aCanvas at: 0@0! !

!DisplayItem methodsFor: 'drawing' stamp: 'stp 07/02/97 10:58'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	self subclassResponsibility! !


!DisplayItem methodsFor: 'printing'!
printCompleteOn: aStream depth: depth
	"Print the receiver in the given Stream."
	aStream cr.
	depth timesRepeat: [aStream tab].
	self printOn: aStream! !

!DisplayItem methodsFor: 'printing'!
printInstVarsOn: aStream
	"Print the receiver's relevant InstVars on the given Stream."
	model isNil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
	origin isNil ifFalse: [aStream nextPutAll: ' origin: '. origin printOn: aStream].
! !

!DisplayItem methodsFor: 'printing'!
printOn: aStream
	"Print the receiver on the given Stream."
	aStream nextPutAll: ' (a ', self class name. 
	self printInstVarsOn: aStream.
	aStream nextPut: $).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayItem class
	instanceVariableNames: ''!

!DisplayItem class methodsFor: 'instance creation'!
model: aM origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."
	^self new model: aM; origin: aPt! !

!DisplayItem class methodsFor: 'instance creation'!
model: aM origin: aPt corner: cPt color: color
	"Answer an instance of DisplayItem with the given instance variables."
	^self new model: aM; origin: aPt; corner: cPt; color: color! !

!DisplayItem class methodsFor: 'instance creation'!
origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."
	^self new origin: aPt! !

!DisplayItem class methodsFor: 'instance creation'!
origin: aPt corner: cPt
	"Answer an instance of DisplayItem with the given instance variables."
	^self new origin: aPt; corner: cPt! !

!DisplayItem class methodsFor: 'instance creation'!
origin: aPt corner: cPt color: color
	"Answer an instance of DisplayItem with the given instance variables."
	^self new origin: aPt; corner: cPt; color: color! !

!DisplayItem class methodsFor: 'instance creation'!
origin: aPt extent: ePt
	"Answer an instance of DisplayItem with the given instance variables."
	^self new origin: aPt; extent: ePt! !

!DisplayItem class methodsFor: 'instance creation'!
origin: aPt extent: ePt color: color
	"Answer an instance of DisplayItem with the given instance variables."
	^self new origin: aPt; extent: ePt; color: color! !


DisplayItem subclass: #DisplayLine
	instanceVariableNames: 'width '
	classVariableNames: 'LineBlt '
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayLine comment:
'Instances of DisplayLine are used for visual lines in display lists.
Instance Variables:
	width		<Number> the object''s line width
Class Variables
	LineBlt		<BitBlt> cached BitBlt for faster display'!

!DisplayLine methodsFor: 'accessing'!
corner
	"Answer the corner of the receiver."
	^corner! !

!DisplayLine methodsFor: 'accessing'!
corner: aPoint
	"Set the corner of the receiver."
	corner := aPoint! !

!DisplayLine methodsFor: 'accessing'!
operatorForCorner: aPoint
	"Answer the message to send to change the given corner point."
	^(aPoint = origin)
		ifTrue: [#origin:]
		ifFalse: [#corner:]! !

!DisplayLine methodsFor: 'accessing'!
selectionHandles
	"Answer the points at which to display selection handles when the receiver is selected."
	^Array with: origin with: corner! !

!DisplayLine methodsFor: 'accessing'!
slope
	"Answer the first derivative of the receiver (in screen orientation--y running negative)."
	^(corner x = origin x)
		ifFalse: [(corner y - origin y) / (corner x - origin x)]
		ifTrue: [(origin y - corner y) / (corner x - origin x + 1)]! !

!DisplayLine methodsFor: 'accessing'!
width: aVal
	width := aVal! !


!DisplayLine methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display a line between startPoint and endPoint."
	"(DisplayList linesX: 600 byY: 400) display"
"	Transcript show: start printString, '	', end printString; cr."
	color == nil
		ifTrue: [LineBlt fillColor: Color black]
		ifFalse: [LineBlt fillColor: color].
	width == nil
		ifFalse: [LineBlt width: width; height: width].
	LineBlt setDestForm: aDisplayMedium.
	LineBlt drawFrom: (origin + aPoint) to: (corner + aPoint)! !


!DisplayLine methodsFor: 'drawing' stamp: 'stp 07/02/97 11:01'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	aCanvas line: (origin + aPoint) to: (corner + aPoint) 
		width: width color: color
! !


!DisplayLine methodsFor: 'testing'!
isHit0: aPoint within: distance
	"Answer whether the given point is a hit on the receiver to within the given distance."
	| expect |
"	(super isHit: aPoint within: distance)
		ifFalse: [^false].
"	expect := aPoint x - origin x * self slope + origin y.		"expected y value"
	^(expect - aPoint y) abs <= distance
! !

!DisplayLine methodsFor: 'testing'!
isHit: aPoint within: distance
	"Answer whether the given point is a hit on the receiver to within the given distance."
	| expect |
	(super isHit: aPoint within: distance)
		ifFalse: [^false].
	expect := aPoint x - origin x * self slope + origin y.		"expected y value"
	^(expect - aPoint y) abs <= distance
! !


!DisplayLine methodsFor: 'printing'!
printInstVarsOn: aStream
	"Print the receiver's relevant instance variables on the given Stream."
	super printInstVarsOn: aStream.
	corner isNil ifFalse: [aStream nextPutAll: ' corner: '. corner printOn: aStream].
	color isNil ifFalse: [aStream nextPutAll: ' color: '. color printOn: aStream].
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayLine class
	instanceVariableNames: ''!

!DisplayLine class methodsFor: 'instance creation'!
origin: oPoint corner: ePoint	
	"Answer an initialized instance."
	^self new origin: oPoint; corner: ePoint! !

!DisplayLine class methodsFor: 'instance creation'!
origin: oPoint corner: ePoint	 color: color width: width
	"Answer an initialized instance."
	^self new origin: oPoint; corner: ePoint; color: color; width: width! !

!DisplayLine class methodsFor: 'instance creation'!
origin: oPoint extent: ePoint	
	"Answer an initialized instance."
	^self new origin: oPoint; extent: ePoint! !

!DisplayLine class methodsFor: 'instance creation'!
origin: oPoint extent: ePoint	 color: color width: width
	"Answer an initialized instance."
	^self new origin: oPoint; extent: ePoint; color: color; width: width! !


!DisplayLine class methodsFor: 'class initialization'!
initialize
	"DisplayLine initialize"
	LineBlt := BitBlt
		destForm: Display
		sourceForm: nil
		fillColor: nil
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: Display boundingBox.

! !


!DisplayLine class methodsFor: 'class var accessing'!
lineBlt
	"Answer the cached BitBlt used for drawing lines."
	^LineBlt! !


!DisplayLine class methodsFor: 'examples'!
lineExample
	"DisplayLine lineExample"
	Display fillWhite.
	(DisplayLine linesX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayLine class methodsFor: 'examples'!
linesX: x byY: y
	"Answer a display list with randomly-placed random-color lines over the given extent."
	"(DisplayLine linesX: Display width byY: Display height) display"
	^self linesX: x byY: y items: 60! !

!DisplayLine class methodsFor: 'examples'!
linesX: x byY: y items: items
	"Answer a display list with randomly-placed random-color lines over the given extent."
	"(DisplayLine linesX: Display width byY: Display height) display"
	| dList rand inset origin extent |
	inset := 32.
	dList := DisplayList new.
	rand := Random new.
	items timesRepeat:
		[origin := (rand nextIntFrom: inset to: x-inset) @ (rand nextIntFrom: inset to: y-inset).
		extent := (rand nextIntFrom: -128 to: 128) @ (rand nextIntFrom: -128 to: 128).
		dList add: (DisplayLine origin: origin extent: extent 
						color: Color random width: (rand nextIntFrom: 1 to: 6))].
	^dList! !


DisplayLine initialize!


DisplayModel subclass: #DisplayList
	instanceVariableNames: 'components origin bounds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayList comment:
'Instances of DisplayList are used for representing composed structured graphics. They can be nested (see the class examples) and can display themselves and their components.

Instance Variables:
	components	<OrderedCollection of DisplayModels> the list''s display items
	offset		<Point> the object''s offset distance relative to its container
	bounds		<Rectangle> the list''s bounding box'!

!DisplayList methodsFor: 'initialize-release'!
initialize
	"Set up the default state of the receiver"
	super initialize.
	components := OrderedCollection new.
	origin := 0@0! !

!DisplayList methodsFor: 'initialize-release'!
release
	"Break the model dependency links of all the receiver's display items."
	super release.
	components do: [ :item | item release]! !


!DisplayList methodsFor: 'accessing'!
flatten
	"Answer a copy of the receiver with its hierarchy flattened."
	| new subnew |
	new := DisplayList new.
	components do:
		[ :item |
		item hasItems
			ifTrue: [subnew := item flatten components.
				subnew do: [ :subitem |
					new add: (subitem translateBy: item origin)]]
			ifFalse: [new add: item]].
	^new! !

!DisplayList methodsFor: 'accessing'!
nodeAt: aPath
	"Answer the element described by the collection of items in the argument."
	| next path2 |
	components isEmpty ifTrue: [^nil].
	next := components
			detect: [ :i | 
					i "component" model == aPath first]
			ifNone: [nil].
	next == nil
		ifFalse: [aPath size = 1
			ifTrue: [^next]
			ifFalse: [path2 := aPath copy.
				path2 removeFirst.
				^next nodeAt: path2]]
		ifTrue: [^nil]! !

!DisplayList methodsFor: 'accessing'!
nodeFor: aModel
	"Answer the element whose model is the argument."
	components isEmpty ifTrue: [^nil].
	^components
			detect: [ :i | i model == aModel]
			ifNone: [nil].! !

!DisplayList methodsFor: 'accessing'!
origin
	^origin! !

!DisplayList methodsFor: 'accessing'!
origin: aPoint
	"Set the origin of the receiver."
	origin := aPoint! !

!DisplayList methodsFor: 'accessing'!
selectionHandles
	"Answer the points at which to display selection handles when the receiver is selected."
	^self boundingBox corners! !


!DisplayList methodsFor: 'bounds accessing'!
boundingBox
	"Answer the receiver's bounds."
	bounds isNil
		ifTrue: [self computeBounds].
	^bounds! !

!DisplayList methodsFor: 'bounds accessing'!
bounds: aRectangle
	"Set the receiver's bounds to the argument."
	bounds _ aRectangle! !

!DisplayList methodsFor: 'bounds accessing'!
computeBounds
	"Compute the receiver's bounds"
	components isEmpty
		ifTrue: [bounds := (0@0) extent: (0@0)]
		ifFalse: [bounds := components first boundingBox].
	1 to: components size do:
		[:i |
		bounds := bounds merge: (components at: i) boundingBox].
	bounds origin < (0@0)
		ifTrue: [bounds := bounds translateBy: bounds origin negated].
	^origin == nil
		ifTrue: [bounds "expandedBy: (64@64)"]
		ifFalse: [(bounds translateBy: origin) "expandedBy: (64@64)"]! !

!DisplayList methodsFor: 'bounds accessing'!
extent
	"Answer the extent of the receiver."
	^self boundingBox extent! !


!DisplayList methodsFor: 'adding-removing'!
add: aVisualComponent
	"Add the argument to the receiver."
	^self addComponent: aVisualComponent! !

!DisplayList methodsFor: 'adding-removing'!
add: aVisual at: aPoint
	"Add aVisual to the receiver's components with its offset set to aPoint."
	aVisual origin: aPoint.
	self add: aVisual! !

!DisplayList methodsFor: 'adding-removing'!
add: anItem atPath: aPath
	"Add the first argument at the node described by the second."
	| node |
	(aPath == nil or: [aPath isEmpty])
		ifTrue: [^self add: anItem].
	node := self nodeAt: aPath.
	node == nil
		ifTrue: [self addComponent: anItem]
		ifFalse: [node addComponent: anItem]! !

!DisplayList methodsFor: 'adding-removing'!
addComponent: aVisual
	"Add the argument to the receiver."
	components addLast: aVisual.
	aVisual container: self! !


!DisplayList methodsFor: 'hit detection'!
itemsFromX: x1 toX: x2
	"Answer the list of items whose origins are within the given X range."
	| dList tList |
	dList := DisplayList new.
	components do: 
		[ :i |
		i hasItems
			ifTrue:
				[tList := i itemsFromX: x1 + i origin x toX: x2 + i origin x.
				tList isEmpty ifFalse: [dList add: tList at: i origin]]
			ifFalse:
				[(i origin x between: x1 and: x2) ifTrue: [dList add: i]]].
	^dList! !

!DisplayList methodsFor: 'hit detection'!
itemsIntersecting: rect
	"Answer the list of items whose boundingBoxes intersect the given rectangle. 
	Clip them to the box's border if necessary."
	| dList dList2 |
	dList := DisplayList new.
	components do: 
		[ :i |
		i hasItems
			ifTrue: [dList2 := i itemsIntersecting: (rect translateBy: self origin).
				dList2 == nil
					ifFalse: [dList add: (dList2 origin: i origin)]]
			ifFalse: [(((i boundingBox) translateBy: self origin) 
								intersects: rect)
					ifTrue: [dList add: (i clipTo: (rect translateBy: ((0@0) - self origin)))]]].
	^(dList components isEmpty)
		ifTrue: [nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [dList components first]
			ifFalse: [dList]]! !

!DisplayList methodsFor: 'hit detection'!
itemsWithin: rect
	"Answer the list of items whose boundingBoxes are entirely within the given rectangle."
	| dList movedRect items first |
	movedRect := rect moveBy: self origin negated.
	dList := DisplayList new.
	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [items := comp itemsWithin: movedRect.
				items == nil ifFalse: [dList add: items]]
			ifFalse: [(movedRect contains: comp boundingBox) 
					ifTrue: [dList add: comp]]].
	(dList components isEmpty)
		ifTrue: [^nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [first := dList components first.
				first hasItems
					ifTrue: [first container: nil].
				^first copy translateBy: self origin]
			ifFalse: [^dList translateBy: self origin]]! !

!DisplayList methodsFor: 'hit detection'!
itemsWithin: dist of: point
	"Answer the list of items whose boundingBoxes are entirely within the given rectangle."
	| dList |
	dList := DisplayList new.
	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [dList add: 
					(comp itemsWithin: dist of: (point - self origin))]
			ifFalse: [((comp boundingBox expandBy: dist) containsPoint: point)
					ifTrue: [dList add: comp]]].
	^(dList components isEmpty)
		ifTrue: [nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [dList components first]
			ifFalse: [dList]]! !

!DisplayList methodsFor: 'hit detection'!
itemWithin: dist of: point
	"Answer the list of items that think the given point and tolerance qualifies as a 'hit.'"
	| item |
	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [(item := comp itemWithin: dist of: (point - self origin)) == nil
					ifFalse: [^item]]
			ifFalse: [(comp isHit: point within: dist) ifTrue: [^comp]]].
	^nil! !

!DisplayList methodsFor: 'hit detection'!
modelsFromX: x1 toX: x2
	"Answer the list of model-space-items whose origins are within the given X range."
	| dList |
	dList := DisplayList new.
	components do: 
		[ :i |
		((i origin x >= x1) and: [i origin x <= x2])
				ifTrue: [dList add: i]].
	^dList collect: [ :i | i model]! !


!DisplayList methodsFor: 'transforming'!
scaledBy: aPoint
	"Scale the receiver's sizes by the argument; answer a new display list."
	| dl |
	dl := self copy.
	components do:
		[ :item |
		dl add: (item scaledBy: aPoint)].
	^dl! !

!DisplayList methodsFor: 'transforming'!
translateBy: aPoint
	"Translate the receiver's origin by the argument."
	self origin: self origin + aPoint! !


!DisplayList methodsFor: 'enumerating'!
do: aBlock
	"Iterate the argument block over the receiver's components."
	components do: aBlock! !


!DisplayList methodsFor: 'testing'!
hasItems
	"Answer whether or not the receiver has items or components."
	^true! !

!DisplayList methodsFor: 'testing'!
isEmpty
	"Amswer whether the receiver has any items."
	^components isEmpty! !


!DisplayList methodsFor: 'displaying'!
display
	"Open a DisplayListView on the receiver."
"	^DisplayListView onListWithoutFrills: self"
	self displayOn: Display! !

!DisplayList methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display each of the receiver's components."
	"(DisplayList rectanglesX: 400 byY: 400) displayOn: Display"
	1 to: components size do:
		[:i | 
		(components at: i) displayOn: aDisplayMedium at: origin + aPoint].
! !

!DisplayList methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm
	"Display each of the receiver's components."
	"(DisplayList rectanglesX: 400 byY: 400) displayOn: Display"
	| item rect |
	rect := 0@0 extent: clipRectangle extent.
	1 to: components size do:
		[:i | 
		item := components at: i.
		(item boundingBox intersects: rect)
			ifTrue: [item displayOn: aDisplayMedium at: origin + aPoint]]
! !


!DisplayList methodsFor: 'printing'!
printCompleteOn: aStream depth: depth
	"Descend the hierarchy printing on the stream."
	aStream cr.
	depth timesRepeat: [aStream tab].
	aStream nextPutAll: '(a DisplayList '.
	origin == nil
		ifFalse: [aStream nextPutAll: ' origin: ', origin printString].
"	model == nil
		ifFalse: [aStream nextPutAll: ' model: ', model printString]."
	components == nil
		ifFalse: [components do: 
			[ :e | 
			e printCompleteOn: aStream depth: depth + 1]].
	aStream nextPut: $)! !

!DisplayList methodsFor: 'printing'!
printOn: aStream
	"Print the receiver on the argument using the recursive method."
	^Sensor leftShiftDown 
		ifTrue: [super printOn: aStream]
		ifFalse: [self printCompleteOn: aStream depth: 0]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayList class
	instanceVariableNames: ''!

!DisplayList class methodsFor: 'instance creation'!
new
	^super new initialize! !


!DisplayList class methodsFor: 'examples'!
exampleHierarchical
	"Create and answer a large display list with a lines, strings, and visuals as sub-lists in different regions."
	"(DisplayList exampleHierarchical) display"
	| dl1 dl2 dl3 dl4 dl5 dl6 rand size pt x y w h |
	size := 100.
	dl1 := DisplayList new.
	dl2 := DisplayList new.
	dl3 := DisplayList new.
	dl4 := DisplayList new.
	dl5 := DisplayList new.
	dl6 := DisplayList new.
	rand := Random new.
	10 timesRepeat:
		[pt := (rand next * size) rounded @ ((rand next * size) rounded).
		dl1 add: ((DisplayString string: (pt printString) 
				origin: pt) color: (Color random))].
	10 timesRepeat:
		[x := (rand next * size) rounded.		y := (rand next * size) rounded.
		w := (rand next * size) rounded.		h :=  (rand next * size) rounded.
		dl2 add: ((DisplayRectangle origin: (x@y) extent: (w@h))
				color: (Color random); fill: true)].
	10 timesRepeat:
		[x := (rand next * size) rounded.		y := (rand next * size) rounded.
		w := (rand next * size) rounded.		h :=  (rand next * size) rounded.
		dl3 add: ((DisplayRectangle origin: (x@y) extent: (w@h))
				color: (Color random); fill: true)].
	10 timesRepeat:
		[pt := (rand next * size) rounded @ ((rand next * size) rounded).
		dl4 add: ((DisplayString string: (pt printString) 
				origin: pt) color: (Color random))].
	dl5 add: dl1; 
		add: dl2 at: (250@0); 
		add: dl3 at: (0@250); 
		add: dl4 at: (250@250).
	dl6 add: dl5 at: (50@50).
	^dl6! !

!DisplayList class methodsFor: 'examples'!
gridExample
	"Create and answer a large display list with lines and strings."
	"(DisplayList gridExample) display"
	"DisplayListView example"
	| dList rand ext step pt |
	ext := 5000.
	step := 50.
	dList := DisplayList new.
	0 to: (ext/step) do:
		[ :i |
		dList add: (DisplayLine origin: (i*step)@0 extent: 0@ext).
		dList add: (DisplayLine origin: 0@(i*step) extent: ext@0)].
	rand := Random new.
	1 to: 1000 do:
		[ :i |
		pt := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: (DisplayString string: (pt printString) origin: pt)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
gridExampleX: x byY: y
	"Create and answer a large display list with lines and strings."
	"(DisplayList gridExampleX: 3000 byY: 500) display"
	| dList rand step pt |
	step := 50.
	dList := DisplayList new.
	0 to: (x/step) do:
		[ :i |
		dList add: (DisplayLine origin: (i*step)@0 extent: 0@y)].
	0 to: (y/step) do:
		[ :i |
		dList add: (DisplayLine origin: 0@(i*step) extent: x@0)].
	rand := Random new.
	1 to: (x * y / 20000) do:
		[ :i |
		pt := (rand next * x) rounded @ ((rand next * y) rounded).
		dList add: (DisplayString string: (pt printString) origin: pt)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
randomExample
	"Create and answer a large display list with a lines, strings, and visuals."
	"(DisplayList randomExample) display"
	| dList rand ext size pt pt2 x y w h coll |
	ext := 1500.
	size := ext / 2.
	dList := DisplayList new.
	rand := Random new.
	20 timesRepeat:
		[x := (rand next * size) rounded.
		y := (rand next * size) rounded.
		w := (rand next * size/10) rounded.
		h :=  (rand next * size/10) rounded.
		dList add: ((DisplayRectangle
				origin: (x@y)
				extent: (w@h))
				color: Color random; fill: true)].
	20 timesRepeat:
		[pt := (rand next * size) rounded @ ((rand next * size) rounded).
		pt2 := (rand next * size) rounded @ ((rand next * size) rounded).
		dList add: ((DisplayLine origin: pt2 extent: (pt - pt2))
				color: Color random)].
	20 timesRepeat:
		[pt := (rand next * size) rounded @ ((rand next * size) rounded).
		dList add: ((DisplayString string: (pt printString) 
				origin: pt) color: Color random)].
	20 timesRepeat:
		[coll := Cursor allInstances.
		pt := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: ((DisplayVisual visual: (coll atRandom) 
				origin: pt) color: Color random)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
stringExample
	"Open a display list view with randomly-placed random-color strings over the given extent."
	"DisplayString stringExample"
	(DisplayList stringsX: 600 byY: 600) display! !

!DisplayList class methodsFor: 'examples'!
testAll
	"Show all of the display items"
	"DisplayList testAll"
	| delay x y |
	delay := Delay forSeconds: 1.
	x := Display extent x.
	y := Display extent y.
	Display fillWhite.
	'Lines' displayOn: Display at: 10@10.
	(DisplayLine linesX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.
	'Polygons' displayOn: Display at: 10@10.
	(DisplayPolyline polylinesX: x byY: y items: 60) displayOn: Display.
	delay wait.		Display fillWhite.
	'Splines' displayOn: Display at: 10@10.
	(DisplaySpline splinesX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.	
	'Circles' displayOn: Display at: 10@10.
	(DisplayCircle circlesX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.	
	'Rectangles' displayOn: Display at: 10@10.
	(DisplayRectangle rectanglesX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.
	'Strings' displayOn: Display at: 10@10.
	(DisplayString stringsX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.
	'Visuals' displayOn: Display at: 10@10.
	(DisplayVisual visualsX: x byY: y) displayOn: Display.
	delay wait.		Display fillWhite.
	'Hierarchical Lists' displayOn: Display at: 10@10.
	(DisplayList exampleHierarchical) display.
	delay wait.
	ScheduledControllers restore
! !


DisplayLine subclass: #DisplayRectangle
	instanceVariableNames: 'fill border '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayRectangle comment:
'Instances of DisplayRectangle can be used to display bordered or filled rectangles in display lists. This is a subclass of DisplayLine for reasons of practicality. a "purist" (e.g., David Liebs), would create an intermediate abstract class (e.g., BoundedDisplayItem) for both DisplayLine and DisplayRectangle.
Instanve Variables:
	border	<Number> the border width
	fill		<Boolean or nil> whether or not to fill the receiver on display (no by default)'!

!DisplayRectangle methodsFor: 'accessing'!
border
	"Answer the receiver's border color."
	^border! !

!DisplayRectangle methodsFor: 'accessing'!
border: aColor
	"Set the receiver's border color."
	border := aColor! !

!DisplayRectangle methodsFor: 'accessing'!
fill: aBoolean
	"Set the receiver's filling Boolean."
	fill := aBoolean! !

!DisplayRectangle methodsFor: 'accessing'!
operatorForCorner: aPoint
	"Answer the message to send to change the given corner point."
	^(aPoint = origin)
		ifTrue: [#origin:]
		ifFalse: [aPoint = corner
			ifTrue: [#corner:]
				ifFalse: [aPoint = self topRight
					ifTrue: [#topRight:]
					ifFalse: [#bottomLeft:]]]! !

!DisplayRectangle methodsFor: 'accessing'!
selectionHandles
	"Answer the points at which to display selection handles when the receiver is selected."
	^self boundingBox corners! !

!DisplayRectangle methodsFor: 'accessing'!
topRight: aPoint
	"Set the point at the top right corner of the receiver's top horizontal line."
	corner x: aPoint x.
	origin y: aPoint y! !


!DisplayRectangle methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver on the first argument."
	"(DisplayList rectanglesX: 600 byY: 400) display"
"	Transcript show: start printString, '	', end printString; cr."
	(fill == nil or: [fill not])
		ifFalse: [aDisplayMedium fill: (self boundingBox translateBy: aPoint)
					 fillColor: color ]
		ifTrue: [aDisplayMedium border: (self boundingBox translateBy: aPoint)
					width: (width  isNil ifTrue: [1] ifFalse: [width]) fillColor: color ]! !


!DisplayRectangle methodsFor: 'drawing' stamp: 'stp 07/02/97 11:04'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	fill 
		ifTrue: [aCanvas fillRectangle: (self bounds translatedBy: aPoint) color: color]
		ifFalse: [aCanvas frameRectangle: (self bounds translatedBy: aPoint) 
						width: width color: color]! !


!DisplayRectangle methodsFor: 'testing'!
isHit: aPoint within: distance
	"Answer whether the given point is a hit 'on' the receiver to within the given distance."
	^(self boundingBox expandBy: distance) containsPoint: aPoint! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayRectangle class
	instanceVariableNames: ''!

!DisplayRectangle class methodsFor: 'examples'!
rectangleExample
	"DisplayRectangle rectangleExample"
	Display fillWhite.
	(DisplayRectangle rectanglesX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayRectangle class methodsFor: 'examples'!
rectanglesX: x byY: y
	"(DisplayRectangle rectanglesX: 600 byY: 400) display"
	^self rectanglesX: x byY: y items: 60! !

!DisplayRectangle class methodsFor: 'examples'!
rectanglesX: x byY: y items: items
	"Answer a display list with randomly-placed random-color rectangles over the given extent."
	"(DisplayRectangle rectanglesX: 600 byY: 400) display"
	| dList rand x0 y0 x1 y1 inset w h |
	dList := DisplayList new.
	rand := Random new.
	inset := 20.
	x0 := x - (inset * 2).
	y0 := y - (inset * 2).
	items timesRepeat:
		[x1 := inset + (rand next * x0) rounded.
		y1 := inset + (rand next * y0) rounded.
		w := (rand next * 64) rounded.
		h :=  (rand next * 64) rounded.
		dList add: ((DisplayRectangle
				origin: x1@y1
				extent: w@h
				color: Color random)
				fill: (rand next > 0.4); 
				width: (rand nextIntFrom: 0 to: 4))].
	^dList! !


DisplayRectangle subclass: #DisplayArc
	instanceVariableNames: 'angle '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayArc comment:
'Instances of DisplayArc represent elliptical arcs drawn within the rectangles described by their bounding boxes. This is not finished, and does not work at present because I am lazy and use an Arc for display--Arcs only come in full 90-degree quadrants.
Instance Variables
	angle <Interval>	the starting and stopping angles in degrees relative to the positive X axis'!

!DisplayArc methodsFor: 'accessing'!
angle: anInterval
	"Set the receiver's angle to be the argument (start-to-stop in degrees)."
	angle := anInterval! !

!DisplayArc methodsFor: 'accessing'!
selectionHandles
	"Answer the points at which to display selection handles when the receiver is selected."
	^self boundingBox corners! !

!DisplayArc methodsFor: 'accessing'!
start: anInterval
	"Set the starting value of the receiver's angle to be the argument (in degrees)."
	angle start: anInterval! !

!DisplayArc methodsFor: 'accessing'!
stop: anInterval
	"Set the stopping value of the receiver's angle to be the argument (in degrees)."
	angle stop: anInterval! !


!DisplayArc methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver as an arc on the given medium."
	| arc |
	arc := Arc new.
	arc center: self origin.
	arc quadrant: 1.			"KLUDJ"
	arc displayOn: aDisplayMedium at: aPoint! !


!DisplayArc methodsFor: 'drawing' stamp: 'stp 07/02/97 11:08'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	fill 
		ifTrue: [aCanvas fillWedge: (self bounds translatedBy: aPoint) 
				startDegrees: angle start spanning: angle size color: color]
		ifFalse: [aCanvas frameWedge: (self bounds translatedBy: aPoint) 
				startDegrees: angle start spanning: angle size 
				width: width color: color]! !


!DisplayArc methodsFor: 'printing'!
printInstVarsOn: aStream
	"Print the receiver's relevant instance variables on the given Stream."
	super printInstVarsOn: aStream.
	angle isNil ifFalse: [aStream nextPutAll: ' angle: '. angle printOn: aStream].
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayArc class
	instanceVariableNames: ''!


DisplayArc subclass: #DisplayEllipse
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayEllipse comment:
'Instances of DisplayEllipse represent ellipses described by their bounding rectangles. These do not work at present.'!

!DisplayEllipse methodsFor: 'accessing' stamp: 'stp 07/02/97 11:16'!
angle: anInterval
	"Set the receiver's angle to be the argument (start-to-stop in degrees)."

	self shouldNotImplement! !

!DisplayEllipse methodsFor: 'accessing' stamp: 'stp 07/02/97 11:16'!
start: anInterval
	"Set the starting value of the receiver's angle to be the argument (in degrees)."

	self shouldNotImplement! !

!DisplayEllipse methodsFor: 'accessing' stamp: 'stp 07/02/97 11:16'!
stop: anInterval
	"Set the stop value of the receiver's angle to be the argument (in degrees)."

	self shouldNotImplement! !


!DisplayEllipse methodsFor: 'drawing' stamp: 'stp 07/02/97 11:15'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	| borderColor |
	borderColor := Color random.
	fill 
		ifTrue: [aCanvas fillOval: (self bounds translatedBy: aPoint) 
				color: color borderWidth: border borderColor: borderColor]
		ifFalse: [aCanvas frameOval: (self bounds translatedBy: aPoint) 
				width: width color: color]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayEllipse class
	instanceVariableNames: ''!


DisplayEllipse subclass: #DisplayCircle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayCircle comment:
'Instances of DisplayCircle represent circles within display lists.'!

!DisplayCircle methodsFor: 'drawing' stamp: 'stp 07/02/97 11:17'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	| borderColor |
	borderColor := Color random.
	fill 
		ifTrue: [aCanvas fillOval: (self bounds translatedBy: aPoint) 
				color: color borderWidth: border borderColor: borderColor]
		ifFalse: [aCanvas frameOval: (self bounds translatedBy: aPoint) 
				width: width color: color]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayCircle class
	instanceVariableNames: ''!

!DisplayCircle class methodsFor: 'examples'!
circleExample
	"DisplayCircle circleExample"
	Display fillWhite.
	(DisplayCircle circlesX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayCircle class methodsFor: 'examples'!
circlesX: x byY: y
	"Answer a display list with randomly-placed random-color circles over the given extent."
	"(DisplayCircle circlesX: 600 byY: 400) display"
	^self circlesX: x byY: y items: 40! !

!DisplayCircle class methodsFor: 'examples'!
circlesX: x byY: y items: items
	"Answer a display list with randomly-placed random-color circles over the given extent."
	"(DisplayList circlesX: 600 byY: 400) display"
	| dList rand inset origin radius |
	inset := 64.
	dList := DisplayList new.
	rand := Random new.
	items timesRepeat:
		[origin := (rand nextIntFrom: inset to: x-inset)
					@(rand nextIntFrom: inset to: y-inset).
		radius := rand nextIntFrom: 0 to: 64.
		dList add: (DisplayCircle
				center: origin
				radius: radius
				color: Color random)].
	^dList! !


!DisplayCircle class methodsFor: 'instance creation'!
center: aPt radius: rad color: color
	"Answer an instance of DisplayCircle with the given instance variables."
	^self new origin: aPt - rad; corner: aPt + rad; color: color! !


DisplayRectangle subclass: #DisplayPolyline
	instanceVariableNames: 'vertices '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayPolyline comment:
'Aninstance of DisplayPolyline can represent an open or closed polygon.
Instance Variables:
	vertices	<Array of Point> the polygon''s vertices'!

!DisplayPolyline methodsFor: 'accessing'!
boundingBox
	"Answer the receiver's bounds." 
	| ext |
	ext := 0@0.
	vertices do: [ :pt | ext := ext max: pt].
	^origin extent: ext! !

!DisplayPolyline methodsFor: 'accessing'!
vertices: pointArray
	"Set the receiver's vertices."
	vertices := pointArray asArray! !


!DisplayPolyline methodsFor: 'transforming'!
scaledBy: aPoint
	"Scale the receiver's origin and extent by the argument; answer a copy."
	| new |
	new := self copy.
	self origin == nil
		ifFalse: [new origin: self origin * aPoint].
	new vertices: (vertices collect: [ :vPt | vPt * aPoint]).
	^new! !


!DisplayPolyline methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Draw the receiver's edges on the supplied DisplayMedium."
	| src dest tColor intPt |
	tColor := color isNil
		ifTrue: [Color black]
		ifFalse: [color].
	LineBlt fillColor: tColor.
	width isNil
		ifFalse: [LineBlt width: width; height: width].
	LineBlt setDestForm: aDisplayMedium.
	src := vertices first + origin + aPoint.
	2 to: vertices size do:
		[ :index |
		dest := (vertices at: index) + origin + aPoint.
		LineBlt drawFrom: src to: dest.
		src := dest].
	(fill isNil or: [fill not])
		ifFalse: [intPt := vertices inject: 0@0 into: [ :sum :item | sum + item].
			intPt := intPt / vertices size.  "The average of the vertices is inside."
			aDisplayMedium shapeFill: tColor interiorPoint: intPt + origin + aPoint].
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayPolyline class
	instanceVariableNames: ''!

!DisplayPolyline class methodsFor: 'instance creation'!
origin: offPt vertices: arrayOfPoints
	"Answer a new DisplayPolyline with the arguments as its vertices."
	^self basicNew origin: offPt; vertices: arrayOfPoints! !

!DisplayPolyline class methodsFor: 'instance creation'!
vertices: arrayOfPoints
	"Answer a new DisplayPolyline with the arguments as its vertices."
	^self basicNew vertices: arrayOfPoints! !


!DisplayPolyline class methodsFor: 'examples'!
polylineExample
	"DisplayPolyline polylineExample"
	Display fillWhite.
	(DisplayPolyline polylinesX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayPolyline class methodsFor: 'examples'!
polylinesX: x byY: y
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"(DisplayPolyline polylinesX: 600 byY: 400) display"
	^self polylinesX: x byY: y items: 40! !

!DisplayPolyline class methodsFor: 'examples'!
polylinesX: x byY: y items: num
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"(DisplayPolyline polylinesX: 600 byY: 600 items: 1000) display"
	| dList rand inset scale x0 y0 size pointArray |
	inset := 16.
	scale := 128.
	dList := DisplayList new.
	rand := Random new.
	x0 := rand nextIntFrom: inset to: x-inset.
	y0 := rand nextIntFrom: inset to: y-inset.
	num timesRepeat:
		[size := rand nextIntFrom: 3 to: 6.
		pointArray := Array new: size.
		pointArray at: 1 put: (0@0).
		pointArray at: size put: (0@0).
		x0 := y0 := 0.
		2 to: size-1 do: 
			[ :i |
			x0 := x0 + (rand nextIntFrom: scale negated to: scale).
			y0 := y0 + (rand nextIntFrom: scale negated to: scale).
			pointArray at: i put: (x0 @ y0)].
		dList add: ((DisplayPolyline
				origin: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				color: Color random;
				width: (rand nextIntFrom: 1 to: 4);
				vertices: pointArray ";
				fill: (rand next > 0.8)")].
	^dList! !


DisplayPolyline subclass: #DisplaySpline
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplaySpline comment:
'An instance of DisplaySpline acts like a Spline and can be used as a DisplayItem.'!

!DisplaySpline methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Stroke the receiver's edges on the supplied DisplayMedium."
	| spline |
	spline := Spline new.
	vertices do: [ :vertex | spline add: vertex].
	spline computeCurve.
	width isNil
		ifFalse: [width ~= 1
			ifTrue: [spline form: (Form dotOfSize: width)]].	"KLUDJ"
	spline displayOn: aDisplayMedium at: aPoint + origin
		clippingBox: aDisplayMedium boundingBox rule: Form over
		fillColor: color! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplaySpline class
	instanceVariableNames: ''!

!DisplaySpline class methodsFor: 'examples'!
splineExample
	"DisplaySpline splineExample"
	Display fillWhite.
	(DisplaySpline splinesX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplaySpline class methodsFor: 'examples'!
splinesX: x byY: y
	"Answer a display list with randomly-placed random-color splines over the given extent."
	"(DisplaySpline splinesX: 600 byY: 400) display"
	^self splinesX: x byY: y items: 40! !

!DisplaySpline class methodsFor: 'examples'!
splinesX: x byY: y items: items
	"Answer a display list with randomly-placed random-color splines over the given extent."
	"(DisplaySpline splinesX: 600 byY: 600) display"
	| dList rand inset scale x0 y0 size pointArray |
	inset := 128.
	scale := 128.
	dList := DisplayList new.
	rand := Random new.
	x0 := rand nextIntFrom: inset to: x-inset.
	y0 := rand nextIntFrom: inset to: y-inset.
	items timesRepeat:
		[size := rand nextIntFrom: 7 to: 12.
		pointArray := Array new: size.
		pointArray at: 1 put: (0@0).
		pointArray at: size put: (0@0).
		x0 := y0 := 0.
		2 to: size-1 do: [ :i |
			x0 := x0 + (rand nextIntFrom: scale negated to: scale).
			y0 := y0 + (rand nextIntFrom: scale negated to: scale).
			pointArray at: i put: (x0 @ y0)].
		dList add: ((DisplaySpline
				origin: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				color: Color random;
				width: (rand nextIntFrom: 1 to: 4);
				vertices: pointArray)].
	^dList! !


DisplayItem subclass: #DisplayString
	instanceVariableNames: 'dText '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayString comment:
'Instances of DisplayString are used for visual text items in display lists.
Instance Variables:
	dText	<DisplayText> the object''s string/text/style'!

!DisplayString methodsFor: 'initialize-release'!
initialize
	"Initialize the instance variables of the receiver."
	super initialize.
	dText := DisplayText new textStyle: TextStyle default; offset: 0@0! !


!DisplayString methodsFor: 'accessing'!
extent
	"Answer a rectangle that circumscribes the receiver."
	^self boundingBox extent! !

!DisplayString methodsFor: 'accessing'!
font
	"Answer the receiver's font."
	^dText textStyle! !

!DisplayString methodsFor: 'accessing'!
font: aFont
	"Set the receiver's font."
	dText textStyle: aFont! !

!DisplayString methodsFor: 'accessing'!
string
	"Answer the receiver's string."
	^dText text string! !

!DisplayString methodsFor: 'accessing'!
string: aString
	"Set the receiver's string."
	dText text: aString asText! !


!DisplayString methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver on the argument."
"	Transcript show: (aGraphicsContext translation  + origin) printString; space."
	dText displayOn: aDisplayMedium at: origin + aPoint! !


!DisplayString methodsFor: 'drawing' stamp: 'stp 07/02/97 11:11'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	aCanvas paragraph: dText bounds: (self bounds translatedBy: aPoint) color: color! !


!DisplayString methodsFor: 'printing'!
printInstVarsOn: aStream
	"Print the receiver's string..."
	super printInstVarsOn: aStream.
	self string printOn: aStream.
! !


!DisplayString methodsFor: 'bounds accessing'!
boundingBox
	"Answer the receiver's actual bounding rectangle."
	^dText boundingBox! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayString class
	instanceVariableNames: ''!

!DisplayString class methodsFor: 'instance creation'!
new
	"Answer a new initialized instance."
	^super new initialize! !

!DisplayString class methodsFor: 'instance creation'!
string: aString
	"Answer a new instance with the argument as its string."
	^self new string: aString! !

!DisplayString class methodsFor: 'instance creation'!
string: aString origin: oPoint
	"Answer a new instance with the arguments as its string and origin point."
	^self new string: aString; origin: oPoint! !


!DisplayString class methodsFor: 'examples'!
stringExample
	"DisplayString stringExample"
	Display fillWhite.
	(DisplayString stringsX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayString class methodsFor: 'examples'!
stringsX: x byY: y
	"Answer a display list with randomly-placed random-color strings over the given extent."
	"(DisplayString stringsX: 600 byY: 600) display"
	^self stringsX: x byY: y items: 80! !

!DisplayString class methodsFor: 'examples'!
stringsX: x byY: y items: items
	"Answer a display list with randomly-placed random-color strings over the given extent."
	"(DisplayStringstringsX: 600 byY: 600) display"
	| dList rand inset |
	inset := 16.
	dList := DisplayList new.
	rand := Random new.
	items timesRepeat:
		[dList add: ((DisplayString
				string: (rand nextIntFrom: 10 to: 100000) printString
				origin: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				color: Color random)].
	^dList! !


DisplayItem subclass: #DisplayVisual
	instanceVariableNames: 'visual '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayLists'!
DisplayVisual comment:
'Instances of DisplayVisual are used for displaying arbitrary visual objects (e.g., cached images) in display lists.
Instance Variables:
	visual		<Form> the object''s visual object, typically an image'!

!DisplayVisual methodsFor: 'accessing' stamp: 'stp 07/09/97 00:27'!
boundingBox
	"Answer the visual's bounds translated by the receiver's translation."
	^origin == nil
		ifTrue: [visual boundingBox]
		ifFalse: [visual boundingBox translateBy: origin]! !

!DisplayVisual methodsFor: 'accessing'!
visual
	"Answer the receiver's visual."
	^visual! !

!DisplayVisual methodsFor: 'accessing'!
visual: aVisual
	"Set the receiver's visual."
	visual := aVisual.
"	aVisual palette == nil
		ifTrue: [aVisual palette: MappedPalette colorDefault]"! !


!DisplayVisual methodsFor: 'displaying'!
displayOn: aDisplayMedium
	"Display the receiver's visual on the given graphics context."
	visual displayOn: aDisplayMedium at: origin
! !

!DisplayVisual methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver's visual on the given display medium."
	visual displayOn: aDisplayMedium at: origin + aPoint
! !


!DisplayVisual methodsFor: 'drawing' stamp: 'stp 07/02/97 11:12'!
drawOn: aCanvas at: aPoint
	"Display the receiver on the given canvas at the given position."

	aCanvas image: visual at: (self boundingBox origin + aPoint)
! !


!DisplayVisual methodsFor: 'printing'!
printInstVarsOn: aStream
	"Print the receiver's relevant inst vars on the given Stream."
	super printInstVarsOn: aStream.
	visual == nil ifFalse: [aStream nextPutAll: ' visual: '. visual printOn: aStream].
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayVisual class
	instanceVariableNames: ''!

!DisplayVisual class methodsFor: 'instance creation'!
model: aM visual: aV origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."
	^self new model: aM; visual: aV; origin: aPt! !

!DisplayVisual class methodsFor: 'instance creation'!
visual: aV
	"Answer an instance of DisplayItem with the given instance variables."
	^(super new initialize) visual: aV! !

!DisplayVisual class methodsFor: 'instance creation'!
visual: aV origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."
	^(super new initialize) visual: aV; origin: aPt! !


!DisplayVisual class methodsFor: 'examples'!
visualExample
	"DisplayVisual visualExample"
	Display fillWhite.
	(DisplayVisual visualsX: Display width byY: Display height) display.
	(Delay forSeconds: 2) wait.
	ScheduledControllers restore! !

!DisplayVisual class methodsFor: 'examples'!
visualsX: x byY: y
	"Answer a display list with randomly-placed random-color visuals over the given extent."
	"(DisplayVisual visualsX: 600 byY: 600) display"
	^self visualsX: x byY: y items: 80! !

!DisplayVisual class methodsFor: 'examples'!
visualsX: x byY: y items: items
	"Answer a display list with randomly-placed random-color visuals over the given extent."
	"(DisplayVisual visualsX: 600 byY: 600) display"
	| dList rand inset cursors |
	inset := 16.
	dList := DisplayList new.
	rand := Random new.
	cursors := Cursor allInstances.
	items timesRepeat:
		[dList add: ((DisplayVisual
				visual: (cursors atRandom: rand)
				origin: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				color: Color random)].
	^dList! !
