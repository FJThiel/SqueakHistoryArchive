'From Squeak 2.4c of May 10, 1999 on 18 July 1999 at 8:04:35 am'!"Change Set:		crispScrollbars-dewDate:			18 June 1999Author:			Doug WaySharpens the 3D look of the Morphic scrollbars, so that they're less muddy and appear larger, even though they don't actually take up any additional space.  (This increases the #inset/#raised highlight contrast, and reduces the borderWidth to 1.  Also fixes the scrollbar shadow so that the pagingArea is its owner, so that it will highlight correctly).BorderedMorph drawOn: tweaked by DI for compatibility."!!BorderedMorph methodsFor: 'drawing' stamp: 'di 7/18/1999 07:59'!drawOn: aCanvas 	"Draw a rectangle with a solid, inset, or raised border.	Note: the raised border color is generated from the receiver's own color,	while the inset border color is generated from the color of its owner.	This behavior is visually more consistent. Thanks to Hans-Martin Mosner."	| insetColor |	borderWidth = 0 ifTrue: [  "no border"		"Note: This is the hook for border styles.			When converting to the new borders we'll just put 0 into the borderWidth"		super drawOn: aCanvas.		^ self].	borderColor == #raised ifTrue: [		"Use a hack for now"		aCanvas fillRectangle: self bounds fillStyle: self fillStyle.		^ aCanvas frameAndFillRectangle: bounds			fillColor: Color transparent			borderWidth: borderWidth			topLeftColor: (borderWidth = 1 ifTrue: [color twiceLighter]										ifFalse: [color lighter])			bottomRightColor: (borderWidth = 1 ifTrue: [color twiceDarker]										ifFalse: [color darker])].	borderColor == #inset ifTrue: [		insetColor _ owner colorForInsets.		aCanvas fillRectangle: self bounds fillStyle: self fillStyle.		^ aCanvas frameAndFillRectangle: bounds			fillColor: Color transparent			borderWidth: borderWidth			topLeftColor: (borderWidth = 1 ifTrue: [insetColor twiceDarker]										ifFalse: [insetColor darker])			bottomRightColor: (borderWidth = 1 ifTrue: [insetColor twiceLighter]										ifFalse: [insetColor lighter])].	"solid color border"	aCanvas fillRectangle: self bounds fillStyle: self fillStyle.	aCanvas frameAndFillRectangle: bounds		fillColor: Color transparent		borderWidth: borderWidth		borderColor: borderColor.! !!Color methodsFor: 'transformations' stamp: 'dew 6/18/1999 19:24'!twiceDarker	"Answer a significantly darker shade of this color."	^ self mixed: 0.5 with: Color black! !!Color methodsFor: 'transformations' stamp: 'dew 6/18/1999 19:24'!twiceLighter	"Answer a significantly lighter shade of this color."	^ self mixed: 0.5 with: Color white! !!ScrollPane methodsFor: 'initialization' stamp: 'dew 6/16/1999 02:15'!initialize	super initialize.	borderWidth _ 2.  borderColor _ Color black.	retractableScrollBar _ scrollBarOnLeft _		(Preferences valueOfFlag: #inboardScrollbars) not.	scrollBar := ScrollBar new model: self slotName: 'scrollBar'.	scrollBar borderWidth: 1; borderColor: Color black.	retractableScrollBar ifFalse: [self addMorph: scrollBar].	scroller := TransformMorph new color: Color transparent.	scroller offset: -3@0.	self addMorph: scroller.	self on: #mouseEnter send: #mouseEnter: to: self.	self on: #mouseLeave send: #mouseLeave: to: self.	self extent: 150@120! !!Slider methodsFor: 'initialize' stamp: 'dew 6/16/1999 02:01'!initialize	super initialize.	bounds := 0@0 corner: 16@100.	color := Color gray.	borderWidth := 1.	borderColor := #inset.	value _ 0.0.	self initializeSlider! !!Slider methodsFor: 'initialize' stamp: 'dew 6/18/1999 19:39'!initializeSlider	slider := RectangleMorph newBounds: self totalSliderArea color: Color veryLightGray.	sliderShadow := RectangleMorph newBounds: self totalSliderArea						color: self pagingArea color.	slider on: #mouseStillDown send: #scrollAbsolute: to: self.	slider on: #mouseDown send: #mouseDownInSlider: to: self.	slider on: #mouseUp send: #mouseUpInSlider: to: self.	slider setBorderWidth: 1 borderColor: #raised.	sliderShadow setBorderWidth: 1 borderColor: #inset.	"(the shadow must have the pagingArea as its owner to highlight properly)"	self pagingArea addMorph: sliderShadow.	sliderShadow hide.	self addMorph: slider.	self computeSlider.! !!ScrollBar methodsFor: 'initialize' stamp: 'dew 6/16/1999 01:59'!initializeDownButton	downButton := RectangleMorph		newBounds: (self innerBounds bottomRight - self buttonExtent extent: self buttonExtent)		color: Color lightGray.	downButton on: #mouseDown send: #scrollDownInit to: self.	downButton on: #mouseStillDown send: #scrollDown to: self.	downButton on: #mouseUp send: #borderRaised to: downButton.	downButton addMorphCentered: (ImageMorph new image: 		(UpArrow rotateBy: (bounds isWide ifTrue: [#right] ifFalse: [#pi]) centerAt: 0@0)).	downButton setBorderWidth: 1 borderColor: #raised.	self addMorph: downButton! !!ScrollBar methodsFor: 'initialize' stamp: 'dew 6/16/1999 02:00'!initializeMenuButton	menuButton := RectangleMorph			newBounds: (self innerBounds topLeft extent: self buttonExtent)			color: Color lightGray.	menuButton on: #mouseEnter send: #menuButtonMouseEnter: to: self.	menuButton on: #mouseDown send: #menuButtonMouseDown: to: self.	menuButton on: #mouseLeave send: #menuButtonMouseLeave: to: self.	menuButton addMorphCentered:		(RectangleMorph newBounds: (0@0 extent: 4@2) color: Color black).	menuButton setBorderWidth: 1 borderColor: #raised.	self addMorph: menuButton! !!ScrollBar methodsFor: 'initialize' stamp: 'dew 6/16/1999 02:00'!initializeUpButton	upButton := RectangleMorph		newBounds: ((bounds isWide ifTrue: [menuButton bounds topRight]									ifFalse: [menuButton bounds bottomLeft])					extent: self buttonExtent)		color: Color lightGray.	upButton on: #mouseDown send: #scrollUpInit to: self.	upButton on: #mouseStillDown send: #scrollUp to: self.	upButton on: #mouseUp send: #borderRaised to: upButton.	upButton addMorphCentered: (ImageMorph new image: 		(bounds isWide ifTrue: [UpArrow rotateBy: #left centerAt: 0@0] ifFalse: [UpArrow])).	upButton setBorderWidth: 1 borderColor: #raised.	self addMorph: upButton! !!ScrollBar methodsFor: 'geometry' stamp: 'dew 6/9/1999 02:02'!buttonExtent	^ bounds isWide		ifTrue: [11 @ self innerBounds height]		ifFalse: [self innerBounds width @ 11]! !