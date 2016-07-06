"      NAME	Windoze 3D borders and title bar
       AUTHOR	ssadams@us.ibm.com (Sam S. Adams and Andreas Raab)
       URL	(none)
       FUNCTION	Adds partial Windoze look and feel to Squeak windows
       KEYWORDS	Squeak Windoze
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 1.3
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.3.0  (first version for Squeak 1.3)
       DATE	27-Jan-98

SUMMARY

This goodie enhances Andreas Raab's NiceSystemView
by adding 3D borders ala Windoze and by enhancing
the title bar for a close emulation.

				Sam S. Adams and Andreas Raab
"!
'From Squeak 1.3 of Jan 16, 1998 on 26 January 1998 at 10:05:10 pm'!
StandardSystemView subclass: #NiceSystemView
	instanceVariableNames: 'growFrame '
	classVariableNames: 'CloseBoxForm GrowBoxForm ShrinkBoxForm SystemBoxForm '
	poolDictionaries: ''
	category: 'Interface-AR'!
StandardSystemController subclass: #NiceSystemController
	instanceVariableNames: 'lastSystemActivity '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-AR'!


!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:32'!
closeBoxFrame

	| boxHeight |
	boxHeight _ self labelDisplayBox height -  11.
	^ Rectangle origin: (self labelDisplayBox topRight + (boxHeight negated-9@7)) extent:
boxHeight+2@boxHeight

	"NiceSystemView initializeCache"! !

!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:35'!
growBoxFrame

	| boxHeight |
	boxHeight _ self labelDisplayBox height -  11.
	^ Rectangle origin: (self labelDisplayBox topRight + (2*boxHeight negated-12@7)) extent:
boxHeight+2@boxHeight

	"NiceSystemView initializeCache"! !

!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:25'!
labelHeight
	^18 + 6  " (LabelStyle fontAt: 2) height + 10"! !

!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:56'!
labelTextRegion
	labelText == nil ifTrue: [^ self labelDisplayBox center extent: 0@0].
	^ (labelText boundingBox
			align: labelText boundingBox leftCenter
			with: self labelDisplayBox leftCenter + (25@0))
		intersect: (self labelDisplayBox origin corner: self labelDisplayBox corner -
(55@0))! !

!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:34'!
shrinkBoxFrame

	| boxHeight |
	boxHeight _ self labelDisplayBox height -  11.
	^ Rectangle origin: (self labelDisplayBox topRight + (3*boxHeight negated-15@7)) extent:
boxHeight+2@boxHeight

	"NiceSystemView initializeCache"! !

!NiceSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:17'!
systemBoxFrame

	| boxHeight |
	boxHeight _ self labelDisplayBox height - 11 .
	^ Rectangle origin: (self labelDisplayBox origin + (7@7)) extent:
boxHeight@boxHeight

	"NiceSystemView initializeCache"! !

!NiceSystemView methodsFor: 'framing' stamp: 'ar 1/22/98 22:47'!
fullScreen
	| portRect |
	portRect _ self viewport.
	growFrame _ portRect topLeft - self labelOffset
				corner: portRect corner.
	^super fullScreen! !

!NiceSystemView methodsFor: 'framing' stamp: 'ar 1/22/98 22:49'!
restore
	self reframeTo: ( growFrame isNil ifTrue:[self initialFrame]
ifFalse:[growFrame])! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:46'!
deEmphasizeLabel
	"Un-Highlight the label."
	labelFrame height = 0 ifTrue: [^ self].  "no label"
	self displayLabelBackground: false.
	self displayLabelTextDeEmphasized.
	self displayLabelBorder! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/26/98 21:02'!
displayDeEmphasized
	"Display this view with emphasis off.
	If windowBits is not nil, then simply BLT if possible,
		but force full display for top window so color is preserved."

	super displayDeEmphasized.
	self deEmphasizeLabel! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/26/98 21:47'!
displayEmphasized
	"Display with label highlighted to indicate that it is active."

	super displayEmphasized.
	self emphasizeLabel ! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:42'!
displayLabelBackground: emphasized
	"Clear or emphasize the inner region of the label"

	emphasized
		ifTrue:[Display
					fill: self labelDisplayBox
					fillColor:(Color r: 0.0 g: 0.4 b: 0.4)]
		ifFalse:[Display
					fill: self labelDisplayBox
					fillColor:(Color r: 0.344 g: 0.344 b: 0.344).].
		self displayLabelBoxes! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:13'!
displayLabelBoxes
	CloseBoxForm ifNil:[CloseBoxForm _ self drawCloseBoxForm].
	GrowBoxForm ifNil:[GrowBoxForm _ self drawGrowBoxForm].
	ShrinkBoxForm ifNil:[ShrinkBoxForm _ self drawShrinkBoxForm].
	SystemBoxForm ifNil:[SystemBoxForm _ self drawSystemBoxForm].
	CloseBoxForm displayOn: Display at: self closeBoxFrame origin.
	GrowBoxForm displayOn: Display at: self growBoxFrame origin.
	self isCollapsed 
		ifFalse:[ShrinkBoxForm displayOn: Display at: self 
shrinkBoxFrame origin].

	SystemBoxForm displayOn: Display at: self 
systemBoxFrame origin.! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelText
	"The label goes in the center of the window"
	| labelRect |
	labelText foregroundColor: Color white
			backgroundColor: (Color r: 0.0 g: 0.4 b: 0.4).
	labelRect _ self labelTextRegion.
	"Display fill: (labelRect expandBy: 3@0) fillColor: self labelColor."
	labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
			rule: Form under fillColor: labelText fillColor! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelTextDeEmphasized
	| labelRect |
	labelText foregroundColor: Color gray
			backgroundColor: (Color r: 0.344 g: 0.344 b: 0.344).
	labelRect _ self labelTextRegion.
	labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
			rule: Form under fillColor: labelText fillColor! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelTextEmphasized
	| labelRect |
	labelText foregroundColor: Color white
			backgroundColor: (Color r: 0.0 g: 0.4 b: 0.4).
	labelRect _ self labelTextRegion.
	labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
			rule: Form under fillColor: labelText fillColor! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ar 1/22/98 22:01'!
drawCloseBoxForm
	| box pen form |
	form _ Form extent: self closeBoxFrame extent depth: 8.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight.
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	pen color: Color black.
	pen defaultNib: 2.
	pen place: box bottomLeft + (3@-4).
	pen goto: box topRight - (4@-3).
	pen place: box topLeft + (3@3).
	pen goto: box bottomRight - (4@4).
	^form! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ar 1/22/98 22:10'!
drawGrowBoxForm
	| box pen form |
	form _ Form extent: self growBoxFrame extent depth: 8.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight.
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	pen color: Color black.
	pen place: box topLeft + (3@3).
	pen goto: box bottomLeft + (3@-3).
	pen goto: box bottomRight + (-3@-3).
	pen goto: box topRight + (-3@3).
	pen defaultNib: 2.
	pen place: box topRight + (-4@3).
	pen goto: box topLeft + (3@3).
	^form! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ar 1/22/98 22:11'!
drawShrinkBoxForm
	| box pen form |
	form _ Form extent: self growBoxFrame extent depth: 8.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight.
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	pen color: Color black.
	pen defaultNib: 2.
	pen place: box bottomRight + (-4@-3).
	pen goto: box bottomLeft + (3@-3).
	^form! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:09'!
drawSystemBoxForm
	| box pen form |
	form _ Form extent: self systemBoxFrame extent depth: 8.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight.
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	^form! !

!NiceSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:49'!
emphasizeLabel
	"Highlight the label."
	labelFrame height = 0 ifTrue: [^ self].  "no label"
	self displayLabelBackground: true.
	self displayLabelBoxes.
	self displayLabelTextEmphasized.
	self displayLabelBorder! !

!NiceSystemView methodsFor: 'bordering' stamp: 'ssa 1/24/98 22:52'!
displayBorder
	"Display the receiver's border (using the receiver's borderColor)."

	| box |
	borderWidth = 0
		ifTrue:
			[insideColor == nil
				ifFalse: 
					[Display fill: self displayBox fillColor: self backgroundColor]]
		ifFalse:
			[box _ self displayBox.
	"left edge"
	Display fill: (box origin extent: 1@(box height - 1))
		fillColor: Color gray.
	Display fill: (box origin +(1@1) extent: 1@(box height - 2))
		fillColor: Color veryLightGray.
	Display fill: (box origin +(2@2) extent: 2@(box height-4))
		fillColor: Color gray.
	Display fill: (box origin +(4@3) extent: 1@(box height-6))
		fillColor: Color veryDarkGray.
	Display fill: (box origin +(5@4) extent: 1@(box height-8))
		fillColor: Color black.

	"right edge"
	Display fill: (box topRight -(1@0)extent: 1@(box height - 1))
		fillColor: Color black.
	Display fill: (box topRight +(-2@1) extent: 1@(box height - 2))
		fillColor: Color veryDarkGray.
	Display fill: (box topRight +(-4@2) extent: 2@(box height-4))
		fillColor: Color gray.
	Display fill: (box topRight +(-5@5) extent: 1@(box height-10))
		fillColor: Color veryLightGray.
	Display fill: (box topRight +(-6@6) extent: 1@(box height-12))
		fillColor: Color gray.

	"top edge"
	Display fill: (box origin extent: box width@1)
		fillColor: Color gray.
	Display fill: (box origin +(1@1) extent: (box width - 2)@1)
		fillColor: Color veryLightGray.
	Display fill: (box origin +(2@2) extent: (box width-4)@2)
		fillColor: Color gray.
	Display fill: (box origin +(4@4) extent: (box width-8)@1)
		fillColor: Color veryDarkGray.
	Display fill: (box origin +(5@5) extent: (box width-10)@1)
		fillColor: Color black.

	"bottom edge"
	Display fill: (box bottomLeft -(0@1) extent: box width+1 @1)
		fillColor: Color black.
	Display fill: (box bottomLeft +(1@-2) extent: (box width - 2)@1)
		fillColor: Color veryDarkGray.
	Display fill: (box bottomLeft +(2@-4) extent: (box width-4)@2)
		fillColor: Color gray.
	Display fill: (box bottomLeft +(4@-5) extent: (box width-7)@1)
		fillColor: Color veryLightGray.
	Display fill: (box bottomLeft +(5@-6) extent: (box width-9)@1)
		fillColor: Color gray.
			insideColor == nil ifFalse:
				[Display fill: self insetDisplayBox fillColor: self backgroundColor]]! !

!NiceSystemView methodsFor: 'bordering' stamp: 'ssa 1/24/98 22:58'!
displayLabelBorder
	"Display the receiver's label border."

	| box |
	borderWidth = 0
		ifTrue:
			[insideColor == nil
				ifFalse: 
					[Display fill: self displayBox fillColor: self backgroundColor]]
		ifFalse:
			[box _ self labelDisplayBox.
	"left edge"
	Display fill: (box origin extent: 1@(box height))
		fillColor: Color gray.
	Display fill: (box origin +(1@1) extent: 1@(box height))
		fillColor: Color veryLightGray.
	Display fill: (box origin +(2@2) extent: 2@(box height))
		fillColor: Color gray.
	Display fill: (box origin +(4@3) extent: 1@(box height))
		fillColor: Color veryDarkGray.
	Display fill: (box origin +(5@4) extent: 1@(box height))
		fillColor: Color black.

	"right edge"
	Display fill: (box topRight -(1@0)extent: 1@(box height))
		fillColor: Color black.
	Display fill: (box topRight +(-2@1) extent: 1@(box height))
		fillColor: Color veryDarkGray.
	Display fill: (box topRight +(-4@2) extent: 2@(box height))
		fillColor: Color gray.
	Display fill: (box topRight +(-5@5) extent: 1@(box height))
		fillColor: Color veryLightGray.
	Display fill: (box topRight +(-6@6) extent: 1@(box height))
		fillColor: Color gray.

	"top edge"
	Display fill: (box origin extent: box width@1)
		fillColor: Color gray.
	Display fill: (box origin +(1@1) extent: (box width - 2)@1)
		fillColor: Color veryLightGray.
	Display fill: (box origin +(2@2) extent: (box width-4)@2)
		fillColor: Color gray.
	Display fill: (box origin +(4@4) extent: (box width-8)@1)
		fillColor: Color veryDarkGray.
	Display fill: (box origin +(5@5) extent: (box width-10)@1)
		fillColor: Color black.

	"bottom edge"
	"Display fill: (box bottomLeft -(0@1) extent: box width+1 @1)
		fillColor: Color black.
	Display fill: (box bottomLeft +(1@-2) extent: (box width - 2)@1)
		fillColor: Color veryDarkGray.
	Display fill: (box bottomLeft +(2@-4) extent: (box width-4)@2)
		fillColor: Color gray.
	Display fill: (box bottomLeft +(4@-5) extent: (box width-7)@1)
		fillColor: Color veryLightGray.
	Display fill: (box bottomLeft +(5@-6) extent: (box width-9)@1)
		fillColor: Color gray."]! !

!NiceSystemView methodsFor: 'testing' stamp: 'ar 1/22/98 22:07'!
cacheBitsAsTwoTone
	^false! !

!NiceSystemView methodsFor: 'testing' stamp: 'ar 1/22/98 22:37'!
isFullScreen
	| frame |
	frame _ model fullScreenSize.
	^(frame topLeft + self labelOffset corner: frame corner) = self viewport
! !

!NiceSystemView methodsFor: 'controller access' stamp: 'ar 1/22/98 22:22'!
defaultControllerClass
	^NiceSystemController! !

!NiceSystemView methodsFor: 'initialize' stamp: 'ssa 1/26/98 21:55'!
initialize
	"change the default border to 5 to support Windoze style edges.  ssa 1/24/98 15:43"


	super initialize. 
	self borderWidthLeft: 5 right: 5 top: 0 bottom: 5! !

!NiceSystemView methodsFor: 'window access' stamp: 'ssa 1/24/98 15:56'!
defaultWindow
	"Build the minimum Rectangle that encloses all the windows of the 
	receiver's subViews. The answer is a Rectangle obtained by expanding 
	this minimal Rectangle by the borderWidth of the receiver. If the 
	receiver has no subViews, then a Rectangle enclosing the entire display 
	screen is answered. It is used internally by View methods if no window 
	has been specified for the View. Specialized subclasses of View should 
	redefine View|defaultWindow to handle the default case for instances 
	that have no subViews."

	| aRectangle |
	subViews isEmpty ifTrue: [^DisplayScreen boundingBox].
	aRectangle _ self firstSubView viewport.
	subViews do: [:aView | aRectangle _ aRectangle merge: aView viewport].
	^aRectangle! !


!Pen methodsFor: 'accessing' stamp: 'ssa 1/15/98 15:53'!
destForm: aForm
	"2/14/97 ssa added for compatibility."
	self flag:#compatibility.

	destForm _ aForm ! !


!StandardSystemController methodsFor: 'borders'!
adjustWindowCorners 
	| box cornerBox p clicked f2 |
	box _ view windowBox.
	clicked _ false.
	#(topLeft topRight bottomRight bottomLeft)
		do: [:readCorner |
			cornerBox _ ((box insetBy: 2) perform: readCorner) - (10@10) extent: 20@20.
			(cornerBox containsPoint: sensor cursorPoint)
				ifTrue: 
				["Display reverse: cornerBox."
				(Cursor perform: readCorner) showWhile:
					[[(cornerBox containsPoint: (p _ sensor cursorPoint))
						and: [(clicked _ sensor anyButtonPressed) not]]
						whileTrue.
				"Display reverse: cornerBox."
				clicked ifTrue:
					[view newFrame:
						[:f | p _ sensor cursorPoint.
						readCorner = #topLeft ifTrue:
							[f2 _ p corner: f bottomRight].
						readCorner = #bottomLeft ifTrue:
							[f2 _ (f withBottom: p y) withLeft: p x].
						readCorner = #bottomRight ifTrue:
							[f2 _ f topLeft corner: p].
						readCorner = #topRight ifTrue:
							[f2 _ (f withTop: p y) withRight: p x].
						f2]]]]].
	^ clicked! !

!StandardSystemController methodsFor: 'borders'!
checkForReframe
	"2/18/97 ssa - added a leftshift hack to prevent over aggressive offerings of reframing help for subpanes."

	| cp |
	view isCollapsed ifTrue: [^ self].
	cp _ sensor cursorPoint.
	((view closeBoxFrame expandBy: 2) containsPoint: cp)
		| ((view growBoxFrame expandBy: 2) containsPoint: cp)
		ifTrue: [^ self].  "Dont let reframe interfere with close/grow"
	self adjustWindowCorners.
	self cursorOnBorder ifFalse: [^ self].
	((view insetDisplayBox insetBy: 2@2) containsPoint: cp)
		ifFalse: [^ self adjustWindowBorders].

	Sensor leftShiftDown 
		ifTrue:[	
			(view subviewWithLongestSide: [:s | ] near: cp) == nil
				ifFalse: [^ self adjustPaneBorders]].! !


!NiceSystemController methodsFor: 'accessing' stamp: 'ar 1/22/98 23:30'!
lastSystemActivity
	^lastSystemActivity ifNil:[lastSystemActivity _ 0]! !

!NiceSystemController methodsFor: 'accessing' stamp: 'ar 1/22/98 23:30'!
lastSystemActivity: aNumber
	lastSystemActivity _ aNumber! !

!NiceSystemController methodsFor: 'basic control sequence' stamp: 'ar 1/22/98
23:33'!
redButtonActivity 	| box p | 	p _ sensor cursorPoint. 	((box _ view
systemBoxFrame) containsPoint: p) 		ifTrue: [^self systemActivity]. 	((box _
view shrinkBoxFrame) containsPoint: p) 		ifTrue: [Utilities awaitMouseUpIn: box
repeating: [] ifSucceed: [self collapse. ^ self]. 				^ self]. 	((box _ view
growBoxFrame) containsPoint: p) 		ifTrue: [Utilities awaitMouseUpIn: box
repeating: [] ifSucceed: 					[view isCollapsed 						ifTrue:[self expand]
						ifFalse:[view isFullScreen ifTrue:[self restore] ifFalse:[self
fullScreen]]. 					 ^ self]. 				^ self].

	super redButtonActivity.! !

!NiceSystemController methodsFor: 'basic control sequence' stamp: 'ar 1/22/98
23:33'!
systemActivity 	"The system menu button has been pressed" 	| time |
	time _ Time millisecondClockValue. 	(time- self lastSystemActivity) < self
doubleClickTime 		ifTrue:[^self close]. 	self lastSystemActivity: time. 	^self
blueButtonActivity! !

!NiceSystemController methodsFor: 'menu messages' stamp: 'ar 1/22/98 22:41'!
restore
	view restore! !

!NiceSystemController methodsFor: 'private' stamp: 'ar 1/22/98 23:32'!
doubleClickTime
	"Return the maximum delay time for double clicks.
	This value is in milliseconds."
	^500! !

!NiceSystemController methodsFor: 'borders' stamp: 'ssa 1/25/98 00:00'!
adjustWindowCorners 
	| box cornerBox p clicked f2 |
	box _ view windowBox.
	clicked _ false.
	#(topLeft topRight bottomRight bottomLeft)
		do: [:readCorner |
			cornerBox _ ((box insetBy: 2) perform: readCorner) - (4@4) extent: 8@8.
			(cornerBox containsPoint: sensor cursorPoint)
				ifTrue: 
				["Display reverse: cornerBox."
				(Cursor perform: readCorner) showWhile:
					[[(cornerBox containsPoint: (p _ sensor cursorPoint))
						and: [(clicked _ sensor anyButtonPressed) not]]
						whileTrue.
				"Display reverse: cornerBox."
				clicked ifTrue:
					[view newFrame:
						[:f | p _ sensor cursorPoint.
						readCorner = #topLeft ifTrue:
							[f2 _ p corner: f bottomRight].
						readCorner = #bottomLeft ifTrue:
							[f2 _ (f withBottom: p y) withLeft: p x].
						readCorner = #bottomRight ifTrue:
							[f2 _ f topLeft corner: p].
						readCorner = #topRight ifTrue:
							[f2 _ (f withTop: p y) withRight: p x].
						f2]]]]].
	^ clicked! !

!NiceSystemController methodsFor: 'borders' stamp: 'ssa 1/24/98 23:47'!
cursorOnBorder 
	| cp i box |
	view isCollapsed ifTrue: [^ false].
	cp _ sensor cursorPoint.
	((view labelDisplayBox insetBy: 5@5) containsPoint: cp)
		ifTrue: [^ false].
	(i _ view subViews findFirst: [:v | v displayBox containsPoint: cp]) = 0
		ifTrue: [box _ view windowBox]
		ifFalse: [box _ (view subViews at: i) insetDisplayBox].
	^ ((box insetBy: 3) containsPoint: cp) not
		and: [(box expandBy: 4) containsPoint: cp]! !


!StandardSystemView class methodsFor: 'instance creation' stamp: 'ar 1/22/98
23:36'!
new 
	"This is a rather dirty hack -- but we don't have a window builder
yet. (ar 1/22/98 23:36)"
	^Preferences nicerSystemViews
 		ifTrue:[NiceSystemView basicNew initialize]
		ifFalse:[self basicNew initialize]! !


!NiceSystemView class methodsFor: 'class initialization' stamp: 'ar 1/22/98
23:13'!
initialize
	"NiceSystemView initialize"! !

!NiceSystemView class methodsFor: 'class initialization' stamp: 'ssa 1/24/98 23:22'!
initializeCache
	"NiceSystemView initializeCache"
	GrowBoxForm _ nil.
	ShrinkBoxForm _ nil.
	SystemBoxForm _ nil.
	CloseBoxForm _ nil.! !

!NiceSystemView class methodsFor: 'class initialization' stamp: 'ar 1/22/98
23:15'!
install 	"NiceSystemView install" 	Preferences setPreference:
#nicerSystemViews toValue: true.! !

!NiceSystemView class methodsFor: 'class initialization' stamp: 'ar 1/22/98
23:14'!
unInstall 	"NiceSystemView unInstall" 	Preferences setPreference:
#nicerSystemViews toValue: false.! !


!ProjectView class methodsFor: 'as yet unclassified' stamp: 'ssa 1/26/98 21:13'!
open: aProject 
	"Answer an instance of me for the argument, aProject. It is created on the
	display screen."
	"Modified to evade the NiceSystemView hack so we can still use ProjectViews - ssa 1/26/98 21:13"
	| topView |
	topView _ self basicNew initialize model: aProject.
	topView minimumSize: 50 @ 30.
	topView borderWidth: 2.
	topView controller open! !


!NiceSystemView reorganize!
('label accessing' closeBoxFrame growBoxFrame labelHeight labelTextRegion shrinkBoxFrame systemBoxFrame)
('framing' fullScreen restore)
('displaying' deEmphasizeLabel displayDeEmphasized displayEmphasized displayLabelBackground: displayLabelBoxes displayLabelText displayLabelTextDeEmphasized displayLabelTextEmphasized drawCloseBoxForm drawGrowBoxForm drawShrinkBoxForm drawSystemBoxForm emphasizeLabel)
('bordering' displayBorder displayLabelBorder)
('testing' cacheBitsAsTwoTone isFullScreen)
('controller access' defaultControllerClass)
('initialize' initialize)
('window access' defaultWindow)
!

NiceSystemView install!

