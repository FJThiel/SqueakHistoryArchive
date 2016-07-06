"Graphics-Containers.st
This is a minimal 'wrapper' framework. It is a pre-cursor to my new MVC implementation.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0 BETA
    PREREQUISITES   Kernel-Values.st, Object-testing.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 21, 1996"!

DisplayObject subclass: #GraphicalContainer
	instanceVariableNames: 'container component '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
GraphicalContainer comment:
'An instance of a subclass of the abstract class GraphicalContainer displays its component, possibly with some alteration such as scaling. translation, clipping, or bordering.

Instance Variables:
	container	<something visual>	the upward pointer
	component	<something visual> 	the downward pointer'!

!GraphicalContainer methodsFor: 'accessing'!
component
	"Answer the receiver's 'component'."

	^component!
component: anObject
	"Set the receiver's instance variable 'component' to be anObject."

	component := anObject!
container
	"Answer the receiver's 'container'."

	^container!
container: anObject
	"Set the receiver's instance variable 'container' to be anObject."

	container := anObject! !

!GraphicalContainer methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver's components on the given display medium."

	self components displayOn: aDisplayMedium at: aPoint! !

!GraphicalContainer methodsFor: 'display box access'!
boundingBox
	"Answer the rectangular area that represents the boundaries of the 
	receiver's space of information."

	^self component boundingBox! !

GraphicalContainer subclass: #BoundedContainer
	instanceVariableNames: 'extent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
BoundedContainer comment:
'An instance of BoundedContainer can draw its component clipped to an arbitrary rectangle.

Instance Variables:
	extent	<Point>	the extent of the component'!

!BoundedContainer methodsFor: 'accessing'!
extent
	"Answer the receiver's 'extent'."

	^extent!
extent: anObject
	"Set the receiver's instance variable 'extent' to be anObject."

	extent := anObject! !

BoundedContainer subclass: #BorderedContainer
	instanceVariableNames: 'borderRectangle insideColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
BorderedContainer comment:
'An instance of BorderedContainer can draw a border around its component.
Instance Variables:
	BorderRectangle	<Rectangle>	the border widths
	insideColor	<Color>	the components background color'!

!BorderedContainer methodsFor: 'accessing'!
borderRectangle
	"Answer the receiver's 'borderRectangle'."

	^borderRectangle!
borderRectangle: anObject
	"Set the receiver's instance variable 'borderRectangle' to be anObject."

	borderRectangle := anObject!
insideColor
	"Answer the receiver's 'insideColor'."

	^insideColor!
insideColor: anObject
	"Set the receiver's instance variable 'insideColor' to be anObject."

	insideColor := anObject! !

GraphicalContainer subclass: #ScalingContainer
	instanceVariableNames: 'scale '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
ScalingContainer comment:
'An instance of ScalingContainer can scaled its component before drawing.

Instance Variables:
	scale	<Point>	the scale of the component'!

!ScalingContainer methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver's components on the given display medium."

	self components displayOn: aDisplayMedium at: aPoint! !

!ScalingContainer methodsFor: 'bounding box access'!
boundingBox
	"Answer the rectangular area that represents the boundaries of the 
	receiver's space of information."

	^self component boundingBox scaleBy: scale! !

!ScalingContainer methodsFor: 'accessing'!
scale
	"Answer the receiver's 'scale'."

	^scale!
scale: anObject
	"Set the receiver's instance variable 'scale' to be anObject."

	scale := anObject! !

GraphicalContainer subclass: #TranslatingContainer
	instanceVariableNames: 'offset '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
TranslatingContainer comment:
'An instance of TranslatingContainer can translate its component.

Instance Variables:
	offset	<Point>	the offset of the component'!

!TranslatingContainer methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
	"Display the receiver's components on the given display medium."

	self components displayOn: aDisplayMedium at: aPoint + offset! !

!TranslatingContainer methodsFor: 'bounding box access'!
boundingBox
	"Answer the rectangular area that represents the boundaries of the 
	receiver's space of information."

	^self component boundingBox translatedBy: offset! !

!TranslatingContainer methodsFor: 'accessing'!
offset
	"Answer the receiver's 'offset'."

	^offset!
offset: anObject
	"Set the receiver's instance variable 'offset' to be anObject."

	offset := anObject! !

TranslatingContainer subclass: #ScrollingContainer
	instanceVariableNames: 'scrollers '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Containers'!
ScrollingContainer comment:
'An instance of ScrollingContainer can translate its component and has special hooks for scrollers.

Instance Variables:
	scrollers	<Scroller>	the scrollers that control the container'!

!ScrollingContainer methodsFor: 'scrolling'!
scrollBy: aPoint
	offset := offset + aPoint! !

!ScrollingContainer methodsFor: 'accessing'!
scrollers
	"Answer the receiver's 'scrollers'."

	^scrollers!
scrollers: anObject
	"Set the receiver's instance variable 'scrollers' to be anObject."

	scrollers := anObject! !