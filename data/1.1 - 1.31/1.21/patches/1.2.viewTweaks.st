"Sundry changes to the view framework to enhance speed and functionality when view caching is off.

	TITLE			viewTweaks.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			1.0
	IMAGE VERSION	1.18, 1.2
	PREREQUISITES	none
	DATE			December 19, 1996"!

ControlManager addInstVarName: 'dirtyRect'!

!ControlManager methodsFor: 'accessing'!
dirtyRect: aRect
	"Mark aRect as dirty. To be called by StandardSystemView>close."

	dirtyRect _ aRect! !
!ControlManager methodsFor: 'displaying'!
restore: aRectangle
	"Restore all windows visible in aRectangle"
	^ self restore: aRectangle from: 1 full: true!
restore: aRectangle from: index full: aBoolean
	"Restore all windows visible in aRectangle at and below index.
	 If aBoolean is false only display the frame."
	| view | 
	index > scheduledControllers size ifTrue: [^ self].
	view _ (scheduledControllers at: index) view.
	(aRectangle intersects: view windowBox)
		ifTrue: [Display clippingTo: aRectangle do:
			[aBoolean ifTrue: [view display] ifFalse: [view displayBorder; displayView]]].
	(aRectangle areasOutside: view windowBox) do:
		[:rect | self restore: rect from: index + 1 full: aBoolean]!
restore: aRectangle belowView: aView
	"Restore all windows visible in aRectangle below aView.
	 Paint the frames immediately, schedule a full restore for later."
	| index |
	index := (scheduledControllers indexOf: aView controller) + 1.
	self restore: aRectangle from: index full: false.
	dirtyRect _ aRectangle!
restoreDirty
	"Restore the dirty rectangle (if any).
	 To be called by StandardSystemController>checkForReframe."

	dirtyRect notNil ifTrue: [
		(dirtyRect areasOutside: activeController view windowBox)
			do: [:rect | self restore: rect].
		dirtyRect _ nil
	]! !

!StandardSystemController methodsFor: 'initialize-release'!
initialize

	super initialize.
	self initializeBlueButtonMenu! !
!StandardSystemController methodsFor: 'control defaults'!
isControlActive
	(ScheduledControllers includes: self) ifFalse: [^ false].
	sensor anyButtonPressed ifFalse: [^ true].
	self viewHasCursor
		ifTrue: [^ true]
		ifFalse: [ScheduledControllers noteNewTop.
				^ false]! !
!StandardSystemController methodsFor: 'basic control sequence'!
controlInitialize
	view displayEmphasized.
	sensor waitNoButton!
controlTerminate

	(ScheduledControllers includes: self) ifFalse: [
		view ~~ nil ifTrue: [view release].
		^self].
	view deEmphasize; cacheBits! !
!StandardSystemController methodsFor: 'menu messages'!
close
	"The receiver's view should be removed from the screen and from the 
	collection of scheduled views."

	model okToChange ifFalse: [^self].
	ScheduledControllers unschedule: self; dirtyRect: view windowBox.
	view erase! !
!StandardSystemController methodsFor: 'scheduling'!
closeAndUnschedule
	"Erase the receiver's view and remove it from the collection of scheduled 
	views."

	view erase.
	view release.
	ScheduledControllers unschedule: self!
closeAndUnscheduleNoErase
	"Remove the scheduled view from the collection of scheduled views. Set 
	its status to closed but do not erase."

	view release.
	ScheduledControllers unschedule: self!
open
	"Create an area on the screen in which the receiver's scheduled view can 
	be displayed. Make it the active view."

	view resizeInitially.
	ScheduledControllers scheduleActive: self!
openDisplayAt: aPoint 
	"Create an area with origin aPoint in which the receiver's scheduled 
	view can be displayed. Make it the active view."

	view align: view viewport center with: aPoint.
	view translateBy:
		(view displayBox amountToTranslateWithin: Display boundingBox).
	ScheduledControllers scheduleActive: self!
openNoTerminate
	"Create an area in which the receiver's scheduled view can be displayed. 
	Make it the active view. Do not terminate the currently active process."

	view resize.
	ScheduledControllers scheduleActiveNoTerminate: self!
openNoTerminateDisplayAt: aPoint 
	"Create an area with origin aPoint in which the receiver's scheduled 
	view can be displayed. Make it the active view. Do not terminate the 
	currently active process."

	view resizeMinimumCenteredAt: aPoint.
	ScheduledControllers scheduleActiveNoTerminate: self! !

!StandardSystemController methodsFor: 'borders'!
checkForReframe
	| cp |
	ScheduledControllers restoreDirty.
	view isCollapsed ifTrue: [^ self].
	cp _ sensor cursorPoint.
	((view closeBoxFrame expandBy: 2) containsPoint: cp)
		| ((view growBoxFrame expandBy: 2) containsPoint: cp)
		ifTrue: [^ self].  "Dont let reframe interfere with close/grow"
	self adjustWindowCorners.
	self cursorOnBorder ifFalse: [^ self].
	((view insetDisplayBox insetBy: 2@2) containsPoint: cp)
		ifFalse: [^ self adjustWindowBorders].
	(view subviewWithLongestSide: [:s | ] near: cp) == nil
		ifFalse: [^ self adjustPaneBorders].! !

!StandardSystemView methodsFor: 'displaying'!
erase
	"Clear the display box of the receiver."

	| oldValid |
	oldValid _ bitsValid.
	bitsValid _ false.
	ScheduledControllers restore: self windowBox belowView: self.
	bitsValid _ oldValid! !

ControlManager removeSelector: #restore:below:without:!
ControlManager removeSelector: #restore:without:!
StandardSystemController removeSelector: #under!
StandardSystemController removeSelector: #status:!
View removeSelector: #displayClippingTo:!
View removeSelector: #clippingTo:do:!
StandardSystemController removeInstVarName: 'status'!

!StandardSystemView methodsFor: 'label access'!
labelColor
	"Answer the color to use as the background for the receiver's label. Visually indicate when caching is off."

	^ CacheBits ifTrue: [ self backgroundColor ] ifFalse: [ self backgroundColor darker ]! !

