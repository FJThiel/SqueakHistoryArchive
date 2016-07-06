"DesktopColor.st
Here's a quick hack to make the desktop more colorful. More work is needed
to bring it to full 'goodie' status.

    AUTHOR          Georg Gollmann (gollmann@edvz.tuwien.ac.at)
    VERSION         1.0 BETA
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 25, 1996"!


!ControlManager methodsFor: 'displaying'!
updateGray
	(screenController view model isMemberOf: InfiniteForm)
		ifTrue: [screenController view model: (InfiniteForm with:
Preferences desktopColor)]! !

!ControlManager methodsFor: 'initialize-release'!
initialize
	"Initialize the receiver to refer to only the background controller."
	| screenView |
	screenController := ScreenController new.
	screenView := FormView new.
	screenView
	 model: (InfiniteForm with: Preferences desktopColor)
	 controller: screenController.
	screenView window: Display boundingBox.
	scheduledControllers := OrderedCollection with: screenController! !

!Preferences class methodsFor: 'ui prefs'!
desktopColor
	"Answer the preferred color for the desktop.
	 Do the message below after changing the color."
	"ScheduledControllers updateGray; restore"

"	^Color lightMagenta
	^Color indexedColors at: 98
"	^Color r: 0.3 g: 0.4 b: 0.5 ! !

ScheduledControllers updateGray; restore!
