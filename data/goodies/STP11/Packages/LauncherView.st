"LauncherView.st
Add buttons to the Transcript view to open the standard applications.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none (Uses PackageBrowser)
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    06/15/96"!

TextCollectorView subclass: #LauncherView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Transcript'!
LauncherView comment:
'Launcher View is a Transcript with text-labeled buttons along the top 
that open the standard applications:
	Br		Open a package browser
	Fi		Open a file list
	Ws		Open a workspace
	Ch		Pop up the change manager menu and select a tool
	Sa		Save a VI snapshot
	??		Open the keyboard shortcuts help list

There is much room for improvement or customization.

stp -- 1997.06.27
'!

!LauncherView methodsFor: 'button actions'!
isOn
	"Answer whether the mouse button is pressed."

	^Sensor anyButtonPressed!
openBrowser
	"Open a package browser, or a regular one if LeftShift."

	Sensor leftShiftDown
		ifFalse: [PackageBrowser open]
		ifTrue: [BrowserView openBrowser]
!
openChanges
	"Pop up the standard screen controller changes menu and select a tool."

	| sc menu choice |
	sc := ScheduledControllers screenController.
	menu := sc changesMenu.
	choice := menu startUp.
	choice isNil ifTrue: [^nil].
	^sc perform: choice!
openFileList
	"Create and schedule a FileList view for specifying files to access."

	FileList open!
openHelp
	"1/18/96 sw Open a window that explains command-keys"

	Utilities openCommandKeyHelp!
openWorkspace 
	"Create and schedule a StringHolderView for use as a workspace."

	StringHolderView open!
save
	"Save an image snapshot."

	^Smalltalk snapshot: true andQuit: false! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

LauncherView class
	instanceVariableNames: ''!

!LauncherView class methodsFor: 'instance creation'!
open
	"Create and schedule an instance of me on the Transcript by default."
	"LauncherView open"

	^self open: Transcript label: 'Squeak Transcript' !
open: aTextCollector label: aString 
	"Answer an instance of me on the argument, aTextCollector. 
	The label of the StandardSystemView should be aString."
	"LauncherView open"

	| topView inst buttons |
	inst := self new model: aTextCollector.
	topView _ StandardSystemView new.
	topView model: aTextCollector.
	topView label: aString.
	topView minimumSize: 360 @ 120.
	buttons := self buttonViewsOn: inst in: (0 @ 0 extent: 360 @ 16).
	inst window: (0 @ 16 extent: 362 @ 96).
	inst borderWidthLeft: 2 right: 2 top: 0 bottom: 2.
	topView addSubView: buttons.
	topView addSubView: inst.
	inst 
		align: inst viewport topLeft 
		with: buttons viewport bottomLeft.
"	inst 
		align: inst viewport bottomRight 
		with: topView viewport bottomRight.
"	topView controller open! !

!LauncherView class methodsFor: 'private'!
addViewOn: model sending: sel1 asking: sel2 label: lab in: rect to: barView

	| switchView |
	switchView _ SwitchView new.
	switchView model: model.
	switchView selector: sel2.
	switchView controller: LockedSwitchController new.
	switchView controller selector: sel1.
	switchView window: rect.
	switchView label: lab asText allBold asParagraph.
	switchView borderWidth: 1.
	barView addSubView: switchView.
	^barView!
buttonViewsOn: model in: rect
	"LauncherView open"

	| barView hi wide i |
	hi := rect height.
	wide := rect width // 6.
	barView := View new.
	barView borderWidthLeft: 1 right: 1 top: 1 bottom: 1.
	i := 0.
	self addViewOn: model sending: #openBrowser asking: #isOn 
			label: 'Br' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	i := i + 1.
	self addViewOn: model sending: #openFileList asking: #isOn 
			label: 'Fi' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	i := i + 1.
	self addViewOn: model sending: #openWorkspace asking: #isOn 
			label: 'Ws' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	i := i + 1.
	self addViewOn: model sending: #openChanges asking: #isOn 
			label: 'Ch' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	i := i + 1.
	self addViewOn: model sending: #save asking: #isOn 
			label: 'Sa' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	i := i + 1.
	self addViewOn: model sending: #openHelp asking: #isOn 
			label: '??' in: (((i*wide)@0) extent: (wide@hi)) to: barView.
	^barView! !

!ScreenController methodsFor: 'menu messages' stamp: 'stp 06/30/97 23:03'!
openTranscript 
	"Create and schedule a System Transcript.
	 2/5/96 sw: if there is already one open, then instead of refusing the user permission, just activate the damned thing."

	(Transcript transcriptOpen)
		ifTrue: [ScheduledControllers activateTranscript]
		ifFalse: [Transcript aTranscriptIsOpen.
				"TextCollectorView open: Transcript label: 'System Transcript'"
			LauncherView open]! !
