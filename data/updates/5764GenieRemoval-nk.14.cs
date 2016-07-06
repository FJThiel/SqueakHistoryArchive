'From Squeak3.7alpha of 11 September 2003 [latest update: #5765] on 6 March 2004 at 8:57:04 am'!"Change Set:		GenieRemoval-nk (v2)Date:			6 March 2004Author:			Ned KonzRemoves Genie so that it can be loaded later as a package.Tested with 3.7a-5707.NOTE that this change set is hand-assembled, and should NOT be filed out from a Change Sorter and re-applied.14 February: First version.6 March: Removed termination of UI process in postscript; removed reference to CRDictionary."HandMorph allSubInstancesDo: [ :ea | ea disableGenie ].Morph allSubInstancesDo: [ :ea | ea gestureDictionaryOrName: nil ].FileList unregisterFileReader: CRRecognizer.!!EventHandler methodsFor: 'copying' stamp: 'nk 2/14/2004 18:24'!veryDeepInner: deepCopier	"ALL fields are weakly copied!!  Can't duplicate an object by duplicating a button that activates it.  See DeepCopier."	super veryDeepInner: deepCopier.	"just keep old pointers to all fields"!]style[(25 108 10 78)f1b,f1,f1LDeepCopier Comment;,f1! !!EventHandler methodsFor: 'initialization' stamp: 'nk 2/15/2004 08:59'!onGestureSend: selector to: recipient! !!EventHandler methodsFor: 'testing' stamp: 'nk 2/15/2004 08:57'!handlesGestureStart: evt	"Does the associated morph want to handle gestures?"	^false! !!Morph methodsFor: 'event handling' stamp: 'nk 2/14/2004 18:42'!handlesMouseDown: evt	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"	"NOTE: The default response is false, except if you have added sensitivity to mouseDown events using the on:send:to: mechanism.  Subclasses that implement these messages directly should override this one to return true." 	self eventHandler ifNotNil: [^ self eventHandler handlesMouseDown: evt].	^ false! !!Morph methodsFor: 'events-processing' stamp: 'nk 2/14/2004 18:42'!handleMouseDown: anEvent	"System level event handling."	anEvent wasHandled ifTrue:[^self]. "not interested"	anEvent hand removePendingBalloonFor: self.	anEvent hand removePendingHaloFor: self.	anEvent wasHandled: true.	anEvent controlKeyPressed ifTrue:[^self invokeMetaMenu: anEvent].	"Make me modal during mouse transitions"	anEvent hand newMouseFocus: self event: anEvent.	anEvent blueButtonChanged ifTrue:[^self blueButtonDown: anEvent].	self mouseDown: anEvent.	anEvent hand removeHaloFromClick: anEvent on: self.	(self handlesMouseStillDown: anEvent) ifTrue:[		self startStepping: #handleMouseStillDown: 			at: Time millisecondClockValue + self mouseStillDownThreshold			arguments: {anEvent copy resetHandlerFields}			stepTime: 1].! !!Morph methodsFor: 'menu' stamp: 'nk 2/15/2004 09:08'!addGestureMenuItems: aMenu hand: aHandMorph	"If the receiver wishes the Genie menu items, add a line to the menu and then those Genie items, else do nothing"! !!Morph methodsFor: 'dispatching' stamp: 'nk 2/15/2004 09:16'!disableSubmorphFocusForHand: aHandMorph	"Check whether this morph or any of its submorph has the Genie focus.	If yes, disable it."! !!HandMorph methodsFor: 'events-processing' stamp: 'nk 2/15/2004 09:01'!isCapturingGesturePoints	^false! !!HandMorph methodsFor: 'focus handling' stamp: 'nk 2/14/2004 18:44'!mouseFocus: aMorphOrNil	mouseFocus _ aMorphOrNil! !!HandMorph methodsFor: 'initialization' stamp: 'nk 2/14/2004 18:28'!interrupted	"Something went wrong - we're about to bring up a debugger. 	Release some stuff that could be problematic."	self releaseAllFoci. "or else debugger might not handle clicks"! !!PluggableTextMorph methodsFor: 'initialization' stamp: 'nk 2/14/2004 18:19'!initialize	"initialize the state of the receiver"	super initialize.	hasUnacceptedEdits _ false.	hasEditingConflicts _ false.	askBeforeDiscardingEdits _ true.! !!TextMorph methodsFor: 'copying' stamp: 'nk 2/14/2004 21:00'!veryDeepInner: deepCopier 	"Copy all of my instance variables. Some need to be not copied at all, but shared.	Warning!!!! Every instance variable defined in this class must be handled.	We must also implement veryDeepFixupWith:.  See DeepCopier class comment."	super veryDeepInner: deepCopier.	textStyle _ textStyle veryDeepCopyWith: deepCopier.	text _ text veryDeepCopyWith: deepCopier.	wrapFlag _ wrapFlag veryDeepCopyWith: deepCopier.	paragraph _ paragraph veryDeepCopyWith: deepCopier.	editor _ editor veryDeepCopyWith: deepCopier.	container _ container veryDeepCopyWith: deepCopier.	predecessor _ predecessor.	successor _ successor.	backgroundColor _ backgroundColor veryDeepCopyWith: deepCopier.	margins _ margins veryDeepCopyWith: deepCopier.! !!TheWorldMenu methodsFor: 'menu' stamp: 'nk 2/15/2004 09:38'!addGestureHelpItemsTo: aMenuMorph ! !!SARInstaller methodsFor: 'fileIn' stamp: 'nk 3/6/2004 08:54'!installAllMembers	"Try to install all the members, in order, based on their filenames and/or contents."	| uninstalled genieSuffix |	genieSuffix _ Smalltalk		at: #CRDictionary		ifPresent: [ :crDictionary | crDictionary fileNameSuffix ].	uninstalled _ OrderedCollection new.	zip members do: [ :member | | memberName extension isGraphic stream |		memberName _ member fileName.		extension _ (FileDirectory extensionFor: memberName) asLowercase.		extension caseOf: {			[ Project projectExtension ] -> [ self fileInProjectNamed: memberName createView: true ].			[ genieSuffix ] -> [ self fileInGenieDictionaryNamed: memberName ].			[ 'st' ] -> [ self fileInPackageNamed: memberName ].			[ 'cs' ] -> [  self fileInMemberNamed: memberName  ].			[ 'mc' ] -> [ self fileInMonticelloPackageNamed: memberName ].			[ 'mcv' ] -> [ self fileInMonticelloVersionNamed: memberName ].			[ 'mcz' ] -> [ self fileInMonticelloZipVersionNamed: memberName ].			[ 'morph' ] -> [ self fileInMorphsNamed: member addToWorld: true ].			[ 'ttf' ] -> [ self fileInTrueTypeFontNamed: memberName ].		} otherwise: [			('t*xt' match: extension) ifTrue: [ self openTextFile: memberName ]				ifFalse: [ stream _ member contentStream.			isGraphic _ ImageReadWriter understandsImageFormat: stream.			stream reset.			isGraphic				ifTrue: [ self openGraphicsFile: member ]				ifFalse: [ "now what?" ]]		]	].	uninstalled _ self uninstalledMembers.	uninstalled isEmpty ifTrue: [ ^self ].	uninstalled inspect.! !EventHandler removeSelector: #gesture:fromMorph:!EventHandler removeSelector: #gestureRecipient!EventHandler removeSelector: #gestureSelector!EventHandler removeSelector: #hasGestureRecipient!!EventHandler reorganize!('access' allRecipients firstMouseSelector messageList methodRefList mouseDownSelector mouseStillDownRecipient mouseStillDownSelector mouseUpSelector)('copying' veryDeepFixupWith: veryDeepInner:)('events' click:fromMorph: doubleClick:fromMorph: doubleClickTimeout:fromMorph: keyStroke:fromMorph: mouseDown:fromMorph: mouseEnter:fromMorph: mouseEnterDragging:fromMorph: mouseLeave:fromMorph: mouseLeaveDragging:fromMorph: mouseMove:fromMorph: mouseStillDown:fromMorph: mouseUp:fromMorph: send:to:withEvent:fromMorph: startDrag:fromMorph:)('fixups' fixAlansOldEventHandlers fixReversedValueMessages replaceSendsIn:with:)('initialization' adaptToWorld: forgetDispatchesTo: on:send:to: on:send:to:withValue: onGestureSend:to:)('objects from disk' convertToCurrentVersion:refStream:)('printing' printOn:)('testing' handlesClickOrDrag: handlesGestureStart: handlesKeyboard: handlesMouseDown: handlesMouseMove: handlesMouseOver: handlesMouseOverDragging: handlesMouseStillDown:)!"Postscript:Clean up unneeded instance variables."| pkg |SystemOrganization renameCategory: 'Tests-Genie-Engine' toBe: 'Genie-Tests'.EventHandler removeInstVarName: 'gestureSelector'.EventHandler removeInstVarName: 'gestureRecipient'.EventHandler removeInstVarName: 'gestureDictionaryOrName'.TextMorph removeInstVarName: 'lastGesture'.pkg _ PackageInfo named: 'Genie'.pkg extensionMethods do: [ :mr | mr setClassAndSelectorIn: [ :cls :sel | cls removeSelector: sel ]].CRDictionaryMorph allInstancesDo: [ :m | m owner ifNotNil: [ m delete ]].PluggableCollectionMorph allInstancesDo: [ :m | m owner ifNotNil: [ m delete ]].pkg classes do: [ :cls | (cls inheritsFrom: Morph) ifTrue: [ cls allInstancesDo: [ :m | m owner ifNotNil: [ m delete ]]].  cls removeFromSystem ].CommandHistory allInstancesDo: [ :ch | ch resetCommandHistory ].Smalltalk garbageCollect.Smalltalk removeEmptyMessageCategories.HandMorph removeInstVarName: 'genieGestureProcessor'.!