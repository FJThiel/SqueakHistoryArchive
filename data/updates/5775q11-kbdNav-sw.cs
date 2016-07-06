'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:53 pm'!
"Change Set:		q11-kbdNav-sw
Date:			3 March 2004
Author:			Scott Wallace

Merges Squeakland updates 0169kbdProjectNav-sw and the (presumed) 0194modularCmdKeys-sw.

Integrated with Squeak 3.7a/5764 (there were  collisions with babel work).

Allows a thread to be navigated from the keyboard, using:

NEXT:	space right-arrow down-arrow.
PREV:	backspace, left-arrow, up-arrow
FIRST:	Home
LAST:	End

Menu items are added to the main ThreadNavigatorMorph's menu to associate the thread with keystroke handling.

Once set up, the keyboard handling remains in place as you navigate from project to project.

Makes the handling of desktop command keys modular.  Users can modify the handling of desktop command keys in a non-invasive way, and individual projects can define their own custom handling for desktop command keys."!


!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 13:52'!
commandKeySelectors
	"Answer my command-key table"

	| aDict |
	aDict _ self valueOfProperty: #commandKeySelectors ifAbsentPut: [self initializeDesktopCommandKeySelectors].
	^ aDict! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 2/23/2004 18:33'!
defaultDesktopCommandKeyTriplets
	"Answer a list of triplets of the form
		<key> <receiver> <selector>   [+ optional fourth element, a <description> for use in desktop-command-key-help]
that will provide the default desktop command key handlers.  If the selector takes an argument, that argument will be the command-key event"

	^ {
		{ $b.	Browser.						#openBrowser.							'Open a new System Browser'}.
		{ $k.	Workspace.						#open.									'Open a new, blank Workspace'}.
		{ $m.	self.							#putUpNewMorphMenu.					'Put up the "New Morph" menu'}.
		{ $o.	ActiveWorld.					#activateObjectsTool.						'Activate the "Objects Tool"'}.
		{ $r.	ActiveWorld.					#restoreMorphicDisplay.					'Redraw the screen'}.		
		{ $t.		self. 							#findATranscript:.						'Make a System Transcript visible'}.
		{ $w.	SystemWindow.					#closeTopWindow.						'Close the topmost window'}.
		{ $z.	self.							#undoOrRedoCommand.					'Undo or redo the last undoable command'}.

		{ $C.	self.							#findAChangeSorter:.					'Make a Change Sorter visible'}.
		{ $F.	CurrentProjectRefactoring.		#currentToggleFlapsSuppressed.			'Toggle the display of flaps'}.

		{ $L.	self.							#findAFileList:.							'Make a File List visible'}.
		{ $N.    self.							#toggleClassicNavigatorIfAppropriate.	'Show/Hide the classic Navigator, if appropriate'}.
		{ $P.	self.							#findAPreferencesPanel:.				'Activate the Preferences tool'}.
		{ $R.	self. 							#openRecentSubmissionsBrowser:	.		'Make a Recent Submissions browser visible'}.

		{ $W.	self. 							#findAMessageNamesWindow:.			'Make a MessageNames tool visible'}.
		{ $Z.	ChangeList. 						#browseRecentLog.			'Browse recently-logged changes'}.

		{ $\.	SystemWindow. 					#sendTopWindowToBack.					'Send the top window to the back'}.}! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 12:19'!
dispatchCommandKeyInWorld: aChar event: evt
	"Dispatch the desktop command key if possible.  Answer whether handled"

	| aMessageSend |
	aMessageSend _ self commandKeySelectors at: aChar ifAbsent: [^ false].
	aMessageSend selector numArgs = 0
		ifTrue:
			[aMessageSend value]
		ifFalse:
			[aMessageSend valueWithArguments: (Array with: evt)].
	^ true
! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 13:52'!
initializeDesktopCommandKeySelectors
	"Provide the starting settings for desktop command key selectors.  Answer the dictionary."

	"ActiveWorld initializeDesktopCommandKeySelectors"
	| dict messageSend |
	dict _ IdentityDictionary new.
	self defaultDesktopCommandKeyTriplets do:
		[:trip |
			messageSend _ MessageSend receiver: trip second selector: trip third.
			dict at: trip first put: messageSend].
	self setProperty: #commandKeySelectors toValue: dict.
	^ dict

! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 5/20/2003 15:04'!
keyboardNavigationHandler
	"Answer the receiver's existing keyboardNavigationHandler, or nil if none."

	| aHandler |
	aHandler _ self valueOfProperty: #keyboardNavigationHandler ifAbsent: [^ nil].
	(aHandler hasProperty: #moribund) ifTrue:  "got clobbered in another project"
		[self removeProperty: #keyboardNavigationHander.
		^ nil].
	^ aHandler! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/18/2003 23:10'!
keyboardNavigationHandler: aHandler
	"Set the receiver's keyboard navigation handler as indicated.  A nil argument means to remove the handler"

	aHandler
		ifNil:
			[self removeProperty: #keyboardNavigationHandler]
		ifNotNil:
			[self setProperty: #keyboardNavigationHandler toValue: aHandler]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 5/20/2003 14:59'!
keystrokeInWorld: evt
	"A keystroke was hit when no keyboard focus was in set, so it is sent here to the world instead."

	|  aChar isCmd ascii |
	aChar _ evt keyCharacter.
	(ascii _ aChar asciiValue) = 27 ifTrue: "escape key"
		[^ self putUpWorldMenuFromEscapeKey].
	(#(1 4 8 28 29 30 31 32) includes: ascii) ifTrue:  "home, end, backspace, arrow keys, space"
		[self keyboardNavigationHandler ifNotNilDo:
			[:aHandler | ^ aHandler navigateFromKeystroke: aChar]].

	isCmd _ evt commandKeyPressed and: [Preferences cmdKeysInText].
	(evt commandKeyPressed and: [Preferences eToyFriendly])
			ifTrue:
				[(aChar == $W) ifTrue: [^ self putUpWorldMenu: evt]].
	(isCmd and: [Preferences honorDesktopCmdKeys]) ifTrue:
		[self dispatchCommandKeyInWorld: aChar event: evt]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 11:51'!
putUpNewMorphMenu
	"Put up the New Morph menu in the world"

	TheWorldMenu new adaptToWorld: self; newMorph! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 13:56'!
respondToCommand: aCharacter bySending: aSelector to: aReceiver
	"Respond to the command-key use of the given character by sending the given selector to the given receiver.  If the selector is nil, retract any prior such setting"

	aSelector
		ifNil:
			[self commandKeySelectors removeKey: aCharacter]
		ifNotNil:
			[self commandKeySelectors at: aCharacter put: (MessageSend receiver: aReceiver selector: aSelector)]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 11:58'!
toggleClassicNavigatorIfAppropriate
	"If appropriate, toggle the presence of classic navigator"

	Preferences classicNavigatorEnabled ifTrue: [^ Preferences togglePreference: #showProjectNavigator]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/13/2003 12:25'!
undoOrRedoCommand
	"Undo or redo the last command recorded in the world"

	^ self commandHistory undoOrRedoCommand! !


!ThreadNavigationMorph methodsFor: 'navigation' stamp: 'sw 3/19/2003 01:08'!
navigateFromKeystroke: aChar
	"A character was typed in an effort to do interproject navigation along the receiver's thread"

	| ascii |
	ascii _ aChar asciiValue.
	(#(29 31 32) includes: ascii) ifTrue: [^ self nextPage].  "right arrow, down arrow, space"
	(#(8 28 30) includes: ascii) ifTrue: [^ self previousPage].  "left arrow, up arrow, backspace"
	(#(1) includes: ascii) ifTrue: [^ self firstPage].
	(#(4) includes: ascii) ifTrue: [^ self lastPage].
	self beep.! !


!InternalThreadNavigationMorph methodsFor: 'navigation' stamp: 'sw 3/3/2004 16:58'!
destroyThread
	"Manually destroy the thread"

	(self confirm: ('Destroy thread <{1}> ?' translated format:{threadName})) ifFalse: [^ self].
	self class knownThreads removeKey: threadName ifAbsent: [].
	self setProperty: #moribund toValue: true.  "In case pointed to in some other project"
	ActiveWorld keyboardNavigationHandler == self ifTrue:
		[self stopKeyboardNavigation]. 
	self delete! !

!InternalThreadNavigationMorph methodsFor: 'navigation' stamp: 'sw 3/3/2004 17:21'!
moreCommands
	"Put up a menu of options"

	| allThreads aMenu others target |
	allThreads _ self class knownThreads.
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'navigation' translated.
	aMenu addStayUpItem.
	self flag: #deferred.  "Probably don't want that stay-up item, not least because the navigation-keystroke stuff is not dynamically handled"
	others _ (allThreads keys reject: [ :each | each = threadName]) asSortedCollection.
	others do: [ :each |
		aMenu add: ('switch to <{1}>' translated format:{each}) selector: #switchToThread: argument: each].
	aMenu addList: {
		{'switch to recent projects' translated.  #getRecentThread}.
		#-.
		{'create a new thread' translated.  #threadOfNoProjects}.
		{'edit this thread' translated.  #editThisThread}.
		{'create thread of all projects' translated.  #threadOfAllProjects}.
		#-.
		{'First project in thread' translated.  #firstPage}.
		{'Last project in thread' translated.  #lastPage}}.
	(target _ self currentIndex + 2) > listOfPages size ifFalse:
		[aMenu 
			add: ('skip over next project ({1})' translated format:{(listOfPages at: target - 1) first})
			action: #skipOverNext].
	aMenu addList: {
		{'jump within this thread' translated.  #jumpWithinThread}.
		{'insert new project' translated.  #insertNewProject}.
		#-.
		{'simply close this navigator' translated.  #delete}.
		{'destroy this thread' destroyThread}.
		#-}.

	(ActiveWorld keyboardNavigationHandler == self)
		ifFalse:
			[aMenu add: 'start keyboard navigation with this thread' translated action: #startKeyboardNavigation]
		ifTrue:
			[aMenu add: 'stop keyboard navigation with this thread' translated action: #stopKeyboardNavigation].

	aMenu popUpInWorld! !

!InternalThreadNavigationMorph methodsFor: 'navigation' stamp: 'sw 3/18/2003 23:12'!
startKeyboardNavigation
	"Tell the active world to starting navigating via desktop keyboard navigation via me"

	ActiveWorld keyboardNavigationHandler: self! !

!InternalThreadNavigationMorph methodsFor: 'navigation' stamp: 'sw 3/18/2003 23:09'!
stopKeyboardNavigation
	"Cease navigating via the receiver in response to desktop keystrokes"

	ActiveWorld removeProperty: #keyboardNavigationHandler! !

!InternalThreadNavigationMorph methodsFor: 'private' stamp: 'sw 3/3/2004 17:03'!
loadPageWithProgress
	"Load the desired page, showing a progress indicator as we go"
	
	| projectInfo projectName beSpaceHandler |
	projectInfo _ listOfPages at: currentIndex.
	projectName _ projectInfo first.
	loadedProject _ Project named: projectName.
	self class know: listOfPages as: threadName.
	beSpaceHandler _ (ActiveWorld keyboardNavigationHandler == self).
	WorldState addDeferredUIMessage:
		[InternalThreadNavigationMorph openThreadNamed: threadName atIndex: currentIndex beKeyboardHandler: beSpaceHandler] fixTemps.

	loadedProject ifNil: [
		ComplexProgressIndicator new 
			targetMorph: self;
			historyCategory: 'project loading' translated;
			withProgressDo: [
				[
					loadedProject _ CurrentProjectRefactoring 
							currentFromMyServerLoad: projectName
				] 
					on: ProjectViewOpenNotification
					do: [ :ex | ex resume: false]		
						"we probably don't want a project view morph in this case"
			].
	].
	loadedProject ifNil: [
		^self inform: 'I cannot find that project' translated
	].
	self delete.

	loadedProject enter.
! !


!InternalThreadNavigationMorph class methodsFor: 'known threads' stamp: 'sw 3/18/2003 23:12'!
openThreadNamed: nameOfThread atIndex: anInteger beKeyboardHandler: aBoolean
	"Activate the thread of the given name, from the given index; set it up to be navigated via desktop keys if indicated"

	| coll nav |

	coll _ self knownThreads at: nameOfThread ifAbsent: [^self].
	nav _ World 
		submorphThat: [ :each | (each isKindOf: self) and: [each threadName = nameOfThread]]
		ifNone:
			[nav _ self basicNew.
			nav
				listOfPages: coll;
				threadName: nameOfThread index: anInteger;
				initialize;
				openInWorld;
				positionAppropriately.
			aBoolean ifTrue: [ActiveWorld keyboardNavigationHandler: nav].
			^ self].
	nav
		listOfPages: coll;
		threadName: nameOfThread index: anInteger;
		removeAllMorphs;
		addButtons.
	aBoolean ifTrue: [ActiveWorld keyboardNavigationHandler: nav]

! !


!InternalThreadNavigationMorph class reorganize!
('thumbnails' cacheThumbnailFor: clearThumbnailCache getThumbnailFor:)
('parts bin' descriptionForPartsBin)
('known threads' know:as: knownThreads openThreadNamed:atIndex: openThreadNamed:atIndex:beKeyboardHandler:)
('sorter' sorterFormForProject:sized:)
!


!InternalThreadNavigationMorph reorganize!
('initialization' addButtons defaultColor ensureSuitableDefaults)
('navigation' buttonForMenu deleteCurrentPage destroyThread editThisThread getRecentThread insertNewProject insertNewProjectActionFor: jumpToIndex: jumpWithinThread moreCommands myThumbnailSize positionAppropriately skipOverNext startKeyboardNavigation stopKeyboardNavigation switchToThread: threadName threadName:index: threadOfAllProjects threadOfNoProjects)
('sorting' acceptSortedContentsFrom: makeThumbnailForPageNumber:scaledToSize:default:)
('menu' showMenuFor:event:)
('stepping' step)
('piano rolls' triggerActionFromPianoRoll)
('private' currentIndex listOfPages: loadPageWithProgress)
('accessing' sizeRatio)
!


!ThreadNavigationMorph reorganize!
('initialization' addButtons colorForButtons defaultColor fontForButtons initialize makeButton:balloonText:for:)
('navigation' deleteCurrentPage ensureSuitableDefaults exitTheSequence firstPage lastPage navigateFromKeystroke: nextPage previousPage)
('stepping' step stepTime wantsSteps)
('buttons' buttonExit buttonFirst buttonForward buttonLast buttonPrevious)
('menu' showMenuFor:event:)
('private' currentIndex listOfPages: loadPage loadPageWithProgress morphicLayerNumber)
!

