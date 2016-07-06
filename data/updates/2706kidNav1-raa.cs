'From Squeak2.9alpha of 17 July 2000 [latest update: #2714] on 26 September 2000 at 2:30:24 pm'!"Change Set:		kidNav1Date:			19 September 2000Author:			Bob Arning- a different navigator for Kim, BJ and the kids. set the preference eToyFriendly to true to use this one- 26 sept 2000 - require mouseUp rather than enter/leave to pop-up/down"!AlignmentMorphBob1 subclass: #ProjectNavigationMorph	instanceVariableNames: 'mouseInside '	classVariableNames: 'LastManualPlacement '	poolDictionaries: ''	category: 'Morphic-Windows'!ProjectNavigationMorph subclass: #KidNavigationMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Windows'!!AlignmentMorphBob1 methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 13:28'!fixLayout	| nextY newBnds r deltaY |	submorphs size < 1 ifTrue: [^super fixLayout].	heights ifNil: [^super fixLayout].	"conversion aid"	submorphs size = heights size ifFalse: [^super fixLayout].	r _ self innerBounds.	nextY _ r top.	submorphs with: heights do: [ :sub :ht |		deltaY _ ht < 1 ifTrue: [(r height * ht) truncated] ifFalse: [ht].		newBnds _ r left @ nextY corner: (r right @ (nextY + deltaY min: r bottom)).		newBnds = sub bounds ifFalse: [sub bounds: newBnds].		nextY _ newBnds bottom.	].! !!Project methodsFor: 'menu messages' stamp: 'RAA 9/19/2000 14:17'!finalEnterActions	| navigator armsLengthCmd navType |	navType _ ProjectNavigationMorph preferredNavigator.	armsLengthCmd _ self parameterAt: #armsLengthCmd ifAbsent: [nil].	navigator _ world findA: navType.	Preferences showProjectNavigator & navigator isNil ifTrue: [		(navigator _ navType new)			bottomLeft: world bottomLeft;			openInWorld: world.	].	navigator notNil & armsLengthCmd notNil ifTrue: [		navigator color: Color lightBlue	].	armsLengthCmd notNil ifTrue: [		armsLengthCmd openInWorld: world	].! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 13:31'!addButton: aString balloonText: anotherString for: aSymbol	| b a |	b _ self makeButton: aString balloonText: anotherString for: aSymbol.	a _ AlignmentMorph newColumn.	a color: Color transparent; borderWidth: 0; inset: 2.	a hResizing: #shrinkWrap; vResizing: #shrinkWrap.	a addMorphBack: b.	self addMorphBack: a.! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/20/2000 12:43'!addButtons	self addARow: {		(self inAColumn: {self buttonNewProject}) inset: 3.		self inAColumn: {self buttonPrev}.		self inAColumn: {self buttonGoTo}.		self inAColumn: {self buttonNext}.		self inAColumn: {self buttonPublish}.		"self inAColumn: {self buttonNewer}."		"self inAColumn: {self buttonTell}."		self inAColumn: {self buttonFind}.		"self inAColumn: {self buttonFullScreen}."		"self inAColumn: {self buttonFlaps}."		"self inAColumn: {self buttonShare}."		self inAColumn: {self buttonQuit}.	}.! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 14:07'!amountToShowWhenSmall	^8! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/20/2000 12:33'!colorForButtons	^Color r: 0.613 g: 0.71 b: 1.0 ! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/20/2000 12:54'!currentNavigatorVersion	^9		"since these guys get saved, we fix them up if they are older versions"! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/20/2000 12:54'!fontForButtons	^((TextStyle named: #ComicBold) fontOfSize: 12) emphasized: 1! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/20/2000 12:28'!makeButton: aString balloonText: anotherString for: aSymbol	^SimpleButtonDelayedMenuMorph new 		target: self;		borderColor: #raised;		color: self colorForButtons;		label: aString font: self fontForButtons;		setBalloonText: anotherString;		actionSelector: aSymbol.! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 14:07'!positionVertically	| wb stickToTop |	wb _ self worldBounds.	stickToTop _ self valueOfProperty: #stickToTop.	stickToTop ifNil: [		stickToTop _ (self top - wb top) abs < (self bottom - wb bottom) abs.		self setProperty: #stickToTop toValue: stickToTop.	].	mouseInside == true ifTrue: [		stickToTop ifTrue: [			self top: wb top		] ifFalse: [			self bottom: wb bottom		].	] ifFalse: [		stickToTop ifTrue: [			self bottom: wb top + self amountToShowWhenSmall		] ifFalse: [			self top: wb bottom - self amountToShowWhenSmall		].	].! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/20/2000 12:40'!buttonFind	^self makeButton: 'FIND' balloonText: 'Find a project' for: #findAProject! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:42'!buttonFlaps	^self inFlapsSuppressedMode ifTrue: [		self makeButton: 'Show tabs' balloonText: 'Show tabs' for: #toggleFlapsSuppressed	] ifFalse: [		self makeButton: 'Hide tabs' balloonText: 'Hide tabs' for: #toggleFlapsSuppressed	].! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:41'!buttonFullScreen	^self inFullScreenMode ifTrue: [		self makeButton: 'Browser Reentry' balloonText: 'Re-enter the browser' for: #fullScreenOff	] ifFalse: [		self makeButton: 'Escape Browser' balloonText: 'Use the full screen' for: #fullScreenOn	]! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:53'!buttonGoTo	^self makeButton: 'GO TO' balloonText: 'Go to another project' for: #gotoAnother! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:54'!buttonNewProject	^self makeButton: 'NEW PROJECT' balloonText: 'Start a new project' for: #newProject! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:37'!buttonNewer	^self makeButton: 'Newer?' balloonText: 'Is there a newer version of this project ?' for: #getNewerVersionIfAvailable! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 14:04'!buttonNext	^self makeButton: 'NEXT >' balloonText: 'Next project' for: #nextProject! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 14:04'!buttonPrev	^self makeButton: '< PREV' balloonText: 'Previous project' for: #previousProject! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/20/2000 12:41'!buttonPublish	^self makeButton: 'PUBLISH IT!!' balloonText: 'Publish this project. Save it where it came from (server, hard disk, etc.) ' for: #publishProject! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 14:03'!buttonQuit	^self makeButton: 'QUIT' balloonText: 'Quit Squeak altogether' for: #quitSqueak! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:43'!buttonShare	^self makeButton: 'Share' 		balloonText: 'Share this project so that others can explore it with you.' 		for: #shareThisWorld! !!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'RAA 9/19/2000 13:38'!buttonTell	^self makeButton: 'Tell!!' balloonText: 'Tell a friend about this project' for: #tellAFriend! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'RAA 9/19/2000 14:12'!gotoAnother	EToyProjectHistoryMorph new openInWorld! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'RAA 9/19/2000 14:11'!newProject	(ProjectViewMorph newMorphicProjectOn: nil) openInWorld.! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'RAA 9/19/2000 14:14'!quitSqueak	(self confirm: 'REALLY quit Squeak?') ifFalse: [^self].	Smalltalk snapshot: false andQuit: true.! !!ProjectNavigationMorph methodsFor: 'object fileIn' stamp: 'RAA 9/19/2000 13:38'!convertbosfcebbochvimlpm0: varDict bosfcebbochvimlphmmm0: smartRefStrm	"These variables are automatically stored into the new instance #('bounds' 'owner' 'submorphs' 'fullBounds' 'color' 'extension' 'borderWidth' 'borderColor' 'orientation' 'centering' 'hResizing' 'vResizing' 'inset' 'minCellSize' 'layoutNeeded' 'priorFullBounds' 'mouseInside').	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('heights' 'minWidth' 'minHeight')  If a non-nil value is needed, please assign it."! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 14:05'!addButtons	self addARow: {		self inAColumn: {self buttonPrev}.		self inAColumn: {self buttonGoTo}.		self inAColumn: {self buttonNext}.	}.	self addARow: {		self inAColumn: {self buttonNewProject}.	}.	self addARow: {		self inAColumn: {self buttonPublish}.	}.	self addARow: {		self inAColumn: {self buttonFind}.		self inAColumn: {self buttonQuit}.	}.	"self addARow: {	}.	self addARow: {	}."		"self inAColumn: {self buttonNewer}.		self inAColumn: {self buttonTell}.		self inAColumn: {self buttonFullScreen}.		self inAColumn: {self buttonFlaps}.		self inAColumn: {self buttonShare}."! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 14:29'!amountToShowWhenSmall	^39! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 13:56'!colorForButtons	^Color r: 0.613 g: 0.71 b: 1.0 ! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 14:01'!fontForButtons	^(TextStyle named: #ComicBold) fontOfSize: 18! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 12:29'!handlesMouseDown: evt	^true	! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 14:27'!initialize	| fill |	super initialize.	inset _ 12.	fill _ GradientFillStyle ramp: {		0.0->(Color r: 0.032 g: 0.0 b: 0.484).		1.0->(Color r: 0.194 g: 0.032 b: 1.0)	}.	fill origin: self bounds topLeft.	fill direction: 0@200.	fill radial: false.	self fillStyle: fill.	self removeAllMorphs.	self addButtons.! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 12:28'!mouseEnter: evt	"kid nav doesn't care"	! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 12:28'!mouseLeave: evt	"kid nav doesn't care"	! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/26/2000 12:30'!mouseUp: evt	mouseInside _ (mouseInside ifNil: [false]) not.	self positionVertically	! !!ProjectNavigationMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 9/19/2000 14:16'!preferredNavigator	Preferences eToyFriendly ifTrue: [^KidNavigationMorph].	^ProjectNavigationMorph! !!ProjectNavigationMorph reorganize!('as yet unclassified' addButton:balloonText:for: addButtons amountToShowWhenSmall color: colorForButtons currentNavigatorVersion fontForButtons handlesMouseOver: inFlapsSuppressedMode inFullScreenMode initialize justDroppedInto:event: makeButton:balloonText:for: morphicLayerNumber mouseEnter: mouseLeave: openInWorld: positionVertically showMenuFor:event: step stepTime wantsSteps)('the buttons' buttonFind buttonFlaps buttonFullScreen buttonGoTo buttonNewProject buttonNewer buttonNext buttonPrev buttonPublish buttonQuit buttonShare buttonTell)('the actions' doFindButtonMenuEvent: doPublishButtonMenuEvent: findAProject findAnything fullScreenOff fullScreenOn getNewerVersionIfAvailable gotoAnother newProject nextProject previousProject publishDifferent publishProject quitSqueak shareThisWorld tellAFriend toggleFlapsSuppressed)('object fileIn' convertbosfcebbochvimlp0:bosfcebbochvimlpm0: convertbosfcebbochvimlpm0:bosfcebbochvimlphmmm0:)!