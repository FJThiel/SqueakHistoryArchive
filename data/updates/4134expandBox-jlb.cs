'From Squeak3.1alpha of 28 February 2001 [latest update: #4103] on 5 June 2001 at 12:23:53 am'!"Change Set:		expandWindow-jlbDate:			5 June 2001Author:			Jim BensonAdd another button to the system window frame to expand/collapse a window fullscreen (Morphic Only).  Tweaked by Scott Wallace."!MorphicModel subclass: #SystemWindow	instanceVariableNames: 'labelString stripes label closeBox collapseBox activeOnlyOnTop paneMorphs paneRects collapsedFrame fullFrame isCollapsed menuBox mustNotClose labelWidgetAllowance updatablePanes allowReframeHandles labelArea expandBox '	classVariableNames: 'TopWindow '	poolDictionaries: ''	category: 'Morphic-Windows'!!SystemWindow methodsFor: 'initialization' stamp: 'jlb 5/30/2001 00:42'!addExpandBox	"If I have a labelArea, add a close box to it"	| frame |	labelArea ifNil: [^ self].	expandBox _ IconicButton new borderWidth: 0;			labelGraphic: (ScriptingSystem formAtKey: 'expandBox'); color: Color transparent; 			actWhen: #buttonDown;			actionSelector: #expandBoxHit; target: self;			setBalloonText: 'expand to full screen'.	frame _ LayoutFrame new.	frame leftFraction: 1; leftOffset: -30; topFraction: 0; topOffset: 0.	expandBox layoutFrame: frame.	labelArea addMorph: expandBox.! !!SystemWindow methodsFor: 'initialization' stamp: 'sw 5/30/2001 14:48'!initialize	"Initialize a system window.  Add label, stripes, etc., if desired"	| aFont |	super initialize.	allowReframeHandles := true.	labelString ifNil: [labelString _ 'Untitled Window'].	isCollapsed _ false.	activeOnlyOnTop _ true.	paneMorphs _ Array new.	borderColor _ Color black.	borderWidth _ 1.	color _ Color black.	self layoutPolicy: ProportionalLayout new.	self wantsLabel ifTrue:		[label _ StringMorph new contents: labelString;				font: Preferences windowTitleFont emphasis: 1.		"Add collapse box so #labelHeight will work"		aFont _ Preferences standardButtonFont.		collapseBox _ SimpleButtonMorph new borderWidth: 0;				label: 'O' font: aFont; color: Color transparent;				actionSelector: #collapseOrExpand; target: self; extent: 14@14;				setBalloonText: 'collapse this window'.		stripes _ Array with: (RectangleMorph newBounds: bounds)  "see extent:"					with: (RectangleMorph newBounds: bounds).		self addLabelArea.		labelArea addMorph: (stripes first borderWidth: 1).		labelArea addMorph: (stripes second borderWidth: 2).		self setLabelWidgetAllowance.		self addCloseBox.		self addMenuControl.		labelArea addMorph: label.		self wantsExpandBox ifTrue: [self addExpandBox].		labelArea addMorph: collapseBox.		self setFramesForLabelArea.		label on: #mouseDown send: #relabelEvent: to: self.		Preferences noviceMode ifTrue:			[closeBox ifNotNil: [closeBox setBalloonText: 'close window'].			menuBox ifNotNil: [menuBox setBalloonText: 'window menu'].			collapseBox ifNotNil: [collapseBox setBalloonText: 'collapse/expand window']]].	self on: #mouseEnter send: #spawnReframeHandle: to: self.	self on: #mouseLeave send: #spawnReframeHandle: to: self.	self extent: 300@200.	mustNotClose _ false.	updatablePanes _ Array new.! !!SystemWindow methodsFor: 'initialization' stamp: 'jlb 5/29/2001 23:24'!maximumExtent	"This returns the maximum extent that the morph may be expanded to.	Return nil if this property has not been set."	^ self valueOfProperty: #maximumExtent! !!SystemWindow methodsFor: 'initialization' stamp: 'jlb 5/29/2001 23:24'!maximumExtent: aPoint	"This returns the maximum extent that the morph may be expanded to.	Return nil if this property has not been set."	^ self setProperty: #maximumExtent toValue: aPoint! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'sw 5/30/2001 10:47'!addExpandBoxIfMissingAndWanted	"Add an expand box if I don't have one and yet would like one"	expandBox ifNil: [self wantsExpandBox ifTrue: [self addExpandBox]]! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'sw 6/5/2001 00:19'!collapseOrExpand	"Collapse or expand the window, depending on existing state"	isCollapsed		ifTrue: 			["Expand -- restore panes to morphics structure"			isCollapsed _ false.			collapsedFrame _ self getBoundsWithFlex.			"First save latest collapsedFrame"			self setBoundsWithFlex: fullFrame.			paneMorphs				reverseDo: 					[:m | 					self addMorph: m.					self world startSteppingSubmorphsOf: m].			self activate "-- mainly for findWindow"]		ifFalse: 			["Collapse -- remove panes from morphics structure"			isCollapsed _ true.			fullFrame _ self getBoundsWithFlex.			"First save latest fullFrame"			paneMorphs do: [:m | m delete; releaseCachedState].			model modelSleep.			collapsedFrame ifNil:				[collapsedFrame _ RealEstateAgent assignCollapseFrameFor: self].			self setBoundsWithFlex: collapsedFrame.			expandBox ifNotNil: [expandBox setBalloonText: 'expand to full screen']].	self layoutChanged! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'jlb 5/30/2001 00:47'!expandBoxHit	"The full screen expand box has been hit"	isCollapsed		ifTrue: [self hide.			self collapseOrExpand.			self unexpandedFrame ifNil: [ self unexpandedFrame: fullFrame. ].			self fullScreen.			expandBox setBalloonText: 'contract to original size'.			^ self show].	self unexpandedFrame		ifNil: [self unexpandedFrame: fullFrame.			self fullScreen.			expandBox setBalloonText: 'contract to original size']		ifNotNil: [self bounds: self unexpandedFrame.			self unexpandedFrame: nil.			expandBox setBalloonText: 'expand to full screen' ]! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'jlb 5/29/2001 23:06'!unexpandedFrame	"Return the frame size of an unexpanded window"	^ self valueOfProperty: #unexpandedFrame! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'jlb 5/29/2001 23:07'!unexpandedFrame: aRectangle	"Set the frame size of an unexpanded window"	^ self setProperty: #unexpandedFrame toValue: aRectangle! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'sw 5/30/2001 10:56'!wantsExpandBox	"Answer whether I'd like an expand box"	^ true! !!SystemWindow methodsFor: 'menu' stamp: 'jlb 5/29/2001 23:22'!fullScreenMaximumExtent	"Zoom Window to Full World size with possible DeskMargins	obey the maximum extent rules"		| left right possibleBounds |	left _ right _ 0.	self paneMorphs		do: [:pane | ((pane isKindOf: ScrollPane)					and: [pane retractableScrollBar])				ifTrue: [pane scrollBarOnLeft						ifTrue: [left _ left max: pane scrollbarWidth]						ifFalse: [right _ right max: pane scrollbarWidth]]].	possibleBounds _ self worldBounds				insetBy: (left @ 0 corner: right @ 0).	self maximumExtent ifNotNil:		[possibleBounds _ possibleBounds origin extent: ( self maximumExtent min: ( possibleBounds extent ))].	((Flaps sharedFlapsAllowed				and: [CurrentProjectRefactoring currentFlapsSuppressed not])			or: [Preferences fullScreenLeavesDeskMargins])		ifTrue: [possibleBounds _ possibleBounds insetBy: 22].	self bounds: possibleBounds! !!PartsWindow methodsFor: 'as yet unclassified' stamp: 'sw 6/5/2001 00:19'!wantsExpandBox	"Answer whether I'd like an expand box"	^ false! !!SystemWindowWithButton methodsFor: 'as yet unclassified' stamp: 'sw 5/30/2001 11:11'!wantsExpandBox	"Answer whether I'd like an expand box"	^ false! !"Postscript:	The postscript adds an expand box form to the Scripting System's form dictionary, and fixes up all existing SystemWindows."ScriptingSystem saveForm: (Form	extent: 10@10	depth: 16	fromArray: #( 65537 65537 65537 0 0 65536 0 1 0 0 65536 0 1 65537 65537 65536 0 1 0 1 65536 0 1 0 1 65537 65537 65537 0 1 0 65536 0 0 1 0 65536 0 0 1 0 65536 0 0 1 0 65537 65537 65537 65537)	offset: 0@0) atKey: 'expandBox'.SystemWindow allInstancesDo: [:w | w addExpandBoxIfMissingAndWanted]!