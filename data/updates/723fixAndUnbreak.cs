'From Squeak 2.3 of January 14, 1999 on 12 March 1999 at 7:36:30 pm'!"Change Set:		fixAndUnbreakDate:			2 March 1999Author:			Scott Wallace(1)  Fixes bug that kept cmd-b, hit in the list pane of a browser, from opening to the actual method in the metaclass case.(2)  Unbreaks the code relating to multiple-selection for mvc-based changelist browsers that somehow involved a faulty fix (in in the updated called 'miscCleanups') by reverting to the code that it supplanted, which still has the bug that the faulty fix falsely purported to fix."!!PluggableListControllerOfMany methodsFor: 'as yet unclassified' stamp: 'tk 4/8/98 11:08'!redButtonActivity	| selection firstHit turningOn lastSelection pt scrollFlag |	model okToChange ifFalse: [^ self].		"Don't change selection if model refuses to unlock"	firstHit _ true.	scrollFlag _ false.	lastSelection _ 0.	[sensor redButtonPressed] whileTrue: 		[selection _ view findSelection: (pt _ sensor cursorPoint).		selection == nil ifTrue:  "Maybe out of box - check for auto-scroll"			[pt y < view insetDisplayBox top ifTrue:				[self scrollView: view list lineGrid.				scrollFlag _ true.				selection _ view firstShown].			pt y > view insetDisplayBox bottom ifTrue:				[self scrollView: view list lineGrid negated.				scrollFlag _ true.				selection _ view lastShown]].		(selection == nil or: [selection = lastSelection]) ifFalse: 			[firstHit ifTrue:				[firstHit _ false.				turningOn _ (model listSelectionAt: selection) not].			view selection: selection.			(model listSelectionAt: selection) == turningOn ifFalse:				[view displaySelectionBox.				model listSelectionAt: selection put: turningOn].			lastSelection _ selection]].	selection notNil ifTrue:		["Normal protocol delivers change, so unchange first (ugh)"		model listSelectionAt: selection put: (model listSelectionAt: selection) not.		self changeModelSelection: selection].	scrollFlag ifTrue: [self moveMarker]! !!StringHolder methodsFor: 'message list menu' stamp: 'sw 3/11/1999 15:54'!messageListKey: aChar from: view	"Respond to a Command key.  I am a model with a code pane, and I also have a listView that has a list of methods.  The view knows how to get the list and selection."	| sel class |	(class _ self selectedClassOrMetaClass) ifNil: [^ self arrowKey: aChar from: view].	sel _ self selectedMessageName.	aChar == $b ifTrue: [^ Browser fullOnClass: class selector: sel].	aChar == $N ifTrue: [^ self browseClassRefs].	sel ifNil: [^ self arrowKey: aChar from: view].	aChar == $m ifTrue: [^ Smalltalk browseAllImplementorsOf: sel].	aChar == $n ifTrue: [^ Smalltalk browseAllCallsOn: sel].	aChar == $v ifTrue: [^ self browseVersions].	^ self arrowKey: aChar from: view! !