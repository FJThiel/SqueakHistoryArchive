'From Squeak 2.0 BETA of May 8, 1998 on 15 May 1998 at 1:35:17 am'!"Change Set:		wodBrowseMessageFixesDate:			15 May 1998Author:			William O. DargelFix 'senders of...' and 'implementors of...' to work in a ChangeSorter when a removed method is selected. (such as '024jhmTweaksMay14 PolygonMorph borderColor:') - was giving a 'key not found' error.Make them work consistently by skipping the menu (with a single item) if the selected method doesn't send any messages. (How 'senders of...' was doing it). Refactor the #browseMessages and #browseSendersOfMessages messages of StringHolder, Browser and Debugger to put all the common functionality into a single method."!!StringHolder methodsFor: 'message list menu' stamp: 'wod 5/15/1998 00:54'!browseMessages	"Present a menu of all messages sent by the currently selected message. 	Open a message set browser of all implementors of the message chosen.	Do nothing if no message is chosen."	self selectMessageAndEvaluate: [:selector | Smalltalk browseAllImplementorsOf: selector]! !!StringHolder methodsFor: 'message list menu' stamp: 'wod 5/15/1998 00:54'!browseSendersOfMessages	"Present a menu of the currently selected message, as well as all	messages sent by it.  Open a message set browser of all implementors	of the message chosen."	self selectMessageAndEvaluate: [:selector | Smalltalk browseAllCallsOn: selector]! !!StringHolder methodsFor: 'message list menu' stamp: 'wod 5/15/1998 00:53'!selectMessageAndEvaluate: aBlock	"Present a menu of the currently selected message, as well as	all messages sent by it. Evalute aBlock with the selector of the 	message chosen. Do nothing if no message is chosen."	| selector method messages |	(selector _ self selectedMessageName) ifNil: [^ self].	method _ self selectedClassOrMetaClass 		compiledMethodAt: selector		ifAbsent: [].	(method isNil or: [(messages _ method messages) size == 0])		 ifTrue: [^ aBlock value: selector].	Smalltalk 		showMenuOf: messages		withFirstItem: selector		ifChosenDo: [:sel | aBlock value: sel]! !!Debugger methodsFor: 'context stack menu' stamp: 'wod 5/15/1998 00:24'!browseMessages	"Present a menu of all messages sent by the currently selected message.	Open a message set browser of all implementors of the message chosen.	Do nothing if no message is chosen."	contextStackIndex = 0 ifTrue: [^ self].	super browseMessages.! !!Debugger methodsFor: 'context stack menu' stamp: 'wod 5/15/1998 00:23'!browseSendersOfMessages	"Present a menu of the currently selected message, as well as all	messages sent by it.  Open a message set browser of all implementors	of the message chosen."	contextStackIndex = 0 ifTrue: [^ self].	super browseSendersOfMessages! !Browser removeSelector: #browseMessages!