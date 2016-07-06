'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 15 April 2003 at 4:16:03 pm'!"Change Set:		KCP-0045-SystDictionDate:			15 April 2003Author:			stephane ducasseNaively we introduce SystemNavigation new everywhere in the browsers while it would have been really smarter to have self systemNavigation so that browsers can customize this methods after. This changeset introduce that. We did not want to reproduce all the changeset so this one fixes that problem."!!StringHolder methodsFor: 'message list menu' stamp: 'sd 4/15/2003 16:11'!browseClassVarRefs	"1/17/96 sw: devolve responsibility to the class, so that the code that does the real work can be shared"	| cls |	cls _ self selectedClass.	cls ifNotNil: [self systemNavigation  browseClassVarRefs: cls]! !!StringHolder methodsFor: 'message list menu' stamp: 'sd 4/15/2003 16:11'!browseClassVariables	"Browse the class variables of the selected class. 2/5/96 sw"	| cls |	cls _ self selectedClass.	cls		ifNotNil: [self systemNavigation  browseClassVariables: cls]! !!StringHolder methodsFor: 'message list menu' stamp: 'sd 4/15/2003 16:11'!browseInstVarRefs	"1/26/96 sw: real work moved to class, so it can be shared"	| cls |	cls _ self selectedClassOrMetaClass.	cls		ifNotNil: [self systemNavigation  browseInstVarRefs: cls]! !!StringHolder methodsFor: 'message list menu' stamp: 'sd 4/15/2003 16:11'!selectMessageAndEvaluate: aBlock	"Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector.  If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any"	| selector method messages |	(selector _ self selectedMessageName) ifNil: [^ self].	method _ (self selectedClassOrMetaClass ifNil: [^ self])		compiledMethodAt: selector		ifAbsent: [].	(method isNil or: [(messages _ method messages) size == 0])		 ifTrue: [^ aBlock value: selector].	(messages size == 1 and: [messages includes: selector])		ifTrue:			[^ aBlock value: selector].  "If only one item, there is no choice"	self systemNavigation 		showMenuOf: messages		withFirstItem: selector		ifChosenDo: [:sel | aBlock value: sel]! !!StringHolder methodsFor: 'system navigation' stamp: 'sd 4/15/2003 16:07'!systemNavigation	^ SystemNavigation new! !!Browser methodsFor: 'message functions' stamp: 'sd 4/15/2003 16:12'!removeMessage	"If a message is selected, create a Confirmer so the user can verify that  	the currently selected message should be removed from the system. If 	so,  	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 	confirmer is bypassed."	| messageName confirmation |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	confirmation _ self systemNavigation   confirmRemovalOf: messageName on: self selectedClassOrMetaClass.	confirmation == 3		ifTrue: [^ self].	self selectedClassOrMetaClass removeSelector: self selectedMessageName.	self messageListIndex: 0.	self changed: #messageList.	self setClassOrganizer.	"In case organization not cached"	confirmation == 2		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !!ChangeSorter methodsFor: 'message list' stamp: 'sd 4/15/2003 16:13'!removeMessage	"Remove the selected msg from the system. Real work done by the 	parent, a ChangeSorter"	| confirmation sel |	self okToChange		ifFalse: [^ self].	currentSelector		ifNotNil: [confirmation _ self systemNavigation   confirmRemovalOf: (sel _ self selectedMessageName) on: self selectedClassOrMetaClass.			confirmation == 3				ifTrue: [^ self].			self selectedClassOrMetaClass removeSelector: sel.			self update.			confirmation == 2				ifTrue: [Smalltalk browseAllCallsOn: sel]]! !!FileContentsBrowser methodsFor: 'removing' stamp: 'sd 4/15/2003 16:13'!removeMessage	| messageName |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	(self systemNavigation  confirmRemovalOf: messageName on: self selectedClassOrMetaClass)		ifFalse: [^ false].	self selectedClassOrMetaClass removeMethod: self selectedMessageName.	self messageListIndex: 0.	self setClassOrganizer.	"In case organization not cached"	self changed: #messageList! !!Inspector methodsFor: 'menu commands' stamp: 'sd 4/15/2003 16:14'!classVarRefs	"Request a browser of methods that store into a chosen instance variable"	| aClass |	(aClass _ self classOfSelection) ifNotNil:		[self systemNavigation  browseClassVarRefs: aClass].! !!Inspector methodsFor: 'menu commands' stamp: 'sd 4/15/2003 16:14'!defsOfSelection	"Open a browser on all defining references to the selected instance variable, if that's what currently selected. "	| aClass sel |	self selectionUnmodifiable ifTrue: [^ self changed: #flash].	(aClass _ self object class) isVariable ifTrue: [^ self changed: #flash].	sel _ aClass allInstVarNames at: self selectionIndex - 2.	self systemNavigation  browseAllStoresInto: sel from: aClass! !!Inspector methodsFor: 'menu commands' stamp: 'sd 4/15/2003 16:14'!referencesToSelection	"Open a browser on all references to the selected instance variable, if that's what currently selected.  1/25/96 sw"	| aClass sel |	self selectionUnmodifiable ifTrue: [^ self changed: #flash].	(aClass _ self object class) isVariable ifTrue: [^ self changed: #flash].	sel _ aClass allInstVarNames at: self selectionIndex - 2.	self systemNavigation   browseAllAccessesTo: sel from: aClass! !!MessageSet methodsFor: 'message functions' stamp: 'sd 4/15/2003 16:12'!removeMessage	"Remove the selected message from the system. 1/15/96 sw"	| messageName confirmation |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	confirmation _ self systemNavigation  confirmRemovalOf: messageName on: self selectedClassOrMetaClass.	confirmation == 3		ifTrue: [^ self].	self selectedClassOrMetaClass removeSelector: messageName.	self deleteFromMessageList: self selection.	self reformulateList.	confirmation == 2		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !!Lexicon methodsFor: 'new-window queries' stamp: 'sd 4/15/2003 16:12'!browseClassVarRefs	"Let the search pertain to the target class regardless of selection"	self systemNavigation  browseClassVarRefs: targetClass theNonMetaClass ! !!Lexicon methodsFor: 'new-window queries' stamp: 'sd 4/15/2003 16:12'!browseInstVarRefs	"Let the search pertain to the target class regardless of selection"	self systemNavigation  browseInstVarRefs: targetClass! !!SystemNavigation methodsFor: 'browse' stamp: 'sd 4/15/2003 16:08'!browseInstVarDefs: aClass	"Copied from browseInstVarRefs.  Should be consolidated some day. 7/29/96 di	7/30/96 sw: did the consolidation"	"Change to use SystemNavigation  27 March 2003 sd"	aClass chooseInstVarThenDo:			[:aVar | self browseAllStoresInto: aVar from: aClass]! !!SystemNavigation methodsFor: 'browse' stamp: 'sd 4/15/2003 16:08'!browseInstVarRefs: aClass	"1/16/96 sw: moved here from Browser so that it could be used from a variety of places.	 7/30/96 sw: call chooseInstVarThenDo: to get the inst var choice"	aClass chooseInstVarThenDo: 		[:aVar | self browseAllAccessesTo: aVar from: aClass]! !