'From Squeak3.4 of 1 March 2003 [latest update: #5170] on 7 April 2003 at 8:21:36 pm'!"Change Set:		KCP-0041-mvconfirmRemovalDate:			7 April 2003Author:			stephane ducassemove the method confirmRemovalOf: from Behavior to SystemNavigation. The method confirmRemovalOf: is now deprecated on Behavior (note that this method will have to be cleaned too once SystemDictionary will be cleaned). Fix all the senders of confirmRemovalOf: to call SystemNavigation>>confirmRemovalOf:on:"!!Behavior methodsFor: 'deprecated'!confirmRemovalOf: aSelector 	self flag: #deprecated.	self error: 'Method Deprecated: Use SystemNavigation>>confirmRemovalOf:on: instead'! !!Browser methodsFor: 'message functions'!removeMessage	"If a message is selected, create a Confirmer so the user can verify that  	the currently selected message should be removed from the system. If 	so,  	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 	confirmer is bypassed."	| messageName confirmation |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	confirmation _ SystemNavigation new  confirmRemovalOf: messageName on: self selectedClassOrMetaClass.	confirmation == 3		ifTrue: [^ self].	self selectedClassOrMetaClass removeSelector: self selectedMessageName.	self messageListIndex: 0.	self changed: #messageList.	self setClassOrganizer.	"In case organization not cached"	confirmation == 2		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !!ChangeSorter methodsFor: 'message list'!removeMessage	"Remove the selected msg from the system. Real work done by the 	parent, a ChangeSorter"	| confirmation sel |	self okToChange		ifFalse: [^ self].	currentSelector		ifNotNil: [confirmation _ SystemNavigation new  confirmRemovalOf: (sel _ self selectedMessageName) on: self selectedClassOrMetaClass.			confirmation == 3				ifTrue: [^ self].			self selectedClassOrMetaClass removeSelector: sel.			self update.			confirmation == 2				ifTrue: [Smalltalk browseAllCallsOn: sel]]! !!FileContentsBrowser methodsFor: 'removing'!removeMessage	| messageName |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	(SystemNavigation new confirmRemovalOf: messageName on: self selectedClassOrMetaClass)		ifFalse: [^ false].	self selectedClassOrMetaClass removeMethod: self selectedMessageName.	self messageListIndex: 0.	self setClassOrganizer.	"In case organization not cached"	self changed: #messageList! !!MessageSet methodsFor: 'message functions'!removeMessage	"Remove the selected message from the system. 1/15/96 sw"	| messageName confirmation |	messageListIndex = 0		ifTrue: [^ self].	self okToChange		ifFalse: [^ self].	messageName _ self selectedMessageName.	confirmation _ SystemNavigation new confirmRemovalOf: messageName on: self selectedClassOrMetaClass.	confirmation == 3		ifTrue: [^ self].	self selectedClassOrMetaClass removeSelector: messageName.	self deleteFromMessageList: self selection.	self reformulateList.	confirmation == 2		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !!SystemNavigation methodsFor: 'ui'!confirmRemovalOf: aSelector on: aClass	"Determine if it is okay to remove the given selector. Answer 1 if it 	should be removed, 2 if it should be removed followed by a senders 	browse, and 3 if it should not be removed."	| count aMenu answer caption allCalls |	allCalls _ aClass environment allCallsOn: aSelector.	(count _ allCalls size) == 0		ifTrue: [^ 1].	"no senders -- let the removal happen without warning"	count == 1		ifTrue: [(allCalls first actualClass == aClass					and: [allCalls first methodSymbol == aSelector])				ifTrue: [^ 1]].	"only sender is itself"	aMenu _ PopUpMenu labels: 'Remove itRemove, then browse sendersDon''t remove, but show me those sendersForget it -- do nothing -- sorry I asked'.	caption _ 'This message has ' , count printString , ' sender'.	count > 1		ifTrue: [caption _ caption copyWith: $s].	answer _ aMenu startUpWithCaption: caption.	answer == 3		ifTrue: [Smalltalk				browseMessageList: allCalls				name: 'Senders of ' , aSelector				autoSelect: aSelector keywords first].	answer == 0		ifTrue: [answer _ 3].	"If user didn't answer, treat it as cancel"	^ answer min: 3! !