'From Squeak3.1alpha of 16 September 2001 [latest update: #4347] on 26 September 2001 at 12:27:43 am'!"Change Set:		BrowseLocal-nkDate:			26 Sept 2001Author:			Ned KonzThis adds two items to the Browser message list menu:browse local implementorsbrowse local sendersThese are just like the non-local versions of these same menu items,except they restrict their search to the selected class and its sub-and super-classes.26 Sept: updated for CS 4347. -dew4 April: added ability to type in a selector if none is selected."!!Behavior methodsFor: 'enumerating' stamp: 'nk 2/14/2001 12:09'!withAllSuperAndSubclassesDoGently: aBlock	self allSuperclassesDo: aBlock.	aBlock value: self.	self allSubclassesDoGently: aBlock! !!Behavior methodsFor: 'user interface' stamp: 'nk 5/23/2001 20:34'!allLocalCallsOn: aSymbol	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."	| aSet special byte cls |	aSet _ Set new.	cls _ self theNonMetaClass.	special _ Smalltalk hasSpecialSelector: aSymbol					ifTrueSetByte: [:b | byte _ b ].	cls withAllSuperAndSubclassesDoGently: [ :class |		(class whichSelectorsReferTo: aSymbol special: special byte: byte)			do: [:sel |				sel ~~ #DoIt ifTrue: [aSet add: class name , ' ', sel]]].	cls class withAllSuperAndSubclassesDoGently: [ :class |		(class whichSelectorsReferTo: aSymbol special: special byte: byte)			do: [:sel |				sel ~~ #DoIt ifTrue: [aSet add: class name , ' ', sel]]].	^aSet! !!StringHolder methodsFor: 'message list menu' stamp: 'nk 4/10/2001 07:32'!browseLocalImplementors	"Present a menu of all messages sent by the currently selected message. 	Open a message set browser of all implementors of the message chosen in or below	the selected class.	Do nothing if no message is chosen."	self getSelectorAndSendQuery: #browseAllImplementorsOf:localTo:		to: Smalltalk		with: { self selectedClass }! !!StringHolder methodsFor: 'message list menu' stamp: 'nk 4/10/2001 07:51'!browseLocalSendersOfMessages	"Present a menu of the currently selected message, as well as all	messages sent by it.  Open a message set browser of all implementors	of the message chosen in or below the selected class"	self getSelectorAndSendQuery: #browseAllCallsOn:localTo:		to: Smalltalk		with: { self selectedClass }! !!CodeHolder methodsFor: 'misc' stamp: 'nk 4/10/2001 07:52'!getSelectorAndSendQuery: querySelector to: queryPerformer	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained as its argument.  If no message is currently selected, then obtain a method name from a user type-in"	self getSelectorAndSendQuery: querySelector to: queryPerformer with: { }.! !!CodeHolder methodsFor: 'misc' stamp: 'nk 4/10/2001 07:53'!getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"	| strm array |	strm _ WriteStream on: (array _ Array new: queryArgs size + 1).	strm nextPut: nil.	strm nextPutAll: queryArgs.	self selectedMessageName ifNil: [ | selector |		selector _ FillInTheBlank request: 'Type selector:' initialAnswer: 'flag:'.		^ selector isEmptyOrNil ifFalse: [			(Symbol hasInterned: selector				ifTrue: [ :aSymbol |					array at: 1 put: aSymbol.					queryPerformer perform: querySelector withArguments: array])				ifFalse: [ self inform: 'no such selector']		]	].	self selectMessageAndEvaluate: [:selector |		array at: 1 put: selector.		queryPerformer perform: querySelector withArguments: array	]! !!Browser methodsFor: 'message functions' stamp: 'nk 8/18/2001 18:17'!messageListMenu: aMenu shifted: shifted	"Answer the message-list menu"	shifted ifTrue: [^ self shiftedMessageListMenu: aMenu].	aMenu addList:#(			('what to show...'						offerWhatToShowMenu)			-			('browse full (b)' 						browseMethodFull)			('browse hierarchy (h)'					classHierarchy)			('browse method (O)'					openSingleMessageBrowser)			('browse protocol (p)'					browseFullProtocol)			-			('fileOut (o)'							fileOutMessage)			('printOut'								printOutMessage)			-			('senders of... (n)'						browseSendersOfMessages)			('local senders of...'						browseLocalSendersOfMessages)			('implementors of... (m)'					browseMessages)			('local implementors of...'				browseLocalImplementors)			('inheritance (i)'						methodHierarchy)			('tile scriptor'							openSyntaxView)			('versions (v)'							browseVersions)			-			('inst var refs...'						browseInstVarRefs)			('inst var defs...'						browseInstVarDefs)			('class var refs...'						browseClassVarRefs)			('class variables'						browseClassVariables)			('class refs (N)'							browseClassRefs)			-			('remove method (x)'					removeMessage)			-			('more...'								shiftedYellowButtonActivity)).	^ aMenu! !!ProtocolBrowser methodsFor: 'private' stamp: 'nk 8/18/2001 18:16'!initListFrom: selectorCollection highlighting: aClass 	"Make up the messageList with items from aClass in boldface."	| defClass item |	messageList := OrderedCollection new.	selectorCollection do: [ :selector |  		defClass := aClass whichClassIncludesSelector: selector.		item _ selector, '     (' , defClass name , ')'.		defClass == aClass ifTrue: [item _ item asText allBold].		messageList add: (			MethodReference new				setClass: defClass 				methodSymbol: selector 				stringVersion: item		)	].	selectedClass _ aClass.! !!ProtocolBrowser methodsFor: 'class list' stamp: 'nk 4/10/2001 08:16'!selectedClassOrMetaClass	^selectedClass! !!SystemDictionary methodsFor: 'browsing' stamp: 'nk 4/10/2001 07:56'!browseAllCallsOn: aLiteral localTo: aClass	"Create and schedule a message browser on each method in or below the given class that refers to	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."	aClass ifNil: [ ^self inform: 'no selected class' ].	(aLiteral isKindOf: LookupKey)		ifTrue: [self browseMessageList: (aClass allLocalCallsOn: aLiteral) asSortedCollection					name: 'Users of ' , aLiteral key, ' local to ', aClass name					autoSelect: aLiteral key]		ifFalse: [self browseMessageList: (aClass allLocalCallsOn: aLiteral) asSortedCollection					name: 'Senders of ' , aLiteral, ' local to ', aClass name					autoSelect: aLiteral keywords first]! !!SystemDictionary methodsFor: 'browsing' stamp: 'nk 4/10/2001 07:55'!browseAllImplementorsOf: selector localTo: aClass	"Create and schedule a message browser on each method in or below the given class	that implements the message whose selector is the argument, selector. For example, 	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."	aClass ifNil: [ ^self inform: 'no class selected' ].	^self browseMessageList: (self allImplementorsOf: selector localTo: aClass)		name: 'Implementors of ' , selector, ' local to ', aClass name! !!SystemDictionary methodsFor: 'retrieving' stamp: 'nk 5/23/2001 20:35'!allImplementorsOf: aSelector  localTo: aClass	"Answer a SortedCollection of all the methods that implement the message 	aSelector in, above, or below the given class."	| aSet cls |	aSet _ Set new.	cls _ aClass theNonMetaClass.	Cursor wait showWhile: [		cls withAllSuperAndSubclassesDoGently:			[:class |			(class includesSelector: aSelector)				ifTrue: [aSet add: class name, ' ', aSelector]].		cls class withAllSuperAndSubclassesDoGently:			[:class |			(class includesSelector: aSelector)				ifTrue: [aSet add: class name, ' ', aSelector]]	].	^aSet asSortedCollection! !