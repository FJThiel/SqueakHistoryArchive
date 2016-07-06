'From Squeak2.8alpha of 4 February 2000 [latest update: #2052] on 30 April 2000 at 5:46:45 pm'!"Change Set:		MarkedMethodFinder-dewDate:			19 April 2000Author:			Doug Way2nd try, this time all methods included --smaAn enhancement to the SelectorBrowser (method finder).  When entering an example (e.g. '3. 4. 7'), and then clicking on a method in the list, classes in the class list will be marked with an asterisk if they are a class/superclass of the example instance.  (These asterisked classes are really the only ones relevant to the example.)"!!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'dew 4/29/2000 13:04'!markMatchingClasses	"If an example is used, mark classes matching the example instance with an asterisk."	| unmarkedClassList firstPartOfSelector receiverString receiver |	"Only 'example' queries can be marked."	(contents asString includes: $.) ifFalse: [^ self].	unmarkedClassList _ classList copy.	"Get the receiver object of the selected statement in the message list."	firstPartOfSelector _ (Scanner new scanTokens: (selectorList at: selectorIndex)) second.	receiverString _ (ReadStream on: (selectorList at: selectorIndex))						upToAll: firstPartOfSelector.	receiver _ Compiler evaluate: receiverString.	unmarkedClassList do:		[:classAndMethod | | class |		class _ Compiler evaluate:				((ReadStream on: classAndMethod) upToAll: firstPartOfSelector).		(receiver isKindOf: class) ifTrue:			[classList add: '*', classAndMethod.			classList remove: classAndMethod]].! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'dew 4/25/2000 18:36'!messageListIndex: anInteger 	"Set the selected message selector to be the one indexed by anInteger.  Find all classes it is in."	selectorIndex _ anInteger.	selectorIndex = 0 ifTrue: [^ self].	classList _ Smalltalk allImplementorsOf: self selectedMessageName.	self markMatchingClasses.	classListIndex _ 0.	self changed: #messageListIndex.		"update my selection"	self changed: #classList.! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'dew 4/19/2000 18:51'!selectedClass	"Answer the currently selected class."	| pairString |	classListIndex = 0 ifTrue: [^nil].	pairString _ classList at: classListIndex.	(pairString includes: $*) ifTrue: [pairString _ pairString allButFirst].	MessageSet parse: pairString		toClassAndSelector: [:cls :sel | ^ cls].! !