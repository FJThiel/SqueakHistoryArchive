"      NAME	Squeak Class Extensions
       AUTHOR	monty@goodstart.com andrzej@turowski.com (Monty Kamath And Andrzej Turowski)
       URL	(none)
       FUNCTION	Add class extensions to squeak
       KEYWORDS	Extensions Squeak
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 2.2 or later
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.0
       DATE	18-Jan-99

SUMMARY

We added class extensions to squeak so you can add methods to base classes without modifyingthe base class.  These are 'Envy-like' class extensions.

				Monty Kamath And Andrzej Turowski
"!
'From Squeak 2.2 of Sept 23, 1998 on 18 January 1999 at 4:58:11 am'!
Object subclass: #ClassCategoryReader
	instanceVariableNames: 'class category changeStamp systemCategory '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!
Object subclass: #ClassOrganizer
	instanceVariableNames: 'globalComment categoryArray categoryStops elementArray commentStamp extendedCategories '
	classVariableNames: 'Default NullCategory '
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!Object methodsFor: 'GS-Additions' stamp: 'mk 1/17/1999 20:12'!
mark: aSymbol
	"Do nothing"! !


!Browser reorganize!
('initialize-release' browserWindowActivated buildClassSwitchView buildCommentSwitchView buildInstanceClassSwitchView buildInstanceSwitchView buildMorphicSwitches defaultBackgroundColor openAsMorphClassEditing: openAsMorphEditing: openAsMorphMessageEditing: openAsMorphMsgCatEditing: openAsMorphSysCatEditing: openEditString: openMessageCatEditString: openMessageEditString: openOnClassWithEditString: openSystemCatEditString: setClass:selector: systemOrganizer:)
('accessing' contents contents:notifying: contentsSelection couldBrowseAnyClass doItReceiver editSelection request:initialAnswer: spawn:)
('system category list' indexIsOne indexIsOne: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton toggleSystemCategoryListIndex:)
('system category functions' addSystemCategory browseAllClasses buildSystemCategoryBrowser buildSystemCategoryBrowserEditString: changeSystemCategories: classNotFound editSystemCategories fileOutSystemCategory findClass printOutSystemCategory removeSystemCategory renameSystemCategory systemCategoryMenu: updateSystemCategories)
('class list' classList classListIndex classListIndex: classListSingleton recent selectClass: selectedClass selectedClassName toggleClassListIndex:)
('class functions' buildClassBrowser buildClassBrowserEditString: defineClass:notifying: editClass editComment explainSpecial: fileOutClass findMethod hierarchy printOutClass removeClass renameClass spawnHierarchy spawnProtocol)
('message category list' messageCatListSingleton messageCategoryList messageCategoryListIndex messageCategoryListIndex: selectedMessageCategoryName toggleMessageCategoryListIndex:)
('message category functions' addCategory buildMessageCategoryBrowser buildMessageCategoryBrowserEditString: changeMessageCategories: editMessageCategories fileOutMessageCategories messageCategoryMenu: printOutMessageCategories removeMessageCategory renameCategory)
('message list' messageListIndex messageListIndex: messageListSingleton selectedMessage selectedMessageName toggleMessageListIndex:)
('message functions' browseImplementors buildMessageBrowser buildMessageBrowserEditString: inspectInstances inspectSubInstances messageListMenu:shifted: removeMessage removeMessageFromBrowser)
('code pane' showBytecodes)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass selectedClassOrMetaClassName setClassOrganizer)
('gs additions' generateClassVariableMethods generateInstanceVariableMethods gsCopy)
('GS' addCurrentMethodToChangeSet)
('Class Extensions' addExtendedClass classListMenu: defineMessage:notifying: messageList systemCategory)
!


!Browser methodsFor: 'system category list' stamp: 'mk 1/18/1999 00:28'!
selectedSystemCategoryName
	"Answer the name of the selected system category or nil."

	systemCategoryListIndex = 0 ifTrue: [^nil].
	^self systemCategoryList at: systemCategoryListIndex! !

!Browser methodsFor: 'class functions' stamp: 'mk 1/18/1999 00:47'!
spawnHierarchy
	"Create and schedule a new class hierarchy browser on the currently selected class or meta."
	| newBrowser aSymbol aBehavior messageCatIndex |

	classListIndex = 0 ifTrue: [^ self].
	newBrowser _ HierarchyBrowser new initHierarchyForClass: self selectedClass 
			meta: self metaClassIndicated.
	(aSymbol _ self selectedMessageName) ifNotNil: [
		aBehavior _ self selectedClassOrMetaClass.
		messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
		newBrowser messageCategoryListIndex: messageCatIndex.
		newBrowser messageListIndex:
			((aBehavior organization listAtCategoryNumber: messageCatIndex)
						indexOf: aSymbol)].
	Browser openBrowserView: (newBrowser openSystemCatEditString: nil)
		label: self selectedClassName , ' hierarchy'! !

!Browser methodsFor: 'message list' stamp: 'mk 1/18/1999 00:27'!
selectedMessageName
	"Answer the message selector of the currently selected message, if any. 
	Answer nil otherwise."

	messageListIndex = 0 ifTrue: [^nil].
	self messageList size < messageListIndex ifTrue:[messageListIndex := 1].
	^self messageList at: messageListIndex! !

!Browser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:48'!
addExtendedClass

	| classToExtend |

	self mark: #ClassExtenstions.

	classToExtend _ self
		request: 'Please Enter The Class To Extend.'
		initialAnswer: ''. 
	SystemOrganization classify: classToExtend asSymbol underExtended: self systemCategory.
	self changed: #classList.! !

!Browser methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:13'!
classListMenu: aMenu
self mark: #ClassExtenstions.

^ aMenu labels:
'browse class
browse full
printOut
fileOut
hierarchy
definition
comment
spawn hierarchy
spawn protocol
inst var refs..
inst var defs..
class var refs...
class vars
class refs
rename...
remove
copy...
unsent methods
find method...
generate instance methods
generate class methods
add extended class'
   lines: #(4 7 9 11 14 17 19)
   selections:
      #(buildClassBrowser browseMethodFull printOutClass fileOutClass
      hierarchy editClass editComment
      spawnHierarchy spawnProtocol
      browseInstVarRefs browseInstVarDefs browseClassVarRefs
      browseClassVariables browseClassRefs
      renameClass removeClass gsCopy browseUnusedMethods findMethod
      generateInstanceVariableMethods generateClassVariableMethods addExtendedClass
      )
! !

!Browser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:18'!
defineMessage: aString notifying: aController 
	"Compile the expressions in aString. Notify aController if a syntax error 
	occurs. Install the compiled method in the selected class classified under 
	the currently selected message category name. Answer true if 
	compilation succeeds, false otherwise."
	| selectedMessageName selector category oldMessageList |

	self mark: #ClassExtenstions.

	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	contents _ nil.
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aController
				inSystemCategory: self systemCategory.
	selector == nil ifTrue: [^ false].
	contents _ aString copy.
	selector ~~ selectedMessageName
		ifTrue: 
			[category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ true
! !

!Browser methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:13'!
messageList
	"Answer an Array of the message selectors of the currently selected 
	message category. Otherwise, answer a new empty Array."

	self mark: #ClassExtenstions.

	messageCategoryListIndex = 0
		ifTrue: [^Array new]
		ifFalse: [^self classOrMetaClassOrganizer 
					listAtCategoryNumber: messageCategoryListIndex 
					inSystemCategoryIndex: self systemCategoryListIndex
					forClass: self selectedClass]! !

!Browser methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 22:06'!
systemCategory

	self mark: #ClassExtenstions.
	^SystemOrganization categories at: self systemCategoryListIndex! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'mk 1/18/1999 04:55'!
scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodText |

	
	[methodText _ aStream nextChunkText.
	 methodText size > 0]
		whileTrue:
		[class compile: methodText classified: category
			withStamp: changeStamp
			notifying: (SyntaxError new category: category).
		self systemCategory ifNotNil:
			[(SystemOrganization categories includes: systemCategory) ifFalse:
				[SystemOrganization addCategory: systemCategory].
			class organization 
				classify: (class parserClass new parseSelector: methodText)
				underExtended: systemCategory]]! !

!ClassCategoryReader methodsFor: 'Accessing' stamp: 'mk 1/18/1999 03:58'!
systemCategory
	"Generated Getter"

	^systemCategory! !

!ClassCategoryReader methodsFor: 'Accessing' stamp: 'mk 1/18/1999 03:58'!
systemCategory: anObject
	"Generated Setter"

	systemCategory := anObject! !


!ClassDescription reorganize!
('initialize-release' obsolete subclassOf:oldClass:instanceVariableNames:variable:words:pointers:ifBad: updateInstancesFrom: validateFrom:in:instanceVariableNames:methods:wasPresent:)
('accessing' classVersion comment comment: comment:stamp: isMeta name theNonMetaClass)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('printing' classVariablesString instanceVariablesString printOn: sharedPoolsString storeOn:)
('instance variables' addInstVarName: browseClassVarRefs browseClassVariables browseInstVarDefs browseInstVarRefs chooseInstVarThenDo: forceNewFrom: instVarNames removeInstVarName: renameInstVar:to: renameSilentlyInstVar:to:)
('method dictionary' removeCategory: removeSelector: removeSelectorUnlogged:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:notifying: compile:notifying:trailer:ifFail:elseSetSelectorAndNode: compileUnlogged:classified:notifying: wantsChangeSetLogging)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition fileOutCategory: fileOutCategory:asHtml: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutMethod:asHtml: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: kindOfSubclass methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: reformatAll reformatMethodAt: reorganize)
('private' errorCategoryName spaceUsed)
('Class Extensions' categories compile:classified:notifying:inSystemCategory: definitionForCategory: fileOutCategory:on:moveSource:toFile:usingSystemCategory: fileOutOn:moveSource:toFile:usingCategory: isExtendedIn: isSelector:in: methodsFor:stamp:inCategory: printMethodChunk:withPreamble:on:moveSource:toFile:usingSystemCategory: removeFromSystemCategory: systemCategoryForSelector:)
!


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'mk 1/18/1999 04:58'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver on aFileStream. If the boolean 
	argument, moveSource, is true, then set the trailing bytes to the position 
	of aFileStream and to fileIndex in order to indicate where to find the 
	source code."

	aFileStream command: 'H3'.
		aFileStream nextChunkPut: self definition.
		aFileStream command: '/H3'.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	self organization categories do: 
		[:heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:18'!
categories
	"Answer the system categories for me"

	self mark: #ClassExtenstions.
	^SystemOrganization categoriesForClass: self! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:17'!
compile: text classified: category notifying: requestor inSystemCategory: aSystemCategory

	| selector newSelector existingSystemCategory |

	self mark: #ClassExtenstions.
	newSelector := self parserClass new parseSelector: text string.
	((existingSystemCategory := self systemCategoryForSelector: newSelector) ~= aSystemCategory
		and: [existingSystemCategory notNil]) ifTrue:	
		[^self inform: (newSelector asString, ' is defined in ',existingSystemCategory asString)].
	selector := self compile: text classified: category notifying: requestor.
	(selector notNil and:[self isExtendedIn: aSystemCategory]) ifTrue:
			[self organization classify: selector underExtended: aSystemCategory].
	^selector
 ! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:24'!
definitionForCategory: aCategory
	"Answer a String that defines the receiver."

	| aStream |

	(self isExtendedIn: aCategory) ifFalse:[^self definition].

	self isMeta ifTrue:[^'''Extended Class Methods'''].
	aStream _ WriteStream on: (String new: 300).

	aStream 
		nextPutAll: 'SystemOrganization classify: #';
		nextPutAll: self name printString, ' underExtended: ''';
		nextPutAll: (aCategory printString,''' asSymbol').

	^aStream contents
! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:44'!
fileOutCategory: aString on: aFileStream moveSource: moveSource toFile: fileIndex usingSystemCategory: systemCategory
	
	aFileStream cr.
	(self organization 
		listAtCategoryNamed: aString 
		forSystemCategory: systemCategory
		forClass: self) do: [:sel |
			(self isExtendedIn: systemCategory)
				ifTrue: 
					[self 
						printMethodChunk: sel 
						withPreamble: true
						on: aFileStream moveSource: 
						moveSource toFile: fileIndex
						usingSystemCategory: systemCategory]
				ifFalse: 
					[self 
						printMethodChunk: sel 
						withPreamble: true
						on: aFileStream moveSource: 
						moveSource toFile: fileIndex]].
! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:19'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex usingCategory: aCategory
	"File a description of the receiver on aFileStream. If the boolean 
	argument, moveSource, is true, then set the trailing bytes to the position 
	of aFileStream and to fileIndex in order to indicate where to find the 
	source code."

	aFileStream command: 'H3'.
		aFileStream nextChunkPut: (self definitionForCategory: aCategory).
		aFileStream command: '/H3'.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	self organization categories do: 
		[:heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex
			usingSystemCategory: aCategory]! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:16'!
isExtendedIn: aSystemCategory

	self mark: #ClassExtenstions.

	^self isMeta 
		ifTrue:[SystemOrganization isClass: self soleInstance extendedIn: aSystemCategory]
		ifFalse:[SystemOrganization isClass: self extendedIn: aSystemCategory]! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 03:17'!
isSelector: aSelector in: systemCategory

	^self organization isSelector: aSelector in: systemCategory forClass: self! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:00'!
methodsFor: categoryName stamp: changeStamp inCategory: aSystemCategory
	| answer |
	answer := self methodsFor: categoryName stamp: changeStamp.
	answer systemCategory: aSystemCategory.
	^answer
	! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 03:42'!
printMethodChunk: selector withPreamble: doPreamble on: outStream
		moveSource: moveSource toFile: fileIndex usingSystemCategory: systemCategory
	| preamble method oldPos newPos sourceFile |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asString printString]
		ifFalse: [preamble _ ''].
	method _ methodDict at: selector.
	((method fileIndex = 0
		or: [(SourceFiles at: method fileIndex) == nil])
		or: [(oldPos _ method filePosition) = 0])
		ifTrue:
		["The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [outStream cr; nextPut: $!!; nextChunkPut: preamble; cr].
		outStream nextChunkPut: (self decompilerClass new decompile: selector
											in: self method: method) decompileString]
		ifFalse:
		[sourceFile _ SourceFiles at: method fileIndex.
		sourceFile position: oldPos.
		preamble size > 0 ifTrue:    "Copy the preamble"
			[outStream copyPreamble: preamble from: sourceFile.
			outStream position: outStream position -2.
			outStream nextPutAll: ' inCategory: ''', systemCategory printString,''' asSymbol !!'.
			outStream cr].
		"Copy the method chunk"
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		sourceFile skipSeparators.	"The following chunk may have ]style["
		sourceFile peek == $] ifTrue: [
			outStream cr; copyMethodChunkFrom: sourceFile].
		moveSource ifTrue:    "Set the new method source pointer"
			[method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [outStream nextChunkPut: ' '].
	^ outStream cr.
! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:10'!
removeFromSystemCategory: aSystemCategory

	| categories |
	self mark: #ClassExtenstions.
	(categories := SystemOrganization categoriesForClass: self) size == 1
		ifTrue: [^self removeFromSystem].
	
	(categories first = aSystemCategory) ifTrue:
		[^self inform: self name asString, ' is extended in ', 
			(categories copyFrom: 2 to: categories size) printString ].

	^SystemOrganization removeExtendedClass: self fromSystemCategory: aSystemCategory

	! !

!ClassDescription methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:09'!
systemCategoryForSelector: aSelector

	self mark: #ClassExtenstions.
	^self organization systemCategoryForSelector: aSelector forClass: self! !


!Class methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:51'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex usingCategory: aCategory
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	self mark: #ClassExtenstions.
	Transcript cr; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		usingCategory: aCategory.
	self class nonTrivial
		ifTrue:
			[aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; cr; cr.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex
				usingCategory: aCategory]! !


!ClassOrganizer reorganize!
('accessing' categories categories: categoryOfElement: changeFromString: classComment classComment: commentRemoteStr commentStamp commentStamp: hasNoComment listAtCategoryNamed: listAtCategoryNumber: numberOfCategoryOfElement: removeElement: removeEmptyCategories)
('compiler access' classify:under: classifyAll:under:)
('method dictionary' addCategory: addCategory:before: removeCategory: renameCategory:toBe:)
('printing' printOn:)
('fileIn/Out' fileOutCommentOn:moveSource:toFile: moveChangedCommentToFile:numbered: putCommentOnFile:numbered:moveSource:forClass: scanFrom:)
('private' setDefaultList:)
('Class Extensions' add:underExtended: allDefingingElements allExtendedElements classify:underExtended: extendedCategories extendedCategories: extendedHeadingForElement: filterExtendedElements:withMessageCategoryIndex: getExtendedElementsIn: getExtendedElementsWithIndex: isClass:extendedIn: isSelector:in:forClass: listAtCategoryNamed:forSystemCategory: listAtCategoryNamed:forSystemCategory:forClass: listAtCategoryNumber:inSystemCategoryIndex:forClass: removeExtendedElement: systemCategoryForSelector:forClass:)
!


!ClassOrganizer methodsFor: 'accessing' stamp: 'mk 1/17/1999 20:16'!
listAtCategoryNumber: anInteger 
	"Answer the array of elements stored at the position indexed by 
	anInteger."

	| firstIndex lastIndex |
	self mark: #ClassExtenstions.
	firstIndex _ 
		(anInteger > 1
			ifTrue: [categoryStops at: anInteger - 1]
			ifFalse: [0])
		+ 1.
	lastIndex _ categoryStops at: anInteger.
	^(elementArray copyFrom: firstIndex to: lastIndex), 
		(self getExtendedElementsWithIndex: anInteger)! !

!ClassOrganizer methodsFor: 'accessing' stamp: 'mk 1/17/1999 21:33'!
removeElement: element 
	"Remove the selector, element, from all categories."
	| categoryIndex elementIndex nextStop newElements |

	self mark: #ClassExtenstions.
	self removeExtendedElement: element.

	categoryIndex _ 1.
	elementIndex _ 0.
	nextStop _ 0.
	"nextStop keeps track of the stops in the new element array"
	newElements _ WriteStream on: (Array new: elementArray size).
	[(elementIndex _ elementIndex + 1) <= elementArray size]
		whileTrue: 
			[[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: 
					[categoryStops at: categoryIndex put: nextStop.
					categoryIndex _ categoryIndex + 1].
			(elementArray at: elementIndex) = element
				ifFalse: 
					[nextStop _ nextStop + 1.
					newElements nextPut: (elementArray at: elementIndex)]].
	[categoryIndex <= categoryStops size]
		whileTrue: 
			[categoryStops at: categoryIndex put: nextStop.
			categoryIndex _ categoryIndex + 1].
	elementArray _ newElements contents! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:28'!
add: element underExtended: heading

	self mark: #ClassExtenstions.

	(self extendedCategories
		at: heading 
		ifAbsent: [self extendedCategories at: heading put: Set new]) 
			add: element.
! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:08'!
allDefingingElements

	| extended |
	self mark: #ClassExtenstions.
	extended := self allExtendedElements.
	^(elementArray reject:[:each | extended includes: each]) asArray! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:23'!
allExtendedElements

	| answer |
	self mark: #ClassExtenstions.
	answer := OrderedCollection new.
	self extendedCategories do:
		[:value |
		answer addAll: value].
	^answer

	! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:14'!
classify: element underExtended: heading
 
	| catName realHeading |
	self mark: #ClassExtenstions.

	((heading = NullCategory) or: [heading == nil])
		ifTrue: [realHeading _ Default]
		ifFalse: [realHeading _ heading asSymbol].
	(catName _ self categoryOfElement: element) = realHeading
		ifTrue: [^self].  "done if already under that category"

	catName ~~ nil ifTrue: 
		[self add: element underExtended: heading].
! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:18'!
extendedCategories

	self mark: #ClassExtenstions.

	extendedCategories ifNil:
		[extendedCategories := Dictionary new].
	^extendedCategories! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:15'!
extendedCategories: anObject
	"Generated Setter"
	
	self mark: #ClassExtenstions.

	extendedCategories := anObject! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:08'!
extendedHeadingForElement: element

	self mark: #ClassExtenstions.
	self extendedCategories do:
		[:each |
		(each includes: element) ifTrue:[^self extendedCategories keyAtValue: each]].
	^nil

! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:09'!
filterExtendedElements: aCollection withMessageCategoryIndex: index

	self mark: #ClassExtenstions.
	^((self listAtCategoryNumber: index) reject:
		[:each | aCollection includes: each]) asArray! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 21:38'!
getExtendedElementsIn: heading

	self mark: #ClassExtenstions.

	^(self extendedCategories at: heading ifAbsent:[#()]) asArray! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:15'!
getExtendedElementsWithIndex: anInteger

	self mark: #ClassExtenstions.
	^self getExtendedElementsIn: (self categories at: anInteger)! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 20:15'!
isClass: aClass extendedIn: systemCategory

	self mark: #ClassExtenstions.
	^(SystemOrganization extendedCategories at: systemCategory ifAbsent:[#()])
		includes: aClass name! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 03:18'!
isSelector: aSelector in: aSystemCategory forClass: aClass

	^(self systemCategoryForSelector: aSelector forClass: aClass) == aSystemCategory

! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 02:10'!
listAtCategoryNamed: aString forSystemCategory: systemCategory

	| extendedMethods |
	extendedMethods := self getExtendedElementsIn: systemCategory.
	^(self listAtCategoryNamed: aString) select:
		[:each | 
		extendedMethods includes: each]! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 04:38'!
listAtCategoryNamed: aString forSystemCategory: systemCategory forClass: aClass

	| definingMethods extendedMethods |

	^(aClass isExtendedIn: systemCategory)
		ifFalse:
			[definingMethods := self allDefingingElements.
			(self listAtCategoryNamed: aString) select:[:each | definingMethods includes: each]]
		ifTrue:
			[extendedMethods := self getExtendedElementsIn: systemCategory.
			(self listAtCategoryNamed: aString) select:[:each | extendedMethods includes: each]]! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/17/1999 23:27'!
listAtCategoryNumber: messageCategoryListIndex 
	inSystemCategoryIndex: systemCategoryListIndex
	forClass: aClass

	| systemCategory |
	self mark: #ClassExtenstions.
	systemCategory := SystemOrganization categories at: systemCategoryListIndex.
	^(self isClass: aClass extendedIn: systemCategory)
		ifTrue:
			[((self listAtCategoryNumber: messageCategoryListIndex) select:
				[:each | 
				(self extendedCategories at: systemCategory ifAbsent:[#()]) 
					includes: each]) asArray]
		ifFalse:
			[self 
				filterExtendedElements: self allExtendedElements 				withMessageCategoryIndex: messageCategoryListIndex]! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:09'!
removeExtendedElement: element
	| aHeading |

	self mark: #ClassExtenstions.
	aHeading := self extendedHeadingForElement: element.
	(self allExtendedElements includes: element) ifFalse:[^nil].
	(self extendedCategories at: aHeading) remove: element.
	(self extendedCategories at: aHeading) isEmpty 
		ifTrue: [self extendedCategories removeKey: aHeading]! !

!ClassOrganizer methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:09'!
systemCategoryForSelector: aSelector forClass: aClass

	self mark: #ClassExtenstions.
	(elementArray includes: aSelector) ifFalse:[^nil].

	self extendedCategories do:
		[:each |
		(each includes: aSelector) ifTrue:[^self extendedCategories keyAtValue: each]].

	^aClass category ! !


!HierarchyBrowser reorganize!
('Class Extensions' classListIndex: initHierarchyForClass:meta: messageList openSystemCatEditString: systemCategory systemCategoryList systemCategoryListIndex systemCategoryListIndex: updateSystemCategories)
('initialization' classList initAlphabeticListing initForClassList: openEditString: selectClass: selectedClassName systemCategorySingleton)
('menu messages' buildClassBrowserEditString: classListMenu: removeSystemCategory)
!


!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:57'!
classListIndex: anInteger 

	| answer |
	anInteger = 0 
		ifTrue:[answer := super classListIndex: self classListIndex ]
		ifFalse:[answer := super classListIndex: anInteger].

	self updateSystemCategories.
	^answer! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:08'!
initHierarchyForClass: theClass meta: meta
	| tab stab index |

	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ meta.
	classList _ OrderedCollection new.
	tab _ ''.
	theClass allSuperclasses reverseDo: 
		[:aClass | 
		classList add: tab , aClass name.
		tab _ tab , '  '].
	index _ classList size + 1.
	theClass allSubclassesWithLevelDo:
		[:aClass :level |
		stab _ ''.  1 to: level do: [:i | stab _ stab , '  '].
		classList add: tab , stab , aClass name]
	 	startingLevel: 0.
	self classListIndex: index! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:05'!
messageList
	"Answer an Array of the message selectors of the currently selected 
	message category. Otherwise, answer a new empty Array."

	self mark: #ClassExtenstions.

	messageCategoryListIndex = 0
		ifTrue: [^Array new]
		ifFalse: [^self classOrMetaClassOrganizer 
					listAtCategoryNumber: messageCategoryListIndex 
					inSystemCategoryIndex: 
						(SystemOrganization categories indexOf: (self systemCategoryList at: self systemCategoryListIndex))
					forClass: self selectedClass]! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:08'!
openSystemCatEditString: aString
	"Create a pluggable version of all the views for a Browser, including views and controllers.  The top list view is of the currently selected system class category--a single item list."
	| systemCategoryListView classListView messageCategoryListView messageListView browserCodeView topView switchView |

	World ifNotNil: [^ self openAsMorphSysCatEditing: aString].

	topView _ (StandardSystemView new) model: self.
	topView borderWidth: 1.
		"label and minSize taken care of by caller"


	systemCategoryListView _ PluggableListView on: self
		list: #systemCategoryList
		selected: #systemCategoryListIndex
		changeSelected: #systemCategoryListIndex:
		menu: #systemCategoryMenu:.
	systemCategoryListView window: (0 @ 0 extent: 200 @ (12 * self systemCategoryList size)).
	topView addSubView: systemCategoryListView.

	classListView _ PluggableListView on: self
		list: #classList
		selected: #classListIndex
		changeSelected: #classListIndex:
		menu: #classListMenu:.
	classListView window: (0 @ 0 extent: 67 @ 62).
	topView addSubView: classListView below: systemCategoryListView.

	messageCategoryListView _ PluggableListView on: self
		list: #messageCategoryList
		selected: #messageCategoryListIndex
		changeSelected: #messageCategoryListIndex:
		menu: #messageCategoryMenu:.
	messageCategoryListView window: (0 @ 0 extent: 66 @ 70).
	topView addSubView: messageCategoryListView toRightOf: classListView.

	switchView _ self buildInstanceClassSwitchView.
	switchView 
		window: switchView window 
		viewport: (classListView viewport bottomLeft 
					corner: messageCategoryListView viewport bottomLeft).
	switchView borderWidth: 1.
	topView addSubView: switchView below: classListView.

	messageListView _ PluggableListView on: self
		list: #messageList
		selected: #messageListIndex
		changeSelected: #messageListIndex:
		menu: #messageListMenu:shifted:
		keystroke: #messageListKey:from:.
	messageListView menuTitleSelector: #messageListSelectorTitle.
	messageListView window: (0 @ 0 extent: 67 @ 70).
	topView addSubView: messageListView toRightOf: messageCategoryListView.

	browserCodeView _ PluggableTextView on: self 
			text: #contents accept: #contents:notifying:
			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.
	browserCodeView window: (0@0 extent: 200@(110-12)).
	topView addSubView: browserCodeView below: switchView.
	aString ifNotNil: [browserCodeView editString: aString.
			browserCodeView hasUnacceptedEdits: true].
	^ topView! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:15'!
systemCategory

	self mark: #ClassExtenstions.
	^(self systemCategoryList at: self systemCategoryListIndex)! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:26'!
systemCategoryList

	| cls |
	cls _ self selectedClass.
	^ cls ifNil: [Array new]
		ifNotNil: [cls categories]! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:50'!
systemCategoryListIndex

	^systemCategoryListIndex! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:06'!
systemCategoryListIndex: index

	(index = 0) ifFalse:[systemCategoryListIndex := index].
	self changed: #systemCategorySelectionChanged.
	self changed: #systemCategoryListIndex.	"update my selection"
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #contents.! !

!HierarchyBrowser methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 00:57'!
updateSystemCategories

	super updateSystemCategories.
	self systemCategoryListIndex: 1.! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'tk 4/3/98 11:03'!
systemCategorySingleton

	| cls |
	cls _ self selectedClass.
	^ cls ifNil: [Array new]
		ifNotNil: [Array with: cls category name]! !

!HierarchyBrowser methodsFor: 'menu messages' stamp: 'mk 1/18/1999 01:24'!
classListMenu: aMenu
self mark: #ClassExtenstions.

^ aMenu labels:
'browse class
browse full
printOut
fileOut
hierarchy
definition
comment
spawn hierarchy
spawn protocol
inst var refs..
inst var defs..
class var refs...
class vars
class refs
unsent methods
find method...
generate instance methods
generate class methods'
   lines: #(4 7 9 11 14 16)
   selections:
      #(buildClassBrowser browseMethodFull printOutClass fileOutClass
      hierarchy editClass editComment
      spawnHierarchy spawnProtocol
      browseInstVarRefs browseInstVarDefs browseClassVarRefs
      browseClassVariables browseClassRefs
     browseUnusedMethods findMethod
      generateInstanceVariableMethods generateClassVariableMethods
      )
! !


!Metaclass methodsFor: 'Class Extensions' stamp: 'mk 1/18/1999 01:54'!
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex usingCategory: aCategory
	super fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		usingCategory: aCategory.
	(moveSource not and: [methodDict includesKey: #initialize]) ifTrue: 
		[aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream cr]! !


!SystemOrganizer methodsFor: 'Class Extenstions' stamp: 'mk 1/18/1999 00:07'!
categoriesForClass: aClass

	self mark: #ClassExtenstions.

	^((Array with: aClass category) asOrderedCollection , 
		(self extendedHeadingsForElement: aClass name) asOrderedCollection) asArray

	! !

!SystemOrganizer methodsFor: 'Class Extenstions' stamp: 'mk 1/18/1999 00:08'!
extendedHeadingsForElement: element
	| systemCategoryCollection |
	self mark: #ClassExtenstions.
	systemCategoryCollection := Set new.
	self extendedCategories do:
		[:each |
		(each includes: element) ifTrue:
			[systemCategoryCollection add: (self extendedCategories keyAtValue: each)]].
	^systemCategoryCollection 	! !

!SystemOrganizer methodsFor: 'Class Extenstions' stamp: 'mk 1/18/1999 00:08'!
removeExtendedClass: aClass fromSystemCategory: aSystemCategory
	"Remove the extended class from the system and 
	all of the extended methods in this systemCategory"

	self mark: #ClassExtenstions.
	(aClass organization getExtendedElementsIn: aSystemCategory) do: [:aSelector |
		aClass removeSelector: aSelector].
	
	(self extendedCategories at: aSystemCategory) remove: aClass name.
	(self extendedCategories at: aSystemCategory) isEmpty 
		ifTrue: [self extendedCategories removeKey: aSystemCategory]! !

!SystemOrganizer methodsFor: 'Class Extenstions' stamp: 'mk 1/18/1999 00:08'!
removeExtendedElement: element
	self mark: #ClassExtenstions.! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'mk 1/18/1999 01:59'!
fileOutCategory: category on: aFileStream 
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes _ (self superclassOrder: category).
	poolSet _ Set new.
	classes do: 
		[:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet size > 0 ifTrue:
		[tempClass _ Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first _ true.
	classes do: 
		[:class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream cr; nextPut: Character newPage; cr].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			usingCategory: category]! !

!SystemOrganizer methodsFor: 'GS' stamp: 'mk 10/18/1998 20:14'!
gsDeleteFileTemp: category
   | directory |

   directory := FileDirectory default.
   directory deleteFileNamed: ('~gs',category,'.st').! !

!SystemOrganizer methodsFor: 'GS' stamp: 'mk 10/18/1998 20:13'!
gsFileInTemp: category
   | directory ff |

   directory := FileDirectory default.
   ff := directory readOnlyFileNamed: ('~gs',category,'.st').
   ff fileIn.! !

!SystemOrganizer methodsFor: 'GS' stamp: 'mk 10/18/1998 20:13'!
gsFileOutTemp: category
   "FileOut all the classes in the named system category."
   | fileStream |
   fileStream _ FileStream newFileNamed: '~gs',category , '.st'.
   self fileOutCategory: category on: fileStream.
   fileStream close! !


ClassDescription removeSelector: #isSelector:extendedIn:!
ClassDescription removeSelector: #fileOutCategory:on:moveSource:toFile:usingCategory:!
ClassOrganizer removeSelector: #removeExtenedElement:!
ClassOrganizer removeSelector: #extenedCategoryArray!
ClassOrganizer removeSelector: #extenedCategories!
ClassOrganizer removeSelector: #test!
ClassOrganizer removeSelector: #listAtCategoryNumber:inSystemCategoryIndex:!
ClassOrganizer removeSelector: #isSelector:extendedIn:forClass:!
ClassOrganizer removeSelector: #extenedCategoryArray:!
ClassOrganizer removeSelector: #extenedCategories:!
ClassOrganizer removeSelector: #test2!

