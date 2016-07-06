"      NAME	FileContentsBrowser
       AUTHOR	raab@isg.cs.uni-magdeburg.de (Andreas Raab)
       URL	(none)
       FUNCTION	Browser for the contents of files containing Smalltalk source code
       KEYWORDS	Browser, Smalltalk source files, Porting
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.2
       DATE	04-Apr-98

SUMMARY

While the class browser is a great tool it can currently not be used to give us a 'structured view' on a (syntactically correct) Smalltalk source code file. That's exactly what the FileContentsBrowser is doing.It just scans the file and you can browse the classes and methods of this file _without_ having to actually install it. You can browse the classes, methods, and comments just like in a real class browser. Even better, you can rename/remove/reorganize all the stuff and once you're done you can either file it in (i.e., install it) or file it out again for later work.Features:  * Browse either single files or entire collections  * Rename/Remove/Reorganize classes or methods  * Remove all unchanged methods so that you see     where actual differences are  * Highlights differences in already existing methods  * Highlights differences to already existing class definitions  * Keeps all unknown doIts so you can decide what to do with them  * Install entire packages, classes, categories,     or methods from a package  * FileOut modified packages so you can distribute     or work later on themHow to use it: After installing FileContentsBrowser.st open a NEW FileList and select 'browse selected file(s)' from it. If you have selected a file it will just browse the single file. If you haven't selected a file than you can give a pattern for the files from the current directory.

				Andreas Raab
"!
'From Squeak 1.31 of Feb 4, 1998 on 4 April 1998 at 12:22:55 am'!
Browser subclass: #FileContentsBrowser
	instanceVariableNames: 'packages infoString '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
Object subclass: #FilePackage
	instanceVariableNames: 'packageName fullName sourceSystem classes doIts classOrder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
Object subclass: #PseudoClass
	instanceVariableNames: 'name definition organization source metaClass '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
ClassOrganizer subclass: #PseudoClassOrganizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
PseudoClass subclass: #PseudoMetaclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
Object subclass: #TextDiffBuilder
	instanceVariableNames: 'realSrc realDst srcMap dstMap srcLines dstLines srcPos dstPos added removed shifted runs matches multipleMatches patchSequence '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
TextDiffBuilder subclass: #ClassDiffBuilder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!


!PseudoClass methodsFor: 'class'!
metaClass
	^metaClass ifNil:[metaClass := PseudoMetaclass new name: (self name)].!
definition
	^definition ifNil:['There is no class definition for this class'].!
classComment: aChangeRecord
	self organization classComment: aChangeRecord!
comment: aString
	self commentString: aString.!
comment
	| rStr |
	rStr := self organization commentRemoteStr.
	^rStr isNil
		ifTrue:[self name,' has not been commented']
		ifFalse:[rStr string]!
renameTo: aString

	self hasDefinition ifTrue:[
		self isMetaClass ifTrue:[
			self definition: (self definition
				copyReplaceAll: name,' class'
				with: aString, ' class').
		] ifFalse:[
			self definition: (self definition 
					copyReplaceAll:'ubclass: #',name
					with:'ubclass: #', aString)]].
	name := aString.
	metaClass ifNotNil:[metaClass renameTo: aString].!
commentString: aString
	self classComment: aString asText. "Just wrap it"!
commentString
	^self organization classComment string!
definition: aString
	definition := aString! !

!PseudoClass methodsFor: 'accessing'!
fullName
	^self name!
realClass
	^Smalltalk at: self name asSymbol!
organization
	^organization ifNil:[organization := PseudoClassOrganizer defaultList: SortedCollection new].!
name
	^name!
name: anObject
	name _ anObject! !

!PseudoClass methodsFor: 'removing'!
removeAllUnmodified
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass comment asString = self commentString]) ifTrue:[ self classComment: nil].
	metaClass isNil ifFalse:[metaClass removeAllUnmodified].!
removeUnmodifiedMethods: aCollection
	| stClass |
	self exists ifFalse:[^self].
	stClass := self realClass.
	aCollection do:[:sel|
		(self sourceCodeAt: sel) = (stClass sourceCodeAt: sel ifAbsent:['']) asString ifTrue:[
			self removeMethod: sel.
		].
	].
	self organization removeEmptyCategories.! !

!PseudoClass methodsFor: 'private'!
evaluate: aString
	^Compiler evaluate: aString for: nil logged: true!
makeSureSuperClassExists: aString
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent:[nil].
	theClass ifNotNil:[^true].
	^self confirm: 'The super class ',aString,' does not exist in the system. Use nil instead?'.!
makeSureClassExists: aString
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent:[nil].
	theClass ifNotNil:[^true].
	^self confirm: aString,' does not exist in the system. Use nil instead?'.!
parserClass
	^Parser!
confirmRemovalOf: aString
	^self confirm:'Remove ',aString,' ?'! !

!PseudoClass methodsFor: 'testing'!
hasComment
	^self organization commentRemoteStr notNil!
exists
	^(Smalltalk at: self name asSymbol ifAbsent:[^false]) isKindOf: Behavior!
nameExists
	^Smalltalk includesKey: self name asSymbol!
hasChanges

	self sourceCode isEmpty ifFalse:[^true].
	self organization hasNoComment ifFalse:[^true].
	definition isNil ifFalse:[^true].
	metaClass isNil ifFalse:[^metaClass hasChanges].
	^false!
needsInitialize
	^self hasMetaclass and:[
		self metaClass selectors includes: #initialize]!
hasMetaclass
	^metaClass notNil!
hasDefinition
	^definition notNil!
isMetaClass
	^false! !

!PseudoClass methodsFor: 'fileIn/fileOut'!
fileOutDefinitionOn: aStream
	self hasDefinition ifFalse:[^self].
	aStream nextChunkPut: self definition; cr.
	self hasComment ifTrue:[
		aStream cr; nextPut: $!!; nextChunkPut: self name,' comment: '; cr.
		aStream nextChunkPut: self commentString printString.
	].!
fileInCategory: aCategory
	^self fileInMethods: (self organization listAtCategoryNamed: aCategory)!
fileOutMethods: aCollection on: aStream
	"FileOut all methods with selectors taken from aCollection"
	| cat categories |
	categories := Dictionary new.
	aCollection do:[:sel|
		cat := self organization categoryOfElement: sel.
		cat = self removedCategoryName ifFalse:[
			(categories includesKey: cat) 
				ifFalse:[categories at: cat put: Set new].
			(categories at: cat) add: sel].
	].
	categories associationsDo:[:assoc|
		cat := assoc key.
		aStream cr; cr; nextPut:$!!; nextChunkPut:(String streamContents:[:s|
			s nextPutAll: self fullName; nextPutAll:' methodsFor: '; print: cat asString]).
		assoc value do:[:sel|
			aStream cr.
			aStream nextChunkPut: (self sourceCodeAt: sel).
		].
		aStream space; nextPut:$!!.
	].!
fileOut
	| f |
	f := (FileStream newFileNamed: self name,'.st').
	self fileOutOn: f.
	self needsInitialize ifTrue:[
		f cr; nextChunkPut: self name,' initialize'.
	].
	f close!
fileOutCategory: categoryName
	| f |
	f := (FileStream newFileNamed: self name,'-',categoryName,'.st').
	self fileOutMethods: (self organization listAtCategoryNamed: categoryName)
			on: f.
	f close
!
fileOutMethod: selector
	| f |
	f := (FileStream newFileNamed: self name,'-', selector, '.st').
	self fileOutMethods: (Array with: selector)
			on: f.
	f close!
fileOutOn: aStream
	"FileOut the receiver"
	self fileOutDefinitionOn: aStream.
	metaClass ifNotNil:[metaClass fileOutDefinitionOn: aStream].
	self fileOutMethods: self selectors on: aStream.
	metaClass ifNotNil:[metaClass fileOutMethods: metaClass selectors on: aStream].!
fileOutMethodsOn: aStream
	^self fileOutMethods: self selectors on: aStream.!
fileInDefinition
	(self makeSureSuperClassExists: (definition copyUpTo: Character space)) ifFalse:[^self].
	self hasDefinition ifTrue:[
		Transcript cr; show:'Defining ', self name.
		self evaluate: self definition].
	self exists ifFalse:[^self].
	self hasComment ifTrue:[self realClass classComment: self comment].!
fileInMethods: aCollection
	"FileIn all methods with selectors taken from aCollection"
	| theClass cat |
	self exists ifFalse:[^self classNotDefined].
	theClass := self realClass.
	aCollection do:[:sel|
		cat := self organization categoryOfElement: sel.
		cat = self removedCategoryName ifFalse:[
			theClass 
				compile: (self sourceCodeAt: sel) 
				classified: cat
				withStamp: (self stampAt: sel)
				notifying: nil.
		].
	].!
fileInMethod: selector
	^self fileInMethods: (Array with: selector)!
fileIn
	"FileIn the receiver"
	self hasDefinition ifTrue:[self fileInDefinition].
	self fileInMethods: self selectors.
	metaClass ifNotNil:[metaClass fileIn].
	self needsInitialize ifTrue:[
		self evaluate: self name,' initialize'.
	].!
fileInMethods
	^self fileInMethods: self selectors! !

!PseudoClass methodsFor: 'errors'!
classNotDefined
	^self inform: self name,' is not defined in the system.
You have to define this class first.'.! !

!PseudoClass methodsFor: 'categories'!
whichCategoryIncludesSelector: aSelector 
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	^ self organization categoryOfElement: aSelector!
removedCategoryName
	^'*** removed methods ***' asSymbol!
removeCategory: selector
	(self organization listAtCategoryNamed: selector) do:[:sel|
		self organization removeElement: sel.
		self sourceCode removeKey: sel.
	].
	self organization removeCategory: selector.! !

!PseudoClass methodsFor: 'methods'!
removeSelector: aSelector
	| catName |
	catName := self removedCategoryName.
	self organization addCategory: catName before: self organization categories first.
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.!
methodChange: aChangeRecord
	aChangeRecord isMetaClassChange ifTrue:[
		^self metaClass addMethodChange: aChangeRecord.
	] ifFalse:[
		^self addMethodChange: aChangeRecord.
	].
!
sourceCodeTemplate
	^''!
sourceCodeAt: sel
	^(self sourceCode at: sel) string!
removeMethod: selector
	self organization removeElement: selector.
	self sourceCode removeKey: selector.
!
selectors
	^self sourceCode keys!
sourceCode
	^source ifNil:[source := Dictionary new]!
sourceCodeAt: sel put: object
	self sourceCode at: sel put: object!
stampAt: selector
	^(self sourceCode at: selector) stamp!
addMethodChange: aChangeRecord
	| selector |
	selector := Parser new parseSelector: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord! !

!FileContentsBrowser methodsFor: 'accessing'!
contents
	self updateInfoView.
	(editSelection == #newClass and:[self selectedPackage notNil])
		ifTrue: [^self selectedPackage packageInfo].
	editSelection == #editClass
		ifTrue:[^self modifiedClassDefinition].
	^super contents!
packages
	^packages!
infoString
	^infoString isNil
		ifTrue:[infoString := StringHolder new]
		ifFalse:[infoString]!
selectedPackage
	| cat |
	cat := self selectedSystemCategoryName.
	cat isNil ifTrue:[^nil].
	^self packages at: cat asString ifAbsent:[nil]!
contents: input notifying: aController 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.

	editSelection == #editComment 
		ifTrue: [theClass _ self selectedClass.
				theClass ifNil: [PopUpMenu notify: 'You must select a class
before giving it a comment.'.
				^ false].
				theClass comment: aText. ^ true].
	editSelection == #editMessageCategories 
		ifTrue: [^ self changeMessageCategories: aString].

	self inform:'You cannot change the current selection'.
	^false
!
packages: aDictionary
	packages := aDictionary.! !

!FileContentsBrowser methodsFor: 'removing'!
removeClass
	| class |
	classListIndex = 0 ifTrue: [^ self].
	class _ self selectedClass.
	(self confirm:'Are you certain that you
want to delete the class ', class name, '?') ifFalse:[^self].
	self selectedPackage removeClass: class.
	self classListIndex: 0.
	self changed: #classListChanged.!
removeUnmodifiedClasses
	| packageList |
	self okToChange ifFalse:[^self].
	packageList := self selectedPackage isNil
						ifTrue:[self packages] 
						ifFalse:[Array with: self selectedPackage].
	packageList do:[:package|
		package classes copy do:[:theClass|
			Cursor wait showWhile:[
				theClass removeAllUnmodified.
			].
			theClass hasChanges ifFalse:[
				package removeClass: theClass.
			].
		]].
	self classListIndex: 0.
	self changed: #classListChanged.!
removeSystemCategory
	systemCategoryListIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	(self confirm: 'Are you sure you want to
remove this package 
and all its classes?') ifFalse:[^self].
	(systemOrganizer listAtCategoryNamed: self selectedSystemCategoryName) do:[:el|
		systemOrganizer removeElement: el].
	self packages removeKey: self selectedPackage packageName.
	systemOrganizer removeCategory: self selectedSystemCategoryName.
	self systemCategoryListIndex: 0.
	self changed: #systemCategoriesChanged!
removeUnmodifiedCategories
	| theClass |
	self okToChange ifFalse:[^self].
	theClass := self selectedClass.
	theClass isNil ifTrue:[^self].
	Cursor wait showWhile:[
		theClass removeUnmodifiedMethods: (theClass selectors).
	].
	self messageCategoryListIndex: 0.
	self changed: #classSelectionChanged.!
removeUnmodifiedMethods
	| theClass cat |
	self okToChange ifFalse:[^self].
	theClass := self selectedClass.
	theClass isNil ifTrue:[^self].
	cat := self selectedMessageCategoryName.
	cat isNil ifTrue:[^self].
	Cursor wait showWhile:[
		theClass removeUnmodifiedMethods: (theClass organization listAtCategoryNamed: cat).
	].
	self messageCategoryListIndex: 0.
	self changed: #classSelectionChanged.!
removeMessage
	| messageName |
	messageListIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	messageName _ self selectedMessageName.
	(self selectedClassOrMetaClass confirmRemovalOf: messageName) ifFalse:[^false].
	self selectedClassOrMetaClass removeMethod: self selectedMessageName.
	self messageListIndex: 0.
	self setClassOrganizer.  "In case organization not cached"
	self changed: #messageListChanged.!
removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	messageCategoryListIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	messageCategoryName _ self selectedMessageCategoryName.
	(self messageList size = 0
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?'])
		ifTrue: 
			[self selectedClassOrMetaClass removeCategory: messageCategoryName.
			self messageCategoryListIndex: 0.
			self changed: #classSelectionChanged]! !

!FileContentsBrowser methodsFor: 'class list'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	self selectedClassName == nil ifTrue: [^nil].
	^self selectedPackage classAt: self selectedClassName!
classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	(systemCategoryListIndex = 0 or:[self selectedPackage isNil])
		ifTrue: [^Array new]
		ifFalse: [^self selectedPackage classes keys asSortedCollection].!
findClass
	| pattern foundClass classNames index foundPackage |
	self okToChange ifFalse: [^ self classNotFound].
	pattern _ (FillInTheBlank request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self].
	classNames := Set new.
	self packages do:[:p| classNames addAll: p classes keys].
	classNames := classNames asArray select: 
		[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
	classNames isEmpty ifTrue: [^ self].
	index _ classNames size == 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUp].
	index = 0 ifTrue: [^ self].
	foundPackage := nil.
	foundClass := nil.
	self packages do:[:p| 
		(p classes includesKey: (classNames at: index)) ifTrue:[
			foundClass := p classes at: (classNames at: index).
			foundPackage := p]].
	foundClass isNil ifTrue:[^self].
 	self systemCategoryListIndex: (self systemCategoryList indexOf: foundPackage packageName asSymbol).
	self classListIndex: (self classList indexOf: foundClass name). !
renameClass
	| oldName newName |
	classListIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	oldName _ self selectedClass name.
	newName _ (self request: 'Please type new class name'
						initialAnswer: oldName) asSymbol.
	(newName isEmpty or:[newName = oldName]) ifTrue: [^ self].
	(self selectedPackage classes includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists in the package'].
	systemOrganizer classify: newName under: self selectedSystemCategoryName.
	systemOrganizer removeElement: oldName.
	self selectedPackage renameClass: self selectedClass to: newName.
	self changed: #classListChanged.
	self classListIndex: ((systemOrganizer listAtCategoryNamed: self selectedSystemCategoryName) indexOf: newName).
!
browseAllClasses
	^self findClass! !

!FileContentsBrowser methodsFor: 'edit pane'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."
	| class selector |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	contents _ class sourceCodeAt: selector.
	^self methodDiffFor: contents class: self selectedClass selector: self selectedMessageName meta: self metaClassIndicated! !

!FileContentsBrowser methodsFor: 'diffs'!
methodDiffFor: aString class: aPseudoClass selector: selector meta: meta
	| theClass source diff |
	theClass := Smalltalk at: aPseudoClass name ifAbsent:[^aString copy].
	meta ifTrue:[theClass := theClass class].
	(theClass includesSelector: selector) ifFalse:[^aString copy].
	source := theClass sourceCodeAt: selector.
	Cursor wait showWhile:[
		diff := TextDiffBuilder buildDisplayPatchFrom: source to: aString.
	].
	^diff!
modifiedClassDefinition
	| pClass rClass old new diff |
	pClass := self selectedClassOrMetaClass.
	pClass hasDefinition ifFalse:[^pClass definition].
	rClass := Smalltalk at: self selectedClass name asSymbol ifAbsent:[nil].
	rClass isNil ifTrue:[^pClass definition].
	self metaClassIndicated ifTrue:[ rClass := rClass class].
	old := rClass definition.
	new := pClass definition.
	Cursor wait showWhile:[
		diff := ClassDiffBuilder buildDisplayPatchFrom: old to: new
	].
	^diff! !

!FileContentsBrowser methodsFor: 'fileIn/fileOut'!
fileOutMessage
	Cursor write showWhile:[
		self selectedClass fileOutMethod: self selectedMessageName.
	].!
fileInClass
	Cursor read showWhile:[
		self selectedClass fileIn.
	].!
printOutSystemCategories
	^self fileInSystemCategories!
fileOutClass
	Cursor write showWhile:[
		self selectedClass fileOut.
	].!
printOutMessage
	^self fileInMessage!
fileInMessage
	Cursor read showWhile:[
		self selectedClass fileInMethod: self selectedMessageName.
	].!
printOutClass
	^self fileInClass!
fileOutSystemCategories
	Cursor read showWhile:[
		self selectedPackage fileOut.
	].!
fileInMessageCategories
	Cursor read showWhile:[
		self selectedClass fileInCategory: self selectedMessageCategoryName.
	].!
fileOutMessageCategories
	Cursor read showWhile:[
		self selectedClass fileOutCategory: self selectedMessageCategoryName.
	].!
printOutMessageCategories
	^self fileInMessageCategories!
fileInSystemCategories
	Cursor read showWhile:[
		self selectedPackage fileIn.
	].! !

!FileContentsBrowser methodsFor: 'infoView'!
packageInfo: p
	| nClasses newClasses oldClasses |
	p isNil ifTrue:[^''].
	nClasses := newClasses := oldClasses := 0.
	p classes do:[:cls|
		nClasses := nClasses + 1.
		(Smalltalk includesKey: (cls name asSymbol))
			ifTrue:[oldClasses := oldClasses + 1]
			ifFalse:[newClasses := newClasses + 1]].
	^nClasses printString,' classes (', newClasses printString, ' new / ', oldClasses printString, ' modified)'!
infoViewContents

	| theClass |
	editSelection == #newClass ifTrue:[^self packageInfo: self selectedPackage].
	self selectedClass isNil ifTrue:[^''].
	theClass := Smalltalk at: (self selectedClass name asSymbol) ifAbsent:[nil].
	editSelection == #editClass ifTrue:[
		^(theClass notNil)
			ifTrue:['Class exists already in the system']
			ifFalse:['New class']].

	(editSelection == #editMessage) ifFalse:[^''].
	(theClass notNil and:[self metaClassIndicated]) ifTrue:[theClass := theClass class].
	^(theClass notNil and:[theClass includesSelector: self selectedMessageName])
		ifTrue:['Method already exists in the system']
		ifFalse:['New method']!
updateInfoView

	self infoString contents: self infoViewContents.
	self infoString changed.! !

!FileContentsBrowser methodsFor: 'metaclass'!
setClassOrganizer
	"Install whatever organization is appropriate"
	| theClass |
	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	classListIndex = 0 ifTrue: [^ self].
	classOrganizer _ (theClass _ self selectedClass) organization.
	metaClassOrganizer _ theClass metaClass organization.
!
selectedClassOrMetaClass
	"Answer the selected class or metaclass."

	self metaClassIndicated
		ifTrue: [^ self selectedClass metaClass]
		ifFalse: [^ self selectedClass]! !

!FileContentsBrowser methodsFor: 'other'!
changeMessageCategories: aString 
	"The characters in aString represent an edited version of the the message 
	categories for the selected class. Update this information in the system 
	and inform any dependents that the categories have been changed. This 
	message is invoked because the user had issued the categories command 
	and edited the message categories. Then the user issued the accept 
	command."

	self classOrMetaClassOrganizer changeFromString: aString.
	self unlock.
	self editClass.
	self classListIndex: classListIndex.
	^ true!
buildMessageCategoryBrowser
	self removeUnmodifiedMethods!
buildClassBrowser
	^self removeUnmodifiedCategories!
buildSystemCategoryBrowser
	^self removeUnmodifiedClasses! !

!FileContentsBrowser class methodsFor: 'interface creation'!
singleFileBrowser: aBrowser editString: aString 
	"Copied from BrowserView for additional panes "

	| browserView systemCategoryListView classListView switchView
		messageCategoryListView messageListView browserCodeView 
		infoView |

	browserView _ BrowserView new model: aBrowser.
	systemCategoryListView _ BrowserView buildSystemCategoryListView: aBrowser.
	classListView _ BrowserView buildClassListView: aBrowser.
	switchView _ BrowserView buildInstanceClassSwitchView: aBrowser.
	messageCategoryListView _ BrowserView buildMessageCategoryListView: aBrowser.
	messageListView _ BrowserView buildMessageListView: aBrowser.
	browserCodeView _ BrowserView buildBrowserCodeView: aBrowser editString: aString.
	infoView _ self buildInfoView: aBrowser infoString.

	systemCategoryListView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	systemCategoryListView singleItemMode: true.
	systemCategoryListView noTopDelimiter.
	systemCategoryListView noBottomDelimiter.
	systemCategoryListView list: systemCategoryListView getList.

	browserView addSubView: systemCategoryListView.
	browserView addSubView: classListView.
	browserView addSubView: switchView.
	browserView addSubView: messageCategoryListView.
	browserView addSubView: messageListView.
	browserView addSubView: browserCodeView.
	browserView addSubView: infoView.

	switchView 
		align: switchView viewport topLeft 
		with: classListView viewport bottomLeft.
	messageCategoryListView 
		align: messageCategoryListView viewport topLeft 
		with: classListView viewport topRight.
	messageListView 
		align: messageListView viewport topLeft 
		with: messageCategoryListView viewport topRight.
	browserCodeView 
		window: browserCodeView window 
		viewport: (switchView viewport bottomLeft 
						corner: messageListView viewport bottomRight + (0 @ 110)).
	systemCategoryListView 
		window: systemCategoryListView window 
		viewport: (classListView viewport topLeft - (0 @ 12) 
						corner: messageListView viewport topRight).
	infoView
		window: infoView window
		viewport: (browserCodeView viewport bottomLeft
						corner: browserCodeView viewport bottomRight + (0 @ 15)).
	aString notNil ifTrue: [aBrowser lock].

	self patchPackageView: systemCategoryListView.
	self patchClassListView: classListView.
	self patchCategoryListView: messageCategoryListView.
	self patchMessageListView: messageListView.
	^browserView!
patchPackageView: aView
	aView controller yellowButtonMenu: (PopUpMenu labels:
'find class...
fileIn
fileOut
remove
remove existing'	lines: #(1 3 4))
				yellowButtonMessages: #(browseAllClasses printOut fileOut remove browse).!
patchMessageListView: aView
	aView controller yellowButtonMenu: (PopUpMenu labels:
'fileIn
fileOut
senders
implementors
remove'	lines: #(2 4))
				yellowButtonMessages: #(printOut fileOut
senders implementors 
rename remove).!
patchCategoryListView: aView
	aView controller yellowButtonMenu: (PopUpMenu labels:
'fileIn
fileOut
reorganize
add item...
rename...
remove
remove existing'	lines: #(2 3 6))
				yellowButtonMessages: #(printOut fileOut reorganize add rename remove browse).!
patchClassListView: aView
	aView controller yellowButtonMenu: (PopUpMenu labels:
'definition
comment
class refs
fileIn
fileOut
rename...
remove
remove existing'	lines: #(2 3 5 7))
				yellowButtonMessages: #(definition comment browseClassRefs printOut fileOut rename remove browse).!
systemCategoryBrowser: aBrowser editString: aString 
	"Answer an instance of me on the model, aBrowser. The instance consists 
	of five subviews, starting with the list view of the currently selected 
	system class category--a single item list. The initial text view part is a 
	view of the characters in aString."

	| browserView systemCategoryListView classListView switchView
		messageCategoryListView messageListView browserCodeView |

	browserView _ BrowserView new model: aBrowser.
	systemCategoryListView _ BrowserView buildSystemCategoryListView: aBrowser.
	classListView _ BrowserView buildClassListView: aBrowser.
	switchView _ BrowserView buildInstanceClassSwitchView: aBrowser.
	messageCategoryListView _ BrowserView buildMessageCategoryListView: aBrowser.
	messageListView _ BrowserView buildMessageListView: aBrowser.
	browserCodeView _ BrowserView buildBrowserCodeView: aBrowser editString: aString.

	systemCategoryListView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	systemCategoryListView singleItemMode: true.
	systemCategoryListView noTopDelimiter.
	systemCategoryListView noBottomDelimiter.
	systemCategoryListView list: systemCategoryListView getList.

	browserView addSubView: systemCategoryListView.
	browserView addSubView: classListView.
	browserView addSubView: switchView.
	browserView addSubView: messageCategoryListView.
	browserView addSubView: messageListView.
	browserView addSubView: browserCodeView.

	switchView 
		align: switchView viewport topLeft 
		with: classListView viewport bottomLeft.
	messageCategoryListView 
		align: messageCategoryListView viewport topLeft 
		with: classListView viewport topRight.
	messageListView 
		align: messageListView viewport topLeft 
		with: messageCategoryListView viewport topRight.
	browserCodeView 
		window: browserCodeView window 
		viewport: (switchView viewport bottomLeft 
						corner: messageListView viewport bottomRight + (0 @ 110)).
	systemCategoryListView 
		window: systemCategoryListView window 
		viewport: (classListView viewport topLeft - (0 @ 12) 
						corner: messageListView viewport topRight).
    
	aString notNil ifTrue: [aBrowser lock].

	^browserView!
multiFileBrowser: aBrowser editString: aString 
	"Answer an instance of me on the model, aBrowser. The instance consists 
	of five subviews, starting with the list view of system categories. The 
	initial text view part is a view of the characters in aString."

	| browserView systemCategoryListView classListView messageCategoryListView
	switchView messageListView browserCodeView infoView |

	browserView _ BrowserView new model: aBrowser.
	systemCategoryListView _ BrowserView buildSystemCategoryListView: aBrowser.
	classListView _ BrowserView buildClassListView: aBrowser.
	switchView _ BrowserView buildInstanceClassSwitchView: aBrowser.
	messageCategoryListView _ BrowserView buildMessageCategoryListView: aBrowser.
	messageListView _ BrowserView buildMessageListView: aBrowser.
	browserCodeView _ BrowserView buildBrowserCodeView: aBrowser editString: aString.
	infoView _ self buildInfoView: aBrowser infoString.

	browserView addSubView: systemCategoryListView.
	browserView addSubView: classListView.
	browserView addSubView: switchView.
	browserView addSubView: messageCategoryListView.
	browserView addSubView: messageListView.
	browserView addSubView: browserCodeView.
	browserView addSubView: infoView.

	classListView 
		align: classListView viewport topLeft 	
		with: systemCategoryListView viewport topRight.
	switchView
		align: switchView viewport topLeft
		with: classListView viewport bottomLeft.
	messageCategoryListView 
		align: messageCategoryListView viewport topLeft 
		with: classListView viewport topRight.
	messageListView 
		align: messageListView viewport topLeft 
		with: messageCategoryListView viewport topRight.
	browserCodeView 
		align: browserCodeView viewport topLeft 
		with: systemCategoryListView viewport bottomLeft.
	infoView
		window: infoView window
		viewport: (browserCodeView viewport bottomLeft
						corner: browserCodeView viewport bottomRight + (0 @ 15)).
	self patchPackageView: systemCategoryListView.
	self patchClassListView: classListView.
	self patchCategoryListView: messageCategoryListView.
	self patchMessageListView: messageListView.
	aString notNil ifTrue: [aBrowser lock].

	^browserView!
buildInfoView: model

	| infoView |
	infoView _ StringHolderView new.
	infoView model: model.
	infoView window: (0 @ 0 extent: 200 @ 110).
	infoView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.
	^infoView! !

!FileContentsBrowser class methodsFor: 'instance creation'!
browseFiles: fileList
	"FileContentsBrowser browseFiles: 'f:\1.31\apps\Finalization\*.st'"
	| browser package organizer packageDict view |
	packageDict := Dictionary new.
	organizer := SystemOrganizer defaultList: Array new.
	fileList do:[:fileName|
		package := FilePackage fromFileNamed: fileName.
		packageDict at: package packageName put: package.
		organizer classifyAll: package classes keys under: package packageName.
	].
	browser := self new.
	browser systemOrganizer: organizer.
	browser packages: packageDict.
	view := self multiFileBrowser: browser editString: nil.
	view insideColor: (Color r: 0.8 g: 0.8 b: 0.5).
	BrowserView
		openBrowserView: view
		label: 'Package Browser'
!
browseFile: aFilename
	| browser package organizer packageDict view |
	package := FilePackage fromFileNamed: aFilename.
	packageDict := Dictionary new.
	packageDict at: package packageName put: package.
	organizer := SystemOrganizer defaultList: Array new.
	organizer addCategory: package packageName before: nil.
	organizer classifyAll: package classes keys under: package packageName.
	browser := self new.
	browser systemOrganizer: organizer.
	browser packages: packageDict.
	browser systemCategoryListIndex: 1.
	view := self singleFileBrowser: browser editString: nil.
	view insideColor: (Color r: 0.8 g: 0.8 b: 0.5).
	BrowserView
		openBrowserView: view
		label: 'Package Browser'! !

!ClassDiffBuilder methodsFor: 'initialize'!
split: aString
	| lines in out c |
	lines := OrderedCollection new.
	in := ReadStream on: aString.
	out := WriteStream on: String new.
	[in atEnd] whileFalse:[
		(c := in next) isSeparator ifTrue:[
			out nextPut: c.
			lines add: out contents.
			out reset.
		] ifFalse:[
			out nextPut: c.
		].
	].
	out position = 0 ifFalse:[
		lines add: out contents.
	].
	^lines! !

!ClassDiffBuilder methodsFor: 'printing'!
printPatchSequence: ps on: aStream
	| type line attr |
	ps do:[:assoc|
		type := assoc key.
		line := assoc value.
		attr := TextEmphasis normal.
		type == #insert ifTrue:[attr := TextColor red].
		type == #remove ifTrue:[attr := TextEmphasis struckOut].
		aStream withAttribute: attr do:[aStream nextPutAll: line].
	].! !

!FileListController methodsFor: 'menu messages'!
browseFile

	self controlTerminate.
	model browseFile.
	self controlInitialize.
! !

!FileListController class methodsFor: 'class initialization'!
initialize   "FileListController initialize"
	"Initialize the file list menu.  6/96 di; modified 7/12/96 sw to add the file-into-new-change-set feature"

	FileListYellowButtonMenu _ PopUpMenu labels:
'fileIn
file into new change set
browse changes
browse selected file(s)
spawn this file
copy name to clipboard
open image in a window
read image into GIFImports
play midi file
import vrml file
parse through C preprocessor
sort by name
sort by size
sort by date
rename
delete
add new file
broadcast as update' lines: # (6 8 11 14).
	FileListYellowButtonMessages _
#(fileInSelection fileIntoNewChangeSet browseChanges browseFile editFile copyName
 openImageInWindow importImage playMidiFile importVRMLFile parseCPP
 sortByName sortBySize sortByDate
 renameFile deleteFile addNewFile putUpdate)! !

!PseudoClassOrganizer methodsFor: 'all'!
classComment: aChangeRecord
	globalComment := aChangeRecord!
setDefaultList: aCollection
	super setDefaultList: aCollection.
	self classComment: nil.! !

!FileModel methodsFor: 'accessing'!
browseFile
	FileContentsBrowser browseFile: self fullName.
! !

!OrderedCollection methodsFor: 'adding'!
add: newObject beforeIndex: index 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just preceding index. Answer newObject."

	self insert: newObject before: firstIndex + index.
	^newObject! !

!FileList methodsFor: 'menu messages'!
browseFile
	| selectionPattern fd |
	fileName isNil ifFalse:[^FileContentsBrowser browseFile: self fullName].
	selectionPattern := FillInTheBlank request:'What files?' initialAnswer:'*.st'.
	fd := directory.
	FileContentsBrowser browseFiles: 
		((fd fileNamesMatching: selectionPattern) 
			collect:[:fn| fd pathName, (String with: FileDirectory pathNameDelimiter), fn]).! !

!PseudoMetaclass methodsFor: 'accessing'!
fullName
	^self name,' class'!
realClass
	^super realClass class! !

!PseudoMetaclass methodsFor: 'testing'!
isMetaClass
	^true! !

!TextDiffBuilder methodsFor: 'printing'!
printPatchSequence: ps on: aStream
	| type line attr |
	ps do:[:assoc|
		type := assoc key.
		line := assoc value.
		attr := TextEmphasis normal.
		type == #insert ifTrue:[attr := TextColor red].
		type == #remove ifTrue:[attr := TextEmphasis struckOut].
		aStream withAttribute: attr do:[aStream nextPutAll: line; cr].
	].
	! !

!TextDiffBuilder methodsFor: 'initialize'!
from: sourceString to: destString
	self sourceString: sourceString.
	self destString: destString.!
sourceString: aString
	realSrc := self split: aString.
	srcLines := OrderedCollection new.
	srcMap := OrderedCollection new.
	realSrc doWithIndex:[:line :realIndex|
		"(line contains:[:anyChar| anyChar isSeparator not]) ifTrue:["
			srcLines add: line.
			srcMap add: realIndex.
		"]."
	].
	srcPos := Dictionary new: srcLines size.
	srcLines doWithIndex:[:line :index| 
		(srcPos includesKey: line)
			ifTrue:[(srcPos at: line) add: index. multipleMatches := true]
			ifFalse:[srcPos at: line put: (OrderedCollection with: index)]].!
destString: aString
	realDst := self split: aString.
	dstLines := OrderedCollection new.
	dstMap := OrderedCollection new.
	realDst doWithIndex:[:line :realIndex|
		"(line contains:[:anyChar| anyChar isSeparator not]) ifTrue:["
			dstLines add: line.
			dstMap add: realIndex.
		"]."
	].
	dstPos := Dictionary new: dstLines size.
	dstLines doWithIndex:[:line :index|
		(dstPos includesKey: line)
			ifTrue:[(dstPos at: line) add: index. multipleMatches := true]
			ifFalse:[dstPos at: line put: (OrderedCollection with: index)]].!
split: aString
	^self split: aString by: self splitCharacter! !

!TextDiffBuilder methodsFor: 'testing'!
hasMultipleMatches
	^multipleMatches == true! !

!TextDiffBuilder methodsFor: 'creating patches'!
incorporateRemovalsInto: aPatchSequence
	"Incorporate removals"
	| index |
	removed ifNil:[^self].
	removed do:[:assoc|
		index := assoc key.
		self assert:[(aPatchSequence at: index) isNil].
		aPatchSequence at: index put: #remove -> assoc value.
	].
!
processDiagonals

	^self processDiagonalsFrom: matches keys asSet
!
incorporateAddsInto: aPatchSequence
	"Incorporate adds"
	| lastMatch lastIndex index |
	added ifNil:[^self].
	added := added sortBy:[:a1 :a2| a1 key < a2 key].
	lastMatch := 1.
	lastIndex := 0.
	1 to: added size do:[:i|
		index := (added at: i) key.
		[index > lastMatch] whileTrue:[
			[lastIndex := lastIndex + 1.
			(aPatchSequence at: lastIndex) key == #match] whileFalse.
			lastMatch := lastMatch + 1.
		].
		aPatchSequence add: #insert->(added at: i) value beforeIndex: lastIndex.
		lastIndex := lastIndex + 1.
		lastMatch := lastMatch + 1.
	].!
incorporateMatchesInto: aPatchSequence
	"Incorporate matches"
	| index |
	runs associationsDo:[:assoc|
		index := assoc key y.
		assoc value do:[:line|
			self assert:[(aPatchSequence at: index) isNil].
			aPatchSequence at: index put: (#match -> line).
			index := index + 1.
		].
	].
!
collectRunFrom: todo startingWith: startIndex into: run
	| next start |
	start := startIndex.
	self remove: start from: todo.
	run add: (matches at: start).
	"Search downwards"
	next := start.
	[next := next + (1@1).
	todo includes: next] whileTrue:[
		run addLast: (matches at: next).
		self remove: next from: todo].
	"Search upwards"
	next := start.
	[next := next - (1@1).
	todo includes: next] whileTrue:[
		run addFirst: (matches at: next).
		self remove: next from: todo.
		start := next. "To use the first index"
	].
	^start!
processShiftedRuns
	| key |
	shifted isNil ifTrue:[^self].
	shifted do:[:assoc|
		key := assoc key.
		assoc value doWithIndex:[:line :idx|
			removed add: (key y + idx - 1) -> line.
			added add: (key x + idx - 1) -> line].
		runs removeKey: assoc key.
	].
!
buildDisplayPatch
	^Text streamContents:[:stream|
		self printPatchSequence: self buildPatchSequence on: stream.
	]!
buildReferenceMap
	dstLines doWithIndex:[:line :index|
		(srcPos includesKey: line) 
			ifTrue:[(srcPos at: line) do:[:index2|
						matches at: index@index2 put: line]]
	].
	srcLines doWithIndex:[:line :index|
		(dstPos includesKey: line)
			ifTrue:[(dstPos at: line) do:[:index2|
						matches at: index2@index put: line]]
	].
!
validateRuns: runList
	| srcPosCopy dstPosCopy lines srcIndex dstIndex |
	srcPosCopy := Dictionary new: srcPos size.
	srcPos associationsDo:[:assoc| srcPosCopy at: assoc key put: assoc value asSet].
	dstPosCopy := Dictionary new: dstPos size.
	dstPos associationsDo:[:assoc| dstPosCopy at: assoc key put: assoc value asSet].
	runList associationsDo:[:assoc|
		srcIndex := assoc key y.
		dstIndex := assoc key x.
		lines := assoc value.
		lines do:[:string|
			(srcPosCopy at: string) remove: srcIndex.
			(dstPosCopy at: string) remove: dstIndex.
			srcIndex := srcIndex + 1.
			dstIndex := dstIndex + 1.
		].
	].
	removed := OrderedCollection new.
	srcPosCopy associationsDo:[:assoc|
		assoc value do:[:index| removed add: (index -> assoc key)].
	].
	removed := removed sortBy:[:a1 :a2| a1 key < a2 key].
	added := OrderedCollection new.
	dstPosCopy associationsDo:[:assoc|
		assoc value do:[:index| added add: (index -> assoc key)].
	].
	added := added sortBy:[:a1 :a2| a1 key < a2 key].
!
generatePatchSequence
	| ps |
	ps := OrderedCollection new: srcLines size.
	srcLines size timesRepeat:[ps add: nil].
	self incorporateMatchesInto: ps.
	self incorporateRemovalsInto: ps.
	self incorporateAddsInto: ps.
	^ps!
detectShiftedRuns
	| sortedRuns lastY run shiftedRuns |
	runs size < 2 ifTrue: [^ nil].
	shiftedRuns _ OrderedCollection new.
	sortedRuns _ SortedCollection sortBlock: [:a1 :a2 | a1 key x < a2 key x].
	runs associationsDo: [:assoc | sortedRuns add: assoc].
	lastY _ sortedRuns first key y.
	2 to: sortedRuns size do:[:i | 
		run _ sortedRuns at: i.
		run key y > lastY
			ifTrue: [lastY _ run key y]
			ifFalse: [shiftedRuns add: run]].
	^ shiftedRuns!
processDiagonalsFrom: todoList
	| runList start run todo |
	todo := todoList copy.
	runList := Dictionary new.
	[todo isEmpty] whileFalse:[
		start := todo detect:[:any| true].
		run := OrderedCollection new.
		start := self 
					collectRunFrom: todo 
					startingWith: start 
					into: run.
		runList at: start put: run.
	].
	"If we have multiple matches we might have chosen a bad sequence.
	There we redo the whole thing recursively"
	self hasMultipleMatches  ifFalse:[^runList].
	runList size < 2 ifTrue:[^runList].

	run := nil.
	start := 0.
	runList associationsDo:[:assoc|
		(run isNil or:[assoc value size > run size]) ifTrue:[
			run := assoc value.
			start := assoc key]].
	"Now found the longest run"
	run := OrderedCollection new.
	start := self
				collectRunFrom: todoList
				startingWith: start
				into: run.
	"Find the diagonals in the remaining set"
	runList := self processDiagonalsFrom: todoList.
	runList at: start put: run.
	^runList!
buildPatchSequence
	"@@ TODO: Das funktioniert noch nicht fŸr n-m matches"
	matches := Dictionary new.
	self buildReferenceMap.
	runs := self processDiagonals.
	self validateRuns: runs.
	"There may be things which have just been moved around. Find those."
	shifted := self detectShiftedRuns.
	self processShiftedRuns.
	"Now generate a patch sequence"
	patchSequence := self generatePatchSequence.
	^patchSequence! !

!TextDiffBuilder methodsFor: 'private'!
assert: aBlock
	aBlock value ifFalse:[self error: 'Assertion failed']!
splitCharacter
	^Character cr!
split: aString by: splitChar
	| lines in out c |
	lines := OrderedCollection new.
	in := ReadStream on: aString.
	out := WriteStream on: String new.
	[in atEnd] whileFalse:[
		(c := in next) = splitChar ifTrue:[
			lines add: out contents.
			out reset.
		] ifFalse:[
			out nextPut: c.
		].
	].
	out position = 0 ifFalse:[
		lines add: out contents.
	].
	^lines!
remove: pointKey from: aSet
	| sArray obj |
	self hasMultipleMatches ifFalse:[^aSet remove: pointKey].
	sArray := aSet asArray.
	1 to: sArray size do:[:i|
		obj := sArray at: i.
		obj x = pointKey x ifTrue:[
			aSet remove: obj.
		] ifFalse:[
			obj y = pointKey y ifTrue:[
				aSet remove: obj.
			].
		]
	].
! !

!TextDiffBuilder class methodsFor: 'instance creation'!
from: srcString to: dstString
	^self new from: srcString to: dstString!
buildDisplayPatchFrom: srcString to: dstString
	^(self from: srcString to: dstString) buildDisplayPatch! !

!ChangeRecord methodsFor: 'access'!
methodClassName
	^class!
isMetaClassChange
	^meta!
category
	^category! !

!FilePackage methodsFor: 'accessing'!
packageName
	^packageName!
classAt: className
	^self classes at: className!
renameClass: aPseudoClass to: newName
	| oldName |
	oldName := aPseudoClass name.
	self classes removeKey: oldName.
	self classes at: newName put: aPseudoClass.
	aPseudoClass renameTo: newName.!
classes
	^classes!
fullPackageName
	^fullName!
packageInfo
	^String streamContents:[:s|
		s nextPutAll:'Package: '.
		s nextPutAll: self fullPackageName; cr; cr.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; cr; cr].
		doIts isEmpty ifFalse:[
			s nextPutAll:'Unresolvable doIts:'; cr; cr.
			doIts do:[:chgRec|
				s nextPut:$!!; nextPutAll: chgRec string; nextPut: $!!; cr]]].!
removeClass: aPseudoClass
	(self classes removeKey: aPseudoClass name).
	classOrder copy do:[:cls|
		cls name = aPseudoClass name ifTrue:[ classOrder remove: cls].
	].! !

!FilePackage methodsFor: 'initialize'!
fromFileNamed: aName
	| stream |
	fullName := aName.
	packageName := FileDirectory localNameFor: fullName.
	stream := FileStream readOnlyFileNamed: aName.
	doIts := OrderedCollection new.
	classOrder := OrderedCollection new.
	sourceSystem := ''.
	self fileInFrom: stream.! !

!FilePackage methodsFor: 'private'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass commentString: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass metaClass commentString: tokens last].
!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.!
removedMethod: string with: chgRec
	| class tokens |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 2) == #removeSelector: ]) ifTrue:[
		class := self getClass: (tokens at: 1).
		^class removeSelector: (tokens at: 3).
	].
	(tokens size = 4 and:[(tokens at: 2) == #class and:[(tokens at: 3) == #removeSelector:]]) ifTrue:[
		class := self getClass: (tokens at: 1).
		^class metaClass removeSelector: (tokens at: 4).
	].
	doIts add: chgRec!
metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass metaClass definition: string.
	classOrder add: theClass metaClass.!
sampleMethod
"	In an existing method there are always a number of changes.
	Other stuff
		will be deleted
	Or even better,
		some things may be just modified.
"!
classDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.!
getClass: className
	| pseudoClass |
	(classes includesKey: className) ifTrue:[
		^classes at: className.
	].
	pseudoClass := PseudoClass new.
	pseudoClass name: className.
	classes at: className put: pseudoClass.
	^pseudoClass.! !

!FilePackage methodsFor: 'change record types'!
method: chgRec
	(self getClass: chgRec methodClassName) methodChange: chgRec!
doIt: chgRec
	| string |
	string := chgRec string.
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[^self metaClassDefinition: string with: chgRec].
	('* removeSelector: *'
		match: string) ifTrue:[^self removedMethod: string with: chgRec].
	('* comment:*'
		match: string) ifTrue:[^self msgClassComment: string with: chgRec].
	('* initialize'
		match: string) ifTrue:[^self]. "Initialization is done based on class>>initialize"
	('''From *'
		match: string) ifTrue:[^self possibleSystemSource: chgRec].
	doIts add: chgRec.!
classComment: chgRec

	(self getClass: chgRec methodClassName) classComment: chgRec!
preamble: chgRec
	self doIt: chgRec! !

!FilePackage methodsFor: 'fileIn/fileOut'!
fileOutDoits: aStream
	doIts do:[:chgRec| chgRec fileOutOn: aStream].!
fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		Transcript cr; show:'Filing in ', cls name.
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls metaClass fileInMethods].
	].
	doitsMark = 3 ifTrue:[self fileInDoits].!
fileOut
	| fileName stream |
	fileName := FillInTheBlank request: 'Enter the file name' initialAnswer:''.
	stream := FileStream newFileNamed: fileName.
	sourceSystem isEmpty ifFalse:[
		stream nextChunkPut: sourceSystem printString;cr ].
	self fileOutOn: stream.
	stream cr; cr.
	self classes do:[:cls|
		cls needsInitialize ifTrue:[
			stream cr; nextChunkPut: cls name,' initialize']].
	stream cr.
	stream close.!
fileInDoits
	doIts do:[:chgRec| chgRec fileIn].!
askForDoits
	| menu choice choices |
	choices := #('do not process' 'at the beginning' 'at the end').
	menu _ SelectionMenu selections: choices.
	choice := nil.
	[choices includes: choice] whileFalse: [
		choice _ menu startUpWithCaption: 
'The package contains unprocessed doIts.
When would like to process those?'].
	^choices indexOf: choice!
fileOutOn: aStream
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 2 ifTrue:[self fileOutDoits: aStream].
	classOrder do:[:cls|
		cls fileOutDefinitionOn: aStream.
	].
	classes do:[:cls|
		cls fileOutMethodsOn: aStream.
		cls hasMetaclass ifTrue:[cls metaClass fileOutMethodsOn: aStream].
	].
	doitsMark = 3 ifTrue:[self fileOutDoits: aStream].! !

!FilePackage methodsFor: 'reading'!
fileInFrom: aStream
	| chgRec changes |
	changes := (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	aStream close.
	classes := Dictionary new.
	('Processing ', self packageName) 
		displayProgressAt: Sensor cursorPoint
		from: 1
		to: changes size
		during:[:bar|
			1 to: changes size do:[:i|
				bar value: i.
				chgRec := changes at: i.
				self perform: (chgRec type copyWith: $:) asSymbol with: chgRec.
			].
		].! !

!FilePackage class methodsFor: 'instance creation'!
fromFileNamed: aName
	^self new fromFileNamed: aName! !


FileListController initialize!

