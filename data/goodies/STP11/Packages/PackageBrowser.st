"PackageBrowser.st
This is a basic 6-paned browser, i.e., it parses the system categories into a 2-level hierarchy. There are still a few bugs.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.1
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE   July 8, 1997"!

Browser subclass: #PackageBrowser
	instanceVariableNames: 'package category '
	classVariableNames: 'CategoryMenu RemoveChangesOnFileOut '
	poolDictionaries: ''
	category: 'Tools-Browser'!
PackageBrowser comment:
'A package browser represents a hierarchical query path through an organization of class and method information.  
It parses class categories into a two-level hierarchy on the first ''-'' character, giving "packages" (e.g.,  Magnitude, Collections, Graphics, etc.), and "categories" (e.g., Magnitude-General and Magnitude-Number).

Instance Variables:
	package		 <Symbol> the "category header," e.g., #Magnitudes or #Collections.
'!

!PackageBrowser methodsFor: 'package'!
fileOutPackage
	"File out all the categories in the current package."

	| fileName fileManager |
	fileName := FillInTheBlank
			request: 'File out on'
			initialAnswer: (self filterFilename: category, '.st'). 
	fileName = '' ifTrue: [^nil].
	fileManager := SourceCodeStream on: fileName asFilename writeStream.
	(self categoriesInPackage: package) do: [ :cat | 
		[systemOrganizer fileOutCategorySource: cat on: fileManager]
			valueNowOrOnUnwindDo: [fileManager close].
		RemoveChangesOnFileOut
			ifTrue: [((systemOrganizer listAtCategoryNamed: cat)
				collect: [ :n | Smalltalk at: n])
					do: [ :c | ChangeSet current removeClassChanges: c ]]]! !

!PackageBrowser methodsFor: 'package'!
package
	"Answer the receiver's 'package'."

	^package! !

!PackageBrowser methodsFor: 'package'!
package: selection
	"Set the receiver's package to the argument, updating the category list if appropriate."

	| cat index |
	editSelection := #none.
	selection == nil
		ifFalse: [(selection includes: $-) 
			ifTrue: [index := selection indexOf: $-.
				package := (selection copyFrom: 1 to: index) asSymbol.
				cat := (selection copyFrom: index to: selection size) asSymbol]
			ifFalse: [package := (selection copyUpTo: $-) asSymbol]]
		ifTrue: [package := nil].
	self newCategoryList: selection.
	cat == nil ifTrue: [self category: cat].
! !

!PackageBrowser methodsFor: 'package'!
packageList
	"Answer a list of the packages in the current system organization."

	| str cats stream |
	str := Set new: 100.
	stream := WriteStream on: (Array new: 100).
	systemOrganizer categories do: [ :categ | 
		cats := categ asString copyUpTo: $-.
		(str includes: cats) ifFalse: 
			[str add: cats.
			stream nextPut: cats]].
	^stream contents! !

!PackageBrowser methodsFor: 'package'!
packageListMenu
	^PopUpMenu new! !

!PackageBrowser methodsFor: 'package'!
packageMenu
	"Answer a Menu of operations on class packages to be 
	displayed when the operate menu button is pressed."

	^SelectionMenu
			labels: 'update\edit all\find class...\find recent...' withCRs
			lines: #(2)
			selectors: #(#updatePackages #editCategories #findClass #recent)! !

!PackageBrowser methodsFor: 'package' stamp: 'stp 07/09/97 02:43'!
recent
	"Let the user select from a list of recently visited classes."

	| items menu choice class |
	items := WriteStream on: String new.
	RecentClasses do: [ :cl | items nextPutAll: cl name, '\'].
	items skip: -1.
	menu := PopUpMenu labels: items contents withCRs.
	choice := menu startUp.
	choice = 0 ifTrue: [^self].
	class := Smalltalk at: (RecentClasses at: choice).
	self package: class category.
	self changed: #package.
	self category: class category.
	self changed: #category.
	self changed: #category.
	self classListIndex: (self classList indexOf: class name)! !

!PackageBrowser methodsFor: 'package'!
updatePackages
	"Update the contents of the package list."

	editSelection := #none.
	self changed: #packageList.
	self changed: #package.
	self newCategoryList: nil! !


!PackageBrowser methodsFor: 'category'!
addSystemCategory
	"Prompt the user for a new category name. Insert the category into 
	the class organization before the currently selected category."

	| aString newCategory |
	self okToChange ifFalse: [^ self].
	aString := FillInTheBlank request: 'Enter new category name' initialAnswer: 'category name'.
	aString isEmpty ifTrue: [^self].
	newCategory := aString asSymbol.
	systemOrganizer addCategory: newCategory before: category.
	ChangeSet current reorganizeSystem.
	self updatePackages.
	self package: newCategory! !

!PackageBrowser methodsFor: 'category'!
category
	"Answer the receiver's 'category'."

	^category! !

!PackageBrowser methodsFor: 'category'!
category: selection 
	"Set the receiver's currently selected category to be selection and 
	update the class list."

	| footer |
	category = '**Hierarchy**' ifTrue: [^nil].
	selection == nil
		ifTrue: [category := selection.
			footer := '']
		ifFalse: [category := selection asSymbol.
			(package == nil or: [(selection indexOf: $-) = 0 or:
				 [(selection copyUpTo: $-) = package]])
					ifTrue: [category := selection asSymbol.
						footer := selection copyFrom: package size + 1 to: category size]
					ifFalse: [category := (package, selection) asSymbol.
						footer := selection]].
"	package == nil ifFalse: [self changed: #category]."
	systemCategoryListIndex := self categoryList indexOf: footer asString.
	self changed: #systemCategorySelectionChanged! !

!PackageBrowser methodsFor: 'category'!
categoryList
	"Answer the sequenceable collection containing the class categories that 
	the receiver accesses."

	| num str stream pname |
	package isNil ifTrue: [^systemOrganizer categories].
	pname := package, '-'.
	num := pname size.
	stream := WriteStream on: (Array new: 100).
	systemOrganizer categories do: [ :cat | 
		cat size >= num ifTrue: [(cat copyFrom: 1 to: num) = pname
					ifTrue: [str := cat copyFrom: (cat indexOf: $- ifAbsent: [1]) to: cat size.
						stream nextPut: str]]].
	^stream contents! !

!PackageBrowser methodsFor: 'category' stamp: 'stp 07/09/97 02:36'!
categoryMenu
	"Answer a Menu of operations on class categories to be 
	displayed when the operate menu button is pressed."
	"CategoryMenu := nil"

	category == nil ifTrue: [^SelectionMenu
			labels: 'add...\update\edit all\find class...\find recent' withCRs
			lines: #(1 3 )
			selectors: #(#addSystemCategory #updateSystemCategories #editSystemCategories #findClass #recent)].
	CategoryMenu == nil ifTrue: [CategoryMenu := SelectionMenu
					labels: 'file out as...\hardcopy\spawn\add...\rename as...\remove...\update\edit all\find class...\find recent' withCRs
					lines: #(3 6 8 )
					selectors: #(#fileOutSystemCategory #printOutSystemCategory #spawnCategory #addSystemCategory #renameSystemCategory #removeSystemCategory #updateCategories #editSystemCategories #findClass #recent)].
	^CategoryMenu! !

!PackageBrowser methodsFor: 'category'!
currentCategory
	"Answer the tail of the receiver's currently selected category."

	category == nil ifTrue: [^nil].
	^category copyFrom: (category indexOf: $- ifAbsent: [1]) to: category size! !

!PackageBrowser methodsFor: 'category'!
fileOutCategory
	"Print a description of each class in the selected category onto a file 
	whose name is the category name followed by .st."

	systemCategoryListIndex ~= 0
		ifTrue: [systemOrganizer fileOutCategory: self category]! !

!PackageBrowser methodsFor: 'category' stamp: 'stp 07/09/97 02:42'!
findClass
	"modified  so that if only 1 class matches the user-supplied string, or if the user-supplied string exactly matches a class name, then the pop-up menu is bypassed"
	| pattern foundClass classNames index reply |
	pattern _ (reply _ FillInTheBlank request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self classNotFound].
	(Smalltalk hasClassNamed: reply)
		ifTrue:
			[foundClass _ Smalltalk at: reply asSymbol]
		ifFalse:
 			[classNames _ Smalltalk classNames asArray select: 
				[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
			classNames isEmpty ifTrue: [^ self classNotFound].
			index _ classNames size == 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUp].
			index = 0 ifTrue: [^ self classNotFound].
			foundClass _ Smalltalk at: (classNames at: index)].
 	self systemCategoryListIndex: (self systemCategoryList indexOf: foundClass category).
	self classListIndex: (self classList indexOf: foundClass name). 
! !

!PackageBrowser methodsFor: 'category'!
newCategoryList: initialSelection
	"Set the currently selected category to be initialSelection."

	| str |
	category := initialSelection.
	category == nil 
		ifFalse: [str := category asString copyUpTo: $-.
			str = package
				ifFalse: [package := str]].
	self changed: #systemCategoriesChanged.
	self changed: #categoryList! !

!PackageBrowser methodsFor: 'category'!
updateCategories
	"Update the contents of the category list."

	self changed: #packageList.
	self newCategoryList: category! !


!PackageBrowser methodsFor: 'class'!
classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	systemCategoryListIndex = 0
		ifTrue: [^Array new]
		ifFalse: [^systemOrganizer listAtCategoryNamed: category]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

PackageBrowser class
	instanceVariableNames: ''!

!PackageBrowser class methodsFor: 'instance creation'!
open
	"PackageBrowser open"

	PackageBrowserView openBrowserView: 
			(PackageBrowserView packageBrowser: self new editString: nil)
		label:
			'Squeak Package Browser'! !


BrowserView subclass: #PackageBrowserView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

PackageBrowserView class
	instanceVariableNames: ''!

!PackageBrowserView class methodsFor: 'instance creation'!
packageBrowser: aBrowser editString: aString 
	"PackageBrowserView packageBrowser: PackageBrowser new editString: nil"

	| browserView packageListView systemCategoryListView classListView switchView
		messageCategoryListView messageListView browserCodeView |
	browserView := self new model: aBrowser.
	packageListView := PluggableListView on: aBrowser 
			list: #packageList selected: #package 
			changeSelected: #package: menu: #packageMenu.
	packageListView window: (0 @ 0 extent: 30 @ 70).
	packageListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	systemCategoryListView := PluggableListView on: aBrowser 
			list: #categoryList selected: #category 
			changeSelected: #category: menu: #categoryMenu.
	systemCategoryListView window: (0 @ 0 extent: 30 @ 70).
	systemCategoryListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	classListView := self buildClassListView: aBrowser.
	switchView := self buildInstanceClassSwitchView: aBrowser.
	messageCategoryListView := self buildMessageCategoryListView: aBrowser.
	messageListView := self buildMessageListView: aBrowser.
	browserCodeView := self buildBrowserCodeView: aBrowser editString: aString.
	browserCodeView window: (0 @ 0 extent: 210 @ 110).
	browserView addSubView: packageListView.
	browserView addSubView: systemCategoryListView.
	browserView addSubView: classListView.
	browserView addSubView: switchView.
	browserView addSubView: messageCategoryListView.
	browserView addSubView: messageListView.
	browserView addSubView: browserCodeView.
	systemCategoryListView 
		align: systemCategoryListView viewport topLeft 
		with: packageListView viewport topRight.
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
		with: packageListView viewport bottomLeft.
	aString notNil ifTrue: [aBrowser lock].
	^browserView! !


!PackageBrowserView class methodsFor: 'instance scheduling'!
open
	"PackageBrowserView open"

	self openBrowserView: 
			(PackageBrowserView packageBrowser: PackageBrowser new editString: nil)
		label:
			'Squeak Package Browser'! !

!PackageBrowserView class methodsFor: 'instance scheduling'!
openPackageBrowser: aBrowser editString: aString 
	"PackageBrowserView openPackageBrowser: PackageBrowser new editString: nil "

	self openBrowserView: 
			(PackageBrowserView packageBrowser: aBrowser editString: aString)
		label:
			'Squeak Package Browser'! !
