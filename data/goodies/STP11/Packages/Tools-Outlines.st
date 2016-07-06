"Tools-Outlines.st
This is my simple outline view application.
An outline is just a keyed collection of texts (with local variable dictionaries).
The outline browser lets one move between these 'chapters' and read/write outlines from/to files (saving local variables in the process).
In this version, outlines are only 1 level deep--are there any volunteers to make it hierarchical? 
Two outline files are included as demonstrations: a simple test, and one that includes the contents of the 3 default Squeak text views (welcome, Squeak UI, change management).

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   changesForOutlines.st, Interface-PluggableMVC.st, Kernel-Values.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 26, 1996"!

Model subclass: #Outline
	instanceVariableNames: 'organization fileName keySeparator entrySeparator '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Outlines'!
Outline comment:
'An outline is a simple interface to a text structure for browsing.
It can hold onto a keyed text list, parse it from/save it to files, and be used with an OutlineBrowser.

Instance variables:
	organization 		<OrderedCollection of Associations> the list of text key -> text entry
	fileName 			<String> the file I was read in from (when relevant)
	keySeparator		<String> the separator between a key and its entry
	entrySeparator 	<String> the separator an entry and the following key'!

!Outline methodsFor: 'initialize-release'!
initialize
	"Set up meaningful default values for the receiver."

	organization :=  OrderedCollection new.
	keySeparator :=  '
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'.
	entrySeparator := '
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'.! !


!Outline methodsFor: 'accessing'!
assAt: aKey
	"Answer the association out of the organization."

	^organization detect: [ :ass | ass key = aKey] ifNone: [nil]! !

!Outline methodsFor: 'accessing'!
at: aKey
	"Answer the value out of the organization."

	| item |
	item := organization detect: [ :ass | ass key = aKey] ifNone: [nil].
	^item isNil
		ifFalse: [item value]
		ifTrue: [nil]! !

!Outline methodsFor: 'accessing'!
at: aKey put: aVal
	"Plug the given value into the organization."

	| item |
	item := self at: aKey.
	item isNil
		ifTrue: [organization add: (aKey -> aVal)]
		ifFalse: [item value: aVal]! !

!Outline methodsFor: 'accessing'!
name: aName
	"Set the receiver's file name"

	fileName :=  aName! !

!Outline methodsFor: 'accessing'!
organization
	"Answer the receiver's organization."

	^organization! !

!Outline methodsFor: 'accessing'!
organization: anOrg
	"Set the receiver's outline (being careful about nils)"

	anOrg isNil ifTrue: [^self].
	organization :=  anOrg! !

!Outline methodsFor: 'accessing'!
organizationList
	"Answer the keys of the outline."

	^organization collect: [ :ass | ass key]! !

!Outline methodsFor: 'accessing'!
removeKey: aKey
	"Remove the named item from the organization."

	| item |
	item := organization detect: [ :ass | ass key = aKey]
			ifNone: [nil].
	item isNil
		ifFalse: [organization remove: item]! !


!Outline methodsFor: 'parsing'!
parseFrom: aStream keySeparator: keyStr entrySeparator: entryStr
	"Read in the receiver's organization using the given separators (as strings)"

	| tmp key body |
	entrySeparator class == Character
		ifTrue: [entrySeparator :=  String with: entryStr]
		ifFalse: [entrySeparator :=  entryStr].
	keySeparator class == Character
		ifTrue: [keySeparator :=  String with: keyStr]
		ifFalse: [keySeparator :=  keyStr].
	[aStream atEnd]
		whileFalse: 
			[tmp :=  ReadStream on: (aStream upToAll: entrySeparator).
			key :=  Object readFromString: (tmp upToAll: keySeparator).
			body :=  Object readFromString: tmp upToEnd.
			body isString
				ifTrue: [body := Environment contents: body asText].
			organization add: (key -> body)]! !


!Outline methodsFor: 'fileIn/Out'!
saveAll
	"prompt the user for a name and write the whole outline on a fileStream"

	| aFS aName |
	aName :=  fileName.
	aName isNil ifTrue: [aName := ''].
	aName :=  FillInTheBlank 
			request: 'file name for saved outline: ' 
			initialAnswer: aName.
	aName = '' ifTrue: [^self].
	fileName := aName.
	aFS :=  FileStream fileNamed: aName.
	Cursor write showWhile:
			[self storeOn: aFS].
	aFS close.! !

!Outline methodsFor: 'fileIn/Out'!
saveCategory: aKey
	"prompt the user for a name and write the current text on a fileStream"

	| aFS aName |
	aName :=  fileName.
	aName == nil ifTrue: [ aName :=  '' ].
	aName :=  FillInTheBlank 
			request: 'file name for saved outline: ' 
			initialAnswer: aName.
	aFS :=  FileStream fileNamed: aName.
	self storeCategory: aKey on: aFS.
	aFS close.! !

!Outline methodsFor: 'fileIn/Out'!
storeCategory: aCategory on: aStream
	"store the chosen category on a stream"

	aCategory storeOn: aStream.
	aStream nextPutAll: keySeparator.
	(self at: aCategory) storeOn: aStream.
	aStream nextPutAll: entrySeparator! !

!Outline methodsFor: 'fileIn/Out'!
storeOn: aStream
	"Store the receiver on the given stream."

	organization do:
		[ :ass |
		ass key storeOn: aStream.
		aStream nextPutAll: keySeparator.
		ass value storeOn: aStream.
		aStream nextPutAll: entrySeparator]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Outline class
	instanceVariableNames: ''!

!Outline class methodsFor: 'instance creation'!
new
	"initialize me by default"

	^super new initialize! !


!Outline class methodsFor: 'loading'!
load: aFileName
	"read an outline in from the given file, using blank lines and double blank lines as key and entry separators"
	"Outline load: 'test.outline'."

	^self load: aFileName keySep: '
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'
		entrySep: '
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'! !

!Outline class methodsFor: 'loading'!
load: aFileName keySep: aKS entrySep: anES
	"Read an outline in from the given file, using the given key and entry separators"
	"Outline load: 'test.outline'."
	"Outline load: 'Squeak.outline"
	"Outline load: '/etc/passwd' keySep: ':' entrySep: (String with: Character cr)."

	| file org |
	file := FileStream oldFileNamed: aFileName.
	org := self new.
	org name: aFileName.
	org parseFrom: file
		keySeparator: aKS
		entrySeparator: anES.
	^org! !


Browser subclass: #OutlineBrowser
	instanceVariableNames: 'model currentCategory '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Outlines'!
OutlineBrowser comment:
'An OutrlineBrowser holds onto an Outline and creates a simple two-paned view on the one-level hierarchy using pluggable view components (list and text views).
The Browser implements key-list menu items for adding new keys, removing or renaming existing ones, and inspecting or saving the outline.
Text menu items are implemented for execution and the like.

Instance variable:
	currentCategory		<Text> selected item in the list of the model''s keys'!

!OutlineBrowser methodsFor: 'accessing'!
currentCategory
	"Answer the selected category."

	^currentCategory! !

!OutlineBrowser methodsFor: 'accessing'!
currentCategory: aVal
	"Set the receiver's selected category."

	currentCategory = aVal
		ifFalse: [currentCategory := aVal]
		ifTrue: [currentCategory := nil].
	self changed: #currentCategory.
	self changed: #text! !

!OutlineBrowser methodsFor: 'accessing'!
model: anOutline
	"Set the receiver's model."

	model := anOutline.
	anOutline addDependent: self! !


!OutlineBrowser methodsFor: 'organization list'!
organization: anOrg
	"stuff the model's outline"

	^model organization: anOrg! !

!OutlineBrowser methodsFor: 'organization list'!
organizationList
	"return the model's list"

	^model organizationList! !

!OutlineBrowser methodsFor: 'organization list'!
organizationMenu
	"Answer the menu for use in the list view"

	currentCategory == nil
		ifTrue: [^SelectionMenu
					labels: 'add category\save\inspect' withCRs
					lines: #(1 2)
					selectors: #(addCategory saveAll inspect)].
	^SelectionMenu
		labels: 'add category\rename\remove\save\inspect' withCRs
		lines: #(3 4)
		selectors: #(addCategory renameCategory removeCategory saveCategory inspect)! !


!OutlineBrowser methodsFor: 'text'!
acceptText: aText
	"accept the new text as the model's text for the current category"

	currentCategory == nil ifTrue: [^false].
	model at: currentCategory put: aText copy.
	self changed: #text.
	^true! !

!OutlineBrowser methodsFor: 'text'!
text
	"Answer the current category's text."

	currentCategory isNil
		ifTrue: [^'' asText]
		ifFalse: [^(model at: currentCategory) contents]! !

!OutlineBrowser methodsFor: 'text'!
textMenu
	"Answer the menu for the text view."

	^SelectionMenu
			labels: 'again\undo\copy\cut\paste\do it\print it\inspect\accept\cancel' withCRs
			lines: #(2 5 8)
			selectors: #(again undo 
				copySelection cut paste 
				doIt printIt inspectIt 
				accept cancel )! !


!OutlineBrowser methodsFor: 'menu messages'!
addCategory
	"Prompt for adding a new category and allow the user to name it."

	| newCategory organization |
	newCategory :=  FillInTheBlank request: 'New Category' initialAnswer: (''). 
	newCategory = '' ifTrue: [^self].
	organization := model organization.
	self currentCategory isNil
		ifTrue: [model at: newCategory put: (Environment new contents: Text new)]
		ifFalse: [organization add: (newCategory -> ((Environment new) contents: Text new)) 
				before: (model assAt: currentCategory)].
	currentCategory :=  newCategory.
	self changed: #organizationList.
	self changed: #text! !

!OutlineBrowser methodsFor: 'menu messages'!
inspect
	"Inspect the receiver's model"

	^ model inspect! !

!OutlineBrowser methodsFor: 'menu messages'!
inspectIt
	"Inspect the receiver's model"

	^ model inspect! !

!OutlineBrowser methodsFor: 'menu messages'!
inspectModel
	"Inspect the receiver's model"

	^ model inspect! !

!OutlineBrowser methodsFor: 'menu messages'!
removeCategory
	"prompt the user and remove the selected category"

	(self confirm: 'Are you certain that you
want to remove category ', currentCategory, '?')
		ifTrue: 
		[model removeKey: currentCategory.
		currentCategory :=  nil.
	].
	self changed: #organizationList.! !

!OutlineBrowser methodsFor: 'menu messages'!
renameCategory
	"prompt the user for a new name and rename the selected category"

	| newCategory |
	newCategory :=  FillInTheBlank request: 'New Category' initialAnswer: currentCategory. 
	newCategory = '' ifTrue: [^self].
	model at: newCategory put: (model at: currentCategory).
	model removeKey: currentCategory.
	currentCategory :=  newCategory.
	self changed: #organizationList.! !

!OutlineBrowser methodsFor: 'menu messages'!
saveAll
	"save the whole outline"

	model saveAll! !

!OutlineBrowser methodsFor: 'menu messages'!
saveCategory
	"save the currentCategory"

	model saveCategory: currentCategory! !


!OutlineBrowser methodsFor: 'do it'!
bindingOf: aString
	"Answer the variable binding of the given name."

	^currentCategory isNil
		ifTrue: [super bindingOf: aString]
		ifFalse: [(model at: currentCategory) bindingOf: aString]! !

!OutlineBrowser methodsFor: 'do it'!
doItContext
	^ nil! !

!OutlineBrowser methodsFor: 'do it'!
doItReceiver
	^ model at: currentCategory! !

!OutlineBrowser methodsFor: 'do it'!
doItValue: ignored! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

OutlineBrowser class
	instanceVariableNames: ''!

!OutlineBrowser class methodsFor: 'view creation'!
openFile: aName
	"OutlineBrowser openFile: 'test.outline'"

	^self openOn: (Outline load: aName) label: aName! !

!OutlineBrowser class methodsFor: 'view creation'!
openOn: anOrganization label: aLabel
	"OutlineBrowser openOn: Outline new label: 'Maintenance' "

	| me topView listView textView|
	me :=  self new model: anOrganization.
	topView := StandardSystemView new.
	topView model: me.
	topView label: aLabel.
	topView minimumSize: 200@300.
	listView := PluggableListView on: me 
		list: #organizationList 
		selected: #currentCategory 
		changeSelected: #currentCategory:
		menu: #organizationMenu.
	listView window: (0 @ 0 extent: 200 @ 80).
	listView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	textView :=  PluggableTextView 
		on: me
		aspect: #text
		change: #acceptText:
		menu: #textMenu.
	textView window: (0 @ 0 extent: 200 @ 300).
	textView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	topView addSubView: listView.
	topView
		addSubView: textView
		align: textView viewport topLeft
		with: listView viewport bottomLeft.
	topView controller open! !
