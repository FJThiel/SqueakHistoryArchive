"      NAME	FileContentsBrowser.st
       AUTHOR	raab@isg.cs.uni-magdeburg.de (Andreas Raab)
       URL	(none)
       FUNCTION	Browser for the contents of files containing Smalltalk source code
       KEYWORDS	Browser, Smalltalk source files
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.1
       DATE	28-Mar-98

SUMMARY

This file contains a browser to view the contentsof Smalltalk source files by a standard classbrowser. The contents of the source file(s) isscanned and grouped into package/class/category.After this package has been filed in choose'browse selected class' from the file list menu.

				Andreas Raab
"!
'From Squeak 1.31 of Feb 4, 1998 on 28 March 1998 at 6:53:33 pm'!
Browser subclass: #FileContentsBrowser
	instanceVariableNames: 'packages '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FileContentsBrowser'!
Object subclass: #FilePackage
	instanceVariableNames: 'packageName stream classes changes '
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

!ChangeRecord methodsFor: 'access' stamp: 'ar 3/28/98 15:58'!
category
	^category! !

!ChangeRecord methodsFor: 'access' stamp: 'ar 3/28/98 15:47'!
isMetaClassChange
	^meta! !

!ChangeRecord methodsFor: 'access' stamp: 'ar 3/28/98 15:46'!
methodClassName
	^class! !

Smalltalk renameClassNamed: #FileBrowser as: #FileContentsBrowser!

!FileContentsBrowser methodsFor: 'class list' stamp: 'ar 3/28/98 18:38'!
classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	systemCategoryListIndex = 0
		ifTrue: [^Array new]
		ifFalse: [^self selectedPackage classes keys asSortedCollection].! !

!FileContentsBrowser methodsFor: 'class list' stamp: 'ar 3/28/98 18:26'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	self selectedClassName == nil ifTrue: [^nil].
	^self selectedPackage classAt: self selectedClassName! !

!FileContentsBrowser methodsFor: 'accessing' stamp: 'ar 3/28/98 17:30'!
contents
	editSelection == #newClass 
		ifTrue: [^'Package: ',self selectedSystemCategoryName].
	^super contents! !

!FileContentsBrowser methodsFor: 'accessing' stamp: 'ar 3/28/98 17:32'!
contents: input notifying: aController 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."
	self inform:'You cannot change an external package'.
	^false! !

!FileContentsBrowser methodsFor: 'accessing' stamp: 'ar 3/28/98 18:23'!
packages
	^packages! !

!FileContentsBrowser methodsFor: 'accessing' stamp: 'ar 3/28/98 18:23'!
packages: aDictionary
	packages := aDictionary.! !

!FileContentsBrowser methodsFor: 'accessing' stamp: 'ar 3/28/98 18:26'!
selectedPackage
	| cat |
	cat := self selectedSystemCategoryName.
	cat isNil ifTrue:[^nil].
	^self packages at: cat asString! !

!FileContentsBrowser methodsFor: 'edit pane' stamp: 'ar 3/28/98 16:36'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."
	| class selector |
	contents == nil ifFalse: [^ contents copy].
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	contents _ class sourceCodeAt: selector.
	^ contents copy! !

!FileContentsBrowser methodsFor: 'metaclass' stamp: 'ar 3/28/98 17:55'!
selectedClassOrMetaClass
	"Answer the selected class or metaclass."

	self metaClassIndicated
		ifTrue: [^ self selectedClass metaClass]
		ifFalse: [^ self selectedClass]! !

!FileContentsBrowser methodsFor: 'metaclass' stamp: 'ar 3/28/98 17:55'!
setClassOrganizer
	"Install whatever organization is appropriate"
	| theClass |
	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	classListIndex = 0 ifTrue: [^ self].
	classOrganizer _ (theClass _ self selectedClass) organization.
	metaClassOrganizer _ theClass metaClass organization.
! !


!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'ar 3/28/98 18:50'!
browseFile: aFilename
	| browser package organizer packageDict view |
	package := FilePackage fromFileNamed: aFilename.
	packageDict := Dictionary new.
	packageDict at: package packageName put: package.
	organizer := SystemOrganizer defaultList: Array new.
	organizer classifyAll: package classes keys under: package packageName.
	browser := self new.
	browser systemOrganizer: organizer.
	browser packages: packageDict.
	browser systemCategoryListIndex: 1.
	view := BrowserView systemCategoryBrowser: browser editString: nil.
	view insideColor: (Color r: 0.8 g: 0.8 b: 0.5).
	BrowserView
		openBrowserView: view
		label: 'Package Browser'! !

!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'ar 3/28/98 18:50'!
browseFiles: fileList
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
	view := BrowserView browser: browser editString: nil.
	view insideColor: (Color r: 0.8 g: 0.8 b: 0.5).
	BrowserView
		openBrowserView: view
		label: 'Package Browser'
! !


!FileListController methodsFor: 'menu messages' stamp: 'ar 3/28/98 15:15'!
browseFile

	self controlTerminate.
	model browseFile.
	self controlInitialize.
! !


!FileListController class methodsFor: 'class initialization' stamp: 'ar 3/28/98 18:39'!
initialize   "FileListController initialize"
	"Initialize the file list menu.  6/96 di; modified 7/12/96 sw to add the file-into-new-change-set feature"

	FileListYellowButtonMenu _ PopUpMenu labels:
'fileIn
file into new change set
browse changes
browse selected file
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


!FileModel methodsFor: 'accessing' stamp: 'ar 3/28/98 18:52'!
browseFile
	FileContentsBrowser browseFile: self fullName.
	"| pattern fd |
	pattern := FillInTheBlank request:'What files?' initialAnswer:'*.st'.
	fd := FileDirectory on: (FileDirectory dirPathFor: self fullName).
	FileContentsBrowser browseFiles: 
		((fd fileNamesMatching: pattern) collect:[:fn| fd pathName, '/', fn])."! !

Smalltalk renameClassNamed: #RemotePackage as: #FilePackage!

!FilePackage methodsFor: 'initialize' stamp: 'ar 3/28/98 18:17'!
fromFileNamed: aName
	packageName := aName.
	stream := FileStream readOnlyFileNamed: aName.
	classes := OrderedCollection new.
	self fileInFrom: stream.! !

!FilePackage methodsFor: 'reading' stamp: 'ar 3/28/98 18:18'!
fileInFrom: aStream
	| chgRec |
	changes := (ChangeList new scanFile: aStream from: 0 to: stream size) changeList.
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

!FilePackage methodsFor: 'change record types' stamp: 'ar 3/28/98 18:03'!
classComment: chgRec

	(self getClass: chgRec methodClassName) classComment: chgRec! !

!FilePackage methodsFor: 'change record types' stamp: 'ar 3/28/98 18:12'!
doIt: chgRec
	| string |
	string := chgRec string.
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[self metaClassDefinition: string with: chgRec].! !

!FilePackage methodsFor: 'change record types' stamp: 'ar 3/28/98 17:53'!
method: chgRec
	(self getClass: chgRec methodClassName) methodChange: chgRec! !

!FilePackage methodsFor: 'change record types' stamp: 'ar 3/28/98 17:58'!
preamble: chgRec
! !

!FilePackage methodsFor: 'accessing' stamp: 'ar 3/28/98 18:23'!
classAt: className
	^self classes at: className! !

!FilePackage methodsFor: 'accessing' stamp: 'ar 3/28/98 16:27'!
classes
	^classes! !

!FilePackage methodsFor: 'accessing' stamp: 'ar 3/28/98 16:28'!
packageName
	^packageName! !

!FilePackage methodsFor: 'private' stamp: 'ar 3/28/98 18:19'!
classDefinition: string with: chgRec
	| tokens |
	tokens := Scanner new scanTokens: string.
	(self getClass: (tokens at: 3)) definition: string.! !

!FilePackage methodsFor: 'private' stamp: 'ar 3/28/98 18:19'!
getClass: className
	| pseudoClass |
	(classes includesKey: className) ifTrue:[
		^classes at: className.
	].
	pseudoClass := PseudoClass new.
	pseudoClass name: className.
	classes at: className put: pseudoClass.
	^pseudoClass.! !

!FilePackage methodsFor: 'private' stamp: 'ar 3/28/98 18:19'!
metaClassDefinition: string with: chgRec
	| tokens |
	tokens := Scanner new scanTokens: string.
	(self getClass: (tokens at: 1)) metaClass definition: string.! !


!FilePackage class methodsFor: 'instance creation' stamp: 'ar 3/28/98 15:11'!
fromFileNamed: aName
	^self new fromFileNamed: aName! !

Smalltalk renameClassNamed: #RemotePseudoClass as: #PseudoClass!

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:17'!
addMethodChange: aChangeRecord
	| selector |
	selector := Parser new parseSelector: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:15'!
classComment: aChangeRecord
	self organization classComment: aChangeRecord! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 18:01'!
comment
	^self organization classComment! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 17:27'!
definition
	^definition ifNil:['There is no class definition for this class'].! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:56'!
definition: aString
	definition := aString! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 17:51'!
metaClass
	^metaClass ifNil:[metaClass := self class new name: (self name,' class')].! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 17:53'!
methodChange: aChangeRecord
	aChangeRecord isMetaClassChange ifTrue:[
		^self metaClass addMethodChange: aChangeRecord.
	] ifFalse:[
		^self addMethodChange: aChangeRecord.
	].
! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 15:54'!
name
	^name! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 15:49'!
name: anObject
	name _ anObject! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 18:03'!
organization
	^organization ifNil:[organization := PseudoClassOrganizer defaultList: SortedCollection new].! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:37'!
parserClass
	^Parser! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:12'!
sourceCode
	^source ifNil:[source := Dictionary new]! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:36'!
sourceCodeAt: sel
	^(self sourceCode at: sel) string! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:12'!
sourceCodeAt: sel put: object
	self sourceCode at: sel put: object! !

!PseudoClass methodsFor: 'accessing' stamp: 'ar 3/28/98 16:35'!
sourceCodeTemplate
	^''! !


!PseudoClassOrganizer methodsFor: 'all' stamp: 'ar 3/28/98 16:16'!
classComment: aChangeRecord
	globalComment := aChangeRecord! !

!PseudoClassOrganizer methodsFor: 'all' stamp: 'ar 3/28/98 18:07'!
setDefaultList: aCollection
	super setDefaultList: aCollection.
	self classComment: nil.! !


FileListController initialize!

