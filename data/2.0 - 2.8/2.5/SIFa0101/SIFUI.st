"	This Squeak format file contains the ANSI SIF Program user interface global, class, and method additions required to read in a SIF file.

 ******
 * $Name:  $
 * $Id: SIFUI.st 1.3 1999-10-14 14:05:51-05 Harmon Alpha $
 * $Log: SIFUI.st $
 * Revision 1.3  1999-10-14 14:05:51-05  Harmon
 * Fixed prompt for file, again.
 *
 * Revision 1.2  1999-10-14 13:15:41-05  Harmon
 * Fixed default prompt file extension.
 *
 * Revision 1.1  1999-10-12 06:34:01-05  Harmon
 * Initial revision
 *
 ******"!

'From Squeak 2.5 of August 6, 1999 on 29 September 1999 at 6:52:15 am'!
StringHolder subclass: #ProgramBrowser
	instanceVariableNames: 'curSelListIndex pgmNameList pgmManager '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-User Interface'!
StringHolder subclass: #ProgramEditor
	instanceVariableNames: 'additionalPane additionalSw addNew componentDisplayList cutHold expandedSets pgmManager program selDisplayListIndex visableComponentList '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-User Interface'!

!ProgramBrowser commentStamp: 'RAH 9/29/1999 06:05' prior: 0!
This is the starting point for the SIF User Interface Protocols documentation.

	Open the program browser with:

		ProgramBrowser openBrowser.


1. SIF User Interface Protocols

	The SIF User Interface protocols are implemented in the ProgramBrowser and ProgramEditor classes.

	The graphs below shows the conformance relationships between the protocols defined in this section.

		<Object>
			|
		<Model>
			|
	<StringHolder>
			|
	------------------------
	|	 				|
<programBrowser>   	<programEditor>
				

	<Object>
		|
	----------------------------------
	|							|
<programBrowser factory>   <programEditor factory>



1.1 Protocol: <programBrowser>

Conforms To
	<StringHolder>

Description
	Class ProgramBrowser is a <StringHolder> that implements the behavior of <programBrowser>.  It is the singleton instance program browser tool that provides the graphical user interface to the program manager <ProgramManager> for adding, renaming, and removing Smalltalk Interchange Format programs <ProgramDefinitionSet>s.
	An individual program is edited in a program editor <programEditor>.

Instance Variables:

	curSelListIndex	<integer>	The index of the currently selected program name, or zero if there is no selection.

	pgmNameList	<OrderedCollection>	The current list of program names.

	pgmManager	<ProgramManager>	The single instance of the program manager.


1.2 Protocol: <programBrowser factory>

Conforms To
	<Object>

Description
	This protocol implements the behavior of creating the singleton instance of the program browser.

 
1.3 Protocol: <programEditor>
1.4 Protocol: <programEditor factory>

	See the ProgramEditor class comment.

Notes:

	The check to insure only one ProgramBrowser is opened has not been implemented.
!

!ProgramBrowser methodsFor: 'accessing' stamp: 'RAH 8/21/1999 09:05'!
programManager
	"Private - Answer a ProgramManager, the single instance of the program manager."
		#SIFUI.
 	^pgmManager
! !

!ProgramBrowser methodsFor: 'accessing' stamp: 'RAH 8/21/1999 09:05'!
selectedProgramOrNil
	"Private - Answer the currently selected program or nil if there is none."
	| pgmName |
		#SIFUI.
	curSelListIndex > 0 ifFalse: [
		^nil
	].
	pgmName := pgmNameList
		at: curSelListIndex
		ifAbsent: [
			^nil
		].
	^self programManager
		programAtName: pgmName
		ifAbsent: [nil].
! !

!ProgramBrowser methodsFor: 'accessing' stamp: 'RAH 8/21/1999 09:05'!
selectedProgramOrNil: programOrNil
	"Private - Sets the currently selected program to programOrNil, or nil if there will be no selection."
	| index |
		#SIFUI.
	programOrNil isNil ifTrue: [
		index := 0.
	] ifFalse: [
		index := pgmNameList
			indexOf: programOrNil programName
			ifAbsent: [0].
	].
	self selectedListIndex: index.
! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 8/21/1999 08:58'!
editProgram
	"Private - Edit the selected program."
	| program |
		#SIFUI.
	program := self selectedProgramOrNil.
	program notNil ifTrue: [
		ProgramEditor openEditorOn: program.	"Chged SQ"
	]
! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 9/21/1999 09:11'!
editProgramPrerequisites
	"Private - Edit the selected program prerequisite program names."
	| program |
		#SIFUI.
	program := self selectedProgramOrNil.
	program notNil ifTrue: [
		program prerequisiteProgramNames inspect.
	]
! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 8/21/1999 08:59'!
evaluateProgram
	"Private - Evaluate the selected program."
	| program |
		#SIFUI.
	program := self selectedProgramOrNil.
	program notNil ifTrue: [
		program evaluateObjects.
		self changed: #programNameList.	"Chged SQ"
	].! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 9/27/1999 08:17'!
installProgram
	"Private - Prompt for a program file and install it in to the receiver's model."
	| pathFilenameExt program  |
		#SIFUI.
	pathFilenameExt := '?.', SIF programFileLocatorExtension.
	pathFilenameExt := FillInTheBlank	"Chg SQ"
		request: 'Open Program'
		initialAnswer: pathFilenameExt.
	(pathFilenameExt notNil and: [pathFilenameExt isEmpty not]) ifTrue: [
		Cursor wait showWhile: [
			[	program := self programManager installProgramFileLocator: pathFilenameExt.
			]	on: PGM duplicateProgramError do: [ :except |
					PopUpMenu notify: except description.	"Chged SQ"
				].
			self changed: #programNameList.
			self selectedProgramOrNil: program.
	]	].
! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 9/27/1999 08:26'!
loadProgram
	"Private - Prompt for a program file and load it in to the receiver's model."
	| pathFilenameExt program  |
		#SIFUI.
	pathFilenameExt := '?.', SIF programFileLocatorExtension.
	pathFilenameExt := FillInTheBlank	"Chg SQ"
		request: 'Open Program'
		initialAnswer: pathFilenameExt.
	(pathFilenameExt notNil and: [pathFilenameExt isEmpty not]) ifTrue: [
		Cursor wait showWhile: [
			[	program := self programManager loadProgramFileLocator: pathFilenameExt
			]	on: PGM programError do: [ :except |
					PopUpMenu notify: except description.	"Chged SQ"
				].
			self changed: #programNameList.
			self selectedProgramOrNil: program.
	]	]! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 8/21/1999 08:59'!
newProgram
	"Private - Prompt for a new program name and add it to the receiver's model."
	| newProgram programName |
		#SIFUI.
	programName := self uniqueProgramNameOrNil.
	programName isNil ifFalse: [
		newProgram := PGM newProgramNamed: programName.
		self programManager addProgram: newProgram.
		self changed: #programNameList.	"Chged SQ"
		self selectedProgramOrNil: newProgram.
	].! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 8/21/1999 08:59'!
renameProgram
	"Private - Prompt for a new program name for the selected program and rename it."
	| program newNameString |
		#SIFUI.
	program := self selectedProgramOrNil.
	program notNil ifTrue: [
		newNameString := self uniqueProgramNameOrNil.
		newNameString isNil ifFalse: [
			self programManager
				renameProgram: program
				toName: newNameString.
			self changed: #programNameList.	"Chged SQ"
			self selectedProgramOrNil: program.
	]	].! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 9/21/1999 09:17'!
saveProgram
	"Private - Save the selected program to its program file."
	| program |
		#SIFUI.
	program := self selectedProgramOrNil.
	program notNil ifTrue: [
		program fileLocator isEmpty ifTrue: [
			^self saveProgramAs.
		].
		program save.
	].! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 9/24/1999 06:56'!
saveProgramAs
	"Private - Prompt for a program file and save the selected program to it."
	| flExt flName flPath pathFilenameExt program |
		#SIFUI.
	program := self selectedProgramOrNil.
	program isNil ifTrue: [
		^self
	].
	self portFunc					"Chged SQ"
		fileLocatorSplit: program fileLocator
		to: [ :flPathString :flNameString :flExtString |
			flPath := flPathString.
			flName := flNameString.
			flExt := flExtString.
		].
	pathFilenameExt := self portFunc
		promptPath: flPath
		fileName: flName
		extension: flExt
		fileType: SIF programFileDialogTypes
		title: 'Save Program ', program programName, ' As'.

	pathFilenameExt notNil ifTrue: [
		program fileLocator: pathFilenameExt.
		program save.
	]! !

!ProgramBrowser methodsFor: 'commands' stamp: 'RAH 8/21/1999 09:00'!
uninstallUnloadProgram
	"Private - Uninstall or unload the current program from the program manager."
	| nameList program msg |
		#SIFUI.
	program := self selectedProgramOrNil.
	program isNil ifTrue: [
		^self
	].
	"First check to see if there are unsaved changes."
	program isChanged ifTrue: [
		(SelectionMenu confirm: ('Yes save ', program programName, ' changes before uninstalling, or No option to cancel save?') 
		) ifTrue: [
			self saveProgram
		] ifFalse: [
			(SelectionMenu confirm: ('Yes uninstall losing changes, ',
					'or No cancel uninstall?') 
			) ifFalse: [
				^self
	]	]	].

	(self programManager canUninstall: program) ifFalse: [
		nameList := String new.
		(self programManager programsRequiring: program)
			do: [ :tmpProgram |
				nameList := nameList, tmpProgram programName.
			]
			separatedBy: [nameList := nameList, ', '].
		PopUpMenu notify: 'Programs: (', nameList, ') require ',
			program programName, ' and must be uninstalled first.',
			'  Canceling uninstall.'.
		^self
	].

	msg := 'Do you really want to uninstall ', program programName, '?'.
	(SelectionMenu confirm: msg) ifFalse: [
		^self
	].

	program isChanged: false.
	Cursor wait showWhile: [
		self programManager uninstallUnloadProgram: program
	].
	self changed: #programNameList.
	self selectedProgramOrNil: nil.! !

!ProgramBrowser methodsFor: 'initialize' stamp: 'RAH 9/23/1999 10:17'!
openLabel: labelString
	"Private - Create a standard system view of the model, me, a StringHolder and open it."
	| topView pgmListView |
		#SIFUI.
	World ifNotNil: [
		^self openAsMorphLabel: labelString
	].

	pgmManager := ProgramManager current.
	curSelListIndex := 0.

	topView _ (StandardSystemView new) model: self.
	topView borderWidth: 1.
	topView label: labelString.
	topView minimumSize: 100 @ 50.

	pgmListView := PluggableListView on: self
		list: #programNameList
		selected: #selectedListIndex
		changeSelected: #selectedListIndex:
		menu: #programMenu:.
	pgmListView autoDeselect: false.
	pgmListView window: (0@0 extent: 200@200).

	topView addSubView: pgmListView.
	topView controller open.
! !

!ProgramBrowser methodsFor: 'private' stamp: 'RAH 7/22/1999 06:18'!
uniqueProgramNameOrNil
	"Private - Answer a String, a new program name not already in use prompted from the user."
	| programName |
		#SIFUI.
	programName := String new.
	[	programName := FillInTheBlank
			request: 'New program name:'
			initialAnswer: programName.
		(programName notNil and: [programName isEmpty not]) ifFalse: [
			^nil
		].
		"Verify that it is not already in use:"
		self programManager programNames includes: programName
	] whileTrue: [
		PopUpMenu notify: '"', programName, '" is already in use'.
	].
	^programName
! !

!ProgramBrowser methodsFor: 'program list' stamp: 'RAH 9/23/1999 10:17'!
programMenu: aMenu
	"Private - Answer a Menu, the menu for the program list pane."
		#SIFUI.
	^aMenu
		labels: ('New Program\Edit Program\Install Program',
			'\Load Program\Evaluate Program\Uninstall/Unload Program',
			'\Save Program\Save As Program\Rename Program') withCRs
		lines: # (2 5 8)
		selections: #(newProgram editProgram installProgram
			loadProgram evaluateProgram uninstallUnloadProgram
			saveProgram saveProgramAs renameProgram)
! !

!ProgramBrowser methodsFor: 'program list' stamp: 'RAH 9/23/1999 10:17'!
programNameList
	"Private - Answer the current list of program names."
		#SIFUI.
	pgmNameList := self programManager programNames asArray.
	^pgmNameList
! !

!ProgramBrowser methodsFor: 'program list' stamp: 'RAH 9/23/1999 10:18'!
selectedListIndex
	"Private - Answer an Integer, the index of the currently selected program name, or zero if there is no selection."
		#SIFUI.
	^curSelListIndex
! !

!ProgramBrowser methodsFor: 'program list' stamp: 'RAH 9/23/1999 10:18'!
selectedListIndex: indexInteger
	"Private - Select the program having the given index in the program name list, or have no selection if zero."
		#SIFUI.
	curSelListIndex := indexInteger.
	self changed: #selectedListIndex.
! !


!ProgramBrowser class methodsFor: 'instance creation' stamp: 'RAH 9/29/1999 05:55'!
openBrowser
	"Open a browser on the single instance of the current program manager.  Example:

		ProgramBrowser openBrowser.

Definition: <programBrowser factory>
"		#SIFUI. "Protocol: <programBrowser factory>"
	self new						"Chged SQ"
		openLabel: 'ProgramBrowser'.
! !


!ProgramEditor commentStamp: 'RAH 9/29/1999 06:39' prior: 0!
The ProgramBrowser class comment is the starting point for the SIF User Interface Protocols documentation.


1.3 Protocol: <programEditor>

Conforms To
	<StringHolder>

Description
	Class ProgramEditor is a <StringHolder> that implements the behavior of <programEditor>.  It is an instance of program editor tool that provides the graphical user interface for adding, renaming, and removing, etc. Smalltalk Interchange Format program <ProgramDefinitionSet> components.

Instance Variables:

	additionalPane		<PluggableTextView>	A pane containing the selected component's additional information (a methods's or intializer's code, a program comment's  comment, etc.) if the selected component type needs any.

	additionalSw		<symbol>			The additional switch setting indicating the additional pane contents information (a methods's or intializer's code, a program comment's  comment, etc.).

	addNew				<symbol>		The current addNew switch setting indicating whether the new component should be added #before, #after, or #into, the selected component.

	componentDisplayList	<Array>		The list of visable program component display strings.

	cutHold			<ProgramAbstractComponent>|<nil> The current cut component, or <nil> if none.

	expandedSets	<collection>				The collection of component set objects that are to be displayed expanded (showing children).

	pgmManager		<ProgramManager>	The singleton program manager.

	program			<ProgramDefinitionSet>	The program currently being edited.

	selDisplayListIndex	<integer>			The index in the component display list of the currently selected component, or zero if none selected.

	visableComponentList	<Array>			The list of visable program component objects.  The components of collapsed sets will not be visable.


1.4 Protocol: <programEditor factory>

Conforms To
	<Object>

Description
	This protocol implements the behavior of creating an instance of a program editor.


Notes:

	The program editor is only crude quick implementation primarily to be used to collect already developed code into programs for filing to another environment in a portable fashion.
	I thought adding program editor functionally to the Refactoring Browser (RB) might be most useful as I think the RB is intended to be a portable environment and has many need functions already present.
	The program editor should probably look and act much like a class hierarchy browser accept include pools, globals, and loose methods.  The Project Manager in ST/V is much like what I have in mind.

Add Buttons:

	The "AddNew" button masquerades as a rotating state button that cycles through adding a new component before, after, or into the selected component.

	The "Add Class", "Add Global", etc. buttons are only used to hang a menu of component types on.  There is a bunch of different types and I couldn't think of any more convenient way of selecting them that didn't require a lot more GUI work which I was trying to avoid.  It is not my strong suite and it varies most between dialects.

Set Expanded/Collapsed Indicators:

	The expandedInd, collapsedInd, and elementInd are strings added to the front of a component to (crudely) indicate a set is expanded or collapsed, or to indicate an element.

Editing Not Fully Implemented:

	Editing of a program component, a program component's annotations, or a program's prerequisite program names is crudely done using inspect.

Find Command Not Implemented:

	The find and find next commands have not been thought through and are not implemented.
!


!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 19:26'!
addNew
	"Private - Answer a <symbol>, the current addNew switch setting indicating whether the new component should be added #before, #after, or #into, the selected component."
		#SIFUI.	"Chged SQ"
	^addNew
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 19:30'!
addNew: aSymbol
	"Private - Set the addNew switch to <symbol>, aSymbol, whether the new component should be added #before, #after, or #into, the selected component."
		#SIFUI.	"Chged SQ"
	(#(before after into) includes: aSymbol) ifFalse: [
		self error: 'Add new is not #before, #after, or #into.'
	].
	addNew := aSymbol
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 15:30'!
additionalSw
	"Private - Answer a <symbol>, the additional switch setting indicating   the additional pane contents information (a methods's or intializer's code, a program comment's  comment, etc.)."
		#SIFUI.	"Chged SQ"
	^additionalSw
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 19:33'!
additionalSw: aSymbol
	"Private - Set the additional switch setting to <symbol>, aSymbol, indicating the additional pane contents information (a methods's or intializer's code, a program comment's  comment, etc.)."
		#SIFUI.	"Chged SQ"
	(#(code none programComment) includes: aSymbol) ifFalse: [
		self error: 'additional switch is not #code or #none.'
	].
	additionalSw := aSymbol
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 8/11/1999 08:46'!
cutHoldOrNil
	"Private - Answer a <ProgramAbstractComponent> or <nil>, the current cut component."
		#SIFUI.	"Chged SQ"
	^cutHold! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 8/11/1999 08:48'!
cutHoldOrNil: cutComponentOrNil
	"Private - Set the cut component to cutComponent, a <ProgramAbstractComponent>, or to <nil>."
		#SIFUI.	"Chged SQ"
	cutHold := cutComponentOrNil! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 8/2/1999 15:45'!
expandedSets
	"Private - Answer a Set, the collection of component set objects that are to be displayed expanded."
		#SIFUI.	"Chged SQ"
	^expandedSets! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 8/2/1999 15:45'!
expandedSets: aSet
	"Private - Set the collection of component set objects that are to be displayed expanded (showing children)."
		#SIFUI.	"Chged SQ"
	expandedSets := aSet! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:33'!
pgmManager
	"Private - Answer a <ProgramManager>, aProgramManager, the singleton program manager."
		#SIFUI.
	^pgmManager
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:33'!
pgmManager: aProgramManager
	"Private - Set the singleton program manager to <ProgramManager>, aProgramManager."
		#SIFUI.
	pgmManager := aProgramManager
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:37'!
program
	"Private - Answer a <ProgramDefinitionSet>, the program being edited."
		#SIFUI.
	^program
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:37'!
program: aProgramDefinitionSet
	"Private - Set the program being edited to a <ProgramDefinitionSet>, aProgramDefinitionSet."
		#SIFUI.
	program := aProgramDefinitionSet
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:45'!
visableComponentList
	"Private - Answer an <Array>, the list of visable program component objects."
		#SIFUI.	"Chged SQ"
	^visableComponentList
! !

!ProgramEditor methodsFor: 'accessing' stamp: 'RAH 9/21/1999 20:46'!
visableComponentList: componentArray
	"Private - Set the list of visable program component objects to an <Array>, componentArray."
		#SIFUI.	"Chged SQ"
	visableComponentList := componentArray
! !

!ProgramEditor methodsFor: 'addtional pane' stamp: 'RAH 9/21/1999 15:28'!
additionalContents
	"Private - Answer a <readableString>, the description of an element's additional information depending on the additional switch setting (a methods's or intializer's code, a program comment's  comment, etc.)."
	| selElement |
		#SIFUI.	"Chged SQ"
	self additionalSw == self additionalNone ifTrue: [
		^String new
	].
	selElement := self selectedComponentOrNil.
	selElement isNil ifTrue: [
		^String new
	].

	(self additionalSw == self additionalCode
	and: [selElement isKindOf: CodeDefinitionionElement]) ifTrue: [
		^selElement source.
	].

	(self additionalSw == self additionalProgramComment
	and: [selElement isKindOf: ProgramCommentElement]) ifTrue: [
		^selElement comment.
	].

	self error: 'Program Editor internal error: unknown edit selection.'! !

!ProgramEditor methodsFor: 'addtional pane' stamp: 'RAH 9/21/1999 15:29'!
additionalContents: additionalText
	"Private - Answer a  <boolean>, the result of updating the description of an element's additional information depending on the additional switch setting (a methods's or intializer's code, a program comment's  comment, etc.)."
	| selElement |
		#SIFUI.	"Chged SQ"
	self additionalSw == self additionalNone ifTrue: [
		PopUpMenu notify: 'No additional information allowed for this element.'.
		self changed: #additionalContents.
		^false
	].
	selElement := self selectedComponentOrNil.
	selElement isNil ifTrue: [
		PopUpMenu notify: 'No additional information allowed for this element.'.
		self changed: #additionalContents.
		^false
	].

	(self additionalSw == self additionalCode
	and: [selElement isKindOf: CodeDefinitionionElement]) ifTrue: [
		selElement source: additionalText asString.
		^true
	].

	(self additionalSw == self additionalProgramComment
	and: [selElement isKindOf: ProgramCommentElement]) ifTrue: [
		selElement comment: additionalText asString.
		self visableComponentsChanged.		"Chged SQ".
		^true
	].

	self error: 'Program Editor internal error: unknown edit selection.'! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 19:50'!
clearComponentMessages
	"Private - Clear the selected component's messages."
	| selectedComponent |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		selectedComponent := self program
	].
	selectedComponent clearMessages.

	self visableComponentsChanged.		"Chged SQ".
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 15:31'!
cutComponent
	"Private - Cut and hold the selected component."
	| selectedComponent parentComponent |
		#SIFUI.		"Chged SQ"
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		^nil
	].
	parentComponent := selectedComponent parentSetOrNil.
	[	parentComponent removeComponent: selectedComponent.
		"I'm not sure if parent should be cleared as this may make
			a future undo operation more diffacult."
		selectedComponent parentSetOrNil: nil.
		self cutHoldOrNil: selectedComponent.

		self programChangedWithSelectedOrNil: parentComponent.
	]	on: PGM programError do: [ :error |
			(SelectionMenu confirm: error description, ' - continue?'
			) ifTrue: [
					error resume
				] ifFalse: [
					error return
		]	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 15:31'!
deleteComponent
	"Private - Remove the selected component."
	| selectedComponent parentComponent |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		^nil
	].
	(SelectionMenu confirm: 'Delete ', selectedComponent displayString, '?'
	) ifTrue: [	"Chged SQ"
		parentComponent := selectedComponent parentSetOrNil.
		[	parentComponent removeComponent: selectedComponent.

			self programChangedWithSelectedOrNil: parentComponent.
		]	on: PGM programError
			do: [ :error |
				(SelectionMenu confirm: error description, ' - continue?'
				) ifTrue: [
					error resume
				] ifFalse: [
					error return
	]		]	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 19:54'!
editComponent
	"Private - Edit the selected component."
	| selectedComponent |
		#SIFUI.
#todoSIFMod."This is temporary to allow the user to change something I had not thought of."
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		selectedComponent := self program
	].
	selectedComponent inspect.

	self programChangedWithSelectedOrNil: selectedComponent.	"Chged SQ".
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 19:54'!
editComponentAnnotations
	"Private - Edit the selected component annotations."
	| selectedComponent |
		#SIFUI.
#todoSIFMod."This is temporary to allow the user to change something I had not thought of."
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		selectedComponent := self program
	].
	selectedComponent annotations inspect.! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 19:56'!
editPrerequisiteProgramNames
	"Private - Edit the prerequisite program names."
		#SIFUI.
#todoSIFMod."Remove this when facility is to automatically generate program prerequisite is implemented."
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	self program prerequisiteProgramNames inspect.! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 19:56'!
expandOrCollapseSet
	"Private - Expand if collapsed, or collapse if expanded, the currently selected program component set.  Ignore program elements."
	| component |
		#SIFUI.			"Chged SQ"
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	component := self selectedComponentOrNil.
	(component isNil or: [component isComponentSet not]) ifTrue: [
		^self
	].
	(self expandedSets includes: component)  ifTrue: [
		self removeExpandedSet: component.
	] ifFalse: [
		self addExpandedSet: component.
	].
	self visableComponentsChanged.		"Chged SQ".


! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 8/2/1999 15:48'!
findComponent
	"Private - Select the component with ??? prompted from the user."
		#SIFUI.
	#todoSIFUI. "Implement."
PopUpMenu notify: 'Not implemented.'	"Chged SQ".! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 8/2/1999 15:48'!
findNextComponent
	"Private - Select the next component with ???."
		#SIFUI.
	#todoSIFUI. "Implement."
PopUpMenu notify: 'Not implemented.'	"Chged SQ".! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:21'!
newClassAllSet
	"Private - Prompt for the class name and add a new component to the receiver's model.
	Note: A ClassAllSet must be of an existing class, and will be built when filed out."
	| component classNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	classNameString := self
		globalNamePrompt: 'Class name:'
		caption: 'Create a new class all set'.
	classNameString notNil ifTrue: [
		component := PGM newClassAllSet: classNameString.
		component createClassAllComponents.

		self currentSetAddComponent: component
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:21'!
newClassDefinitionElement
	"Private - Prompt for the class name and add a new component to the receiver's model.
	Note: A class def. must be of an existing class, and will be completed when filed out."
	| classNameString component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	classNameString := self
		globalNamePrompt: 'Class name:'
		caption: 'Create a new class definition'.
	classNameString notNil ifTrue: [
		component := PGM
			newClassDefinitionOf: classNameString
			superclass: nil
			indexedInstanceVariables: nil
			instanceVariableNames: nil
			classVariableNames: nil
			sharedPools: nil
			classInstanceVariableNames: nil.
		self currentSetAddComponent: component
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:22'!
newClassInitializerElement
	"Private - Prompt for the class name and add a new component to the receiver's model.
	Note: A class init. must be of an existing class."
	| classNameString component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	classNameString := self
		globalNamePrompt: 'Class name:'
		caption: 'Create a new class initializer'.
	classNameString notNil ifTrue: [
		component := PGM
			newClass: classNameString
			initializer: String new.
		self currentSetAddComponent: component.
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:22'!
newClassMethodDefinitionElement
	"Private - Prompt for the class and method names and add a new component to the receiver's model."
	| classNameString component methodNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	classNameString := self
		globalNamePrompt: 'Class name:'
		caption: 'Create a new class method definition'.
	classNameString notNil ifTrue: [
		methodNameString := self
			methodNamePrompt: 'Class method:'
			caption: ('Create a new ', classNameString, ' class method').
		methodNameString notNil ifTrue: [
			component := PGM
				newClassMethodDefinitionOf: methodNameString
				in: classNameString.
			self currentSetAddComponent: component
	]	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:22'!
newGlobalAllSet
	"Private - Prompt for the global name and add a new component to the receiver's model.
	Note: A GlobalAllSet must be of an existing global, and will be built when filed out."
	| component globalNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	globalNameString := self
		globalNamePrompt: 'Global name:'
		caption: 'Create a new global all set'.
	globalNameString notNil ifTrue: [
		component := PGM newGlobalAllSet: globalNameString.
		component createGlobalAllComponents: self constantPrompt.
		self currentSetAddComponent: component
	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:23'!
newGlobalDefinitionElement
	"Private - Prompt for the global name and add a new component to the receiver's model.
	Note: A global def. must be of an existing global."
	| globalNameString component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	globalNameString := self
		globalNamePrompt: 'Global name:'
		caption: 'Create a new global definition'.
	globalNameString notNil ifTrue: [
		component := PGM
			newGlobalDefinitionOf: globalNameString
			constant: self constantPrompt.
		self currentSetAddComponent: component
	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:23'!
newGlobalInitializerElement
	"Private - Prompt for the global name and add a new component to the receiver's model.
	Note: A global init. must be of an existing global."
	| globalNameString component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	globalNameString := self
		globalNamePrompt: 'Global name:'
		caption: 'Create a new global initializer'.
	globalNameString notNil ifTrue: [
		component := PGM
			newGlobal: globalNameString
			initializer: String new.
		self currentSetAddComponent: component
	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:24'!
newGroupingSet
	"Private - Prompt for the grouping name and add a new component to the receiver's model."
	| component groupingNameString |
		#SIFUI.			"Chged SQ"
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	groupingNameString := FillInTheBlank request: 'Grouping name:'.
	groupingNameString notNil ifTrue: [
		component := PGM newGroupingSet: groupingNameString.
		self currentSetAddComponent: component
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:24'!
newInstanceMethodDefinitionElement
	"Private - Prompt for the class and method names and add a new component to the receiver's model."
	| classNameString component methodNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	classNameString := self
		globalNamePrompt: 'Class name:'
		caption: 'Create a new instance method definition'.
	classNameString notNil ifTrue: [
		methodNameString := self
			methodNamePrompt: 'Instance method:'
			caption: ('Create a new ', classNameString, ' class method').
		methodNameString notNil ifTrue: [
			component := PGM
				newInstanceMethodDefinitionOf: methodNameString
				in: classNameString.
			self currentSetAddComponent: component
	]	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:24'!
newPoolAllSet
	"Private - Prompt for the pool name and add a new component to the receiver's model.
	Note: A PoolAllSet must be of an existing pool, and will be built when filed out."
	| component poolNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	poolNameString := self
		globalNamePrompt: 'Pool name:'
		caption: 'Create a new pool all set'.
	poolNameString notNil ifTrue: [
		component := PGM newPoolAllSet: poolNameString.
		component createPoolAllComponents.
		self currentSetAddComponent: component
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:25'!
newPoolDefinitionElement
	"Private - Prompt for the pool name and add a new component to the receiver's model.
	Note: A pool def. must be of an existing pool."
	| poolNameString component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	poolNameString := self
		globalNamePrompt: 'Pool name:'
		caption: 'Create a new pool definition'.
	poolNameString notNil ifTrue: [
		component := PGM newPoolDefinitionOf: poolNameString.
		self currentSetAddComponent: component
	].! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:25'!
newPoolValueDefinitionElement
	"Private - Prompt for the pool and pool value names and add a new component to the receiver's model."
	| poolNameString component poolValueNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	poolNameString := self
		globalNamePrompt: 'Pool name:'
		caption: 'Create a new pool value definition'.
	poolNameString notNil ifTrue: [
		poolValueNameString := self
			poolValueNamePrompt: 'Pool value:'
			caption: ('Create a new ', poolNameString, ' pool value').
		poolValueNameString notNil ifTrue: [
			component := PGM
				newPoolValueDefinitionOf: poolValueNameString
				in: poolNameString
				constant: self constantPrompt.
			self currentSetAddComponent: component
	]	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:25'!
newPoolValueInitializerElement
	"Private - Prompt for the pool and pool value names and add a new component to the receiver's model."
	| poolNameString component poolValueNameString |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	poolNameString := self
		globalNamePrompt: 'Pool name:'
		caption: 'Create a new pool value initializer'.
	poolNameString notNil ifTrue: [
		poolValueNameString := self
			poolValueNamePrompt: 'Pool value:'
			caption: ('Create a new ', poolNameString, ' pool value initializer').
		poolValueNameString notNil ifTrue: [
			component := PGM
				newPoolValue: poolValueNameString
				in: poolNameString
				initializer: String new.
			self currentSetAddComponent: component
	]	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:26'!
newProgramCommentElement
	"Private - Prompt for the comment and add a new component to the receiver's model."
	| component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	component := PGM newProgramComment: (String with: $" with: $").
	self currentSetAddComponent: component.
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:26'!
newProgramInitializerElement
	"Private - Add a new component to the receiver's model."
	| component |
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	component := PGM newProgramInitializer: String new.
	self currentSetAddComponent: component! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 9/21/1999 20:27'!
pasteComponent
	"Private - Paste the cut-hold component in to the receiver's model."
		#SIFUI.
	"Are there outstanding additional pane changes?"
	self promptToSaveChanges ifFalse: [		"Chged SQ"
		^self	"Don't lose changes."
	].

	self cutHoldOrNil notNil ifTrue: [
		(self currentSetAddComponent: self cutHoldOrNil) ifTrue: [
			self cutHoldOrNil: nil.
	]	].
! !

!ProgramEditor methodsFor: 'commands' stamp: 'RAH 8/17/1999 11:42'!
showComponentAnnotations
	"Private - Show the selected component annotations."
		#SIFUI.
	#todoSIFUI. "Write this later"
PopUpMenu notify: 'Not implemented.'	"Chged SQ"! !

!ProgramEditor methodsFor: 'testing' stamp: 'RAH 9/21/1999 15:37'!
promptToSaveChanges
	"Private - Answer a <boolean>, false if the additional pane has been changed and user responds to the prompt that the changes should be retained, or true if there are no changes or user responds to ignore them."
		#SIFUI.
	"Test additional pane"
	self okToChange ifFalse: [	"Chged SQ"
		"(SelectionMenu confirm: 'There are unsaved additional changes.
Do you wish to retain them?'
		) ifTrue: [
			additionalPane view ensureVisible.
			^false
		].
		additionalPane isModified: false.
"
		^false
	].
	^true
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 8/21/1999 10:28'!
buildComponentAndDisplayList
	"Private - Rebuild the list of component objects, with the corresponding list of display strings."
	| displayList tmpComponentList |
		#SIFUI.	"Chged SQ"
	tmpComponentList := OrderedCollection new.
	displayList := OrderedCollection new.
	self program do: [ :component |
		(self expandedSets includes: component)  ifTrue: [
			"Display all expanded component sets:"
			tmpComponentList add: component.
			displayList add: ((self indent: component),
					self expandedInd, component displayString).
	 	] ifFalse: [
			component parentSetOrNil = self program
			| (self expandedSets includes: component parentSetOrNil)  ifTrue: [
				"Display all roots and children of expanded sets:"
				tmpComponentList add: component.
				component isComponentSet ifTrue: [
					displayList add: ((self indent: component),
								self collapsedInd, component displayString).
				] ifFalse: [
					displayList add: ((self indent: component),
								self elementInd, component displayString).
	]	]	]	].
	self visableComponentList: tmpComponentList asArray.
	self componentDisplayList: displayList asArray.
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 19:35'!
componentDisplayList
	"Private - Answer an <Array>, the list of visable program component display strings."
		#SIFUI.	"Chged SQ"
	^componentDisplayList
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 19:37'!
componentDisplayList: arrayOfComponentDisplayStrings
	"Private - Set the list of visable program component display strings to an  <Array>, arrayOfComponentDisplayStrings."
		#SIFUI.	"Chged SQ"
	componentDisplayList := arrayOfComponentDisplayStrings
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 19:50'!
componentMenu: aMenu
	"Private - Answer a Menu, the component pane menu."
		#SIFUI.
	^aMenu		"Chged SQ"
		labels: ('Edit Component\Expand|Collapse Set\Cut Component',
			'\Paste Component\Delete Component\Find...\Find Next',
			'\Edit Annotations\Show Annotations',
			'\Clear Messages\Show Messages',
			'\Edit Prerequisite Program Names') withCRs
		lines: # (1 2 4 5 7 11)
		selections: #(editComponent expandOrCollapseSet cutComponent
			pasteComponent deleteComponent findComponent findNextComponent
			editComponentAnnotations showComponentAnnotations
			clearComponentMessages showComponentMessages
			editPrerequisiteProgramNames)

! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 8/5/1999 05:28'!
programChangedWithSelectedOrNil: componentOrNil
	"Private - The program changed, so recreate the list of component objects, with the corresponding list of display strings, with componentOrNil the currently selected component or no selection."
		#SIFUI.	"Chged SQ"
	self visableComponentsChanged.
	self selectedComponentOrNil: componentOrNil.

! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 20:44'!
selectedComponentOrNil
	"Private - Answer a <ProgramAbstractComponent>, the currently selected component, or <nil> if there is none."
		#SIFUI.
	self selectedDisplayListIndexOrZero > 0 ifFalse: [	"Chged SQ"
		^nil
	].
	^self visableComponentList
		at: self selectedDisplayListIndexOrZero
		ifAbsent: [
			^nil
		].
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 20:44'!
selectedComponentOrNil: componentOrNil
	"Private - Sets the currently selected component to componentOrNil, or <nil> if there will be no selection.
	Note: Occasionally the parent of a component needs to be the selected component.  When this is the program it isn't displayed and therefore can not be selected, so set selected to none.  Caution - this may cause nested programs in a program to break."
	| index |
		#SIFUI.
	(componentOrNil isNil or: [componentOrNil isKindOf: ProgramDefinitionSet]
	) ifTrue: [
		index := 0.	"Chged SQ"
	] ifFalse: [
		index := self visableComponentList	"Chged SQ"
			indexOf: componentOrNil
			ifAbsent: [0].
	].
	self selectedDisplayListIndexOrZero: index.
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 8/2/1999 16:11'!
selectedDisplayListIndexOrZero
	"Private - Answer an <integer>, the currently selected component display list index, or zero if there is none."
		#SIFUI.	"Chged SQ"
	^selDisplayListIndex! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 9/21/1999 15:37'!
selectedDisplayListIndexOrZero: indexInteger
	"Private - Select the item having the given index in the component display list, or have no selection if zero."
	| selElement |
		#SIFUI.	"Chged SQ"
	selDisplayListIndex := indexInteger.
	self changed: #selectedDisplayListIndexOrZero.

	indexInteger = 0 ifTrue: [
		self additionalSw: self additionalNone.
	] ifFalse: [
		selElement := self selectedComponentOrNil.
		(selElement isKindOf: CodeDefinitionionElement) ifTrue: [
			self additionalSw: self additionalCode
		] ifFalse: [
			(selElement isKindOf: ProgramCommentElement) ifTrue: [
				self additionalSw: self additionalProgramComment
			] ifFalse: [
				self additionalSw: self additionalNone.
	]	]	].
	self changed: #additionalContents.
! !

!ProgramEditor methodsFor: 'component list' stamp: 'RAH 8/5/1999 05:28'!
visableComponentsChanged
	"Private - The visable program components changed, so recreate the list of component objects, with the corresponding list of display strings."
		#SIFUI.	"Chged SQ"
	self buildComponentAndDisplayList.
	self changed: #componentDisplayList.

! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 8/2/1999 15:27'!
addAfter
	"Private - Answer a <symbol>, the after add new switch setting."
		#SIFUI.	"Chged SQ"
	^#'after'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 8/2/1999 15:27'!
addBefore
	"Private - Answer a <symbol>, the before add new switch setting."
		#SIFUI.	"Chged SQ"
	^#'before'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 8/2/1999 15:26'!
addInto
	"Private - Answer a <symbol>, the into add new switch setting."
		#SIFUI.	"Chged SQ"
	^#'into'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 15:26'!
additionalCode
	"Private - Answer a <symbol>, the code additional switch setting."
		#SIFUI.	"Chged SQ"
	^#'code'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 15:29'!
additionalNone
	"Private - Answer a <symbol>, the none additional switch setting."
		#SIFUI.	"Chged SQ"
	^#'none'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 15:30'!
additionalProgramComment
	"Private - Answer a <symbol>, the program comment additional switch setting."
		#SIFUI.	"Chged SQ"
	^#'programComment'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 20:04'!
collapsedInd
	"Private - Answer a <readableString>, the indicator of a collapsed program component set."
		#SIFUI.	"Chged SQ"
	^'>'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 20:06'!
elementInd
	"Private - Answer a <readableString>, the indicator of an program element."
		#SIFUI.	"Chged SQ"
	^' '
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 20:05'!
expandedInd
	"Private - Answer a <readableString>, the indicator of a expanded program component set."
		#SIFUI.	"Chged SQ"
	^'v'
! !

!ProgramEditor methodsFor: 'constants' stamp: 'RAH 9/21/1999 20:07'!
tabingSpace
	"Private - Answer a <readableString>, the spacing for one level of indentation of a program component."
		#SIFUI.	"Chged SQ"
	^'   '! !

!ProgramEditor methodsFor: 'formatting' stamp: 'RAH 7/30/1999 15:13'!
indent: component
	"Private - Answer a <readableString>, the indentation spacing for the program component display string."
	| indent parentList |
		#SIFUI.	"Chged SQ"
	parentList := self program allParentsOf: component.
	indent := String new.
	(parentList size - 1) timesRepeat: [
		indent := indent, self tabingSpace
	].
	^indent! !

!ProgramEditor methodsFor: 'initialize' stamp: 'RAH 9/21/1999 20:31'!
openProgram: aProgramDefinitionSet
	"Private - Create a standard system view of the program, aProgramDefinitionSet, and open it."
	| componentsPane menuButton newButton previousButton topView |
		#SIFUI.			"Chged SQ"
	self additionalSw: self additionalNone.
	self expandedSets: Set new.
	self addNew: self addAfter.
	self pgmManager: ProgramManager current.
	self program: aProgramDefinitionSet.
	selDisplayListIndex := 0.

	self buildComponentAndDisplayList.

	topView := (StandardSystemView new) model: self.
	topView borderWidth: 1.
	topView label: (aProgramDefinitionSet programName, ' Editor').
	topView minimumSize: 100 @ 100.

		"Components pane:"
	componentsPane := PluggableListView on: self
		list: #componentDisplayList
		selected: #selectedDisplayListIndexOrZero
		changeSelected: #selectedDisplayListIndexOrZero:
		menu: #componentMenu:.
	componentsPane window: (0@0 extent: 100 @ 50).
	topView addSubView: componentsPane.

		"New button:"
	newButton := PluggableButtonView on: self
		getState: #newButtontState
		action: #newButtonRotate
		label: #newButtonLabel.
	newButton
		borderWidth: 1;
		label: self newButtonLabel;
		window: (0@0 extent: 20 @ 8).
	topView
		addSubView: newButton
		below: componentsPane.
	previousButton := newButton.

		"New Class menu button:"
"	menuButton := PluggableButtonView on: self
		getState: nil
		action: nil
		label: #newClassLabel
		menu: #newClassMenu:.
"
	menuButton := PluggableTextView on: self
		text: #newClassLabel
		accept: nil
		readSelection: nil
		menu: #newClassMenu:.
	menuButton
		borderWidth: 1;
"		label: self newClassLabel;"
		window: (0@0 extent: 20 @ 8).
	topView
		addSubView: menuButton
		toRightOf: previousButton.
	previousButton := menuButton.

		"New Global menu button:"
	menuButton := PluggableTextView on: self
		text: #newGlobalLabel
		accept: nil
		readSelection: nil
		menu: #newGlobalMenu:.
	menuButton
		borderWidth: 1;
		"label: self newGlobalLabel;"
		window: (0@0 extent: 20 @ 8).
	topView
		addSubView: menuButton
		toRightOf: previousButton.
	previousButton := menuButton.

		"New Pool menu button:"
	menuButton := PluggableTextView on: self
		text: #newPoolLabel
		accept: nil
		readSelection: nil
		menu: #newPoolMenu:.
	menuButton
		borderWidth: 1;
		"label: self newPoolLabel;"
		window: (0@0 extent: 20 @ 8).
	topView
		addSubView: menuButton
		toRightOf: previousButton.
	previousButton := menuButton.

		"New Program menu button:"
	menuButton := PluggableTextView on: self
		text: #newProgramLabel
		accept: nil
		readSelection: nil
		menu: #newProgramMenu:.
	menuButton
		borderWidth: 1;
		"label: self newProgramLabel;"
		window: (0@0 extent: 20 @ 8).
	topView
		addSubView: menuButton
		toRightOf: previousButton.
	previousButton := menuButton.

		"Additional pane:"
	additionalPane := PluggableTextView on: self
		text: #additionalContents
		accept: #additionalContents:
		readSelection: nil
		menu: #codePaneMenu:shifted:.
	additionalPane window: (0@0 extent: 100 @ 42).
	topView
		addSubView: additionalPane
		below: newButton.

	topView controller open.
! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:27'!
newButtonLabel
	"Private - Answer a <readableString>, the add new switch label depending on its current setting."
		#SIFUI.			"Chged SQ"
	^'new ', self addNew asString, ':'
! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:28'!
newButtonRotate
	"Private - Rotate the add new switch to the next setting of whether the new component should be added #'before', #'after', or #'into', the selected component."
		#SIFUI.	"Chged SQ"
	self addNew = self addAfter ifTrue: [
		self addNew: self addBefore
	] ifFalse: [
		self addNew = self addBefore ifTrue: [
			self addNew: self addInto
		] ifFalse: [
			self addNew: self addAfter
	]	].
	self changed: #newButtonLabel.

! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:28'!
newButtontState
	"Private - Answer a <boolean>, the add new switch state as always true so it is highlighted."
		#SIFUI.			"Chged SQ"
	^true
! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:28'!
newClassLabel
	"Private - Answer a <readableString>, the add new class button label."
		#SIFUI.			"Chged SQ"
	^'   Add Class'! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:28'!
newClassMenu: aMenu
	"Private - Answer a Menu, the new class component button menu."
		#SIFUI.			"Chged SQ"
	^aMenu
		labels: ('Class All\Definition',
			'\Class Method',
			'\Instance Method\Initializer') withCRs
		lines: #()
		selections: #(newClassAllSet newClassDefinitionElement
			newClassMethodDefinitionElement
			newInstanceMethodDefinitionElement newClassInitializerElement)

! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:28'!
newGlobalLabel
	"Private - Answer a <readableString>, the add new global button label."
		#SIFUI.			"Chged SQ"
	^'  Add Global'! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:29'!
newGlobalMenu: aMenu
	"Private - Answer a Menu, the new global component button menu."
		#SIFUI.		"Chged SQ"
	^aMenu
		labels: ('Global All\Definition',
			'\Initializer') withCRs
		lines: #()
		selections: #(newGlobalAllSet newGlobalDefinitionElement
			newGlobalInitializerElement)

! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:29'!
newPoolLabel
	"Private - Answer a <readableString>, the add new pool button label."
		#SIFUI.		"Chged SQ"
	^'   Add Pool'! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:29'!
newPoolMenu: aMenu
	"Private - Answer a Menu, the new pool component button menu."
		#SIFUI.		"Chged SQ"
	^aMenu
		labels: ('Pool All\Definition\Value Definition',
			'\Value Initializer') withCRs
		lines: #()
		selections: #(newPoolAllSet newPoolDefinitionElement
			newPoolValueDefinitionElement newPoolValueInitializerElement)

! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:29'!
newProgramLabel
	"Private - Answer a <readableString>, the add new program button label."
		#SIFUI.		"Chged SQ"
	^'Add Program'! !

!ProgramEditor methodsFor: 'new component' stamp: 'RAH 9/21/1999 20:30'!
newProgramMenu: aMenu
	"Private - Answer a Menu, the new program component button menu."
		#SIFUI.		"Chged SQ"
	^aMenu
		labels: ('Program Comment',
			'\Program Initializer\Grouping Set') withCRs
		lines: #()
		selections: #(newProgramCommentElement 			newProgramInitializerElement newGroupingSet)

! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 19:49'!
addExpandedSet: aProgramComponentSet
	"Private - Add aProgramComponentSet to the collection of program component sets that are to be displayed expanded (showing children)."
		#SIFUI.			"Chged SQ"
	aProgramComponentSet isComponentSet ifFalse: [
		self errorMessage: 'Object to be added is not a ProgramComponentSet.'.
	].
	self expandedSets add: aProgramComponentSet
! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 19:42'!
constantPrompt
	"Private - Answer a <boolean>, prompted from the user whether the new component being added is a constant."
		#SIFUI.
	(SelectionMenu confirm: 'Is it a constant?') ifTrue: [		"Chged SQ"
		^true
	].
	^false.
! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 19:50'!
currentSetAddComponent: componentToAdd
	"Private - Answer a <boolean>, true if the add operation succeeds or false if it fails.  Add a new component before, or after the currently selected component, or if none selected into the program."
	| errBlock selectedComponent |
		#SIFUI.
	errBlock := [ :except | "okCancel"
		PopUpMenu notify: ('Not added - ', except description, '.').
		^false
	].

	selectedComponent := self selectedComponentOrNil.
	selectedComponent isNil ifTrue: [
		"None selected so add new component into the program."
		[self program addComponent: componentToAdd.
		] on: PGM programError do: errBlock.
		self programChangedWithSelectedOrNil: componentToAdd.
		^true
	].

	self addNew = self addAfter ifTrue: [
		[selectedComponent parentSetOrNil
			addComponent: componentToAdd
			after: selectedComponent.
		] on: PGM programError do: errBlock.
		self programChangedWithSelectedOrNil: componentToAdd.
		^true
	].

	self addNew = self addBefore ifTrue: [
		[selectedComponent parentSetOrNil
			addComponent: componentToAdd
			before: selectedComponent.
		] on: PGM programError do: errBlock.
		self programChangedWithSelectedOrNil: componentToAdd.
		^true
	].


	"Must be add new component into the currently selected component."
	(selectedComponent isKindOf: ProgramComponentSet) ifFalse: [
		PopUpMenu notify: ('Not added into - the selected component is not a set.').
		^false
	].

	[selectedComponent addComponent: componentToAdd.
	] on: Error do: errBlock.
	self programChangedWithSelectedOrNil: componentToAdd.
	^true
! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 20:18'!
globalNamePrompt: promptString caption: captionString
	"Private - Answer a <readableString>, a global name prompted from the user, or <nil> if user canceled or an error."
	| globalNameString |
		#SIFUI.
	globalNameString := String new.
	[	globalNameString := FillInTheBlank	"Chged SQ"
			request: promptString
			initialAnswer: globalNameString.
		(globalNameString isNil or: [globalNameString isEmpty]) ifTrue: [
			^nil
		].
		Smalltalk includesKey: globalNameString asSymbol
	] whileFalse: [
		PopUpMenu notify: ('Global "', globalNameString, '" is not defined.').
		^nil
	].
	^globalNameString! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 20:18'!
methodNamePrompt: promptString caption: captionString
	"Private - Answer a <readableString>, a method name prompted from the user, or <nil> if user canceled."
	| methodNameString |
		#SIFUI.
	methodNameString := FillInTheBlank request: promptString.	"Chged SQ"
	(methodNameString isNil or: [methodNameString isEmpty]) ifTrue: [
		^nil
	].
	^methodNameString
! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 20:35'!
poolValueNamePrompt: promptString caption: captionString
	"Private - Answer a <readableString>, a pool value name prompted from the user, or nil if user canceled."
	| poolValueNameString |
		#SIFUI.	"Chged SQ"
	poolValueNameString := FillInTheBlank request:  promptString.
	(poolValueNameString isNil or: [poolValueNameString isEmpty]) ifTrue: [
		^nil
	].
	^poolValueNameString
! !

!ProgramEditor methodsFor: 'private' stamp: 'RAH 9/21/1999 20:40'!
removeExpandedSet: aProgramComponentSet
	"Private - Remove aProgramComponentSet from the collection of program component sets that are to be displayed expanded (showing children)."
		#SIFUI.		"Chged SQ"
	aProgramComponentSet isComponentSet ifFalse: [
		self errorMessage: 'Object to be removed is not a ProgramComponentSet.'.
	].
	"First remove sub-component sets recursively:"
	self expandedSets copy do: [ :aSet |
		(aProgramComponentSet components includes: aSet) ifTrue: [
			self removeExpandedSet: aSet
	]	].
	"Then remove aProgramComponentSet set:"
	self expandedSets
		remove: aProgramComponentSet
		ifAbsent: [
			self errorMessage: 'ProgramComponentSet to be removed is not expanded.'.
		].
	! !


!ProgramEditor class methodsFor: 'instance creation' stamp: 'RAH 8/21/1999 08:57'!
openEditorOn: aProgramDefinitionSet
	"Open an editor on the program, aProgramDefinitionSet.  Example:

		ProgramEditor openEditorOn: (PGM newProgramNamed: 'Test1').
"
		#SIFUI. "Protocol: <programEditor factory>"
	self new							"Chged SQ"
		openProgram: aProgramDefinitionSet.

! !


