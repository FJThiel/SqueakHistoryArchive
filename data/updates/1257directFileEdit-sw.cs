'From Squeak 2.4c of May 10, 1999 on 21 June 1999 at 4:09:42 pm'!"Change Set:		directFileEdit-swDate:			9 June 1999Author:			Scott WallaceExploiting the StandardFile capability kindly contributed by Andrew Greenberg, adds a direct way that a file can be open for editing from the screen menu."!!FileList class methodsFor: 'instance creation' stamp: 'sw 6/9/1999 12:29'!openFileDirectly	| aResult |	(aResult _ StandardFileMenu oldFile) ifNotNil:		[self openEditorOn: (aResult directory readOnlyFileNamed: aResult name) editString: nil]! !!HandMorph methodsFor: 'world menu' stamp: 'sw 6/21/1999 16:09'!openMenu	"Build the open window menu for the world."	| menu |	menu _ (MenuMorph entitled: 'open...') defaultTarget: self.	menu addStayUpItem.	menu add: 'browser' action: #openBrowser.	menu add: 'workspace' action: #openWorkspace.	menu add: 'file list' action: #openFileList.	menu add: 'file...' target: FileList action: #openFileDirectly.	menu add: 'transcript' target: Transcript action: #open.	menu add: 'selector finder' target: ScreenController new action: #openSelectorBrowser.	menu addLine.	menu add: 'simple change sorter' selector: #openChangeSorter: argument: 1.	menu add: 'dual change sorter' selector: #openChangeSorter: argument: 2.	menu addLine.	menu add: 'email reader' action: #openEmail.	menu add: 'web browser' action: #openWebBrowser.	menu add: 'IRC chat' action: #openIRC.	menu addLine.	(Preferences allowMVCprojects and: [Smalltalk includesKey: #StandardSystemView])		ifTrue: [menu add: 'project (mvc)' action: #openMVCProject].	menu add: 'project (morphic)' action: #openMorphicProject.	menu add: 'project (construction)' action: #openConstructionProject.	^ menu! !!ScreenController methodsFor: 'menu messages' stamp: 'sw 6/9/1999 12:30'!openFile	FileList openFileDirectly! !!ScreenController methodsFor: 'nested menus' stamp: 'sw 6/9/1999 12:03'!openMenu	^ SelectionMenu labelList:		#(	'keep this menu up'			'browser'			'workspace'			'file list'			'file...'			'transcript'			'selector finder'			'simple change sorter'			'dual change sorter'			'project (mvc)'			'project (morphic)'			'project (construction)'			)		lines: #(1 7 9)		selections: #(durableOpenMenuopenBrowser openWorkspace openFileList openFile openTranscript openSelectorBrowseropenSimpleChangeSorter openChangeManageropenProject  openMorphicProject  openConstructionProject )"ScreenController  new openMenu startUp"! !!StandardFileMenuResult methodsFor: 'accessing' stamp: 'sw 6/9/1999 11:50'!printOn: aStream	super printOn: aStream.	aStream nextPutAll: ' with directory: '.	directory printOn: aStream.	aStream nextPutAll: ' name: '.	name printOn: aStream"StandardFileMenu oldFile"! !