'From Squeak3.7beta of ''1 April 2004'' [latest update: #5954] on 14 June 2004 at 9:49:11 am'!"Change Set:		FileList2VolMenuFix-nkDate:			14 June 2004Author:			Ned KonzBrings back the FileList2 volume menu.Makes it update correctly when adding or removing directories.Adds an interface to change the directory."!!FileList2 methodsFor: 'initialization' stamp: 'nk 6/14/2004 09:39'!updateDirectory	"directory has been changed externally, by calling directory:.	Now change the view to reflect the change."	self changed: #currentDirectorySelected.	self postOpen.! !!FileList2 methodsFor: 'own services' stamp: 'nk 6/14/2004 09:43'!addNewDirectory	super addNewDirectory.	self updateDirectory.! !!FileList2 methodsFor: 'own services' stamp: 'nk 6/14/2004 09:42'!deleteDirectory	super deleteDirectory.	self updateDirectory.! !!FileList2 methodsFor: 'user interface' stamp: 'rww 12/13/2003 13:07'!morphicDirectoryTreePaneFiltered: aSymbol	^(SimpleHierarchicalListMorph 		on: self		list: aSymbol		selected: #currentDirectorySelected		changeSelected: #setSelectedDirectoryTo:		menu: #volumeMenu:		keystroke: nil)			autoDeselect: false;			enableDrag: false;			enableDrop: true;			yourself		! !!FileList2 methodsFor: 'volume list and pattern' stamp: 'nk 6/14/2004 09:45'!changeDirectoryTo: aFileDirectory	"Change directory as requested."	self directory: aFileDirectory.	self updateDirectory! !!FileList2 reorganize!('drag''n''drop' dropDestinationDirectory:event: isDirectoryList:)('initialization' dirSelectionBlock: directory: directoryChangeBlock: fileSelectionBlock: initialDirectoryList labelString limitedSuperSwikiDirectoryList limitedSuperSwikiPublishDirectoryList optionalButtonSpecs optionalButtonSpecs: publishingServers universalButtonServices updateDirectory)('initialize-release' initialize)('own services' addNewDirectory deleteDirectory importImage okayAndCancelServices openImageInWindow openProjectFromFile removeLinefeeds serviceCancel serviceOkay serviceOpenProjectFromFile servicesForFolderSelector servicesForProjectLoader)('user interface' blueButtonForService:textColor:inWindow: morphicDirectoryTreePane morphicDirectoryTreePaneFiltered: morphicFileContentsPane morphicFileListPane morphicPatternPane)('volume list and pattern' changeDirectoryTo: directory listForPattern: listForPatterns:)('private' cancelHit currentDirectorySelected directoryNamesFor: getSelectedDirectory getSelectedFile modalView: okHit okHitForProjectLoader postOpen saveLocalOnlyHit setSelectedDirectoryTo:)!