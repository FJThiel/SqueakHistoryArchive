'From Squeak 2.3 beta of Nov 25, 1998 on 23 December 1998 at 3:12 pm'!"Change Set:		QuickSortDate:			23 December 1998Author:			Dan IngallsAdds an optional cachedThumbnail property to PasteUpMorphs.  This greatly accelerates the generation of page sorters.  The cache can be flushed (to save space or bring images current) using the 'uncache page sorter' item in the book menu.Also makes PageSorters resistant to dropping into bookmorphs."!!BookMorph methodsFor: 'menu' stamp: 'di 12/23/1998 15:11'!addBookMenuItemsTo: aMenu hand: aHandMorph	| controlsShowing subMenu |	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'previous page' action: #previousPage.	subMenu add: 'next page' action: #nextPage.	subMenu add: 'insert a page' action: #insertPage.	subMenu add: 'delete this page' action: #deletePage.	controlsShowing _ self hasSubmorphWithProperty: #pageControl.	controlsShowing		ifTrue: [subMenu add: 'hide page controls' action: #hidePageControls]		ifFalse: [subMenu add: 'show page controls' action: #showPageControls].	subMenu addLine.	subMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.	subMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.	subMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.	subMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.	subMenu addLine.	subMenu add: 'sort pages' action: #sortPages:.	subMenu add: 'uncache page sorter' action: #uncachePageSorter.	subMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil:		[subMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	(aHandMorph classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[subMenu add: 'paste book page'	action: #pasteBookPage].	subMenu add: 'send other pages to server' action: #savePagesOnURL.	aMenu add: 'book...' subMenu: subMenu! !!BookMorph methodsFor: 'menu' stamp: 'di 12/23/1998 15:12'!invokeBookMenu	"Invoke the book's control panel menu."	| aMenu hand |	aMenu _ MenuMorph new defaultTarget: self.	aMenu addList:	#(			('sort pages'				sortPages)			('uncache page sorter'	uncachePageSorter)			('make bookmark'		bookmarkForThisPage)			('make thumbnail'		thumbnailForThisPage)			('remove control panel'	hidePageControls)		).	aMenu addLine.	aMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.	aMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.	aMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.	aMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.	aMenu addLine.	(self primaryHand classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[aMenu add: 'paste book page'	action: #pasteBookPage].	aMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil: [		aMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	aMenu add: (openToDragNDrop ifTrue: ['close'] ifFalse: ['open']) , ' dragNdrop'			action: #openCloseDragNDrop.	aMenu add: 'send all pages to server' action: #savePagesOnURL.	aMenu add: 'send this page to server' action: #saveOneOnURL.	aMenu add: 'reload all from server' action: #reload.	aMenu add: 'keep in one file' action: #keepTogether.	hand _ self world primaryHand.	aMenu popUpAt: hand position forHand: hand."	sel _ aMenu invokeAt: self primaryHand position in: self world.	sel ifNotNil: [self perform: sel]."! !!BookMorph methodsFor: 'menu' stamp: 'di 12/23/1998 14:55'!uncachePageSorter	pages do: [:aPage | aPage removeProperty: #cachedThumbnail].! !!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'di 12/23/1998 14:04'!wantsDroppedMorph: aMorph event: evt	(self isPartsBin or: [self openToDragNDrop]) not ifTrue: [^ false].	(self bounds containsPoint: (self pointFromWorld: evt cursorPoint)) ifFalse: [^ false].	((aMorph isKindOf: SystemWindow) and: [Preferences allowSysWindowEmbedding not])		ifTrue:	[^ false].	((aMorph isKindOf: BookPageSorterMorph) and: [self isWorldMorph not])		ifTrue:	[^ false].	^ true! !!PasteUpMorph methodsFor: 'misc' stamp: 'di 12/23/1998 14:44'!cachedOrNewThumbnailFrom: newThumbnail	"If I have a cached thumbnail, and it is of the desired extent, then ruturn it.	Otherwise produce one in newThumbnail and return it (after caching).	This code parallels what happens in page: to match resultant extent."	| cachedThumbnail scale ext |	scale _ newThumbnail height / self fullBounds height.	ext _ (self fullBounds extent * scale) truncated.	(cachedThumbnail _ self valueOfProperty: #cachedThumbnail) ifNotNil:		[cachedThumbnail extent = ext ifTrue: [^ cachedThumbnail]].	self setProperty: #cachedThumbnail toValue: (newThumbnail page: self).	^ newThumbnail! !!PasteUpMorph methodsFor: 'misc' stamp: 'di 12/23/1998 14:45'!smallThumbnailForPageSorter	^ self cachedOrNewThumbnailFrom: BookPageThumbnailMorph new smaller! !!PasteUpMorph methodsFor: 'misc' stamp: 'di 12/23/1998 14:44'!thumbnailForPageSorter	^ self cachedOrNewThumbnailFrom: BookPageThumbnailMorph new! !