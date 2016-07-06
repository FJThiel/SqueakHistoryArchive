'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:54 pm'!
"Change Set:		q13-resizeBook-sw
Date:			7 June 2003
Author:			Scott Wallace

Derived from Squeakland updates:
 0166fullScreenBookFix (4 March 2003)
 0176pageSize-sw (7 June 2003)
 0177storyBdFix-sw (25 July 2003)

Makes the resize handle on a BookMorph or StackMorph or TabbedPalette behave the way any reasonable person would expect.  Finally, after all these years.

Also adds an option for any BookMorph or StackMorph to have all its pages be maintained at the same dimensions.

Also, a bug fix:  In a bookMorph's full-screen mode, going to a different page was resulting in the disappearance and non-recoverability of the floating page controls"!


!BooklikeMorph methodsFor: 'page controls' stamp: 'sw 6/6/2003 14:10'!
addPageControlMorph: aMorph
	"Add the morph provided as a page control, at the appropriate place"

	aMorph setProperty: #pageControl toValue: true.
	self addMorph: aMorph asElementNumber: self indexForPageControls! !

!BooklikeMorph methodsFor: 'page controls' stamp: 'sw 6/6/2003 17:00'!
indexForPageControls
	"Answer which submorph should hold the page controls"

	^ (submorphs size > 0 and: [submorphs first hasProperty: #header])
		ifTrue:	[2]
		ifFalse:	[1]! !

!BooklikeMorph methodsFor: 'page controls' stamp: 'sw 6/6/2003 22:44'!
setEventHandlerForPageControls: controls
	"Set the controls' event handler if appropriate.  Default is to let the tool be dragged by the controls"

	controls eventHandler: (EventHandler new on: #mouseDown send: #move to: self)! !

!BooklikeMorph methodsFor: 'page controls' stamp: 'sw 6/6/2003 13:58'!
showPageControls: controlSpecs  
	"Remove any existing page controls, and add fresh controls at the top of the receiver (or in position 2 if the receiver's first submorph is one with property #header).  Add a single column of controls."

	| pageControls column |
	self hidePageControls.
	column _ AlignmentMorph newColumn beTransparent.
	pageControls _ self makePageControlsFrom: controlSpecs.
	pageControls borderWidth: 0; layoutInset: 4.
	pageControls beSticky.
	pageControls setNameTo: 'Page Controls'.
	self setEventHandlerForPageControls: pageControls.
	column addMorphBack: pageControls.
	self addPageControlMorph: column! !


!BookMorph methodsFor: 'menu' stamp: 'sw 3/3/2004 18:40'!
invokeBookMenu
	"Invoke the book's control panel menu."
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Book' translated.
	aMenu addStayUpItem.
	aMenu add: 'find...' translated action: #textSearch.
	aMenu add: 'go to page...' translated action: #goToPage.
	aMenu addLine.

	aMenu addList: {
		{'sort pages' translated.		#sortPages}.
		{'uncache page sorter' translated.	#uncachePageSorter}}.
	(self hasProperty: #dontWrapAtEnd)
		ifTrue: [aMenu add: 'wrap after last page' translated selector: #setWrapPages: argument: true]
		ifFalse: [aMenu add: 'stop at last page' translated selector: #setWrapPages: argument: false].
	aMenu addList: {
		{'make bookmark' translated.		#bookmarkForThisPage}.
		{'make thumbnail' translated.		#thumbnailForThisPage}}.
	aMenu addUpdating: #showingPageControlsString action: #toggleShowingOfPageControls.
	aMenu addUpdating: #showingFullScreenString action: #toggleFullScreen.

	aMenu addLine.
	aMenu add: 'sound effect for all pages' translated action: #menuPageSoundForAll:.
	aMenu add: 'sound effect this page only' translated action: #menuPageSoundForThisPage:.
	aMenu add: 'visual effect for all pages' translated action: #menuPageVisualForAll:.
	aMenu add: 'visual effect this page only' translated action: #menuPageVisualForThisPage:.

	aMenu addLine.
	(self primaryHand pasteBuffer class isKindOf: PasteUpMorph class) ifTrue:
		[aMenu add: 'paste book page' translated   action: #pasteBookPage].

	aMenu add: 'save as new-page prototype' translated action: #setNewPagePrototype.
	newPagePrototype ifNotNil: [
		aMenu add: 'clear new-page prototype' translated action: #clearNewPagePrototype].

	aMenu add: (self dragNDropEnabled ifTrue: ['close dragNdrop'] ifFalse: ['open dragNdrop']) translated
			action: #toggleDragNDrop.
	aMenu add: 'make all pages this size' translated action: #makeUniformPageSize.
	
	aMenu
		addUpdating: #keepingUniformPageSizeString
		target: self
		action: #toggleMaintainUniformPageSize.
	aMenu addLine.

	aMenu add: 'send all pages to server' translated action: #savePagesOnURL.
	aMenu add: 'send this page to server' translated action: #saveOneOnURL.
	aMenu add: 'reload all from server' translated action: #reload.
	aMenu add: 'copy page url to clipboard' translated action: #copyUrl.
	aMenu add: 'keep in one file' translated action: #keepTogether.

	aMenu addLine.
	aMenu add: 'load PPT images from slide #1' translated action: #loadImagesIntoBook.
	aMenu add: 'background color for all pages...' translated action: #setPageColor.
	aMenu add: 'make a thread of projects in this book' translated action: #buildThreadOfProjects.

	aMenu popUpEvent: self world activeHand lastEvent in: self world
! !

!BookMorph methodsFor: 'other' stamp: 'sw 6/6/2003 13:55'!
adjustCurrentPageForFullScreen
	"Adjust current page to conform to whether or not I am in full-screen mode.  Also, enforce uniform page size constraint if appropriate"

	self isInFullScreenMode
		ifTrue:
			[(currentPage hasProperty: #sizeWhenNotFullScreen) ifFalse:
				[currentPage setProperty: #sizeWhenNotFullScreen toValue: currentPage extent].
			currentPage extent: Display extent]
		ifFalse:
			[(currentPage hasProperty: #sizeWhenNotFullScreen) ifTrue:
				[currentPage extent: (currentPage valueOfProperty: #sizeWhenNotFullScreen).
				currentPage removeProperty: #sizeWhenNotFullScreen].
			self uniformPageSize ifNotNilDo:
				[:anExtent | currentPage extent: anExtent]].
	(self valueOfProperty: #floatingPageControls) ifNotNilDo:
		[:pc | pc isInWorld ifFalse: [pc openInWorld]]! !

!BookMorph methodsFor: 'other' stamp: 'sw 6/6/2003 17:21'!
setExtentFromHalo: anExtent
	"The user has dragged the grow box such that the receiver's extent would be anExtent.  Do what's needed.  For a BookMorph, we assume any resizing attempt is a request that the book-page currently being viewed be resized accoringly; this will typically not affect unseen book pages, though there is a command that can be issued to harmonize all book-page sizes, and also an option to set that will maintain all pages at the same size no matter what."

	currentPage isInWorld
		ifFalse: "doubtful case mostly"
			[super setExtentFromHalo: anExtent]
		ifTrue:
			[currentPage width: anExtent x.
			currentPage height: (anExtent y - (self innerBounds height - currentPage height)).
			self maintainsUniformPageSize ifTrue:
				[self setProperty: #uniformPageSize toValue: currentPage extent]]! !

!BookMorph methodsFor: 'uniform page size' stamp: 'sw 3/3/2004 18:39'!
keepingUniformPageSizeString
	"Answer a string characterizing whether I am currently maintaining uniform page size"

	^ (self maintainsUniformPageSize
		ifTrue: ['<yes>']
		ifFalse: ['<no>']), 'keep all pages the same size' translated! !

!BookMorph methodsFor: 'uniform page size' stamp: 'sw 6/6/2003 13:56'!
maintainsUniformPageSize
	"Answer whether I am currently set up to maintain uniform page size"

	^ self uniformPageSize notNil! !

!BookMorph methodsFor: 'uniform page size' stamp: 'sw 6/6/2003 13:56'!
maintainsUniformPageSize: aBoolean
	"Set the property governing whether I maintain uniform page size"

	aBoolean
		ifFalse:
			[self removeProperty: #uniformPageSize]
		ifTrue:
			[self setProperty: #uniformPageSize toValue: currentPage extent]! !

!BookMorph methodsFor: 'uniform page size' stamp: 'sw 6/6/2003 13:57'!
toggleMaintainUniformPageSize
	"Toggle whether or not the receiver should maintain uniform page size"

	self maintainsUniformPageSize: self maintainsUniformPageSize not! !

!BookMorph methodsFor: 'uniform page size' stamp: 'sw 6/6/2003 13:57'!
uniformPageSize
	"Answer the uniform page size to maintain, or nil if the option is not set"

	^ self valueOfProperty: #uniformPageSize ifAbsent: [nil]! !


!StackMorph methodsFor: 'initialization' stamp: 'sw 6/5/2003 04:04'!
addPane: aPane paneType: aType
	| anIndex |
	anIndex _ self insertionIndexForPaneOfType: aType.
	self privateAddMorph: aPane atIndex: anIndex! !

!StackMorph methodsFor: 'menu' stamp: 'sw 6/6/2003 13:53'!
offerBookishMenu
	"Offer a menu with book-related items in it"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Stack / Book'.
	aMenu addStayUpItem.
	aMenu addList:
		#(('sort pages'			sortPages)
		('uncache page sorter'	uncachePageSorter)).
	(self hasProperty: #dontWrapAtEnd)
		ifTrue: [aMenu add: 'wrap after last page' selector: #setWrapPages: argument: true]
		ifFalse: [aMenu add: 'stop at last page' selector: #setWrapPages: argument: false].
	aMenu addList:
		#(('make bookmark'		bookmarkForThisPage)
		('make thumbnail'		thumbnailForThisPage)).

	aMenu addLine.
	aMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.
	aMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.
	aMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.
	aMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.

	aMenu addLine.
	(self primaryHand pasteBuffer class isKindOf: PasteUpMorph class) ifTrue:
		[aMenu add: 'paste book page'   action: #pasteBookPage].

	aMenu add: 'save as new-page prototype' action: #setNewPagePrototype.
	newPagePrototype ifNotNil: [
		aMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].

	aMenu add: (self dragNDropEnabled ifTrue: ['close'] ifFalse: ['open']) , ' dragNdrop'
			action: #toggleDragNDrop.
	aMenu addLine.
	aMenu add: 'make all pages this size' action: #makeUniformPageSize.
	aMenu addUpdating: #keepingUniformPageSizeString target: self action: #toggleMaintainUniformPageSize.
	aMenu addLine.
	aMenu add: 'send all pages to server' action: #savePagesOnURL.
	aMenu add: 'send this page to server' action: #saveOneOnURL.
	aMenu add: 'reload all from server' action: #reload.
	aMenu add: 'copy page url to clipboard' action: #copyUrl.
	aMenu add: 'keep in one file' action: #keepTogether.

	aMenu addLine.
	aMenu add: 'load PPT images from slide #1' action: #loadImagesIntoBook.
	aMenu add: 'background color for all pages...' action: #setPageColor.

	aMenu popUpEvent: self world activeHand lastEvent in: self world


! !

!StackMorph methodsFor: 'page controls' stamp: 'sw 6/6/2003 13:59'!
addPageControlMorph: aMorph
	"Add the given morph as a page-control, at the appropriate place"

	aMorph setProperty: #pageControl toValue: true.
	self addPane: aMorph paneType: #pageControl! !

!StackMorph methodsFor: 'submorphs-accessing' stamp: 'sw 6/5/2003 04:01'!
insertionIndexForPaneOfType: aType
	| naturalIndex insertionIndex |
	naturalIndex _ self naturalPaneOrder indexOf: aType.
	insertionIndex _ 1.
	(self naturalPaneOrder copyFrom: 1 to: (naturalIndex - 1)) do: "guys that would precede"
		[:sym | (self hasSubmorphWithProperty: sym)
			ifTrue:
				[insertionIndex _ insertionIndex + 1]].
	^ insertionIndex! !

!StackMorph methodsFor: 'submorphs-accessing' stamp: 'sw 6/5/2003 04:02'!
naturalPaneOrder
	^ #(header pageControl retrieve search index content)! !


!StoryboardBookMorph methodsFor: 'navigation' stamp: 'sw 7/25/2003 16:47'!
insertPageMorphInCorrectSpot: aPageMorph
	"Insert the page morph at the correct spot"
	
	| place |
	place _ submorphs size > 1 ifTrue: [submorphs second] ifFalse: [submorphs first].
	"Old architecture had a tiny spacer morph as the second morph; now architecture does not"
	self addMorph: (currentPage _ aPageMorph) behind: place.
	self changeTiltFactor: self getTiltFactor.
	self changeZoomFactor: self getZoomFactor.
	zoomController target: currentPage.

! !


!TabbedPalette methodsFor: 'other' stamp: 'sw 6/6/2003 17:38'!
setExtentFromHalo: anExtent
	"The user has dragged the grow box such that the receiver's extent would be anExtent.  Do what's needed.  For a BookMorph, we assume any resizing attempt is a request that the book-page currently being viewed be resized accoringly; this will typically not affect unseen book pages, though there is a command that can be issued to harmonize all book-page sizes, and also an option to set that will maintain all pages at the same size no matter what."

	currentPage isInWorld
		ifFalse: "doubtful case mostly"
			[super setExtentFromHalo: anExtent]
		ifTrue:
			[currentPage setExtentFromHalo: ((anExtent x @ (anExtent y - (self innerBounds height - currentPage height))) - (2 * (self borderWidth @ self borderWidth))).
			self maintainsUniformPageSize ifTrue:
				[self setProperty: #uniformPageSize toValue: currentPage extent]]! !

