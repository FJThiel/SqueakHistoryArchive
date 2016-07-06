'From Squeak 2.0 BETA of May 8, 1998 on 14 May 1998 at 11:59:25 am'!"Change Set:		sw_pasteBookpageDate:			14 May 1998Author:			ScottAllows book pages (any pasteup/playfield/holder) to be pasted into books from the book control panel"!!BookMorph methodsFor: 'menu' stamp: 'sw 5/14/1998 11:12'!addBookMenuItemsTo: aCustomMenu hand: aHandMorph	aCustomMenu		add: (copyContents				ifTrue: ['don''t be parts bin when closed']				ifFalse: ['be parts bin when closed'])		action: #toggleCopyContents.	aCustomMenu add: 'previous page' action: #previousPage.	aCustomMenu add: 'next page' action: #nextPage.	aCustomMenu add: 'insert a page' action: #insertPage.	aCustomMenu add: 'delete this page' action: #deletePage.	aCustomMenu add: 'page controls' action: #pageControls:.	aCustomMenu add: 'sort pages' action: #sortPages:.	(aHandMorph classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[aCustomMenu add: 'paste book page'	action: #pasteBookPage]! !!BookMorph methodsFor: 'menu' stamp: 'sw 5/14/1998 11:06'!insertPage: aPage pageSize: aPageSize atIndex: anIndex	| sz  predecessor |	sz _ aPageSize		ifNil: [currentPage == nil			ifTrue: [pageSize]			ifFalse: [currentPage extent]]		ifNotNil:			[aPageSize].	aPage extent: sz.	((pages isEmpty | anIndex == nil) or: [anIndex > pages size])		ifTrue:			[pages add: aPage]		ifFalse:			[anIndex <= 1				ifTrue:					[pages addFirst: aPage]				ifFalse:					[predecessor _ anIndex == nil						ifTrue:							[currentPage]						ifFalse:							[pages at: anIndex].					self pages add: aPage after: predecessor]].	self goToPageMorph: aPage! !!BookMorph methodsFor: 'menu' stamp: 'sw 5/14/1998 10:26'!invokeBookMenu	"Answer a menu to be popped up from the book-control panel"	| aMenu |	aMenu _ CustomMenu new.	aMenu addList:	#(		"	('border color...' 		changeBorderColor:)			('border width...' 		changeBorderWidth:)			('lock'					lock)"			('make bookmark'		bookmarkForThisPage)			('sort pages'				sortPages:)			('remove control panel'	deleteControls)		).	(self primaryHand classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[aMenu add: 'paste book page'	action: #pasteBookPage].	aMenu add: (openToDragNDrop ifTrue: ['close'] ifFalse: ['open']) , ' dragNdrop'			action: #openCloseDragNDrop.	aMenu invokeOn: self defaultSelection: nil! !!BookMorph methodsFor: 'menu' stamp: 'sw 5/14/1998 11:04'!pasteBookPage	| aPage |	aPage _ self primaryHand objectToPaste.	self insertPage: aPage pageSize: aPage extent atIndex: ((pages indexOf: currentPage) - 1).	"self goToPageMorph: aPage"! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/14/1998 10:23'!classOfPasteBuffer	"avoids the weight of duplication but preserves privacy of PasteBuffer, for when we only need to know the *type* of data on the buffer, for arming menus"	^ PasteBuffer class! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/14/1998 11:51'!copyToPasteBuffer	"Save this morph in the paste buffer. This is mostly useful for copying morphs between projects."	argument isMorph		ifTrue: [PasteBuffer _ argument usableDuplicate]		ifFalse: [PasteBuffer _ nil].! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/14/1998 10:28'!objectToPaste	"It may need to be sent #startRunning by the client"	^ PasteBuffer usableDuplicate! !