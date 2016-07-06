'From Squeak 2.3 beta of Nov 25, 1998 on 11 January 1999 at 12:16:36 pm'!!BookMorph methodsFor: 'navigation' stamp: 'tk 1/11/1999 11:52'!goToPageUrl: aUrl	self goToPageMorph: (pages detect: [:pg | pg url = aUrl] ifNone: [pages at: 1]).! !!BookMorph methodsFor: 'menu' stamp: 'tk 1/11/1999 12:09'!addBookMenuItemsTo: aMenu hand: aHandMorph	| controlsShowing subMenu |	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'previous page' action: #previousPage.	subMenu add: 'next page' action: #nextPage.	subMenu add: 'insert a page' action: #insertPage.	subMenu add: 'delete this page' action: #deletePage.	controlsShowing _ self hasSubmorphWithProperty: #pageControl.	controlsShowing		ifTrue: [subMenu add: 'hide page controls' action: #hidePageControls]		ifFalse: [subMenu add: 'show page controls' action: #showPageControls].	subMenu addLine.	subMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.	subMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.	subMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.	subMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.	subMenu addLine.	subMenu add: 'sort pages' action: #sortPages:.	subMenu add: 'uncache page sorter' action: #uncachePageSorter.	subMenu addLine.	subMenu add: 'search for text' action: #textSearch.	(aHandMorph classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[subMenu add: 'paste book page'	action: #pasteBookPage].	subMenu add: 'send all pages to server' action: #savePagesOnURL.	subMenu add: 'send this page to server' action: #saveOneOnURL.	subMenu add: 'reload all from server' action: #reload.	subMenu add: 'copy page url to clipboard' action: #copyUrl.	subMenu add: 'keep in one file' action: #keepTogether.	subMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil:		[subMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	aMenu add: 'book...' subMenu: subMenu! !!BookMorph methodsFor: 'menu' stamp: 'tk 1/11/1999 12:13'!copyUrl	"Copy this page's url to the clipboard"	| str |	str _ currentPage url ifNil: [str _ 'Page does not have a url.  Send page to server first.'].	ParagraphEditor new clipboardTextPut: str asText.! !!BookMorph methodsFor: 'menu' stamp: 'tk 1/11/1999 12:16'!invokeBookMenu	"Invoke the book's control panel menu."	| aMenu hand |	aMenu _ MenuMorph new defaultTarget: self.	aMenu addList:	#(			('sort pages'				sortPages)			('uncache page sorter'	uncachePageSorter)			('make bookmark'		bookmarkForThisPage)			('make thumbnail'		thumbnailForThisPage)			('remove control panel'	hidePageControls)		).	aMenu addLine.	aMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.	aMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.	aMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.	aMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.	aMenu addLine.	(self primaryHand classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[aMenu add: 'paste book page'	action: #pasteBookPage].	aMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil: [		aMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	aMenu add: (openToDragNDrop ifTrue: ['close'] ifFalse: ['open']) , ' dragNdrop'			action: #openCloseDragNDrop.	aMenu add: 'search for text' action: #textSearch.	aMenu add: 'send all pages to server' action: #savePagesOnURL.	aMenu add: 'send this page to server' action: #saveOneOnURL.	aMenu add: 'reload all from server' action: #reload.	aMenu add: 'copy page url to clipboard' action: #copyUrl.	aMenu add: 'keep in one file' action: #keepTogether.	hand _ self world primaryHand.	aMenu popUpAt: hand position forHand: hand."	sel _ aMenu invokeAt: self primaryHand position in: self world.	sel ifNotNil: [self perform: sel]."! !!BookMorph methodsFor: 'menu' stamp: 'tk 1/10/1999 11:22'!reload	"Fetch the pages of this book from the server again.  For all pages that have not been modified, keep current ones.  Use new pages.  For each, look up in cache, if time there is equal to time of new, and its in, use the current morph.	Later do fancy things when a page has changed here, and also on the server."	| url onServer onPgs sq which |	(url _ self valueOfProperty: #url) ifNil: ["for .bo index file"		url _ FillInTheBlank 			request: 'url of the place where this book''s index is stored.Must begin with file:// or ftp://' 			initialAnswer: (self getStemUrl, '.bo').		url size > 0 ifTrue: [self setProperty: #url toValue: url]			ifFalse: [^ self]].	onServer _ self class new fromURL: url.	"Later: test book times?"	onPgs _ onServer pages collect: [:out |		sq _ SqueakPageCache pageCache at: out url ifAbsent: [nil].		(sq ~~ nil and: [sq contentsMorph isInMemory])			ifTrue: [((out sqkPage lastChangeTime > sq lastChangeTime) or: 					  [sq contentsMorph == nil]) 						ifTrue: [SqueakPageCache atURL: out url put: out sqkPage.							out]						ifFalse: [sq contentsMorph]]			ifFalse: [SqueakPageCache atURL: out url put: out sqkPage.				out]].	which _ (onPgs findFirst: [:pg | pg url = currentPage url]) max: 1.	self newPages: onPgs currentIndex: which.		"later stay at current page"	self setProperty: #modTime toValue: (onServer valueOfProperty: #modTime).	self setProperty: #allText toValue: (onServer valueOfProperty: #allText).	self setProperty: #allTextUrls toValue: (onServer valueOfProperty: #allTextUrls).! !!URLMorph methodsFor: 'event handling' stamp: 'tk 1/11/1999 11:44'!mouseUp: evt	| pg ow newPage mm ll bookUrl bk |	"If url of a book, bring in book with first page and grab it."	book==true ifTrue: [		ll _ url findLast: [:char | char == $.].		ll = 0 ifTrue: [^ self].		bookUrl _ url copyFrom: 1 to: ll-1.	"remove .sp"		bookUrl _ (bookUrl stemAndNumericSuffix) at: 1.		"remove trailing number"		[bookUrl last == $x] whileTrue: [bookUrl _ bookUrl allButLast].		bk _ BookMorph new fromURL: (bookUrl _ bookUrl, '.bo').			"If this book is already in, we will steal the pages out of it!!!!!!!!"		bk goToPageUrl: url.	"turn to the page"		^ World primaryHand attachMorph: bk].	"If inside a SqueakPage, replace it!!"	pg _ self enclosingPage.	pg ifNotNil: [		(ow _ pg contentsMorph owner) ifNotNil: [			pg contentsMorph delete.	"from its owner"			newPage _ SqueakPageCache atURL: url.			mm _ newPage fetchContents.			mm ifNotNil: [ow addMorph: mm.				page _ newPage].			^ self]].	"If I am a project, jump  -- not done yet"	"For now, just put new page on the hand"	newPage _ SqueakPageCache atURL: url.	mm _ newPage fetchInformIfError.	mm ifNotNil: [self primaryHand attachMorph: mm.		page _ newPage].! !