'From Squeak 2.3 beta of Nov 25, 1998 on 5 January 1999 at 2:29:49 pm'!!BookMorph methodsFor: 'initialization' stamp: 'tk 1/5/1999 13:49'!fromRemoteStream: strm	"Make a book from an index and a bunch of pages on a server.  NOT showing any page!!"	| remote dict |	remote _ strm fileInObjectAndCode.	self initialize.	pages _ OrderedCollection new.	2 to: remote size do: [:ii | pages add: (remote at: ii)].	currentPage fullReleaseCachedState; delete.	"the blank one"	currentPage _ remote at: 2.	dict _ remote at: 1.	self setProperty: #modTime toValue: (dict at: #modTime).	dict at: #allText ifPresent: [:val |		self setProperty: #allText toValue: val].	dict at: #allTextUrls ifPresent: [:val |		self setProperty: #allTextUrls toValue: val].	#(color borderWidth borderColor pageSize) with: 		#(color: borderWidth: borderColor: pageSize:) do: [:key :sel |			dict at: key ifPresent: [:val | 			 	self perform: sel with: val]].	^ self! !!BookMorph methodsFor: 'menu' stamp: 'tk 1/5/1999 13:48'!saveIndexOnURL	"Make up an index to the pages of this book, with thumbnails, and store it on the server.  (aDictionary, aMorphObjectOut, aMorphObjectOut, aMorphObjectOut).  The last part corresponds exactly to what pages looks like when they are all out.  Each holds onto a SqueakPage, which holds a url and a thumbnail."	| dict list pg holder mine sf remoteFile |	pages size = 0 ifTrue: [^ self].	dict _ Dictionary new.  dict at: #modTime put: Time totalSeconds.	"self getAllText MUST have been called at start of this operation."	dict at: #allText put: (self valueOfProperty: #allText).	dict at: #allTextUrls put: (self valueOfProperty: #allTextUrls).	#(color borderWidth borderColor pageSize) do: [:sel |		dict at: sel put: (self perform: sel)].	list _ pages copy.	"paste dict on front below"	"Fix up the entries, should already be done"	list doWithIndex: [:out :ind |		out isInMemory ifTrue: [  			(pg _ out valueOfProperty: #SqueakPage) ifNil: [				out saveOnURLbasic].			pg _ (out valueOfProperty: #SqueakPage) copy.			holder _ MorphObjectOut new xxxSetUrl: pg url page: pg.			pg contentsMorph: holder.			list at: ind put: holder]].	list _ (Array with: dict), list.	mine _ self valueOfProperty: #url.	mine ifNil: [mine _ self getStemUrl, '.bo'.		self setProperty: #url toValue: mine].	sf _ ServerDirectory new fullPath: mine.	Cursor wait showWhile: [		remoteFile _ sf fileNamed: mine.		remoteFile fileOutClass: nil andObject: list.		remoteFile close].! !!SimpleButtonMorph methodsFor: 'copying' stamp: 'tk 1/4/1999 18:21'!veryDeepFixupWith: deepCopier	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"target _ deepCopier references at: target ifAbsent: [target].arguments _ arguments collect: [:each |	deepCopier references at: each ifAbsent: [each]].! !!URLMorph commentStamp: 'tk 1/5/1999 10:43' prior: 0!This morph represents a URL for a SqueakPage. It displays the thumbnail for the associated page, if available. Used in page sorters and for bookmarks.This morph has several options:  a. It can act like a thumbnail for sorting (in which case it can be picked up and dragged) or it acts as a bookmark (in which case clicking on it activates it).  b. If it has book set to true, it is a page in a book.  Clicking fetches the index of the book, opens it to the first page, and puts it in the hand.A thumbnail on a known book:	(URLMorph grabURL: 'ftp://doltest1.disney.com/squeak/test/p1.sp')		book: true.A thumbnail ona single PasteUpMorph:Make a PasteUpMorph with any morphs in it.Decide where it should live, make a url string, and copy it.	'file://HardDisk/books/book1/myPage.sp'	'file://Ted''s/books/tryThis/p1.sp'	'ftp://doltest1.disney.com/squeak/test/p1.sp'Choose 'Save as Web Morph'Paste in the url.Drop the resulting thumbnail into some morph.See SqueakPage's comment for the stages of in/out.!!URLMorph methodsFor: 'drawing' stamp: 'tk 1/5/1999 10:57'!drawOn: aCanvas	"Draw thumbnail for my page, if it is available. Otherwise, just draw a rectangle." 	| thumbnail oldExt |	thumbnail _ self thumbnailOrNil.	thumbnail		ifNil: [aCanvas frameRectangle: bounds width: 1 color: Color blue.			aCanvas fillRectangle: (bounds insetBy: 1) color: color]		ifNotNil: [oldExt _ bounds extent.			bounds _ bounds origin extent: thumbnail extent + (2@2).			aCanvas frameRectangle: bounds width: 1 color: Color blue.			aCanvas image: thumbnail at: bounds origin + 1.			oldExt = thumbnail extent ifFalse: [self layoutChanged]].! !!URLMorph methodsFor: 'event handling' stamp: 'tk 1/5/1999 10:34'!mouseUp: evt	| pg ow newPage mm ll bookUrl |	"If url of a book, bring in book with first page and grab it."	book==true ifTrue: [		ll _ url findLast: [:char | char == $.].		ll = 0 ifTrue: [^ self].		bookUrl _ url copyFrom: 1 to: ll-1.	"remove .sp"		bookUrl _ (bookUrl stemAndNumericSuffix) at: 1.		"remove trailing number"		[bookUrl last == $x] whileTrue: [bookUrl _ bookUrl allButLast].		^ BookMorph grabURL: (bookUrl _ bookUrl, '.bo')].	"If inside a SqueakPage, replace it!!"	pg _ self enclosingPage.	pg ifNotNil: [		(ow _ pg contentsMorph owner) ifNotNil: [			pg contentsMorph delete.	"from its owner"			newPage _ SqueakPageCache atURL: url.			mm _ newPage fetchContents.			mm ifNotNil: [ow addMorph: mm.				page _ newPage].			^ self]].	"If I am a project, jump  -- not done yet"	"For now, just put new page on the hand"	newPage _ SqueakPageCache atURL: url.	mm _ newPage fetchInformIfError.	mm ifNotNil: [self primaryHand attachMorph: mm.		page _ newPage].! !!URLMorph class methodsFor: 'instance creation' stamp: 'tk 1/5/1999 10:40'!grabURL: aURLString	"Create a URLMorph for this url.  Drop it and click it to get the SqueakPage."	| um |	World primaryHand attachMorph: 		((um _ self new) isBookmark: true; setURL: aURLString page: nil).	^ um! !