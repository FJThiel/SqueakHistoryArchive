'From Squeak2.9alpha of 17 July 2000 [latest update: #3002] on 11 November 2000 at 11:35:31 pm'!"Change Set:		unbookingDate:			11 November 2000Author:			Bob Arning- thread editing goes directly to page sorter- thread name displayed at bottom ala PVM"!Morph subclass: #SquishedNameMorph	instanceVariableNames: 'target getSelector setSelector '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Navigators'!!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 22:58'!acceptSortedContentsFrom: aHolder	"Update my page list from the given page sorter."	| nameOfThisProject |	listOfPages _ OrderedCollection new.	aHolder submorphs doWithIndex: [:m :i |		(nameOfThisProject _ m valueOfProperty: #nameOfThisProject) ifNotNil: [			listOfPages add: {nameOfThisProject}.		].	].	self class know: listOfPages as: threadName.	self removeAllMorphs; addButtons.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:32'!addButtonsAlan	| marginPt i sz data images pageNumber proj f m b1 b2 dot arrowWidth arrowCenter vertices arrowHeight allProjects nameMorph |	allProjects _ Project allProjects.	self changeNoLayout.	self hResizing: #rigid. "... this is very unlikely..."	self vResizing: #rigid.	marginPt _ 4@4.	i _ self currentIndex.	sz _ 50@40.	arrowWidth _ 14.	arrowHeight _ 17.	data _ {		{i - 1. 'Previous:'. #previousPage. #rightCenter. arrowWidth negated}.		{i + 1. 'Next:'. #nextPage. #leftCenter. arrowWidth}	}.	images _ data collect: [ :tuple |		pageNumber _ tuple first.		((pageNumber between: 1 and: listOfPages size) and: 				[(proj _ Project named: (listOfPages at: pageNumber) first in: allProjects) notNil]) ifTrue: [			f _ proj thumbnail scaledToSize: sz.			arrowCenter _ f boundingBox perform: tuple fourth.			vertices _ {				arrowCenter - (0@arrowHeight).				arrowCenter + (0@arrowHeight).				arrowCenter + (tuple fifth @ 0).			}.			f getCanvas				drawPolygon: vertices 				color: Color orange 				borderWidth: 0 				borderColor: Color transparent.			m _ ImageMorph new image: f.			m setBalloonText: tuple second,' ',(listOfPages at: pageNumber) first.			m on: #mouseUp send: tuple third to: self.		] ifFalse: [			f _ (Form extent: sz depth: 16) fillColor: Color lightGray.			m _ ImageMorph new image: f.		].		m	].	b1 _ images first.	b2 _ images second.	dot _ EllipseMorph new extent: 16@16; color: Color orange lighter; borderWidth: 0.	self addMorph: (b1 position: self position + marginPt).	self addMorph: (b2 position: b1 topRight + (marginPt x @ 0)).	self extent: b2 bottomRight - self position + marginPt.	self addMorph: dot.	dot align: dot center with: b1 bounds rightCenter + ((marginPt x @ 0) // 2).	dot setBalloonText: threadName,'more commands'.	dot on: #mouseUp send: #moreCommands to: self.	self fullBounds.	self addMorph: (nameMorph _ SquishedNameMorph new).	nameMorph		target: self getSelector: #threadName setSelector: nil;		color: Color transparent;		width: self width;		height: 15;		align: nameMorph bottomLeft with: self bottomLeft.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:09'!editThisThread	| sz morphsForPageSorter pvm sorter allProjects |	sz _ (100@80 / (listOfPages size / 35 max: 1)) rounded.	allProjects _ Project allProjects.	'Assembling thumbnail images...'		displayProgressAt: self cursorPoint		from: 0 to: listOfPages size		during: [:bar |			morphsForPageSorter _ listOfPages withIndexCollect: [ :each :index | 				bar value: index.				pvm _ ProjectViewMorph on: (Project named: each first in: allProjects).				pvm _ pvm imageForm scaledToSize: sz.				pvm _ pvm asMorph.				pvm setProperty: #nameOfThisProject toValue: each first.				pvm setBalloonText: each first.				pvm			].		].	sorter _ BookPageSorterMorph new		book: self 		morphsToSort: morphsForPageSorter.	sorter pageHolder cursor: self currentIndex.	self world addMorphFront: sorter.	sorter align: sorter center with: self world center.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 22:42'!editThisThreadOLD	| projects |	projects _ listOfPages collect: [ :each | Project named: each first].	BookMorph 		makeBookOfProjects: projects		named: threadName.	self delete.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:27'!initialize	super initialize.	! !!Project class methodsFor: 'utilities' stamp: 'RAA 11/11/2000 23:05'!named: projName in: aListOfProjects	"Answer the project with the given name, or nil if there is no project of that given name."	"Use given collection for speed until we get faster #allProjects"	^ aListOfProjects		detect: [:proj | proj name = projName]		ifNone: [nil]! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:19'!colorAroundName	^Color gray: 0.8! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:17'!drawOn: aCanvas	| font stringToShow nameForm rectForName |	super drawOn: aCanvas.	self isEditingName ifTrue: [^self].	font _ self fontForName.	stringToShow _ self stringToShow.	nameForm _ (StringMorph contents: stringToShow font: font) imageForm.	nameForm _ nameForm scaledToSize: (self extent - (4@2) min: nameForm extent).	rectForName _ self bottomLeft + 			(self width - nameForm width // 2 @ (nameForm height + 2) negated)				extent: nameForm extent.	rectForName topLeft eightNeighbors do: [ :pt |		aCanvas			stencil: nameForm 			at: pt			color: self colorAroundName.	].	aCanvas		stencil: nameForm 		at: rectForName topLeft 		color: Color black.	! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:18'!fontForName	| pickem |	pickem _ 3.	pickem = 1 ifTrue: [		^(((TextStyle named: #Helvetica) ifNil: [TextStyle default]) fontOfSize: 13) emphasized: 1.	].	pickem = 2 ifTrue: [		^(((TextStyle named: #Palatino) ifNil: [TextStyle default]) fontOfSize: 12) emphasized: 1.	].	^((TextStyle default) fontAt: 1) emphasized: 1! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:17'!isEditingName	^((self findA: UpdatingStringMorph) ifNil: [^false]) hasFocus! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:32'!stringToShow	(target isNil or: [getSelector isNil]) ifTrue: [^'????'].	^target perform: getSelector! !!SquishedNameMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 23:31'!target: aTarget getSelector: symbol1 setSelector: symbol2	target _ aTarget.	getSelector _ symbol1.	setSelector _ symbol2.! !