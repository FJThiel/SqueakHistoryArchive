'From Squeak2.9alpha of 15 June 2000 [latest update: #2471] on 5 August 2000 at 1:21:31 pm'!"Change Set:		appendCat-swDate:			5 August 2000Author:			Scott WallaceAs per request from Open School, when addition of a script or instance variable brings about the reformulation of an existing Viewer, the new category appears in addition to any already present, rather than replacing an existing category.   If it gets newly added, it is inserted at the top of the Viewer"!!Presenter methodsFor: 'viewer' stamp: 'sw 8/4/2000 14:59'!updateViewer: aViewer forceToShow: aCategory	| aPlayer aPosition newViewer oldOwner wasSticky barHeight cats |	cats _ aViewer categoriesCurrentlyShowing asOrderedCollection.	(cats includes: aCategory) ifFalse: [cats addFirst: aCategory].	aPlayer _ aViewer scriptedPlayer.	aPosition _ aViewer position.	wasSticky _ aViewer isSticky.	newViewer _ aViewer species new visible: false.	barHeight _ aViewer submorphs first orientation == #vertical		ifTrue:			[aViewer submorphs first submorphs first height]		ifFalse:			[0].	newViewer initializeFor: aPlayer barHeight: barHeight includeDismissButton: aViewer hasDismissButton showCategories: cats.	wasSticky ifTrue: [newViewer beSticky].	oldOwner _ aViewer owner.	oldOwner ifNotNil:		[oldOwner replaceSubmorph: aViewer by: newViewer].		"It has happened that old readouts are still on steplist.  We may see again!!"	newViewer position: aPosition.	newViewer enforceTileColorPolicy.	newViewer visible: true.	newViewer world startSteppingSubmorphsOf: newViewer.	newViewer layoutChanged! !!StandardViewer methodsFor: 'as yet unclassified' stamp: 'sw 8/4/2000 13:02'!initializeFor: aPlayer barHeight: anInteger includeDismissButton: aBoolean	self initializeFor: aPlayer barHeight: anInteger includeDismissButton: aBoolean showCategories: nil! !!StandardViewer methodsFor: 'as yet unclassified' stamp: 'sw 8/4/2000 13:02'!initializeFor: aPlayer barHeight: anInteger includeDismissButton: aBoolean showCategories: cats	scriptedPlayer _ aPlayer.	self orientation: #vertical;		hResizing: #spaceFill;		vResizing: #shrinkWrap;		borderWidth: 1.	self color: self standardViewerColor.	self addHeaderMorphWithBarHeight: anInteger includeDismissButton: aBoolean.	cats isEmptyOrNil		ifFalse:			[cats do:				[:aCat | self addCategoryViewerFor: aCat]]		ifTrue:			[self addCategoryViewer.			self addCategoryViewer].! !