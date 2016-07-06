'From Squeak2.9alpha of 13 June 2000 [latest update: #2995] on 10 November 2000 at 5:30:26 pm'!"Change Set:		BookSorterFixDate:			10 November 2000Author:			Andreas RaabFixes BookSorter weirdness and also an issue in #shrinkWrap morphs with no children. Some more issues are left open but I suspect that they relate much more to when the layout is computed than anything else (and I will look into these issues later on)"!!LayoutCell methodsFor: 'accessing' stamp: 'ar 11/10/2000 17:09'!extraSpace	^extraSpace ifNil:[0@0]! !!Morph methodsFor: 'layout' stamp: 'ar 11/10/2000 17:06'!minExtent	"Layout specific. Return the minimum size the receiver can be represented in.	Implementation note: When this message is sent from an owner trying to lay out its children it will traverse down the morph tree and recompute the minimal arrangement of the morphs based on which the minimal extent is returned. When a morph with some layout strategy is encountered, the morph will ask its strategy to compute the new arrangement. However, since the final size given to the receiver is unknown at the point of the query, the assumption is made that the current bounds of the receiver are the base on which the layout should be computed. This scheme prevents strange layout changes when for instance, a table is contained in another table. Unless the inner table has been resized manually (which means its bounds are already enlarged) the arrangement of the inner table will not change here. Thus the entire layout computation is basically an iterative process which may have different results depending on the incremental changes applied."	| layout minExtent extra hFit vFit |self flag: #arNote. "take a clipping receiver into account..."	hFit _ self hResizing.	vFit _ self vResizing.	"optimize for #rigid receiver (doesn't require layout computation)"	(hFit == #rigid and:[vFit == #rigid]) 		ifTrue:[^self fullBounds extent].	"An exception -- a receiver with #shrinkWrap constraints but no children is being treated #rigid (the equivalent to a #spaceFill receiver in a non-layouting owner)"	self hasSubmorphs ifFalse:[		hFit == #shrinkWrap ifTrue:[hFit _ #rigid].		vFit == #shrinkWrap ifTrue:[vFit _ #rigid]].	layout _ self layoutPolicy.	layout == nil		ifTrue:[minExtent _ 0@0]		ifFalse:[minExtent _ layout minExtentOf: self in: self layoutBounds].	hFit == #rigid		ifTrue:[	minExtent _ self fullBounds extent x @ minExtent y]		ifFalse:[	extra _ self bounds width - self layoutBounds width.				minExtent _ (minExtent x + extra) @ minExtent y].	vFit == #rigid		ifTrue:[minExtent _ minExtent x @ self fullBounds extent y]		ifFalse:[extra _ self bounds height - self layoutBounds height.				minExtent _ minExtent x @ (minExtent y + extra)].	minExtent _ minExtent max: (self minWidth@self minHeight).	^minExtent! !!BookPageSorterMorph methodsFor: 'as yet unclassified' stamp: 'ar 11/10/2000 16:45'!initialize	super initialize.	self extent: Display extent - 100;		listDirection: #topToBottom;		wrapCentering: #topLeft;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		layoutInset: 3;		color: Color lightGray;		borderWidth: 2.	pageHolder _ PasteUpMorph new behaveLikeHolder extent: self extent - borderWidth.	pageHolder hResizing: #shrinkWrap.	pageHolder cursor: 0.	self addControls.	self addMorphBack: pageHolder.! !!TableLayout methodsFor: 'layout' stamp: 'ar 11/10/2000 17:14'!computeCellArrangement: cellHolder in: newBounds horizontal: aBool target: aMorph	"Compute number of cells we can put in each row/column. The returned array contains a list of all the cells we can put into the row/column at each level.	Note: The arrangement is so that the 'x' value of each cell advances along the list direction and the 'y' value along the wrap direction. The returned arrangement has an extra cell at the start describing the width and height of the row."	| cells wrap spacing output maxExtent n sum index max cell first last w cellMax maxCell hFill vFill inset |	maxCell _ cellHolder key.	cells _ cellHolder value.	aMorph wrapDirection == #none 		ifTrue:[wrap _ SmallInteger maxVal]		ifFalse:[wrap _ aBool ifTrue:[newBounds width] ifFalse:[newBounds height].				wrap < maxCell x ifTrue:[wrap _ maxCell x]].	spacing _ aMorph cellSpacing.	(spacing == #globalRect or:[spacing = #globalSquare]) ifTrue:[		"Globally equal spacing is a very special case here, so get out fast and easy"		^self computeGlobalCellArrangement: cells 			in: newBounds horizontal: aBool 			wrap: wrap spacing: spacing].	output _ (WriteStream on: Array new).	inset _ aMorph cellInset asPoint.	aBool ifFalse:[inset _ inset transposed].	first _ last _ nil.	maxExtent _ 0@0.	sum _ 0.	index _ 1.	n _ 0.	hFill _ vFill _ false.	[index <= cells size] whileTrue:[		w _ sum.		cell _ cells at: index.		cellMax _ maxExtent max: cell cellSize. "e.g., minSize"		(spacing == #localRect or:[spacing == #localSquare]) ifTrue:[			"Recompute entire size of current row"			spacing == #localSquare 				ifTrue:[max _ cellMax x max: cellMax y]				ifFalse:[max _ cellMax x].			sum _ (n + 1) * max.		] ifFalse:[			sum _ sum + (cell cellSize x).		].		((sum + (n * inset x)) > wrap and:[first notNil]) ifTrue:[			"It doesn't fit and we're not starting a new line"			(spacing == #localSquare or:[spacing == #localRect]) ifTrue:[				spacing == #localSquare 					ifTrue:[maxExtent _ (maxExtent x max: maxExtent y) asPoint].				first do:[:c| c cellSize: maxExtent]].			w _ w + ((n - 1) * inset x).			"redistribute extra space"			first nextCell ifNotNil:[first nextCell do:[:c| c addExtraSpace: inset x@0]].			last _ LayoutCell new.			last cellSize: w @ (maxExtent y).			last hSpaceFill: hFill.			last vSpaceFill: vFill.			last nextCell: first.			output position = 0 ifFalse:[last addExtraSpace: 0@inset y].			output nextPut: last.			first _ nil.			maxExtent _ 0@0.			sum _ 0.			n _ 0.			hFill _ vFill _ false.		] ifFalse:[			"It did fit; use next item from input"			first ifNil:[first _ last _ cell] ifNotNil:[last nextCell: cell. last _ cell].			index _ index+1.			n _ n + 1.			maxExtent _ cellMax.			hFill _ hFill or:[cell hSpaceFill].			vFill _ vFill or:[cell vSpaceFill].		].	].	first ifNotNil:[		last _ LayoutCell new.		sum _ sum + ((n - 1) * inset x).		first nextCell ifNotNil:[first nextCell do:[:c| c addExtraSpace: inset x@0]].		last cellSize: sum @ maxExtent y.		last hSpaceFill: hFill.		last vSpaceFill: vFill.		last nextCell: first.		output position = 0 ifFalse:[last addExtraSpace: 0@inset y].		output nextPut: last].	output _ output contents.	aMorph listSpacing == #equal ifTrue:[		"Make all the heights equal"		max _ output inject: 0 into:[:size :c| size max: c cellSize y].		output do:[:c| c cellSize: c cellSize x @ max].	].	^output! !!TableLayout methodsFor: 'layout' stamp: 'ar 11/10/2000 17:10'!minExtentOf: aMorph in: box	"Return the minimal size aMorph's children would require given the new bounds"	| cells arrangement horizontal newBounds minX minY minExtent |	aMorph hasSubmorphs ifFalse:[^0@0].	newBounds _ box origin asIntegerPoint corner: (box corner asIntegerPoint).	(aMorph listDirection == #topToBottom or:[aMorph listDirection == #bottomToTop])		ifTrue:[	horizontal _ false]		ifFalse:[	horizontal _ true].	"Step 1: Compute the minimum extent for all the children of aMorph"	cells _ self computeCellSizes: aMorph 				in: (0@0 corner: newBounds extent) 				horizontal: horizontal.	"Step 2: Compute the arrangement of the cells for each row and column"	arrangement _ self computeCellArrangement: cells 						in: newBounds 						horizontal: horizontal						target: aMorph.	"Step 3: Extract the minimum size out of the arrangement"	minX _ minY _ 0.	arrangement do:[:cell|		minX _ minX max: cell cellSize x + cell extraSpace x.		minY _ minY + cell cellSize y + cell extraSpace y].	horizontal 		ifTrue:[minExtent _ minX@minY]		ifFalse:[minExtent _ minY@minX].	^minExtent! !