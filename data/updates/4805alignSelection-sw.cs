'From Squeak3.3alpha of 12 January 2002 [latest update: #4804] on 19 March 2002 at 11:26:03 pm'!"Change Set:		alignSelection-swDate:			20 March 2002Author:			Scott WallaceAdds six alignment commands to SelectionMorph's halo manu, allowing groups of morphs to be statically aligned (relative to each other) in various ways."!!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/19/2002 22:55'!addCustomMenuItems: aMenu hand: aHandMorph	"Add custom menu items to the menu"	super addCustomMenuItems: aMenu hand: aHandMorph.	aMenu addLine.	aMenu add: 'add or remove items' target: self selector: #addOrRemoveItems: argument: aHandMorph.	aMenu addList: #(		-		('place into a row' organizeIntoRow)		('place into a column' organizeIntoColumn)		-		('align left edges' alignLeftEdges)		('align top edges' alignTopEdges)		('align right edges' alignRightEdges)		('align bottom edges' alignBottomEdges)		-		('align centers vertically' alignCentersVertically)		('align centers horizontally' alignCentersHorizontally))! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/11/2002 18:32'!alignBottomEdges	"Make the bottom coordinate of all my elements be the same"	| maxBottom |	maxBottom _ (selectedItems collect: [:itm | itm bottom]) max.	selectedItems do:		[:itm | itm bottom: maxBottom]! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/19/2002 22:50'!alignCentersHorizontally	"Make every morph in the selection have the same vertical center as the topmost item."	| minLeft leftMost |	selectedItems size > 1 ifFalse: [^ self].	minLeft _ (selectedItems collect: [:itm | itm left]) min.	leftMost _ selectedItems detect: [:m | m left = minLeft].	selectedItems do:		[:itm | itm center: (itm center x @ leftMost center y)]! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/19/2002 22:48'!alignCentersVertically	"Make every morph in the selection have the same horizontal center as the topmost item."	| minTop topMost |	selectedItems size > 1 ifFalse: [^ self].	minTop _ (selectedItems collect: [:itm | itm top]) min.	topMost _ selectedItems detect: [:m | m top = minTop].	selectedItems do:		[:itm | itm center: (topMost center x @ itm center y)]! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/11/2002 18:28'!alignLeftEdges	"Make the left coordinate of all my elements be the same"	| minLeft |	minLeft _ (selectedItems collect: [:itm | itm left]) min.	selectedItems do:		[:itm | itm left: minLeft]! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/11/2002 18:31'!alignRightEdges	"Make the right coordinate of all my elements be the same"	| maxRight |	maxRight _ (selectedItems collect: [:itm | itm right]) max.	selectedItems do:		[:itm | itm right: maxRight]! !!SelectionMorph methodsFor: 'halo commands' stamp: 'sw 3/11/2002 18:32'!alignTopEdges	"Make the top coordinate of all my elements be the same"	| minTop |	minTop _ (selectedItems collect: [:itm | itm top]) min.	selectedItems do:		[:itm | itm top: minTop]! !