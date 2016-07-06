'From Squeak3.1alpha of 5 February 2001 [latest update: #3538] on 7 February 2001 at 6:47:27 am'!"Change Set:		partsPinTwoDate:			7 February 2001Author:			Bob ArningEnsure that the second coming of the standard parts bin is as well sized as the first."!!Presenter methodsFor: 'palette & parts bin' stamp: 'RAA 2/7/2001 06:44'!newStandardPartsBinTitled: aTitle includeControls: includeControls	| aBook aPage aSize |	aSize _ 360 @ 190.	aBook _ BookMorph new color: Color blue veryMuchLighter.	aBook borderWidth: 0.	aBook removeEverything.	aBook disableDragNDrop.	includeControls ifTrue:		[aBook addMorphBack: (aBook makeMinimalControlsWithColor: Color transparent title: aTitle)].	self classNamesForStandardPartsBin do:		[:aList |			aPage _ self newPageForStandardPartsBin.			aList do:				[:sym | aPage addMorphBack: (Smalltalk at: sym) authoringPrototype].			aPage replaceTallSubmorphsByThumbnails.			aBook insertPage: aPage pageSize: aSize].	self customPagesForPartsBin do:		[:pg | aBook insertPage: pg pageSize: aSize].	self tilesPagesForPartsBin do:		[:pg | aBook insertPage: pg pageSize: aSize].	aBook goToPage: 1.	aBook currentPage addMorphBack: RectangleMorph roundRectPrototype.	aBook fullBounds.		"be sure we are set"	^ aBook! !