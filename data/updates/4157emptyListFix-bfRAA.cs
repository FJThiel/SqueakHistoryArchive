'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 12 February 2001 update 3910] on 17 April 2001 at 8:55:06 pm'!"Change Set:		emptyListFix-bfDate:			17 April 2001Author:			Bert FreudenbergFixes a walkback when using cursor keys in an empty list."!!PluggableListMorph methodsFor: 'model access' stamp: 'bf 4/17/2001 20:49'!specialKeyPressed: asciiValue 	| oldSelection nextSelection max howManyItemsShowing |	max _ self maximumSelection.	max > 0 ifFalse: [^ self].	nextSelection _ oldSelection _ self getCurrentSelectionIndex.	asciiValue == 31		ifTrue: 			[" down arrow"			nextSelection _ oldSelection + 1.			nextSelection > max ifTrue: [nextSelection _ 1]].	asciiValue == 30		ifTrue: 			[" up arrow"			nextSelection _ oldSelection - 1.			nextSelection < 1 ifTrue: [nextSelection _ max]].	asciiValue == 1 ifTrue: [" home"		nextSelection _ 1].	asciiValue == 4 ifTrue: [" end"		nextSelection _ max].	howManyItemsShowing _ self numSelectionsInView.	asciiValue == 11 ifTrue: [" page up"		nextSelection _ 1 max: oldSelection - howManyItemsShowing].	asciiValue == 12 ifTrue: [" page down"		nextSelection _ oldSelection + howManyItemsShowing min: max].	model okToChange ifFalse: [^ self].	"No change if model is locked"	oldSelection == nextSelection ifTrue: [^ self flash].	^ self changeModelSelection: nextSelection! !