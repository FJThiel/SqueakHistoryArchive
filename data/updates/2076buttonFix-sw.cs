'From Squeak2.8alpha of 13 January 2000 [latest update: #2075] on 7 May 2000 at 2:13:10 am'!"Change Set:		buttonFix-swDate:			7 May 2000Author:			Scott WallaceRestores the ability to work the optional buttons in a browser even when there are unsubmitted edits."!!Browser methodsFor: 'initialize-release' stamp: 'sw 5/7/2000 02:08'!optionalButtonRow	| aRow aButton |	aRow _ AlignmentMorph newRow.	aRow beSticky.	aRow hResizing: #spaceFill.	aRow setProperty: #clipToOwnerWidth toValue: true.	aRow addTransparentSpacerOfSize: (5@0).	self optionalButtonPairs  do:			[:pair |				aButton _ PluggableButtonMorph					on: self					getState: nil					action: pair second.				aButton useRoundedCorners;					label: pair first asString;					onColor: Color transparent offColor: Color transparent.				aRow addMorphBack: aButton.				aRow addTransparentSpacerOfSize: (3 @ 0)].	^ aRow! !