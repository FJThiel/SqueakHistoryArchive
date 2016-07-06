'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #278] on 31 August 2004 at 11:52:29 pm'!"Change Set:		viewerFeedback-swDate:			31 August 2004Author:			Scott WallaceFixes another in viewer mouseover feedback for the three special casees involving tests that have arguments."!!ViewerLine methodsFor: 'slot' stamp: 'sw 8/31/2004 23:50'!addGetterFeedback
	"Add feedback during mouseover of a getter"

	| aMorph endMorph |	endMorph _		(#(touchesA: #seesColor: #overlaps:) includes: self elementSymbol)			ifTrue:				[submorphs eighth]			ifFalse:				[submorphs sixth].
	aMorph _ RectangleMorph new useRoundedCorners bounds: ((submorphs fourth topLeft - (2@-1)) corner: (endMorph bottomRight + (2@-1))).
	aMorph beTransparent; borderWidth: 2; borderColor: (Color r: 1.0 g: 0.355 b: 0.839); lock.
	aMorph setProperty: #highlight toValue: true.
	ActiveWorld addMorphFront: aMorph

"
Color fromUser (Color r: 1.0 g: 0.355 b: 0.839)
"! !