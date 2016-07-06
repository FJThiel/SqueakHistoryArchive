'From Squeak3.7beta of ''1 April 2004'' [latest update: #5905] on 1 May 2004 at 1:09:38 pm'!"Change Set:		BalloonCanvasImageToDisplayFix-nkDate:			1 May 2004Author:			Ned KonzI caught this by accident; if you have a BalloonCanvas on a different form than the Display, the old code would write to the Display instead of your form.Also fixes a problem seen with framed rectangles and nil fillColors."!!BalloonCanvas methodsFor: 'drawing' stamp: 'nk 5/1/2004 12:25'!frameRectangle: r width: w color: c	"Draw a frame around the given rectangle"	^self frameAndFillRectangle: r			fillColor: Color transparent			borderWidth: w			borderColor: c! !!BalloonCanvas methodsFor: 'private' stamp: 'nk 5/1/2004 12:54'!image: aForm at: aPoint sourceRect: sourceRect rule: rule	| warp dstRect srcQuad dstOffset center |	(self ifNoTransformWithIn: sourceRect) & false		ifTrue:[^super image: aForm at: aPoint sourceRect: sourceRect rule: rule].	dstRect _ (transform localBoundsToGlobal: (aForm boundingBox translateBy: aPoint)).	dstOffset _ 0@0. "dstRect origin."	"dstRect _ 0@0 corner: dstRect extent."	center _ 0@0."transform globalPointToLocal: dstRect origin."	srcQuad _ transform globalPointsToLocal: (dstRect innerCorners).	srcQuad _ srcQuad collect:[:pt| pt - aPoint].	warp _ (WarpBlt current toForm: form)			sourceForm: aForm;			cellSize: 2;  "installs a new colormap if cellSize > 1"			combinationRule: Form over.	warp copyQuad: srcQuad toRect: (dstRect translateBy: dstOffset).	self frameRectangle: (aForm boundingBox translateBy: aPoint) color: Color green.	"... TODO ... create a bitmap fill style from the form and use it for a simple rectangle."! !