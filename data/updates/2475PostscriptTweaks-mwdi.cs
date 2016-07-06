'From Squeak2.9alpha of 12 June 2000 [latest update: #2473] on 7 August 2000 at 10:09:10 am'!"Change Set:		PostscriptTweaksDate:			7 August 2000Author:			Marcel Weiher, Dan IngallsA few tweaks necessary for printing bookmorphs in portrait and landscape mode with proper scaling and position on the page."!Canvas subclass: #PostscriptCanvas	instanceVariableNames: 'origin clipRect currentColor currentFont morphLevel gstateStack fontMap usedFonts psBounds topLevelMorph initialScale '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Postscript Canvases'!!BookMorph methodsFor: 'initialization' stamp: 'di 8/7/2000 09:11'!setInitialState	orientation _ #vertical.	centering _ #topLeft.	hResizing _ #shrinkWrap.	vResizing _ #shrinkWrap.	inset _ 5.	color _ Color white.	pageSize _ 1060@800.	self enableDragNDrop! !!PostscriptCanvas methodsFor: 'initialization' stamp: 'mpw 8/7/2000 09:09'!bounds:newBounds	psBounds _ newBounds.! !!PostscriptCanvas methodsFor: 'initialization' stamp: 'di 8/5/2000 22:22'!reset	super reset.	origin _ 0@0.							"origin of the top-left corner of this cavas"	clipRect _ (0@0 corner: 10000@10000).		"default clipping rectangle"	morphLevel _ 0.	gstateStack _ OrderedCollection new.	self initializeFontMap.	usedFonts _ Set new.     initialScale _ 1.0! !!PostscriptCanvas methodsFor: 'initialization' stamp: 'mpw 8/7/2000 09:11'!writeSetupForRect:aRect	target print:'% psBounds origin'; cr.	target translate: psBounds origin.	target print:'% flip'; cr.	target translate: 0 @ aRect extent y;		scale:initialScale @ initialScale negated;		print:' [ {true setstrokeadjust} stopped ] pop[ currenttransfer /exec cvx 1.2 /exp cvx ] cvx bind  settransfer'; cr.! !!PostscriptCanvas methodsFor: 'misc canvas' stamp: 'mpw 8/6/2000 13:36'!contentsOfArea: aRectangle into: aForm	"not supported for PS canvas"! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'di 8/3/2000 14:18'!defaultMargin  "In Points"	^ (0.25 * 72) asInteger.! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'di 8/5/2000 22:56'!defaultPageSize	" This is Letter size in points.  European A4 is 595 @ 842 "	^ 0 @ 0 extent: ((8.5 @ 11.0) * 72) asIntegerPoint.! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'mpw 8/7/2000 08:46'!pageBBox	| pageSize offset bbox |	pageSize _ self defaultImageableArea.	offset _ (pageSize extent x - psBounds extent x) / 2 @ 			((pageSize extent y - psBounds extent y) /2 ).	offset _ offset max: 0@0.	offset _ offset + self defaultMargin.	bbox _ offset extent:psBounds extent.	^bbox! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'mpw 8/7/2000 08:34'!pageOffset	|  offset  |	offset _ self pageBBox origin.	^ (offset x @ offset y).! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'mpw 8/7/2000 08:31'!setupGStateForMorph:aMorph	" position the morph on the page  "	morphLevel == 2 ifTrue:[ 		target print:'% pageOffset'; cr.		target translate:self pageOffset.		self writeSetupForRect:aMorph bounds.		target print:'% negate morph offset';cr.		target translate:aMorph bounds origin negated.	].! !!DSCPostscriptCanvas methodsFor: 'as yet unclassified' stamp: 'di 8/7/2000 09:26'!writePSIdentifierRotated: rotateFlag	| morphExtent pageExtent scaledBox |	target	print:'%!!PS-Adobe-2.0'; cr; 			print:'%%Pages: (atend)'; cr.	"Define initialScale so that the morph will fit the page rotated or not"	morphExtent _ rotateFlag ifTrue: [psBounds extent transposed]							ifFalse: [psBounds extent].	pageExtent _ self defaultImageableArea extent asFloatPoint.	initialScale _ pageExtent x/morphExtent x min: pageExtent y/morphExtent y.	target print:'% initialScale: '; write:initialScale; cr.	scaledBox _ self pageBBox rounded.	rotateFlag		ifTrue: [					target print: '%%BoundingBox: '; write: scaledBox; cr;					print: '90 rotate'; cr;					write: self defaultMargin; space; write: (self defaultMargin + scaledBox height) negated; print: ' translate'; cr]		ifFalse: [target print: '%%BoundingBox: '; write: scaledBox rounded; cr].	target print:'%%EndComments'; cr.! !!EPSCanvas methodsFor: 'as yet unclassified' stamp: 'mpw 8/6/2000 14:28'!setupGStateForMorph:aMorph.	target comment:'setupGState in EPSCanvas'.	morphLevel == 1 ifTrue:[ 		target translate:aMorph bounds origin negated.		self writeSetupForRect:aMorph bounds.	].! !