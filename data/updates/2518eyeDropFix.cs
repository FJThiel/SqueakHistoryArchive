'From Squeak2.9alpha of 17 July 2000 [latest update: #2504] on 17 August 2000 at 5:09:08 pm'!"Change Set:		eyeDropFixDate:			17 August 2000Author:			Bob Arning- fixed recent glitch were eyedropper was not propagating new color to hand"!!PaintBoxMorph methodsFor: 'actions' stamp: 'RAA 8/17/2000 17:06'!currentColor: aColor evt: evt	"Accept a color from the outside.  (my colorMemoryMorph must call takeColorEvt: evt from: colorPicker instead)"	currentColor _ aColor.	colorMemory currentColor: aColor.	self notifyWeakDependentsWith: {#currentColor. evt. currentColor}.	self showColor.	self colorable ifFalse: [self setAction: #paint: evt: evt].	"User now thinking of painting"! !