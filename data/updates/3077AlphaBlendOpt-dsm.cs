'From Squeak2.9alpha of 13 June 2000 [latest update: #2988] on 26 November 2000 at 10:22:29 am'!"Change Set:		258AlphaBlendOpt-dsmDate:			22 November 2000Author:			Duane Maxwell, Paul Phillips/exoboxShort circuits full opaque and full transparent pixels when alpha blending."!!BitBltSimulation methodsFor: 'combination rules' stamp: 'DSM 11/22/2000 13:45'!alphaBlend: sourceWord with: destinationWord	"Blend sourceWord with destinationWord, assuming both are 32-bit pixels.	The source is assumed to have 255*alpha in the high 8 bits of each pixel,	while the high 8 bits of the destinationWord will be ignored.	The blend produced is alpha*source + (1-alpha)*dest, with	the computation being performed independently on each color	component.  The high byte of the result will be 0."	| alpha unAlpha colorMask result blend shift |	self inline: false.	alpha _ sourceWord >> 24.  "High 8 bits of source pixel"	alpha = 0 ifTrue: [ ^ destinationWord ].	alpha = 255 ifTrue: [ ^ sourceWord ].	unAlpha _ 255 - alpha.	colorMask _ 16rFF.	result _ 0.	"ar 9/9/2000 - include alpha in computation"	1 to: 4 do:		[:i | shift _ (i-1)*8.		blend _ (((sourceWord>>shift bitAnd: colorMask) * alpha)					+ ((destinationWord>>shift bitAnd: colorMask) * unAlpha))			 	+ 254 // 255 bitAnd: colorMask.		result _ result bitOr: blend<<shift].	^ result! !!FXBltSimulation methodsFor: 'combination rules' stamp: 'DSM 11/22/2000 13:45'!alphaBlend: sourceWord with: destinationWord	"Blend sourceWord with destinationWord, assuming both are 32-bit pixels.	The source is assumed to have 255*alpha in the high 8 bits of each pixel,	while the high 8 bits of the destinationWord will be ignored.	The blend produced is alpha*source + (1-alpha)*dest, with	the computation being performed independently on each color	component.  The high byte of the result will be 0."	| alpha unAlpha colorMask result blend shift |	self inline: false.	alpha _ sourceWord >> 24.  "High 8 bits of source pixel"	alpha = 0 ifTrue: [ ^ destinationWord ].	alpha = 255 ifTrue: [ ^ sourceWord ].	unAlpha _ 255 - alpha.	colorMask _ 16rFF.	result _ 0.	"ar 9/9/2000 - include alpha in computation"	1 to: 4 do:		[:i | shift _ (i-1)*8.		blend _ (((sourceWord>>shift bitAnd: colorMask) * alpha)					+ ((destinationWord>>shift bitAnd: colorMask) * unAlpha))			 	+ 254 // 255 bitAnd: colorMask.		result _ result bitOr: blend<<shift].	^ result! !