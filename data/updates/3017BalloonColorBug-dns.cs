'From Squeak2.9alpha of 13 June 2000 [latest update: #2915] on 11 November 2000 at 2:48:13 pm'!"Change Set:		210BalloonColorBug-dnsDate:			11 November 2000Author:			David N. SmithA Balloon is initialized with the color 'Color paleYellow', but this color is never used. Instead, the color is obtained from 'Morph balloonColor' which answers white or 'Color r: 1.0 g: 1.0 b: 0.6' depending on the display depth.Note that paleYellow is' Color r: 1.0 g: 1.0 b: 0.85'.Since the initialized value is never used this bug seems harmless. It's another candidate for most trivial bug report. A fix is attached."!!BalloonMorph methodsFor: 'initialization' stamp: 'sma 11/11/2000 14:44'!initialize	super initialize.	self beSmoothCurve.	color _ self balloonColor.	borderColor _ Color black.	borderWidth _ 1.	offsetFromTarget _ 0@0! !