'From Squeak3.2gamma of 15 January 2002 [latest update: #4743] on 19 February 2002 at 9:45:48 pm'!"Change Set:		fixMatrixTfm-mas.cs-masDate:			19 February 2002Author:			Mark SchwenkIn a 3.2 gamma image with the latest updates or a 3.3 alpha image, clicking on the following expression in the 'Welcome to...' window causes a walkback:(FlashMorphReader on: (HTTPSocket		httpGet: 'http://www.audi.co.uk/flash/intro1.swf' 		accept:'application/x-shockwave-flash'))	processFile startPlaying openInWorld.This change set fixes the problem by reverting the MatrixTransformMorph>>transform method which was changed by 4713TTSampleStrRotFix-mdrI originally reported this bug on Feb. 7."!!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 9/11/2000 21:16'!transform	^ transform ifNil: [MatrixTransform2x3 identity]! !