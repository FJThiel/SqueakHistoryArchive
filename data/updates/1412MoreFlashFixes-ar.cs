'From Squeak 2.5 of August 6, 1999 on 2 September 1999 at 2:38:47 pm'!"Change Set:		MoreFlashFixes-arDate:			2 September 1999Author:			Andreas RaabJust another set of flash fixes"!OrientedFillStyle subclass: #GradientFillStyle	instanceVariableNames: 'colorRamp pixelRamp radial isTranslucent '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-Fills'!!BalloonEngine methodsFor: 'drawing' stamp: 'ar 9/2/1999 14:28'!registerFills: fills	| fillIndexList index fillIndex |	(fills contains:[:any| any notNil and:[any isTranslucent]])		ifTrue:[	self flush.				self reset].	"Check if we need to flush the engine.	We do need to do this if any of the fills is a bitmap fill"	(fills contains:[:any| any notNil and:[any isBitmapFill]])		ifTrue:[	self preFlushIfNeeded.				postFlushNeeded _ true].	fillIndexList _ WordArray new: fills size.	index _ 1.	[index <= fills size] whileTrue:[		fillIndex _ self registerFill: (fills at: index).		fillIndex == nil 			ifTrue:[index _ 1] "Need to start over"			ifFalse:[fillIndexList at: index put: fillIndex.					index _ index+1]	].	^fillIndexList! !!FillStyle methodsFor: 'testing' stamp: 'ar 9/2/1999 14:28'!isTranslucent	^true "Since we don't know better"! !!BitmapFillStyle methodsFor: 'testing' stamp: 'ar 9/2/1999 14:31'!isTranslucent	"Return true since the bitmap may be translucent and we don't really want to check"	^true! !!GradientFillStyle methodsFor: 'accessing' stamp: 'ar 9/2/1999 14:30'!colorRamp: anArray	colorRamp _ anArray.	pixelRamp _ nil.	isTranslucent _ nil.! !!GradientFillStyle methodsFor: 'testing' stamp: 'ar 9/2/1999 14:29'!isTranslucent	^isTranslucent ifNil:[isTranslucent _ self checkTranslucency]! !!GradientFillStyle methodsFor: 'private' stamp: 'ar 9/2/1999 14:30'!checkTranslucency	^colorRamp contains:[:any| any value isTranslucent]! !!InfiniteForm methodsFor: 'fillstyle protocol' stamp: 'ar 9/2/1999 14:32'!isTranslucent	"Return true since the bitmap may be translucent and we don't really want to check"	^true! !!SolidFillStyle methodsFor: 'testing' stamp: 'ar 9/2/1999 14:30'!isTranslucent	^color isTranslucent! !