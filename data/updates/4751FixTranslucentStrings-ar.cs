'From Squeak3.2gamma of 12 January 2002 [latest update: #4743] on 6 February 2002 at 12:23:25 pm'!"Change Set:		FixTranslucentStrings-arDate:			6 February 2002Author:			Andreas RaabFixes a problem when trying to draw translucent StringMorphs."!!FormCanvas methodsFor: 'drawing-text' stamp: 'ar 2/5/2002 19:03'!drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c	| font |	port colorMap: nil.	font _ fontOrNil ifNil: [TextStyle defaultFont].	port combinationRule: Form paint.	font installOn: port		foregroundColor: (self shadowColor ifNil:[c]) 		backgroundColor: Color transparent.	font displayString: aString on: port 		from: firstIndex to: lastIndex at: (origin + aPoint) kern: 0.! !!FormCanvas methodsFor: 'drawing-text' stamp: 'ar 2/5/2002 19:03'!drawString: aString from: firstIndex to: lastIndex in: bounds font: fontOrNil color: c	| font portRect |	port colorMap: nil.	portRect _ port clipRect.	port clipByX1: bounds left + origin x 		y1: bounds top + origin y 		x2: bounds right + origin x 		y2: bounds bottom + origin y.	font _ fontOrNil ifNil: [TextStyle defaultFont].	port combinationRule: Form paint.	font installOn: port		foregroundColor: (self shadowColor ifNil:[c]) 		backgroundColor: Color transparent.	font displayString: aString asString on: port 		from: firstIndex to: lastIndex at: (bounds topLeft + origin) kern: 0.	port clipRect: portRect.! !!GrafPort methodsFor: 'private' stamp: 'ar 2/5/2002 19:03'!installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor	super installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor.	alpha _ foregroundColor privateAlpha.	alpha = 255 ifFalse:[		combinationRule = Form paint			ifTrue:[combinationRule _ 31]			ifFalse:[combinationRule _ 30].	].! !