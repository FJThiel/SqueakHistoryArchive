'From Squeak 2.3 of January 14, 1999 on 11 February 1999 at 8:47:24 am'!"Change Set:		TransEllipseFixDate:			11 February 1999Author:			Dan IngallsMakes ellipses with translucent borders display properly.Not optimum in speed or space, but behaves properly.Also includes a slight speedup for translucent polygon borders."!!EllipseMorph methodsFor: 'all' stamp: 'di 2/11/1999 08:35'!drawOn: aCanvas 	| tempCanvas fillForm borderForm |	(borderColor isColor and: [borderColor isTranslucentColor])		ifTrue: ["Translucent border requires use of temporary forms"				tempCanvas _ (FormCanvas extent: bounds extent depth: 1)					setShadowDrawing; stipple: Color black;					copyOffset: bounds topLeft negated.				tempCanvas fillOval: bounds color: color					borderWidth: borderWidth borderColor: Color transparent.				fillForm _ tempCanvas form.				tempCanvas _ (FormCanvas extent: bounds extent depth: 1)					setShadowDrawing; stipple: Color black;					copyOffset: bounds topLeft negated.				tempCanvas fillOval: bounds color: Color transparent					borderWidth: borderWidth borderColor: borderColor.				borderForm _ tempCanvas form copy offset: 0@0.				fillForm copy: borderForm boundingBox from: borderForm							to: 0@0 rule: Form erase.				aCanvas stencil: fillForm at: bounds topLeft color: color.				aCanvas stencil: borderForm at: bounds topLeft color: borderColor]		ifFalse: [aCanvas fillOval: bounds color: color					borderWidth: borderWidth borderColor: borderColor]! !!PolygonMorph methodsFor: 'private' stamp: 'di 2/11/1999 08:31'!borderForm	"A form must be created for drawing the border whenever the borderColor is translucent."	| borderCanvas |	borderForm ifNotNil: [^ borderForm].	borderCanvas _ (FormCanvas extent: bounds extent depth: 1)		setShadowDrawing; stipple: Color black;		copyOffset: bounds topLeft negated.	self drawBorderOn: borderCanvas.	borderForm _ borderCanvas form.	self arrowForms do:		[:f |  "Eliminate overlap between line and arrowheads if transparent."		borderForm copy: f boundingBox from: f to: f offset - self position rule: Form erase].	^ borderForm! !