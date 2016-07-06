'From Squeak 2.2 of Sept 23, 1998 on 16 November 1998 at 3:03:59 am'!"Change Set:		Balloon-3-MatrixTransformDate:			2 November 1998Author:			Andreas RaabAdds a 2x3 matrix representation for 2D geometric transformations.Also includes a Matrix2x3Plugin for several transformation primitives."!Object subclass: #DisplayTransform	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Transformations'!DisplayTransform subclass: #CompositeTransform	instanceVariableNames: 'globalTransform localTransform '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Transformations'!InterpreterPlugin subclass: #Matrix2x3Plugin	instanceVariableNames: 'm23ResultX m23ResultY m23ArgX m23ArgY '	classVariableNames: ''	poolDictionaries: ''	category: 'Squeak-Plugins'!DisplayTransform variableWordSubclass: #MatrixTransform2x3	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Transformations'!DisplayTransform subclass: #MorphicTransform	instanceVariableNames: 'offset angle scale '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Transformations'!!DisplayTransform commentStamp: '<historical>' prior: 0!This class represents a base for generic transformations of 2D points between different coordinate systems (including scaling and rotation). The transformations map objects between one coordinate system and another where it is assumed that a nested hierarchy of transformations can be defined.It is assumed that transformations deal with Integer points. All transformations should return Integer coordinates (even though float points may be passed in as argument).Compositions of transformations MUST work in the following order. A 'global' transformation (the argument in #composedWithGlobal:) is defined as a transformation that takes place between the receiver (the 'local') transformation and any 'global' point computations, whereas a 'local' transformation (e.g., the argument in #composedWithLocal:) takes place between the receiver ('global') and any 'local' points. For the transformation methods this means that combining a global and a local transformation will result in the following order:		globalPointToLocal: globalPoint			"globalPoint -> globalTransform -> localTransform -> locaPoint"			^localTransform globalPointToLocal:				(globalTransform globalPointToLocal: globalPoint)		localPointToGlobal: localPoint			"localPoint -> localTransform -> globalTransform -> globalPoint"			^globalTransform localPointToGlobal:				(localTransform localPointToGlobal: localPoint)!!DisplayTransform methodsFor: 'initialize' stamp: 'ar 11/2/1998 23:18'!setIdentity	"Initialize the receiver to the identity transformation (e.g., not affecting points)"	^self subclassResponsibility! !!DisplayTransform methodsFor: 'accessing' stamp: 'ar 11/2/1998 19:43'!inverseTransformation	"Return the inverse transformation of the receiver"	^self subclassResponsibility! !!DisplayTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 22:47'!isCompositeTransform	"Return true if the receiver is a composite transformation.	Composite transformations may have impact on the accuracy."	^false! !!DisplayTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 16:17'!isIdentity	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."	^self subclassResponsibility! !!DisplayTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 22:48'!isMatrixTransform2x3	"Return true if the receiver is 2x3 matrix transformation"	^false! !!DisplayTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 22:48'!isMorphicTransform	"Return true if the receiver is a MorphicTransform, that is specifies the transformation values explicitly."	^false! !!DisplayTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 16:16'!isPureTranslation	"Return true if the receiver specifies no rotation or scaling."	^self subclassResponsibility! !!DisplayTransform methodsFor: 'composing' stamp: 'ar 11/2/1998 16:15'!composedWithGlobal: aTransformation	"Return the composition of the receiver and the global transformation passed in.	A 'global' transformation is defined as a transformation that takes place	between the receiver (the 'local') transformation and any 'global' point	computations, e.g., for the methods		globalPointToLocal: globalPoint			globalPoint -> globalTransform -> localTransform -> locaPoint		localPointToGlobal: localPoint			localPoint -> localTransform -> globalTransform -> globalPoint		"	^aTransformation composedWithLocal: self! !!DisplayTransform methodsFor: 'composing' stamp: 'ar 11/2/1998 16:41'!composedWithLocal: aTransformation	"Return the composition of the receiver and the local transformation passed in.	A 'local' transformation is defined as a transformation that takes place	between the receiver (the 'global') transformation and any 'local' point	computations, e.g., for the methods		globalPointToLocal: globalPoint			globalPoint -> globalTransform -> localTransform -> locaPoint		localPointToGlobal: localPoint			localPoint -> localTransform -> globalTransform -> globalPoint		"	self isIdentity ifTrue:[^ aTransformation].	aTransformation isIdentity ifTrue:[^ self].	^ CompositeTransform new globalTransform: self							localTransform: aTransformation! !!DisplayTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:17'!globalPointToLocal: aPoint	"Transform aPoint from global coordinates into local coordinates"	^self subclassResponsibility! !!DisplayTransform methodsFor: 'transforming points' stamp: 'ar 11/9/1998 14:35'!globalPointsToLocal: inArray	"Transform all the points of inArray from global into local coordinates"	^inArray collect:[:pt| self globalPointToLocal: pt]! !!DisplayTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:18'!localPointToGlobal: aPoint	"Transform aPoint from local coordinates into global coordinates"	^self subclassResponsibility! !!DisplayTransform methodsFor: 'transforming points' stamp: 'ar 11/9/1998 14:35'!localPointsToGlobal: inArray	"Transform all the points of inArray from local into global coordinates"	^inArray collect:[:pt| self localPointToGlobal: pt]! !!DisplayTransform methodsFor: 'transforming rects' stamp: 'ar 11/2/1998 16:19'!globalBoundsToLocal: aRectangle	"Transform aRectangle from global coordinates into local coordinates"	^Rectangle encompassing: (self globalPointsToLocal: aRectangle corners)! !!DisplayTransform methodsFor: 'transforming rects' stamp: 'ar 11/2/1998 16:19'!localBoundsToGlobal: aRectangle	"Transform aRectangle from local coordinates into global coordinates"	^Rectangle encompassing: (self localPointsToGlobal: aRectangle corners)! !!DisplayTransform methodsFor: 'converting' stamp: 'ar 11/2/1998 19:59'!asCompositeTransform	"Represent the receiver as a composite transformation"	^CompositeTransform new		globalTransform: self		localTransform: self species identity! !!DisplayTransform methodsFor: 'converting' stamp: 'ar 11/2/1998 20:01'!asMatrixTransform2x3	"Represent the receiver as a 2x3 matrix transformation"	^self subclassResponsibility! !!CompositeTransform commentStamp: '<historical>' prior: 0!A composite transform provides the effect of several levels of coordinate transformations.!!CompositeTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 20:00'!isCompositeTransform	^true! !!CompositeTransform methodsFor: 'accessing' stamp: 'ar 11/2/1998 19:45'!inverseTransformation	"Return the inverse transformation of the receiver"	^self species new		globalTransform: localTransform inverseTransformation		localTransform: globalTransform inverseTransformation! !!CompositeTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:39'!globalPointToLocal: aPoint	"Transform aPoint from global coordinates into local coordinates"	^localTransform globalPointToLocal:		(globalTransform globalPointToLocal: aPoint)! !!CompositeTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:39'!localPointToGlobal: aPoint	"Transform aPoint from global coordinates into local coordinates"	^globalTransform localPointToGlobal:		(localTransform localPointToGlobal: aPoint)! !!CompositeTransform methodsFor: 'converting' stamp: 'ar 11/2/1998 20:00'!asCompositeTransform	^self! !!CompositeTransform methodsFor: 'converting' stamp: 'ar 11/2/1998 19:56'!asMatrixTransform2x3	^globalTransform asMatrixTransform2x3		composedWithLocal: localTransform asMatrixTransform2x3! !!DisplayTransform class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 20:55'!identity	^self new setIdentity! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/9/1998 16:30'!m23PrimitiveComposeMatrix	| m1 m2 m3 result |	self export: true.	self inline: false.	self var: #m1 declareC:'float *m1'.	self var: #m2 declareC:'float *m2'.	self var: #m3 declareC:'float *m3'.	m3 _ self loadArgumentMatrix: (result _ interpreterProxy stackObjectValue: 0).	m2 _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 1).	m1 _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 2).	interpreterProxy failed ifTrue:[^nil].	self matrix2x3ComposeMatrix: m1 with: m2 into: m3.	interpreterProxy pop: 3.	interpreterProxy push: result.! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/2/1998 19:27'!m23PrimitiveInvertPoint	| matrix |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	self loadArgumentPoint: (interpreterProxy stackObjectValue: 0).	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 1).	interpreterProxy failed ifTrue:[^nil].	self matrix2x3InvertPoint: matrix.	self roundAndStoreResultPoint: 2.! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/10/1998 14:09'!m23PrimitiveInvertRectInto	| matrix srcOop dstOop originX originY cornerX cornerY minX maxX minY maxY |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	self var: #originX declareC:'double originX'.	self var: #originY declareC:'double originY'.	self var: #cornerX declareC:'double cornerX'.	self var: #cornerY declareC:'double cornerY'.	self var: #minX declareC:'double minX'.	self var: #maxX declareC:'double maxX'.	self var: #minY declareC:'double minY'.	self var: #maxY declareC:'double maxY'.	dstOop _ interpreterProxy stackObjectValue: 0.	srcOop _ interpreterProxy stackObjectValue: 1.	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 2).	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy fetchClassOf: srcOop) = (interpreterProxy fetchClassOf: dstOop)		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy isPointers: srcOop)		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy slotSizeOf: srcOop) = 2		ifFalse:[^interpreterProxy primitiveFail].	"Load top-left point"	self loadArgumentPoint: (interpreterProxy fetchPointer: 0 ofObject: srcOop).	interpreterProxy failed ifTrue:[^nil].	originX _ m23ArgX.	originY _ m23ArgY.	self matrix2x3InvertPoint: matrix.	minX _ maxX _ m23ResultX.	minY _ maxY _ m23ResultY.	"Load bottom-right point"	self loadArgumentPoint:(interpreterProxy fetchPointer: 1 ofObject: srcOop).	interpreterProxy failed ifTrue:[^nil].	cornerX _ m23ArgX.	cornerY _ m23ArgY.	self matrix2x3InvertPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	"Load top-right point"	m23ArgX _ cornerX.	m23ArgY _ originY.	self matrix2x3InvertPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	"Load bottom-left point"	m23ArgX _ originX.	m23ArgY _ cornerY.	self matrix2x3InvertPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	dstOop _ self roundAndStoreResultRect: dstOop x0: minX y0: minY x1: maxX y1: maxY.	interpreterProxy failed ifFalse:[		interpreterProxy pop: 3.		interpreterProxy push: dstOop.	].! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/9/1998 16:27'!m23PrimitiveIsIdentity	| matrix |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 0).	interpreterProxy failed ifTrue:[^nil].	interpreterProxy pop: 1.	interpreterProxy pushBool:(		((matrix at: 0) = (self cCoerce: 1.0 to: 'float')) &		((matrix at: 1) = (self cCoerce: 0.0 to: 'float')) &		((matrix at: 2) = (self cCoerce: 0.0 to: 'float')) &		((matrix at: 3) = (self cCoerce: 0.0 to: 'float')) &		((matrix at: 4) = (self cCoerce: 1.0 to: 'float')) &		((matrix at: 5) = (self cCoerce: 0.0 to: 'float'))).! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/9/1998 16:27'!m23PrimitiveIsPureTranslation	| matrix |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 0).	interpreterProxy failed ifTrue:[^nil].	interpreterProxy pop: 1.	interpreterProxy pushBool:(		((matrix at: 0) = (self cCoerce: 1.0 to: 'float')) &		((matrix at: 1) = (self cCoerce: 0.0 to: 'float')) &		((matrix at: 3) = (self cCoerce: 0.0 to: 'float')) &		((matrix at: 4) = (self cCoerce: 1.0 to: 'float'))).! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/2/1998 19:27'!m23PrimitiveTransformPoint	| matrix |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	self loadArgumentPoint: (interpreterProxy stackObjectValue: 0).	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 1).	interpreterProxy failed ifTrue:[^nil].	self matrix2x3TransformPoint: matrix.	self roundAndStoreResultPoint: 2.! !!Matrix2x3Plugin methodsFor: 'primitives' stamp: 'ar 11/10/1998 14:09'!m23PrimitiveTransformRectInto	| matrix srcOop dstOop originX originY cornerX cornerY minX maxX minY maxY |	self export: true.	self inline: false.	self var: #matrix declareC:'float *matrix'.	self var: #originX declareC:'double originX'.	self var: #originY declareC:'double originY'.	self var: #cornerX declareC:'double cornerX'.	self var: #cornerY declareC:'double cornerY'.	self var: #minX declareC:'double minX'.	self var: #maxX declareC:'double maxX'.	self var: #minY declareC:'double minY'.	self var: #maxY declareC:'double maxY'.	dstOop _ interpreterProxy stackObjectValue: 0.	srcOop _ interpreterProxy stackObjectValue: 1.	matrix _ self loadArgumentMatrix: (interpreterProxy stackObjectValue: 2).	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy fetchClassOf: srcOop) = (interpreterProxy fetchClassOf: dstOop)		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy isPointers: srcOop)		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy slotSizeOf: srcOop) = 2		ifFalse:[^interpreterProxy primitiveFail].	"Load top-left point"	self loadArgumentPoint: (interpreterProxy fetchPointer: 0 ofObject: srcOop).	interpreterProxy failed ifTrue:[^nil].	originX _ m23ArgX.	originY _ m23ArgY.	self matrix2x3TransformPoint: matrix.	minX _ maxX _ m23ResultX.	minY _ maxY _ m23ResultY.	"Load bottom-right point"	self loadArgumentPoint:(interpreterProxy fetchPointer: 1 ofObject: srcOop).	interpreterProxy failed ifTrue:[^nil].	cornerX _ m23ArgX.	cornerY _ m23ArgY.	self matrix2x3TransformPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	"Load top-right point"	m23ArgX _ cornerX.	m23ArgY _ originY.	self matrix2x3TransformPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	"Load bottom-left point"	m23ArgX _ originX.	m23ArgY _ cornerY.	self matrix2x3TransformPoint: matrix.	minX _ minX min: m23ResultX.	maxX _ maxX max: m23ResultX.	minY _ minY min: m23ResultY.	maxY _ maxY max: m23ResultY.	dstOop _ self roundAndStoreResultRect: dstOop x0: minX y0: minY x1: maxX y1: maxY.	interpreterProxy failed ifFalse:[		interpreterProxy pop: 3.		interpreterProxy push: dstOop.	].! !!Matrix2x3Plugin methodsFor: 'transforming' stamp: 'ar 9/28/1998 00:55'!matrix2x3ComposeMatrix: m1 with: m2 into: m3	"Multiply matrix m1 with m2 and store the result into m3."	| a11 a12 a13 a21 a22 a23 |	self var: #m1 declareC:'const float *m1'.	self var: #m2 declareC:'const float *m2'.	self var: #m3 declareC:'float *m3'.	self var: #a11 declareC:'double a11'.	self var: #a12 declareC:'double a12'.	self var: #a13 declareC:'double a13'.	self var: #a21 declareC:'double a21'.	self var: #a22 declareC:'double a22'.	self var: #a23 declareC:'double a23'.	a11 _ ((m1 at: 0) * (m2 at: 0)) + ((m1 at: 1) * (m2 at: 3)).	a12 _ ((m1 at: 0) * (m2 at: 1)) + ((m1 at: 1) * (m2 at: 4)).	a13 _ ((m1 at: 0) * (m2 at: 2)) + ((m1 at: 1) * (m2 at: 5)) + (m1 at: 2).	a21 _ ((m1 at: 3) * (m2 at: 0)) + ((m1 at: 4) * (m2 at: 3)).	a22 _ ((m1 at: 3) * (m2 at: 1)) + ((m1 at: 4) * (m2 at: 4)).	a23 _ ((m1 at: 3) * (m2 at: 2)) + ((m1 at: 4) * (m2 at: 5)) + (m1 at: 5).	m3 at: 0 put: (self cCoerce: a11 to: 'float').	m3 at: 1 put: (self cCoerce: a12 to: 'float').	m3 at: 2 put: (self cCoerce: a13 to: 'float').	m3 at: 3 put: (self cCoerce: a21 to: 'float').	m3 at: 4 put: (self cCoerce: a22 to: 'float').	m3 at: 5 put: (self cCoerce: a23 to: 'float').! !!Matrix2x3Plugin methodsFor: 'transforming' stamp: 'ar 11/2/1998 03:37'!matrix2x3InvertPoint: m	"Invert the pre-loaded argument point by the given matrix"	| x y det detX detY |	self var: #m declareC:'float *m'.	self var: #x declareC:'double x'.	self var: #y declareC:'double y'.	self var: #det declareC:'double det'.	self var: #detX declareC:'double detX'.	self var: #detY declareC:'double detY'.	x _ m23ArgX - (m at: 2).	y _ m23ArgY - (m at: 5).	det _ ((m at: 0) * (m at: 4)) - ((m at: 1) * (m at: 3)).	det = 0.0 ifTrue:[^interpreterProxy primitiveFail]."Matrix is singular."	det _ 1.0 / det.	detX _ (x * (m at: 4)) - ((m at: 1) * y).	detY _ ((m at: 0) * y) - (x * (m at: 3)).	m23ResultX _ detX * det.	m23ResultY _ detY * det.! !!Matrix2x3Plugin methodsFor: 'transforming' stamp: 'ar 11/9/1998 16:23'!matrix2x3TransformPoint: m	"Transform the pre-loaded argument point by the given matrix"	self var: #m declareC:'float *m'.	m23ResultX _ (m23ArgX * (m at: 0)) + (m23ArgY * (m at: 1)) + (m at: 2).	m23ResultY _ (m23ArgX * (m at: 3)) + (m23ArgY * (m at: 4)) + (m at: 5).! !!Matrix2x3Plugin methodsFor: 'private' stamp: 'ar 11/9/1998 15:17'!loadArgumentMatrix: matrix	"Load the argument matrix"	self returnTypeC:'float *'.	interpreterProxy failed ifTrue:[^nil].	((interpreterProxy isWords: matrix) and:[(interpreterProxy slotSizeOf: matrix) = 6]) 		ifFalse:[interpreterProxy primitiveFail.				^nil].	^self cCoerce: (interpreterProxy firstIndexableField: matrix) to:'float *'.! !!Matrix2x3Plugin methodsFor: 'private' stamp: 'ar 11/9/1998 16:17'!loadArgumentPoint: point	"Load the argument point into m23ArgX and m23ArgY"	| oop isInt |	interpreterProxy failed ifTrue:[^nil].	"Check class of point"	(interpreterProxy fetchClassOf: point) = (interpreterProxy classPoint) 		ifFalse:[^interpreterProxy primitiveFail].	"Load X value"	oop _ interpreterProxy fetchPointer: 0 ofObject: point.	isInt _ interpreterProxy isIntegerObject: oop.	(isInt or:[interpreterProxy isFloatObject: oop])		ifFalse:[^interpreterProxy primitiveFail].	isInt		ifTrue:[m23ArgX _ interpreterProxy integerValueOf: oop]		ifFalse:[m23ArgX _ interpreterProxy floatValueOf: oop].	"Load Y value"	oop _ interpreterProxy fetchPointer: 1 ofObject: point.	isInt _ interpreterProxy isIntegerObject: oop.	(isInt or:[interpreterProxy isFloatObject: oop])		ifFalse:[^interpreterProxy primitiveFail].	isInt		ifTrue:[m23ArgY _ interpreterProxy integerValueOf: oop]		ifFalse:[m23ArgY _ interpreterProxy floatValueOf: oop].! !!Matrix2x3Plugin methodsFor: 'private' stamp: 'ar 11/14/1998 02:37'!okayIntValue: value	^(value >= -1073741824 asFloat and:[m23ResultX <= 1073741823 asFloat]) ! !!Matrix2x3Plugin methodsFor: 'private' stamp: 'ar 11/14/1998 02:39'!roundAndStoreResultPoint: nItemsToPop	"Store the result of a previous operation.	Fail if we cannot represent the result as SmallInteger"	m23ResultX _ m23ResultX + 0.5.	m23ResultY _ m23ResultY + 0.5.	(self okayIntValue: m23ResultX) ifFalse:[^interpreterProxy primitiveFail].	(self okayIntValue: m23ResultY) ifFalse:[^interpreterProxy primitiveFail].	interpreterProxy pop: nItemsToPop.	interpreterProxy push:		(interpreterProxy makePointwithxValue: m23ResultX asInteger 							yValue: m23ResultY asInteger).! !!Matrix2x3Plugin methodsFor: 'private' stamp: 'ar 11/14/1998 02:38'!roundAndStoreResultRect: dstOop x0: x0 y0: y0 x1: x1 y1: y1	"Check, round and store the result of a rectangle operation"	| minX maxX minY maxY originOop cornerOop rectOop |	self var: #x0 declareC:'double x0'.	self var: #y0 declareC:'double y0'.	self var: #x1 declareC:'double x1'.	self var: #y1 declareC:'double y1'.	self var: #minX declareC:'double minX'.	self var: #maxX declareC:'double maxX'.	self var: #minY declareC:'double minY'.	self var: #maxY declareC:'double maxY'.	minX _ x0 + 0.5.	(self okayIntValue: minX) ifFalse:[^interpreterProxy primitiveFail].	maxX _ x1 + 0.5.	(self okayIntValue: maxX) ifFalse:[^interpreterProxy primitiveFail].	minY _ y0 + 0.5.	(self okayIntValue: minY) ifFalse:[^interpreterProxy primitiveFail].	maxY _ y1 + 0.5.	(self okayIntValue: maxY) ifFalse:[^interpreterProxy primitiveFail].	interpreterProxy pushRemappableOop: dstOop.	originOop _ interpreterProxy makePointwithxValue: minX asInteger yValue: minY asInteger.	interpreterProxy pushRemappableOop: originOop.	cornerOop _ interpreterProxy makePointwithxValue: maxX asInteger yValue: maxY asInteger.	originOop _ interpreterProxy popRemappableOop.	rectOop _ interpreterProxy popRemappableOop.	interpreterProxy storePointer: 0 ofObject: rectOop withValue: originOop.	interpreterProxy storePointer: 1 ofObject: rectOop withValue: cornerOop.	^rectOop! !!Matrix2x3Plugin class methodsFor: 'class initialization' stamp: 'ar 11/2/1998 03:39'!declareCVarsIn: cg	cg var: 'm23ResultX' declareC:'double m23ResultX'.	cg var: 'm23ResultY' declareC:'double m23ResultY'.	cg var: 'm23ArgX' declareC:'double m23ArgX'.	cg var: 'm23ArgY' declareC:'double m23ArgY'.! !!MatrixTransform2x3 commentStamp: '<historical>' prior: 0!This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix containing the transformation from the local coordinate system in the global coordinate system. Thus, transforming points from local to global coordinates is fast and cheap whereas transformations from global to local coordinate systems are relatively expensive.Implementation Note: It is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here).!!MatrixTransform2x3 reorganize!('initialize' setIdentiy)('accessing' at: at:put: inverseTransformation offset offset:)('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)('testing' isIdentity isMatrixTransform2x3 isPureTranslation)('comparing' = hash)('composing' composedWithLocal: composedWithLocal:into:)('transforming points' globalPointToLocal: invertPoint: localPointToGlobal: transformPoint:)('transforming rects' globalBounds:toLocal: globalBoundsToLocal: localBounds:toGlobal: localBoundsToGlobal:)('converting' asMatrixTransform2x3)('printing' printOn:)('private' basicFloatAt: basicFloatAt:put: setAngle: setOffset: setScale:)!!MatrixTransform2x3 methodsFor: 'initialize' stamp: 'ar 11/2/1998 23:17'!setIdentiy	"Initialize the receiver to the identity transformation (e.g., not affecting points)"	self		a11: 1.0; a12: 0.0; a13: 0.0;		a21: 0.0; a22: 1.0; a23: 0.0.! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 16:28'!at: index	<primitive: 'primitiveFloatArrayAt'>	^self basicFloatAt: index! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 16:28'!at: index put: value	<primitive: 'primitiveFloatArrayAtPut'>	^self basicFloatAt: index put: value asFloat! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 23:15'!inverseTransformation	"Return the inverse transformation of the receiver.	The inverse transformation is computed by first calculating	the inverse offset and then computing transformations	for the two identity vectors (1@0) and (0@1)"	| r1 r2 r3 m |	r3 _ self invertPoint: 0@0.	r1 _ (self invertPoint: 1@0) - r1.	r2 _ (self invertPoint: 0@1) - r1.	m _ self species new.	m		a11: r1 x; a12: r2 x; a13: r3 x;		a21: r1 y; a22: r2 y; a23: r3 y.	^m! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 23:19'!offset	^self a13 @ self a23! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 23:05'!offset: aPoint	self a13: aPoint x asFloat.	self a23: aPoint y asFloat.! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a11	^self at: 1! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a11: value	 self at: 1 put: value! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a12	^self at: 2! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a12: value	 self at: 2 put: value! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a13	^self at: 3! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a13: value	 self at: 3 put: value! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a21	 ^self at: 4! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a21: value	 self at: 4 put: value! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a22	 ^self at: 5! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a22: value	 self at: 5 put: value! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!a23	 ^self at: 6! !!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!a23: value	 self at: 6 put: value! !!MatrixTransform2x3 methodsFor: 'testing' stamp: 'ar 11/2/1998 23:05'!isIdentity	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."	<primitive: 'm23PrimitiveIsIdentity'>	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]! !!MatrixTransform2x3 methodsFor: 'testing' stamp: 'ar 11/2/1998 23:15'!isMatrixTransform2x3	"Return true if the receiver is 2x3 matrix transformation"	^true! !!MatrixTransform2x3 methodsFor: 'testing' stamp: 'ar 11/2/1998 23:06'!isPureTranslation	"Return true if the receiver specifies no rotation or scaling."	<primitive: 'm23PrimitiveIsPureTranslation'>	^self a11 = 1.0 and:[self a12 = 0.0 and:[self a22 = 0.0 and:[self a21 = 1.0]]]! !!MatrixTransform2x3 methodsFor: 'comparing' stamp: 'ar 11/2/1998 19:32'!= MatrixTransform2x3	| length |	<primitive:'primitiveFloatArrayEqual'>	self class = MatrixTransform2x3 class ifFalse:[^false].	length _ self size.	(length = MatrixTransform2x3 size) ifFalse:[^false].	1 to: self size do:[:i| (self at: i) = (MatrixTransform2x3 at: i) ifFalse:[^false]].	^true! !!MatrixTransform2x3 methodsFor: 'comparing' stamp: 'ar 11/2/1998 19:31'!hash	| result |	<primitive:'primitiveFloatArrayHash'>	result _ 0.	1 to: self size do:[:i| result _ result + (self basicAt: i) ].	^result bitAnd: 16r1FFFFFFF! !!MatrixTransform2x3 methodsFor: 'composing' stamp: 'ar 11/2/1998 19:50'!composedWithLocal: aTransformation	"Return the composition of the receiver and the local transformation passed in"	aTransformation isMatrixTransform2x3 ifFalse:[^super composedWith: aTransformation].	^self composedWithLocal: aTransformation asMatrixTransform2x3 into: self class new! !!MatrixTransform2x3 methodsFor: 'composing' stamp: 'ar 11/2/1998 23:08'!composedWithLocal: aTransformation into: result	"Return the composition of the receiver and the local transformation passed in.	Store the composed matrix into result."	| a11 a12 a13 a21 a22 a23 b11 b12 b13 b21 b22 b23 matrix |	<primitive: 'm23PrimitiveComposeMatrix'>	matrix _ aTransformation asMatrixTransform2x3.	a11 _ self a11.		b11 _ matrix a11.	a12 _ self a12.		b12 _ matrix a12.	a13 _ self a13.		b13 _ matrix a13.	a21 _ self a21.		b21 _ matrix a21.	a22 _ self a22.		b22 _ matrix a22.	a23 _ self a23.		b23 _ matrix a23.	result a11: (a11 * b11) + (a12 * b21).	result a12: (a11 * b12) + (a12 * b22).	result a13: a13 + (a11 * b13) + (a12 * b23).	result a21: (a21 * b11) + (a22 * b21).	result a22: (a21 * b12) + (a22 * b22).	result a23: a23 + (a21 * b13) + (a22 * b23).	^result! !!MatrixTransform2x3 methodsFor: 'transforming points' stamp: 'ar 11/9/1998 13:46'!globalPointToLocal: aPoint	"Transform aPoint from global coordinates into local coordinates"	<primitive: 'm23PrimitiveInvertPoint'>	^(self invertPoint: aPoint) rounded! !!MatrixTransform2x3 methodsFor: 'transforming points' stamp: 'ar 11/2/1998 23:09'!invertPoint: aPoint	"Transform aPoint from global coordinates into local coordinates"	| x y det a11 a12 a21 a22 detX detY |	x _ aPoint x asFloat - (self a13).	y _ aPoint y asFloat - (self a23).	a11 _ self a11.	a12 _ self a22.	a21 _ self a21.	a22 _ self a22.	det _ (a11 * a22) - (a12 * a21).	det = 0.0 ifTrue:[^0@0]. "So we have at least a valid result"	det _ 1.0 / det.	detX _ (x * a22) - (a12 * y).	detY _ (a11 * y) - (x * a21).	^(detX * det) @ (detY * det)! !!MatrixTransform2x3 methodsFor: 'transforming points' stamp: 'ar 11/3/1998 03:04'!localPointToGlobal: aPoint	"Transform aPoint from local coordinates into global coordinates"	<primitive: 'm23PrimitiveTransformPoint'>	^(self transformPoint: aPoint) rounded! !!MatrixTransform2x3 methodsFor: 'transforming points' stamp: 'ar 11/2/1998 23:09'!transformPoint: aPoint	"Transform aPoint from local coordinates into global coordinates"	| x y |	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.	^x @ y! !!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'ar 11/9/1998 14:41'!globalBounds: srcRect toLocal: dstRect	"Transform aRectangle from global coordinates into local coordinates"	<primitive:'m23PrimitiveInvertRectInto'>	^super globalBoundsToLocal: srcRect! !!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'ar 11/9/1998 14:40'!globalBoundsToLocal: aRectangle	"Transform aRectangle from global coordinates into local coordinates"	^self globalBounds: aRectangle toLocal: Rectangle new! !!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'ar 11/9/1998 14:41'!localBounds: srcRect toGlobal: dstRect	"Transform aRectangle from local coordinates into global coordinates"	<primitive:'m23PrimitiveTransformRectInto'>	^super localBoundsToGlobal: srcRect! !!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'ar 11/9/1998 14:40'!localBoundsToGlobal: aRectangle	"Transform aRectangle from local coordinates into global coordinates"	^self localBounds: aRectangle toGlobal: Rectangle new! !!MatrixTransform2x3 methodsFor: 'converting' stamp: 'ar 11/2/1998 15:34'!asMatrixTransform2x3	^self! !!MatrixTransform2x3 methodsFor: 'printing' stamp: 'ar 11/2/1998 23:11'!printOn: aStream	aStream 		nextPutAll: self class name;		nextPut: $(;		cr; print: self a11; tab; print: self a12; tab; print: self a13;		cr; print: self a21; tab; print: self a22; tab; print: self a23;		cr; nextPut:$).! !!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 16:28'!basicFloatAt: index	"Note: Relies on IEEE floats!!"	| word sign mantissa exponent newFloat |	word _ self basicAt: index.	word = 0 ifTrue:[^0.0].	mantissa _ word bitAnd:  16r7FFFFF.	exponent _ ((word bitShift: -23) bitAnd: 16rFF) - 127.	sign _ word bitAnd: 16r80000000.	exponent = 128 ifTrue:["Either NAN or INF"		mantissa = 0 ifFalse:[^Float nan].		sign = 0 			ifTrue:[^Float infinity]			ifFalse:[^Float infinity negated]].	"Create new float"	newFloat _ Float new: 2.	newFloat basicAt: 1 put: sign + (1023 + exponent bitShift: 20) + (mantissa bitShift: -3).	newFloat basicAt: 2 put: ((mantissa bitAnd: 7) bitShift: 29).	^newFloat! !!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/15/1998 23:12'!basicFloatAt: index put: value	"Note: Relies on IEEE floats!!"	| word1 word2 sign mantissa exponent destWord |	value = 0.0 ifTrue:[^self basicAt: index put: 0].	word1 _ value basicAt: 1.	word2 _ value basicAt: 2.	mantissa _ (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).	exponent _ ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.	exponent < 0 ifTrue:[^self basicAt: index put: 0].	exponent > 254 ifTrue:[		exponent _ 255.		mantissa _ 0].	sign _ word1 bitAnd: 16r80000000.	destWord _ sign + (exponent bitShift: 23) + mantissa.	self basicAt: index put: destWord.	^value! !!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 23:17'!setAngle: angle	"Set the raw rotation angle in the receiver"	| rad s c |	rad := angle degreesToRadians.	s := rad sin.	c := rad cos.	self a11: c.	self a12: s negated.	self a21: s.	self a22: c.! !!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 23:17'!setOffset: aPoint	"Set the raw offset in the receiver"	| pt |	pt _ aPoint asPoint.	self a13: pt x asFloat.	self a23: pt y asFloat.! !!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 23:16'!setScale: aPoint	"Set the raw scale in the receiver"	| pt |	pt _ aPoint asPoint.	self a11: pt x asFloat.	self a22: pt y asFloat.! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 22:50'!identity	^self new setScale: 1.0! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 7/9/1998 20:09'!new	^self new: 6! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/12/1998 01:25'!transformFromLocal: localBounds toGlobal: globalBounds	^((self withOffset: (globalBounds center)) composedWithLocal:		(self withScale: (globalBounds extent / localBounds extent) asFloatPoint))			composedWithLocal: (self withOffset: localBounds center negated)"	^(self identity)		setScale: (globalBounds extent / localBounds extent) asFloatPoint;		setOffset: localBounds center negated asFloatPoint;		composedWithGlobal:(self withOffset: globalBounds center asFloatPoint)"! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 02:49'!withAngle: angle	^self new setAngle: angle! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/3/1998 02:52'!withOffset: aPoint	^self identity setOffset: aPoint! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 23:17'!withRotation: angle	^self new setAngle: angle! !!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 02:49'!withScale: aPoint	^self new setScale: aPoint! !!MorphicTransform commentStamp: '<historical>' prior: 0!This class implements simple translation, scaling and rotation for points, as well as inverse transformations.  These transformations are used in TransformMorphs (clipping scrollers) and TransformationMorphs (general flex-morph wrappers) to map, eg, global mouse coords into local coords, and to invert, eg, local damage rectangles into global damage rectangles.!!MorphicTransform methodsFor: 'accessing' stamp: 'ar 11/9/1998 14:33'!inverseTransformation	"Return the inverse transformation of the receiver"	^MorphicTransform		offset: (self transform: 0@0) - (self transform: offset)		angle: angle negated		scale: scale reciprocal! !!MorphicTransform methodsFor: 'initialize' stamp: 'ar 11/2/1998 20:58'!setIdentiy	scale _ 1.0.	offset _ 0@0.	angle _ 0.0.! !!MorphicTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 20:57'!isIdentity	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."	^ self isPureTranslation and: [offset = (0@0)]! !!MorphicTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 19:51'!isMorphicTransform	^true! !!MorphicTransform methodsFor: 'testing' stamp: 'ar 11/2/1998 20:57'!isPureTranslation	"Return true if the receiver specifies no rotation or scaling."	^ angle = 0.0 and: [scale = 1.0]! !!MorphicTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:13'!globalPointToLocal: aPoint	"Transform aPoint from global coordinates into local coordinates"	^self transform: aPoint! !!MorphicTransform methodsFor: 'transforming points' stamp: 'ar 11/2/1998 16:32'!localPointToGlobal: aPoint	"Transform aPoint from global coordinates into local coordinates"	^self invert: aPoint! !!MorphicTransform methodsFor: 'converting' stamp: 'ar 11/2/1998 20:14'!asMatrixTransform2x3	^((MatrixTransform2x3 withRotation: angle radiansToDegrees negated) composedWithLocal:		(MatrixTransform2x3 withScale: scale))			offset: offset negated! !