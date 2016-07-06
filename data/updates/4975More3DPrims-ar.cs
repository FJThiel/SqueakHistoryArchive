'From TeaSqueak3.2 of 5 April 2002 [latest update: #215] on 18 September 2002 at 5:33:49 pm'!"Change Set:		More3DPrims-arDate:			18 September 2002Author:			Andreas RaabMake four more matrix operations run in primitives for speed and less pressure on GC."!!B3DMatrix4x4 methodsFor: 'transforming' stamp: 'ar 9/18/2002 16:55'!localDirToGlobal: aVector	"Multiply direction vector with the receiver"	| x y z rx ry rz |	<primitive: 'b3dTransformDirection' module: 'Squeak3D'>	x := aVector x.	y := aVector y.	z := aVector z.	rx := (x * self a11) + (y * self a12) + (z * self a13).	ry := (x * self a21) + (y * self a22) + (z * self a23).	rz := (x * self a31) + (y * self a32) + (z * self a33).	^B3DVector3 x: rx y: ry z: rz! !!B3DMatrix4x4 methodsFor: 'transforming' stamp: 'ar 9/18/2002 17:30'!localPointToGlobal: aVector	"Multiply aVector (temporarily converted to 4D) with the receiver"	| x y z rx ry rz rw |	<primitive: 'b3dTransformPoint' module: 'Squeak3D'>	x := aVector x.	y := aVector y.	z := aVector z.	rx := (x * self a11) + (y * self a12) + (z * self a13) + self a14.	ry := (x * self a21) + (y * self a22) + (z * self a23) + self a24.	rz := (x * self a31) + (y * self a32) + (z * self a33) + self a34.	rw := (x * self a41) + (y * self a42) + (z * self a43) + self a44.	^B3DVector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !!B3DMatrix4x4 methodsFor: 'transforming' stamp: 'ar 9/18/2002 17:30'!orthoNormInverse	| m x y z rx ry rz |	<primitive: 'b3dOrthoNormInverseMatrix' module: 'Squeak3D'>	m := self clone.	"transpose upper 3x3 matrix"	m a11: self a11; a12: self a21; a13: self a31.	m a21: self a12; a22: self a22; a23: self a32.	m a31: self a13; a32: self a23; a33: self a33.	"Compute inverse translation vector"	x := self a14.	y := self a24.	z := self a34.	rx := (x * m a11) + (y * m a12) + (z * m a13).	ry := (x * m a21) + (y * m a22) + (z * m a23).	rz := (x * m a31) + (y * m a32) + (z * m a33).	m a14: 0.0-rx; a24: 0.0-ry; a34: 0.0-rz.	^m" Used to be:	m _ self clone.	v _ m translation.	m translation: B3DVector3 zero.	m _ m transposed.	v _ (m localPointToGlobal: v) negated.	m translation: v.	^ m."! !!B3DMatrix4x4 methodsFor: 'transforming' stamp: 'ar 9/18/2002 17:30'!transposed	"Return a transposed copy of the receiver"	| matrix |	<primitive: 'b3dTransposeMatrix' module: 'Squeak3D'>	matrix := self class new.	matrix 		a11: self a11; a12: self a21; a13: self a31; a14: self a41;		a21: self a12; a22: self a22; a23: self a32; a24: self a42;		a31: self a13; a32: self a23; a33: self a33; a34: self a43;		a41: self a14; a42: self a24; a43: self a34; a44: self a44.	^matrix! !!B3DTransformerPlugin methodsFor: 'primitives' stamp: 'ar 9/18/2002 17:33'!b3dOrthoNormInverseMatrix	| srcOop dstOop src dst x y z rx ry rz |	self export: true.	self var: #src declareC:'float *src'.	self var: #dst declareC:'float *dst'.	self var: #x declareC:'double x'.	self var: #y declareC:'double y'.	self var: #z declareC:'double z'.	self var: #rx declareC:'double rx'.	self var: #ry declareC:'double ry'.	self var: #rz declareC:'double rz'.	interpreterProxy methodArgumentCount = 0		ifFalse:[^interpreterProxy primitiveFail].	srcOop := interpreterProxy stackObjectValue: 0.	interpreterProxy failed ifTrue:[^nil].	((interpreterProxy isWords: srcOop) and:[(interpreterProxy slotSizeOf: srcOop) = 16])		ifFalse:[^interpreterProxy primitiveFail].	dstOop := interpreterProxy clone: srcOop.	"reload srcOop in case of GC"	srcOop := interpreterProxy stackObjectValue: 0.	src := interpreterProxy firstIndexableField: srcOop.	dst := interpreterProxy firstIndexableField: dstOop.	"Transpose upper 3x3 matrix"	"dst at: 0 put: (src at: 0)."	dst at: 1 put: (src at: 4). 	dst at: 2 put: (src at: 8). 	dst at: 4 put: (src at: 1). 	"dst at: 5 put: (src at: 5)."	dst at: 6 put: (src at: 9). 	dst at: 8 put: (src at: 2). 	dst at: 9 put: (src at: 6). 	"dst at: 10 put: (src at: 10)."	"Compute inverse translation vector"	x := src at: 3..	y := src at: 7.	z := src at: 11.	rx := (x * (dst at: 0)) + (y * (dst at: 1)) + (z * (dst at: 2)).	ry := (x * (dst at: 4)) + (y * (dst at: 5)) + (z * (dst at: 6)).	rz := (x * (dst at: 8)) + (y * (dst at: 9)) + (z * (dst at: 10)).	dst at: 3 put: (self cCoerce: 0.0-rx to: 'float').	dst at: 7 put: (self cCoerce: 0.0-ry to: 'float').	dst at: 11 put: (self cCoerce: 0.0-rz to: 'float').	interpreterProxy pop: 1.	interpreterProxy push: dstOop.! !!B3DTransformerPlugin methodsFor: 'primitives' stamp: 'ar 9/18/2002 16:55'!b3dTransformDirection	| x y z rx ry rz matrix vertex v3Oop |	self export: true.	self var: #vertex declareC:'float *vertex'.	self var: #matrix declareC:'float *matrix'.	self var: #x declareC:'double x'.	self var: #y declareC:'double y'.	self var: #z declareC:'double z'.	self var: #rx declareC:'double rx'.	self var: #ry declareC:'double ry'.	self var: #rz declareC:'double rz'.	interpreterProxy methodArgumentCount = 1		ifFalse:[^interpreterProxy primitiveFail].	v3Oop := interpreterProxy stackObjectValue: 0.	interpreterProxy failed ifTrue:[^nil].	((interpreterProxy isWords: v3Oop) and:[(interpreterProxy slotSizeOf: v3Oop) = 3])		ifFalse:[^interpreterProxy primitiveFail].	vertex := interpreterProxy firstIndexableField: v3Oop.	matrix := self stackMatrix: 1.	(matrix == nil) ifTrue:[^interpreterProxy primitiveFail].	x := vertex at: 0.	y := vertex at: 1.	z := vertex at: 2.	rx _ (x * (matrix at: 0)) + (y * (matrix at: 1)) + (z * (matrix at: 2)).	ry _ (x * (matrix at: 4)) + (y * (matrix at: 5)) + (z * (matrix at: 6)).	rz _ (x * (matrix at: 8)) + (y * (matrix at: 9)) + (z * (matrix at: 10)).	v3Oop := interpreterProxy clone: v3Oop.	vertex := interpreterProxy firstIndexableField: v3Oop.	vertex at: 0 put: (self cCoerce: rx to: 'float').	vertex at: 1 put: (self cCoerce: ry to:'float').	vertex at: 2 put: (self cCoerce: rz to: 'float').! !!B3DTransformerPlugin methodsFor: 'primitives' stamp: 'ar 9/18/2002 16:53'!b3dTransformPoint	| x y z rx ry rz rw matrix vertex v3Oop |	self export: true.	self var: #vertex declareC:'float *vertex'.	self var: #matrix declareC:'float *matrix'.	self var: #x declareC:'double x'.	self var: #y declareC:'double y'.	self var: #z declareC:'double z'.	self var: #rx declareC:'double rx'.	self var: #ry declareC:'double ry'.	self var: #rz declareC:'double rz'.	self var: #rw declareC:'double rw'.	interpreterProxy methodArgumentCount = 1		ifFalse:[^interpreterProxy primitiveFail].	v3Oop := interpreterProxy stackObjectValue: 0.	interpreterProxy failed ifTrue:[^nil].	((interpreterProxy isWords: v3Oop) and:[(interpreterProxy slotSizeOf: v3Oop) = 3])		ifFalse:[^interpreterProxy primitiveFail].	vertex := interpreterProxy firstIndexableField: v3Oop.	matrix := self stackMatrix: 1.	(matrix == nil) ifTrue:[^interpreterProxy primitiveFail].	x := vertex at: 0.	y := vertex at: 1.	z := vertex at: 2.	rx _ (x * (matrix at: 0)) + (y * (matrix at: 1)) + (z * (matrix at: 2)) + (matrix at: 3).	ry _ (x * (matrix at: 4)) + (y * (matrix at: 5)) + (z * (matrix at: 6)) + (matrix at: 7).	rz _ (x * (matrix at: 8)) + (y * (matrix at: 9)) + (z * (matrix at: 10)) + (matrix at: 11).	rw _ (x * (matrix at: 12)) + (y * (matrix at: 13)) + (z * (matrix at: 14)) + (matrix at: 15).	v3Oop := interpreterProxy clone: v3Oop.	vertex := interpreterProxy firstIndexableField: v3Oop.	rw = 1.0 ifTrue:[		vertex at: 0 put: (self cCoerce: rx to: 'float').		vertex at: 1 put: (self cCoerce: ry to:'float').		vertex at: 2 put: (self cCoerce: rz to: 'float').	] ifFalse:[		rw = 0.0 			ifTrue:[rw _ 0.0]			ifFalse:[rw _ 1.0 / rw].		vertex at: 0 put: (self cCoerce: rx*rw to:'float').		vertex at: 1 put: (self cCoerce: ry*rw to:'float').		vertex at: 2 put: (self cCoerce: rz*rw to: 'float').	].	interpreterProxy pop: 2.	interpreterProxy push: v3Oop.! !!B3DTransformerPlugin methodsFor: 'primitives' stamp: 'ar 9/18/2002 17:33'!b3dTransposeMatrix	| srcOop dstOop src dst |	self export: true.	self var: #src declareC:'float *src'.	self var: #dst declareC:'float *dst'.	interpreterProxy methodArgumentCount = 0		ifFalse:[^interpreterProxy primitiveFail].	srcOop := interpreterProxy stackObjectValue: 0.	interpreterProxy failed ifTrue:[^nil].	((interpreterProxy isWords: srcOop) and:[(interpreterProxy slotSizeOf: srcOop) = 16])		ifFalse:[^interpreterProxy primitiveFail].	dstOop := interpreterProxy clone: srcOop.	"reload srcOop in case of GC"	srcOop := interpreterProxy stackObjectValue: 0.	src := interpreterProxy firstIndexableField: srcOop.	dst := interpreterProxy firstIndexableField: dstOop.	"dst at: 0 put: (src at: 0)."	dst at: 1 put: (src at: 4). 	dst at: 2 put: (src at: 8). 	dst at: 3 put: (src at: 12).	dst at: 4 put: (src at: 1). 	"dst at: 5 put: (src at: 5)."	dst at: 6 put: (src at: 9). 	dst at: 7 put: (src at: 13).	dst at: 8 put: (src at: 2). 	dst at: 9 put: (src at: 6). 	"dst at: 10 put: (src at: 10)."	dst at: 11 put: (src at: 14).	dst at: 12 put: (src at: 3). 	dst at: 13 put: (src at: 7). 	dst at: 14 put: (src at: 11). 	"dst at: 15 put: (src at: 15)."	interpreterProxy pop: 1.	interpreterProxy push: dstOop.! !