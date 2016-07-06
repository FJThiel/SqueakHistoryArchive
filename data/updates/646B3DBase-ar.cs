'From Squeak 2.3 of January 14, 1999 on 4 February 1999 at 8:28:50 pm'!"Change Set:		B3DBaseDate:			4 February 1999Author:			Andreas RaabThe very first version of the Balloon 3D framework.A lot of things don't work at all - what's basically working are the examples in B3DBox ;-)Note: This change set is primarily for Jeff to get an idea of how B3D will look alike."!Object subclass: #B3DCamera	instanceVariableNames: 'position target up perspective '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Viewing'!Object subclass: #B3DCameraPerspective	instanceVariableNames: 'nearDistance farDistance fieldOfView aspectRatio '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Viewing'!Object subclass: #B3DEnginePart	instanceVariableNames: 'engine '	classVariableNames: 'PrimitiveActions '	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DEnginePart subclass: #B3DClipper	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!FloatArray variableWordSubclass: #B3DFloatArray	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!B3DFloatArray variableWordSubclass: #B3DColor4	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!B3DFloatArray variableWordSubclass: #B3DColor4Array	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!Object subclass: #B3DGeometry	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Objects'!B3DGeometry subclass: #B3DBox	instanceVariableNames: 'vertices '	classVariableNames: 'BoxColors BoxFaceIndexes BoxNormals '	poolDictionaries: ''	category: 'Balloon-3D-Objects'!Object subclass: #B3DLightSource	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Lights'!B3DLightSource subclass: #B3DAmbientLight	instanceVariableNames: 'ambient '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Lights'!B3DFloatArray variableWordSubclass: #B3DMatrix4x4	instanceVariableNames: ''	classVariableNames: 'B3DIdentityMatrix B3DZeroMatrix '	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!Morph subclass: #B3DMorph	instanceVariableNames: 'camera geometry angle '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Balloon-Demos'!Object variableWordSubclass: #B3DPrimitiveVertex	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!ArrayedCollection variableWordSubclass: #B3DPrimitiveVertexArray	instanceVariableNames: ''	classVariableNames: 'B3DPrimitiveVertexSize '	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DEnginePart subclass: #B3DRasterizer	instanceVariableNames: 'viewport '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!Object subclass: #B3DRenderEngine	instanceVariableNames: 'vertexBuffer transformer shader clipper rasterizer '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DFloatArray variableWordSubclass: #B3DRotation	instanceVariableNames: ''	classVariableNames: 'B3DIdentityRotation '	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!B3DRasterizer subclass: #B3DSqueakFormRasterizer	instanceVariableNames: 'bb dotForm lineForm canvas '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DFloatArray variableWordSubclass: #B3DVector3	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!B3DFloatArray variableWordSubclass: #B3DVector4	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Vectors'!Object subclass: #B3DVertexBuffer	instanceVariableNames: 'current vertexArray vertexCount bounds primitive '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DEnginePart subclass: #B3DVertexShader	instanceVariableNames: 'lights material '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DEnginePart subclass: #B3DVertexTransformer	instanceVariableNames: 'modelMatrix viewMatrix textureMatrix currentMatrix needsUpdate matrixStack matrixState '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!B3DFloatArray variableWordSubclass: #B3DViewingFrustum	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Viewing'!Rectangle subclass: #B3DViewport	instanceVariableNames: 'center scale '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Viewing'!!B3DBox class methodsFor: 'class initialization' stamp: 'ar 2/4/1999 20:20'!initialize
	"B3DBox initialize"
	| nrmls |
	nrmls := #(	(-1.0 0.0 0.0) (0.0 1.0 0.0) (1.0 0.0 0.0)
				(0.0 -1.0 0.0) (0.0 0.0 1.0) (0.0 0.0 -1.0)) 
			collect:[:spec| B3DVector3 x: spec first y: spec second z: spec third].
	BoxNormals := nrmls.
	"BoxNormals := Array new: 6.
	1 to: 6 do:[:i|
		BoxNormals at: i put: (FloatVector3 new).
		1 to: 3 do:[:j| (BoxNormals at: i) at: j put: ((nrmls at: i) at: j)]]."
	BoxFaceIndexes := #(	(1 2 3 4) (4 3 7 8) (8 7 6 5)
						(5 6 2 1) (6 7 3 2) (8 5 1 4)).	BoxColors _ #(red green blue yellow gray cyan) collect:[:s| (Color perform: s) alpha: 0.5].! !!B3DBox class methodsFor: 'instance creation'!from: origin to: corner
	^self new buildBoxFrom: origin to: corner! !!B3DBox class methodsFor: 'examples' stamp: 'ar 2/4/1999 17:53'!example1	"B3DBox example1"
	"Simple rotating box drawn without perspective"
	| box b3d p r m1 time f |
	p _ 0.1@0.1@0.1.
	box _ B3DBox from: p negated to: p.
	b3d _ B3DRenderEngine new.	b3d viewport: (0@0 corner: 800@600).
	b3d rotateBy: (B3DRotation x: 1 y: 0 z: 0 a: 45).
	r _ 312@241 corner: 480@355.
	f _ Form fromDisplay: r.
	m1 _ (B3DRotation x: 0 y: 1 z: 0 a: 5) asMatrix4x4.
	Display deferUpdates: true.
	[Sensor anyButtonPressed] whileFalse:[
		time _ Time millisecondsToRun:[
			b3d transformBy: m1.
			"Display fill: r fillColor: Color white."
			f displayAt: r origin.
			box renderOn: b3d].
		time printString displayAt: r origin.
		Display forceToScreen: r.
		"(Delay forMilliseconds: 100) wait"].
	Display deferUpdates: false.
	f displayAt: r origin.	! !!B3DBox class methodsFor: 'examples' stamp: 'ar 2/4/1999 17:53'!example2	"B3DBox example2"
	"Simple two-way rotating box drawn without perspective"
	| box b3d p r m1 m2 time f |
	p _ 0.1@0.1@0.1.
	box _ B3DBox from: p negated to: p.
	b3d _ B3DRenderEngine new.	b3d viewport: (0@0 corner: 800@600).
	r _ 312@241 corner: 480@355.
	m1 _ (B3DRotation x: 0 y: 1 z: 0 a: 5) asMatrix4x4.
	m2 _ (B3DRotation x: 1 y: 1 z: 0 a: 1) asMatrix4x4.
	f _ Form fromDisplay: r.
	Display deferUpdates: true.
	[Sensor anyButtonPressed] whileFalse:[
		time _ Time millisecondsToRun:[
			b3d transformBy: m1.
			b3d transformBy: m2.
			"Display fill: r fillColor: Color white."
			f displayAt: r origin.
			box renderOn: b3d].
		time printString displayAt: r origin.
		Display forceToScreen: r.
		"(Delay forMilliseconds: 100) wait"].
	Display deferUpdates: false.
	f displayAt: r origin.	! !!B3DBox class methodsFor: 'examples' stamp: 'ar 2/4/1999 17:53'!example3	"MessageTally spyOn:[B3DBox example3]"
	"Simple rotating box drawn with perspective"
	| box b3d p r m1 c time f max |
	p _ 0.1@0.1@0.1.
	box _ B3DBox from: p negated to: p.
	b3d _ B3DRenderEngine new.	b3d viewport: (0@0 corner: 800@600).
	c _ B3DCamera new.
	c nearDistance: 1.0.
	c renderOn: b3d.
	"b3d rotateBy: (B3DRotation x: 1 y: 0 z: 0 a: 45)."
	r _ 312@241 corner: 480@355.
	f _ Form fromDisplay: r.
	m1 _ (B3DRotation x: 0 y: 1 z: 0 a: 5) asMatrix4x4.
	Display deferUpdates: true.
	max _ 0.
	[Sensor anyButtonPressed] whileFalse:[
		time _ Time millisecondsToRun:[
			b3d transformBy: m1.
			f displayAt: r origin.
			box renderOn: b3d].
		max _ max max: time.
		time printString,'/',max printString displayAt: r origin.
		Display forceToScreen: r.
		"(Delay forMilliseconds: 100) wait"].
	Display deferUpdates: false.
	f displayAt: r origin.! !!B3DCamera methodsFor: 'accessing'!aspectRatio
	^perspective aspectRatio! !!B3DCamera methodsFor: 'accessing'!aspectRatio: aFloat
	^perspective aspectRatio: aFloat! !!B3DCamera methodsFor: 'accessing'!farDistance
	^perspective farDistance! !!B3DCamera methodsFor: 'accessing'!farDistance: aFloat
	^perspective farDistance: aFloat! !!B3DCamera methodsFor: 'accessing'!fieldOfView
	^perspective fieldOfView! !!B3DCamera methodsFor: 'accessing'!fieldOfView: aFloat
	^perspective fieldOfView: aFloat! !!B3DCamera methodsFor: 'accessing'!fov
	^self fieldOfView! !!B3DCamera methodsFor: 'accessing'!fov: aNumber
	self fieldOfView: aNumber! !!B3DCamera methodsFor: 'accessing'!nearDistance
	^perspective nearDistance! !!B3DCamera methodsFor: 'accessing'!nearDistance: aFloat
	^perspective nearDistance: aFloat! !!B3DCamera methodsFor: 'accessing'!perspective
	^perspective! !!B3DCamera methodsFor: 'accessing'!perspective: aPerspective
	perspective _ aPerspective! !!B3DCamera methodsFor: 'accessing'!position
	^position! !!B3DCamera methodsFor: 'accessing'!position: aVector
	position _ aVector! !!B3DCamera methodsFor: 'accessing'!target
	^target! !!B3DCamera methodsFor: 'accessing'!target: aVector
	target _ aVector! !!B3DCamera methodsFor: 'accessing'!up
	^up! !!B3DCamera methodsFor: 'accessing'!up: aVector
	up _ aVector! !!B3DCamera methodsFor: 'initialize'!from: positionVector to: targetVector up: upVector

	position := positionVector.
	target := targetVector.
	up := upVector.! !!B3DCamera methodsFor: 'initialize'!initialize
	position := B3DVector3 x: 0.0 y: 0.0 z: -1.0.
	target := B3DVector3 x: 0.0 y: 0.0 z: 0.0.
	up := B3DVector3 x: 0.0 y: 1.0 z: 0.0.
	perspective := B3DCameraPerspective new.
	self fov: 45.0.
	self aspectRatio: 1.0.
	self nearDistance: 0.0001.
	self farDistance: 10000.0.! !!B3DCamera methodsFor: 'rendering'!renderOn: aRenderer
	aRenderer
		lookFrom: self position
		to: self target
		up: self up.
	aRenderer
		perspective: self perspective.! !!B3DCamera class methodsFor: 'instance creation'!new
	^super new initialize! !!B3DCameraPerspective methodsFor: 'converting'!asFrustum
	^B3DViewingFrustum near: nearDistance far: farDistance fov: fieldOfView aspect: aspectRatio! !!B3DCameraPerspective methodsFor: 'converting'!asMatrix4x4
	^self asFrustum asPerspectiveMatrix! !!B3DCameraPerspective methodsFor: 'accessing'!aspectRatio
	^aspectRatio! !!B3DCameraPerspective methodsFor: 'accessing'!aspectRatio: aNumber
	aspectRatio _ aNumber! !!B3DCameraPerspective methodsFor: 'accessing'!farDistance
	^farDistance! !!B3DCameraPerspective methodsFor: 'accessing'!farDistance: aNumber
	farDistance _ aNumber! !!B3DCameraPerspective methodsFor: 'accessing'!fieldOfView
	^fieldOfView! !!B3DCameraPerspective methodsFor: 'accessing'!fieldOfView: aNumber
	fieldOfView _ aNumber! !!B3DCameraPerspective methodsFor: 'accessing'!nearDistance
	^nearDistance! !!B3DCameraPerspective methodsFor: 'accessing'!nearDistance: aNumber
	nearDistance _ aNumber! !!B3DEnginePart methodsFor: 'initialize'!initialize
! !!B3DEnginePart methodsFor: 'private'!setEngine: aB3DRenderEngine
	engine _ aB3DRenderEngine! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 04:20'!primProcessVB: vertexArray count: nVertices type: primitiveType
	"Process the entire vertexArray using nVertices of the given primitiveType.
	Return true if successful, false otherwise.
	Note: This is a shortcut for special primitive types but is not yet implemented."
	^false! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 04:21'!processLineLoop: vertexBuffer
	"Process a closed line defined by the vertex buffer"! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 04:21'!processLines: vertexBuffer
	"Process a series of lines defined by each two points the vertex buffer"! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 04:21'!processPoints: vertexBuffer
	"Process a series of points defined by the vertex buffer"! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 04:22'!processPolygon: vertexBuffer
	"Process a polygon defined by the vertex buffer"! !!B3DEnginePart methodsFor: 'processing' stamp: 'ar 2/4/1999 06:09'!processVertexBuffer: vb
	"Process the given vertex buffer in this part of the engine."
	(self primProcessVB: vb vertexArray count: vb vertexCount type: vb primitive) 
		ifFalse:[self perform: (PrimitiveActions at: vb primitive) with: vb].! !!B3DEnginePart class methodsFor: 'instance creation'!engine: aB3DRenderEngine
	^self new setEngine: aB3DRenderEngine! !!B3DEnginePart class methodsFor: 'instance creation'!new
	^super new initialize! !!B3DFloatArray methodsFor: 'accessing'!floatAt: index
	"For subclasses that override #at:"
	<primitive: 'primitiveFloatArrayAt'>
	^Float fromIEEE32Bit: (self basicAt: index)! !!B3DFloatArray methodsFor: 'accessing'!floatAt: index put: value
	"For subclasses that override #at:put:"
	<primitive: 'primitiveFloatArrayAtPut'>
	self basicAt: index put: value asIEEE32BitWord.
	^value! !!B3DFloatArray methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:23'!numElements
	^self class numElements! !!B3DFloatArray methodsFor: 'initialize'!loadFrom: srcObject
	self == srcObject ifTrue:[^self].
	self class == srcObject class
		ifTrue:[self replaceFrom: 1 to: self size with: srcObject startingAt: 1]
		ifFalse:[self privateLoadFrom: srcObject]! !!B3DFloatArray methodsFor: 'private'!privateLoadFrom: srcObject
	"Load the receiver from the given source object."
	self error:'Cannot load a ', srcObject class name,' into a ', self class name.! !!B3DColor4 commentStamp: '<historical>' prior: 0!I represent an RGBA color value in floating point format. I am used during the lighting and shading computations.!!B3DColor4 methodsFor: 'accessing'!alpha
	^self floatAt: 4! !!B3DColor4 methodsFor: 'accessing'!alpha: aNumber
	self floatAt: 4 put: aNumber! !!B3DColor4 methodsFor: 'accessing'!blue
	^self floatAt: 3! !!B3DColor4 methodsFor: 'accessing'!blue: aNumber
	self floatAt: 3 put: aNumber! !!B3DColor4 methodsFor: 'accessing'!green
	^self floatAt: 2! !!B3DColor4 methodsFor: 'accessing'!green: aNumber
	self floatAt: 2 put: aNumber! !!B3DColor4 methodsFor: 'accessing'!red
	^self floatAt: 1! !!B3DColor4 methodsFor: 'accessing'!red: aNumber
	self floatAt: 1 put: aNumber! !!B3DColor4 methodsFor: 'converting'!asColor
	^Color r: self red g: self green b: self blue alpha: self alpha! !!B3DColor4 methodsFor: 'converting' stamp: 'ar 2/4/1999 20:21'!pixelValue32
	^self asColor pixelWordForDepth: 32! !!B3DColor4 methodsFor: 'private'!privateLoadFrom: srcObject
	| color |
	color _ srcObject asColor.
	self red: color red.
	self green: color green.
	self blue: color blue.
	self alpha: color alpha.! !!B3DColor4Array commentStamp: '<historical>' prior: 0!I am an inplace storage area for B3DColor4 items used during lighting and shading.!!B3DColor4Array methodsFor: 'special ops' stamp: 'ar 2/4/1999 01:50'!+= aColor
	"Add the given color to all the elements in the receiver"
	| r g b a |
	r _ aColor red.
	g _ aColor green.
	b _ aColor blue.
	a _ aColor alpha.
	1 to: self basicSize by: 4 do:[:i|
		self floatAt: i put: (self floatAt: i) + r.
		self floatAt: i+1 put: (self floatAt: i+1) + g.
		self floatAt: i+2 put: (self floatAt: i+2) + b.
		self floatAt: i+3 put: (self floatAt: i+3) + a.
	].! !!B3DColor4Array methodsFor: 'special ops'!clampAllFrom: minValue to: maxValue
	"Clamp all elements in the receiver to be in the range (minValue, maxValue)"
	| value |
	1 to: self basicSize do:[:i|
		value _ self floatAt: i.
		value _ value min: maxValue.
		value _ value max: minValue.
		self floatAt: i put: value.
	].! !!B3DColor4Array methodsFor: 'special ops'!fillWith: anInteger
	<primitive: 145>
	self primitiveFailed! !!B3DColor4Array methodsFor: 'accessing'!at: index
	"Return the primitive vertex at the given index"
	| vtx |
	(index < 1 or:[index > self size]) ifTrue:[^self errorSubscriptBounds: index].
	vtx _ B3DColor4 new.
	vtx replaceFrom: 1 to: vtx size with: self startingAt: index-1*4+1.
	^vtx! !!B3DColor4Array methodsFor: 'accessing'!at: index put: aB3DColor4
	"Store the primitive vertex at the given index in the receiver"
	| idx |
	(index < 1 or:[index > self size]) ifTrue:[^self errorSubscriptBounds: index].
	idx _ index-1*4+1
	self replaceFrom: idx to: idx+4 with: aB3DColor4 startingAt: 1.
	^aB3DColor4! !!B3DColor4Array methodsFor: 'accessing'!size
	"Return the number of primitive vertices that can be stored in the receiver"
	^self basicSize // 4! !!B3DFloatArray class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:20'!new
	^super new: self numElements! !!B3DFloatArray class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!numElements
	^0! !!B3DColor4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:22'!numElements
	^4! !!B3DColor4Array class methodsFor: 'instance creation' stamp: 'ar 2/4/1999 01:59'!new: n
	^super new: 4*n! !!B3DBox methodsFor: 'displaying' stamp: 'ar 2/4/1999 20:21'!renderOn: aRenderer
	1 to: 6 do:[:i|
		aRenderer
			normal: (BoxNormals at: i);			color: (BoxColors at: i);
			drawPolygonAfter:[
				aRenderer
					vertex: (vertices at: ((BoxFaceIndexes at: i) at: 1));
					vertex: (vertices at: ((BoxFaceIndexes at: i) at: 2));
					vertex: (vertices at: ((BoxFaceIndexes at: i) at: 3));
					vertex: (vertices at: ((BoxFaceIndexes at: i) at: 4)).
			].
	].! !!B3DBox methodsFor: 'private'!buildBoxFrom: origin to: corner
	vertices := Array new: 8.
	1 to: 8 do:[:i| vertices at: i put: B3DVector3 new].

	(vertices at: 1) x: origin x.	(vertices at: 1) y: origin y.	(vertices at: 1) z: origin z.
	(vertices at: 2) x: origin x.	(vertices at: 2) y: origin y.	(vertices at: 2) z: corner z.
	(vertices at: 3) x: origin x.	(vertices at: 3) y: corner y.	(vertices at: 3) z: corner z.
	(vertices at: 4) x: origin x.	(vertices at: 4) y: corner y.	(vertices at: 4) z: origin z.
	(vertices at: 5) x: corner x.	(vertices at: 5) y: origin y.	(vertices at: 5) z: origin z.
	(vertices at: 6) x: corner x.	(vertices at: 6) y: origin y.	(vertices at: 6) z: corner z.
	(vertices at: 7) x: corner x.	(vertices at: 7) y: corner y.	(vertices at: 7) z: corner z.
	(vertices at: 8) x: corner x.	(vertices at: 8) y: corner y.	(vertices at: 8) z: origin z.
! !!B3DLightSource methodsFor: 'shading'!shadeVertexBuffer: vb with: aMaterial into: colorArray
	^self subclassResponsibility! !!B3DAmbientLight methodsFor: 'shading'!shadeVertexBuffer: vb with: aMaterial into: colorArray
	| color |
	color _ aMaterial ambientColor * ambient.
	colorArray += color.! !!B3DMatrix4x4 commentStamp: '<historical>' prior: 0!I represent a general 4x4 transformation matrix commonly used in computer graphics.!!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:26'!setBSplineBase
	"Set the receiver to the BSpline base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0 / 6.0;	a12: 3.0 / 6.0;	a13: -3.0 / 6.0;	a14: 1.0 / 6.0;
		a21: 3.0 / 6.0;	a22: -6.0 / 6.0;	a23: 3.0 / 6.0;	a24: 0.0 / 6.0;
		a31: -3.0 / 6.0;	a32: 0.0 / 6.0;	a33: 3.0 / 6.0;	a34: 0.0 / 6.0;
		a41: 1.0 / 6.0;	a42: 4.0 / 6.0;	a43: 1.0 / 6.0;	a44: 0.0 / 6.0
! !!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:26'!setBetaSplineBaseBias: beta1 tension: beta2
	"Set the receiver to the betaSpline base matrix 
	if beta1=1 and beta2=0 then the bSpline base matrix will be returned"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	| b12 b13 delta |
	b12 := beta1 * beta1.
	b13 := beta1 * b12.
	delta := 1.0 / (beta2 + (2.0 * b13) + 4.0 * (b12 + beta1) +2.0).
	
	self
		a11: delta * -2.0 * b13;
		a12: delta * 2.0 * (beta2 + b13 + b12 + beta1);
		a13: delta * -2.0 * (beta2 + b12 + beta1 + 1.0);
		a14: delta * 2.0;
		a21: delta * 6.0 * b13;
		a22: delta * -3.0 * (beta2 + (2.0 * (b13 + b12)));
		a23: delta * 3.0 * (beta2 + (2.0 * b12));
		a24: 0.0;
		a31: delta * -6.0 * b13;
		a32: delta * 6.0 * (b13 - beta1);
		a33: delta * 6.0 * beta1;
		a34: 0.0;
		a41: delta * 2.0 * b13;
		a42: delta * (beta2 + 4.0 * (b12 + beta1));
		a43: delta * 2.0;
		a44: 0.0
! !!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!setBezierBase
	"Set the receiver to the bezier base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 3.0;		a22: -6.0;	a23: 3.0;	a24: 0.0;
		a31: -3.0;	a32: 3.0;	a33: 0.0;	a34: 0.0;
		a41: 1.0;		a42: 0.0;	a43: 0.0;	a44: 0.0! !!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!setCardinalBase
	"Set the receiver to the cardinal spline base matrix - just catmull * 2"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -1.0;		a12: 3.0;		a13: -3.0;	a14: 1.0;
		a21: 2.0;		a22: -5.0;	a23: 4.0;	a24: -1.0;
		a31: -1.0;	a32: 0.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 2.0;	a43: 0.0;	a44: 0.0
! !!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!setCatmullBase
	"Set the receiver to the Catmull-Rom base matrix"
	"for further information see:
		Foley, van Dam, Feiner, Hughes
		'Computer Graphics: Principles and Practice'
		Addison-Wesley Publishing Company
		Second Edition, pp. 505"
	self
		a11: -0.5;	a12: 1.5;		a13: -1.5;	a14: 0.5;
		a21: 1.0;		a22: -2.5;	a23: 2.0;	a24: -0.5;
		a31: -0.5;	a32: 0.0;	a33: 0.5;	a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !!B3DMatrix4x4 methodsFor: 'initialize'!setIdentity
	"Set the receiver to the identity matrix"
	self loadFrom: B3DIdentityMatrix! !!B3DMatrix4x4 methodsFor: 'initialize' stamp: 'ar 2/1/1999 21:27'!setPolylineBase
	"Set the receiver to the polyline base matrix :)"
	self
		a11: 0.0;		a12: 0.0;		a13: 0.0;		a14: 0.0;
		a21: 0.0;		a22: 0.0;	a23: 0.0;	a24: 0.0;
		a31: 0.0;		a32: -1.0;	a33: 1.0;		a34: 0.0;
		a41: 0.0;		a42: 1.0;		a43: 0.0;	a44: 0.0
! !!B3DMatrix4x4 methodsFor: 'initialize'!setTranslation: aVector
	self 
		a14: aVector x;
		a24: aVector y;
		a34: aVector z! !!B3DMatrix4x4 methodsFor: 'initialize'!setZero
	"Set the receiver to the zero matrix"
	self loadFrom: B3DZeroMatrix! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a11
	"Return the element a11"
	^self at: 1! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a11: aNumber
	"Store the element a11"
	self at: 1 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a12
	"Return the element a12"
	^self at: 2! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a12: aNumber
	"Store the element a12"
	self at: 2 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a13
	"Return the element a13"
	^self at: 3! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a13: aNumber
	"Store the element a13"
	self at: 3 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a14
	"Return the element a14"
	^self at: 4! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a14: aNumber
	"Store the element a14"
	self at: 4 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a21
	"Return the element a21"
	^self at: 5! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a21: aNumber
	"Store the element a21"
	self at: 5 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a22
	"Return the element a22"
	^self at: 6! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a22: aNumber
	"Store the element a22"
	self at: 6 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a23
	"Return the element a23"
	^self at: 7! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a23: aNumber
	"Store the element a23"
	self at: 7 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a24
	"Return the element a24"
	^self at: 8! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a24: aNumber
	"Store the element a24"
	self at: 8 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a31
	"Return the element a31"
	^self at: 9! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a31: aNumber
	"Store the element a31"
	self at: 9 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a32
	"Return the element a32"
	^self at: 10! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a32: aNumber
	"Store the element a32"
	self at: 10 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a33
	"Return the element a33"
	^self at: 11! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a33: aNumber
	"Store the element a33"
	self at: 11 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a34
	"Return the element a34"
	^self at: 12! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a34: aNumber
	"Store the element a34"
	self at: 12 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a41
	"Return the element a41"
	^self at: 13! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a41: aNumber
	"Store the element a41"
	self at: 13 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a42
	"Return the element a42"
	^self at: 14! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a42: aNumber
	"Store the element a42"
	self at: 14 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a43
	"Return the element a43"
	^self at: 15! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a43: aNumber
	"Store the element a43"
	self at: 15 put: aNumber! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:29'!a44
	"Return the element a44"
	^self at: 16! !!B3DMatrix4x4 methodsFor: 'element-access' stamp: 'ar 2/1/1999 21:28'!a44: aNumber
	"Store the element a44"
	self at: 16 put: aNumber! !!B3DMatrix4x4 methodsFor: 'accessing'!rotation: anAngle around: aVector3
	"set up a rotation matrix around the direction aVector3"

	self loadFrom: (B3DRotation angle: anAngle axis: aVector3) asMatrix4x4! !!B3DMatrix4x4 methodsFor: 'accessing'!rotation: anAngle aroundX: xValue y: yValue z: zValue
	"set up a rotation matrix around the direction x/y/z"
	^self rotation: anAngle around:(B3DVector3 with: xValue with: yValue with: zValue)! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:34'!rotationAroundX: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a22: c.
	self a23: s negated.
	self a33: c.
	self a32: s.
	^self! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:34'!rotationAroundY: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a13: s.
	self a33: c.
	self a31: s negated.
	^self! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:35'!rotationAroundZ: anAngle
	| rad s c |
	rad := anAngle degreesToRadians.
	s := rad sin.
	c := rad cos.
	self a11: c.
	self a12: s negated.
	self a22: c.
	self a21: s.
	^self! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:35'!scaling: aVector

	^self scalingX: aVector x y: aVector y z: aVector z! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:35'!scalingX: xValue y: yValue z: zValue

	self a11: xValue.
	self a22: yValue.
	self a33: zValue.
	^self! !!B3DMatrix4x4 methodsFor: 'accessing'!translation

	^(B3DVector3 x: self a14 y: self a24 z: self a34)! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:36'!translation: aVector

	^self translationX: aVector x y: aVector y z: aVector z! !!B3DMatrix4x4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:36'!translationX: xValue y: yValue z: zValue

	self a14: xValue.
	self a24: yValue.
	self a34: zValue.
	^self! !!B3DMatrix4x4 methodsFor: 'arithmetic' stamp: 'ar 2/1/1999 21:39'!+ aB3DMatrix
	"Optimized for Matrix/Matrix operations"
	<primitive: 'primitiveFloatArrayAddFloatArray'>
	^super + aB3DMatrix! !!B3DMatrix4x4 methodsFor: 'arithmetic' stamp: 'ar 2/1/1999 21:39'!- aB3DMatrix
	"Optimized for Matrix/Matrix operations"
	<primitive: 'primitiveFloatArraySubFloatArray'>
	^super - aB3DMatrix! !!B3DMatrix4x4 methodsFor: 'transforming'!inverseTransformation
	"Return the inverse matrix of the receiver. The following method is probably the slowest one can think of - but I need a matrix inverter NOW!!"
	| e1 e2 e3 e4 |
	e1 := self solve: (B3DVector4 x:1 y:0 z:0 w:0).
	e2 := self solve: (B3DVector4 x:0 y:1 z:0 w:0).
	e3 := self solve: (B3DVector4 x:0 y:0 z:1 w:0).
	e4 := self solve: (B3DVector4 x:0 y:0 z:0 w:1).
	^(B3DMatrix4x4 new) 
		a11: e1 x; 	a12: e2 x; 	a13: e3 x; 	a14: e4 x;
		a21: e1 y; 	a22: e2 y; 	a23: e3 y; 	a24: e4 y;
		a31: e1 z; 	a32: e2 z; 	a33: e3 z; 	a34: e4 z;
		a41: e1 w; 	a42: e2 w; 	a43: e3 w; 	a44: e4 w;
		yourself! !!B3DMatrix4x4 methodsFor: 'transforming' stamp: 'ar 2/1/1999 21:42'!transposed
	"Return a transposed copy of the receiver"
	| matrix |
	matrix := self class new.
	matrix 
		a11: self a11; a12: self a21; a13: self a31; a14: self a41;
		a21: self a12; a22: self a22; a23: self a32; a24: self a42;
		a31: self a13; a32: self a23; a33: self a33; a34: self a43;
		a41: self a14; a42: self a24; a43: self a34; a44: self a44.
	^matrix! !!B3DMatrix4x4 methodsFor: 'double dispatching' stamp: 'ar 2/1/1999 21:49'!printOn: aStream
	"Print the receiver on aStream"
	1 to: 4 do:[:r|
		1 to: 4 do:[:c| 
			(self at: r-1*4+c) printOn: aStream.
			aStream nextPut: Character space].
		(r < 4) ifTrue:[aStream nextPut: Character cr]].! !!B3DMatrix4x4 methodsFor: 'double dispatching' stamp: 'ar 2/1/1999 21:46'!productFromMatrix4x4: matrix
	"Multiply a 4x4 matrix with the receiver."
	| result |
	result := self class new new.
	result a11: ((matrix a11 * self a11) + (matrix a12 * self a21) + 
				(matrix a13 * self a31) + (matrix a14 * self a41)).
	result a12: ((matrix a11 * self a12) + (matrix a12 * self a22) + 
				(matrix a13 * self a32) + (matrix a14 * self a42)).
	result a13: ((matrix a11 * self a13) + (matrix a12 * self a23) + 
				(matrix a13 * self a33) + (matrix a14 * self a43)).
	result a14: ((matrix a11 * self a14) + (matrix a12 * self a24) + 
				(matrix a13 * self a34) + (matrix a14 * self a44)).

	result a21: ((matrix a21 * self a11) + (matrix a22 * self a21) + 
				(matrix a23 * self a31) + (matrix a24 * self a41)).
	result a22: ((matrix a21 * self a12) + (matrix a22 * self a22) + 
				(matrix a23 * self a32) + (matrix a24 * self a42)).
	result a23: ((matrix a21 * self a13) + (matrix a22 * self a23) + 
				(matrix a23 * self a33) + (matrix a24 * self a43)).
	result a24: ((matrix a21 * self a14) + (matrix a22 * self a24) + 
				(matrix a23 * self a34) + (matrix a24 * self a44)).

	result a31: ((matrix a31 * self a11) + (matrix a32 * self a21) + 
				(matrix a33 * self a31) + (matrix a34 * self a41)).
	result a32: ((matrix a31 * self a12) + (matrix a32 * self a22) + 
				(matrix a33 * self a32) + (matrix a34 * self a42)).
	result a33: ((matrix a31 * self a13) + (matrix a32 * self a23) + 
				(matrix a33 * self a33) + (matrix a34 * self a43)).
	result a34: ((matrix a31 * self a14) + (matrix a32 * self a24) + 
				(matrix a33 * self a34) + (matrix a34 * self a44)).

	result a41: ((matrix a41 * self a11) + (matrix a42 * self a21) + 
				(matrix a43 * self a31) + (matrix a44 * self a41)).
	result a42: ((matrix a41 * self a12) + (matrix a42 * self a22) + 
				(matrix a43 * self a32) + (matrix a44 * self a42)).
	result a43: ((matrix a41 * self a13) + (matrix a42 * self a23) + 
				(matrix a43 * self a33) + (matrix a44 * self a43)).
	result a44: ((matrix a41 * self a14) + (matrix a42 * self a24) + 
				(matrix a43 * self a34) + (matrix a44 * self a44)).

	^result! !!B3DMatrix4x4 methodsFor: 'double dispatching'!productFromVector3: aVector3
	"Multiply aVector (temporarily converted to 4D) with the receiver"
	| x y z rx ry rz rw |
	x := aVector3 x.
	y := aVector3 y.
	z := aVector3 z.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + self a41.
	ry := (x * self a12) + (y * self a22) + (z * self a32) + self a42.
	rz := (x * self a13) + (y * self a23) + (z * self a33) + self a43.
	rw := (x * self a14) + (y * self a24) + (z * self a34) + self a44.

	^B3DVector3 x:(rx/rw) y: (ry/rw) z: (rz/rw)! !!B3DMatrix4x4 methodsFor: 'double dispatching'!productFromVector4: aVector4
	"Multiply aVector with the receiver"
	| x y z w rx ry rz rw |
	x := aVector4 x.
	y := aVector4 y.
	z := aVector4 z.
	w := aVector4 w.

	rx := (x * self a11) + (y * self a21) + (z * self a31) + (w * self a41).
	ry := (x * self a12) + (y * self a22) + (z * self a32) + (w * self a42).
	rz := (x * self a13) + (y * self a23) + (z * self a33) + (w * self a43).
	rw := (x * self a14) + (y * self a24) + (z * self a34) + (w * self a44).

	^B3DVector4 x:rx y: ry z: rz w: rw! !!B3DMatrix4x4 methodsFor: 'solving' stamp: 'ar 2/1/1999 21:50'!inplaceDecomposeLU
	"Decompose the receiver in place by using gaussian elimination w/o pivot search"
	| x |
	1 to: 4 do:[:j|
		"i-th equation (row)"
		j+1 to: 4 do:[:i|
			x := (self at: i at: j) / (self at: j at: j).
			j to: 4 do:[:k|
				self at: i at: k put: (self at: i at: k) - ((self at: j at: k) * x)].
			self at: i at: j put: x]].
! !!B3DMatrix4x4 methodsFor: 'solving'!inplaceHouseHolderTransform: aVector
	"Solve the linear equation self * aVector = x by using HouseHolder's transformation.
	Note: This scheme is numerically better than using gaussian elimination even though it takes
	somewhat longer"
	| d x sigma beta sum s|
	x := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	d := Array new: 4.
	1 to: 4 do:[:j|
		sigma := 0.0.
		j to: 4 do:[:i| sigma := sigma + ((self at: i at: j) squared)].
		sigma isZero ifTrue:[^nil]. "matrix is singular"
		((self at: j at: j) < 0.0) 
			ifTrue:[ s:= d at: j put: (sigma sqrt)]
			ifFalse:[ s:= d at: j put: (sigma sqrt negated)].
		beta := 1.0 / ( s * (self at: j at: j) - sigma).
		self at: j at: j put: ((self at: j at: j) - s).
		"update remaining columns"
		j+1 to: 4 do:[:k|
			sum := 0.0.
			j to: 4 do:[:i| sum := sum + ((self at: i at: j) * (self at: i at: k))].
			sum := sum * beta.
			j to: 4 do:[:i| 
				self at: i at: k put: ((self at: i at: k) + ((self at: i at: j) * sum))]].
		"update vector"
		sum := nil.
		j to: 4 do:[:i| 
			sum := sum isNil 
				ifTrue:[(x at: i) * (self at: i at: j)] 
				ifFalse:[sum + ((x at: i) * (self at: i at: j))]].
		sum := sum * beta.
		j to: 4 do:[:i| 
			x at: i put:((x at: i) + (sum * (self at: i at: j)))].
	].
	"Now calculate result"
	4 to: 1 by: -1 do:[:i|
		i+1 to: 4 do:[:j|
			x at: i put: ((x at: i) - ((x at: j) * (self at: i at: j))) ].
		x at: i put: ((x at: i) / (d at: i))].
	^B3DVector4 x: (x at: 1) y: (x at: 2) z: (x at: 3) w: (x at: 4)
! !!B3DMatrix4x4 methodsFor: 'solving' stamp: 'ar 2/1/1999 21:52'!solve: aVector

	^self clone inplaceHouseHolderTransform: aVector
	"or:
	^self clone inplaceDecomposeLU solveLU: aVector
	"! !!B3DMatrix4x4 methodsFor: 'solving'!solveLU: aVector
	"Given a decomposed matrix using gaussian elimination solve the linear equations."
	| x v |
	v := Array with: aVector x with: aVector y with: aVector z with: aVector w.
	"L first"
	1 to: 4 do:[:i| "Top to bottom"
		x := 0.0.
		1 to: i-1 do:[:j|
			"From left to right w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"No need to divide by the diagonal element - this is always 1.0 in L"
		v at: i put: (v at: i) - x].
	"Now U"
	4 to: 1 by: -1 do:[:i| "Bottom to top"
		x := 0.0.
		4 to: i+1 by: -1 do:[:j|
			"From right to left w/o diagonal element"
			x := x + ((v at: j) * (self at: i at: j))].
		"Divide by diagonal element"
		v at: i put: (v at: i) - x / (self at: i at: i)].
	^B3DVector4 x: (v at: 1) y: (v at: 2) z: (v at: 3) w: (v at: 4)
! !!B3DMatrix4x4 methodsFor: 'comparing' stamp: 'ar 2/1/1999 21:53'!squaredErrorDistanceTo: anotherMatrix
	| result temp |
	result := self - anotherMatrix.
	temp := 0.
	1 to: 4 do: [:i | 1 to: 4 do: [:j| temp := temp + ((result at: i-1*4+j) squared)]].
	^temp sqrt.! !!B3DMatrix4x4 methodsFor: 'testing' stamp: 'ar 2/1/1999 21:54'!isIdentity
	^self = B3DIdentityMatrix! !!B3DMatrix4x4 methodsFor: 'testing' stamp: 'ar 2/1/1999 21:54'!isZero
	^self = B3DZeroMatrix! !!B3DMatrix4x4 methodsFor: 'converting'!asMatrix4x4
	^self! !!B3DMatrix4x4 class methodsFor: 'class initialization' stamp: 'ar 2/1/1999 21:58'!initialize
	"B3DMatrix4x4 initialize"
	B3DZeroMatrix _ self new.
	B3DIdentityMatrix _ self new.
	B3DIdentityMatrix a11: 1.0; a22: 1.0; a33: 1.0; a44: 1.0.! !!B3DMatrix4x4 class methodsFor: 'instance creation'!identity
	^self new setIdentity! !!B3DMatrix4x4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:25'!numElements
	^16! !!B3DMatrix4x4 class methodsFor: 'instance creation'!zero
	^self new! !!B3DMorph methodsFor: 'initialize' stamp: 'ar 2/4/1999 20:19'!initialize	super initialize.	geometry _ B3DBox from: (-0.7@-0.7@-0.7) to: (0.7@0.7@0.7).	camera _ B3DCamera new.	camera position: 0@0@-2.	self extent: 100@100.	angle _ 0.! !!B3DMorph methodsFor: 'drawing' stamp: 'ar 2/4/1999 20:13'!drawOn: aCanvas	aCanvas asBalloonCanvas render: self.! !!B3DMorph methodsFor: 'drawing' stamp: 'ar 2/4/1999 20:19'!renderOn: aRenderer	camera ifNotNil:[		aRenderer viewport: self bounds.		aRenderer loadIdentity.		camera renderOn: aRenderer].	aRenderer transformBy: (B3DRotation angle: angle axis: 0@1@0).	geometry ifNotNil:[geometry renderOn: aRenderer].! !!B3DMorph methodsFor: 'stepping' stamp: 'ar 2/4/1999 20:15'!step	angle _ angle + 5.	self changed.! !!B3DMorph methodsFor: 'stepping' stamp: 'ar 2/4/1999 20:15'!stepTime	^50! !!B3DMorph methodsFor: 'stepping' stamp: 'ar 2/4/1999 20:15'!wantsSteps	^true! !!B3DPrimitiveVertex commentStamp: '<historical>' prior: 0!I represent all per vertex information used in Balloon 3D primitive operations. I store either 32bit floats or integers depending on what is requested.

	typedef struct B3DPrimitiveVertex {
		float position[3];
		float normal[3];
		float texCoord[2];
		float rasterPos[4];
		int pixelValue32;
	} B3DPrimitiveVertex;!!B3DPrimitiveVertex methodsFor: 'accessing'!color
	^self pixelValue32 asColorOfDepth: 32! !!B3DPrimitiveVertex methodsFor: 'accessing' stamp: 'ar 2/4/1999 20:21'!color: aColor
	self pixelValue32: (aColor asColor pixelWordForDepth: 32)! !!B3DPrimitiveVertex methodsFor: 'accessing'!floatAt: index
	<primitive:'primitiveFloatArrayAt'>
	^self primitiveFailed! !!B3DPrimitiveVertex methodsFor: 'accessing'!floatAt: index put: value
	<primitive:'primitiveFloatArrayAtPut'>
	^self primitiveFailed! !!B3DPrimitiveVertex methodsFor: 'accessing'!normal
	^B3DVector3 
		x: (self floatAt: 4) 
		y: (self floatAt: 5) 
		z: (self floatAt: 6)! !!B3DPrimitiveVertex methodsFor: 'accessing'!normal: aVector
	self floatAt: 4 put: aVector x.
	self floatAt: 5 put: aVector y.
	self floatAt: 6 put: aVector z.
! !!B3DPrimitiveVertex methodsFor: 'accessing'!pixelValue32
	^self wordAt: 13! !!B3DPrimitiveVertex methodsFor: 'accessing'!pixelValue32: aNumber
	self wordAt: 13 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'accessing'!position
	^B3DVector3 
		x: (self floatAt: 1) 
		y: (self floatAt: 2) 
		z: (self floatAt: 3)! !!B3DPrimitiveVertex methodsFor: 'accessing'!position: aVector
	self floatAt: 1 put: aVector x.
	self floatAt: 2 put: aVector y.
	self floatAt: 3 put: aVector z.
! !!B3DPrimitiveVertex methodsFor: 'accessing'!rasterPos
	^B3DVector4
		x: (self floatAt: 9) 
		y: (self floatAt: 10) 
		z: (self floatAt: 11)
		w: (self floatAt: 12)! !!B3DPrimitiveVertex methodsFor: 'accessing'!rasterPos: aVector
	self floatAt: 9 put: aVector x.
	self floatAt: 10 put: aVector y.
	self floatAt: 11 put: aVector z.
	self floatAt: 12 put: aVector w.! !!B3DPrimitiveVertex methodsFor: 'accessing'!texCoords
	^(self floatAt: 7) @ (self floatAt: 8)! !!B3DPrimitiveVertex methodsFor: 'accessing'!texCoords: aVector
	self floatAt: 7 put: aVector u.
	self floatAt: 8 put: aVector v.
! !!B3DPrimitiveVertex methodsFor: 'accessing'!wordAt: index
	<primitive: 60>
	^self primitiveFailed! !!B3DPrimitiveVertex methodsFor: 'accessing'!wordAt: index put: value
	<primitive: 61>
	^self primitiveFailed! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionX
	^self floatAt: 1! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionX: aNumber
	self floatAt: 1 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionY
	^self floatAt: 2! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionY: aNumber
	self floatAt: 2 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionZ
	^self floatAt: 3! !!B3DPrimitiveVertex methodsFor: 'transform-support'!positionZ: aNumber
	self floatAt: 3 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosW
	^self floatAt: 12! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosW: aNumber
	self floatAt: 12 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosX
	^self floatAt: 9! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosX: aNumber
	self floatAt: 9 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosY
	^self floatAt: 10! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosY: aNumber
	self floatAt: 10 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosZ
	^self floatAt: 11! !!B3DPrimitiveVertex methodsFor: 'transform-support'!rasterPosZ: aNumber
	self floatAt: 11 put: aNumber! !!B3DPrimitiveVertex methodsFor: 'private'!privateReplaceFrom: start to: stop with: replacement startingAt: repStart 
	<primitive: 105>
	start to: stop do:[:i|
		self basicAt: i put: (replacement basicAt: i - start + repStart).
	].! !!B3DPrimitiveVertex class methodsFor: 'instance creation'!new
	^self new: 13! !!B3DPrimitiveVertexArray methodsFor: 'accessing'!at: index
	"Return the primitive vertex at the given index"
	| vtx |
	(index < 1 or:[index > self size]) ifTrue:[^self errorSubscriptBounds: index].
	vtx _ B3DPrimitiveVertex new.
	vtx privateReplaceFrom: 1 to: vtx size with: self startingAt: index-1*B3DPrimitiveVertexSize+1.
	^vtx! !!B3DPrimitiveVertexArray methodsFor: 'accessing'!at: index put: aB3DPrimitiveVertex
	"Store the primitive vertex at the given index in the receiver"
	| idx |
	(index < 1 or:[index > self size]) ifTrue:[^self errorSubscriptBounds: index].
	idx _ index-1*B3DPrimitiveVertexSize.
	self privateReplaceFrom: idx+1 to: idx+B3DPrimitiveVertexSize with: aB3DPrimitiveVertex startingAt: 1.
	^aB3DPrimitiveVertex! !!B3DPrimitiveVertexArray methodsFor: 'accessing'!size
	"Return the number of primitive vertices that can be stored in the receiver"
	^self basicSize // B3DPrimitiveVertexSize! !!B3DPrimitiveVertexArray methodsFor: 'private'!privateReplaceFrom: start to: stop with: replacement startingAt: repStart 
	<primitive: 105>
	start to: stop do:[:i|
		self basicAt: i put: (replacement at: i - start + repStart).
	].! !!B3DPrimitiveVertexArray class methodsFor: 'instance creation'!new: n
	^super new: (n * B3DPrimitiveVertexSize)! !!B3DPrimitiveVertexArray class methodsFor: 'class initialization'!initialize
	"B3DPrimitiveVertexArray initialize"
	B3DPrimitiveVertexSize _ B3DPrimitiveVertex new basicSize.
! !!B3DRasterizer methodsFor: 'initialize' stamp: 'ar 2/4/1999 20:17'!canvas: aCanvas	"Set the 2D output device"! !!B3DRasterizer methodsFor: 'initialize'!initialize! !!B3DRasterizer methodsFor: 'accessing'!viewport
	^viewport! !!B3DRasterizer methodsFor: 'accessing'!viewport: aRectangle
	viewport _ B3DViewport origin: aRectangle origin corner: aRectangle corner! !!B3DRasterizer methodsFor: 'testing'!needsClip
	"Return true if we need to clip polygons before rasterization.
	Generally, this should not be the case."
	^self subclassResponsibility! !!B3DRenderEngine methodsFor: 'attributes'!color
	^vertexBuffer color! !!B3DRenderEngine methodsFor: 'attributes'!color: aColor
	^vertexBuffer color: aColor! !!B3DRenderEngine methodsFor: 'attributes'!normal
	^vertexBuffer normal! !!B3DRenderEngine methodsFor: 'attributes'!normal: aVector
	^vertexBuffer normal: aVector! !!B3DRenderEngine methodsFor: 'attributes'!texCoords
	^vertexBuffer texCoords! !!B3DRenderEngine methodsFor: 'attributes'!texCoords: aVector
	^vertexBuffer texCoords: aVector! !!B3DRenderEngine methodsFor: 'attributes'!vertex
	^vertexBuffer vertex! !!B3DRenderEngine methodsFor: 'attributes'!vertex: aVector
	^vertexBuffer vertex: aVector.! !!B3DRenderEngine methodsFor: 'attributes' stamp: 'ar 2/4/1999 17:52'!viewport	^rasterizer viewport! !!B3DRenderEngine methodsFor: 'attributes' stamp: 'ar 2/4/1999 17:52'!viewport: aRect	^rasterizer viewport: aRect! !!B3DRenderEngine methodsFor: 'draw primitives' stamp: 'ar 2/2/1999 23:57'!drawPolygonAfter: aBlock
	vertexBuffer reset.
	aBlock value.
	self renderPrimitive.! !!B3DRenderEngine methodsFor: 'draw primitives' stamp: 'ar 2/2/1999 23:56'!drawPolygonMesh: aB3DPolygonMesh
	"Draw a generic polygon mesh"
	| hasVtxNormals hasTexCoords hasVtxColors |
	aB3DPolygonMesh polygonsDo:[:poly|
		hasVtxNormals _ poly hasVertexNormals.
		hasTexCoords _ poly hasTextureCoords.
		hasVtxColors _ poly hasVertexColors.
		"Set the normal of the polygon if we don't have normals per vertex"
		hasVtxNormals 
			ifFalse:[self normal: poly normal].
		self drawPolygonAfter:[
			poly verticesDo:[:vtx|
				hasVtxColors ifTrue:[self color: (poly colorOfVertex: vtx)].
				hasVtxNormals ifTrue:[self normal: (poly normalOfVertex: vtx)].
				hasTexCoords ifTrue:[self texCoord: (poly texCoordOfVertex: vtx)].
				self vertex: vtx.
			].
		].
	].! !!B3DRenderEngine methodsFor: 'draw primitives' stamp: 'ar 2/4/1999 20:16'!render: anObject	anObject renderOn: self.! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/4/1999 04:26'!privateClipVB: vb
	"Clip the objects in the vertex buffer.
	@@: Where do we get the primitive information?!!"
	^clipper processVertexBuffer: vb! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/2/1999 19:39'!privateNeedsClipVB: visibleFlag
	"Determine if a vertex buffer with the given visibility flag must be clipped.
	Return false if either visibleFlag == true (meaning the vertex buffer is completely inside the view frustum) or the rasterizer can clip by itself (it usually can)."
	^visibleFlag ~~ true and:[rasterizer needsClip]! !!B3DRenderEngine methodsFor: 'private-rendering'!privateNeedsShadingVB
	"Return true if the objects in the vertex buffer needs separate shading.
	This is determined by checking if
		a) lighting is enabled
		b) at least one light exists
		c) at least one material exists
	"
	^true! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/4/1999 04:26'!privateRasterizeVB: vb
	"Rasterize the current primitive from the vertex buffer."
	^rasterizer processVertexBuffer: vb! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/4/1999 04:26'!privateShadeVB: vb
	"Shade all the vertices in the vertex buffer using selected materials and lights"
	^shader processVertexBuffer: vb! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/4/1999 04:26'!privateTransformVB: vb
	"Transform the contents of the vertex buffer.
	Transforming may include normals (if lighting enabled) and textures (if textures enabled)."
	^transformer processVertexBuffer: vb! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/2/1999 19:31'!privateVisibleVB: vb
	"Return the visibility of the objects in the vertex buffer.
	Return:
		true - if completely inside view frustum
		false - if completely outside view frustum
		nil - if partly inside/outside view frustum
	"! !!B3DRenderEngine methodsFor: 'private-rendering' stamp: 'ar 2/4/1999 03:39'!renderPrimitive
	"This is the main rendering loop for all operations"
	| visible |
	"Step 1: Check if the mesh is visible at all"
	visible _ self privateVisibleVB: vertexBuffer.
	visible == false ifTrue:[^self].

	"Step 2: Transform vertices, normals, texture coords of the mesh"
	self privateTransformVB: vertexBuffer.

	"Step 3: Light the vertices of the mesh."
	self privateNeedsShadingVB
		ifTrue:[self privateShadeVB: vertexBuffer].

	"Step 4: Clip the mesh if necessary"
	(self privateNeedsClipVB: visible)
		ifTrue:[self privateClipVB: vertexBuffer].

	"Step 5: Rasterize the mesh"
	self privateRasterizeVB: vertexBuffer.! !!B3DRenderEngine methodsFor: 'transforming' stamp: 'ar 2/4/1999 20:18'!loadIdentity	^transformer loadIdentity! !!B3DRenderEngine methodsFor: 'transforming'!lookFrom: position to: target up: upDirection
	^transformer lookFrom: position to: target up: upDirection! !!B3DRenderEngine methodsFor: 'transforming'!perspective: aPerspective
	^transformer perspective: aPerspective! !!B3DRenderEngine methodsFor: 'transforming'!rotateBy: aRotation
	^transformer rotateBy: aRotation! !!B3DRenderEngine methodsFor: 'transforming'!transformBy: aTransformation
	^transformer transformBy: aTransformation! !!B3DRenderEngine methodsFor: 'transforming' stamp: 'ar 2/4/1999 03:56'!translateBy: aVector
	^transformer translateBy: aVector! !!B3DRenderEngine methodsFor: 'initialize' stamp: 'ar 2/4/1999 20:16'!canvas: aCanvas	"Set the 2D output device"	rasterizer canvas: aCanvas.! !!B3DRenderEngine methodsFor: 'initialize' stamp: 'ar 2/4/1999 04:28'!initialize
	vertexBuffer _ B3DVertexBuffer new.
	transformer _ B3DVertexTransformer engine: self.
	shader _ B3DVertexShader engine: self.
	clipper _ B3DClipper engine: self.
	rasterizer _ B3DSqueakFormRasterizer engine: self.! !!B3DRenderEngine class methodsFor: 'instance creation'!new
	^super new initialize! !!B3DRotation commentStamp: '<historical>' prior: 0!I represent general 3d rotations by using Unit-Quaternions. Unit-Quaternions are one of the best available representation for rotations in computer graphics because they provide an easy way of doing arithmetic with them and also because they allow us to use spherical linear interpolation (so-called "slerps") of rotations.

Indexed Variables:
	a	<Float>	the real part of the quaternion
	b	<Float>	the first imaginary part of the quaternion
	c	<Float>	the second imaginary part of the quaternion
	d	<Float>	the third imaginary part of the quaternion

!!B3DRotation methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:02'!a: aValue b: bValue c: cValue d: dValue

	self a: aValue.
	self b: bValue.
	self c: cValue.
	self d: dValue.
	(aValue < 0.0) ifTrue:[self *= -1.0].
	self normalize.! !!B3DRotation methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:02'!angle: anAngle axis: aVector3

	self radiansAngle: anAngle degreesToRadians axis: aVector3
! !!B3DRotation methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:02'!from: startVector to: endVector
	"Create a rotation from startVector to endVector"
	| axis cos sin |
	axis := startVector cross: endVector.
	cos := (startVector dot: endVector) arcCos.
	sin := axis length.
	axis safelyNormalize.
	self a: cos b: axis x * sin c: axis y * sin d: axis z * sin. ! !!B3DRotation methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:03'!radiansAngle: anAngle axis: aVector3

	| angle sin cos |
	angle := anAngle / 2.0.
	cos := angle cos.
	sin := angle sin.
	self a: cos b: aVector3 x * sin c: aVector3 y * sin d: aVector3 z * sin.! !!B3DRotation methodsFor: 'initialize'!setIdentity
	^self loadFrom: B3DIdentityRotation! !!B3DRotation methodsFor: 'initialize' stamp: 'ar 2/1/1999 22:03'!x: xValue y: yValue z: zValue a: anAngle

	| angle sin cos |
	angle := (anAngle degreesToRadians) / 2.0.
	cos := angle cos.
	sin := angle sin.
	self a: cos b: xValue * sin c: yValue * sin d: zValue * sin! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!a
	^self at: 1! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!a: aFloat
	self at: 1 put: aFloat! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:04'!angle
	^(self a arcCos * 2.0 radiansToDegrees)! !!B3DRotation methodsFor: 'accessing'!angle: newAngle
	self angle: newAngle axis: self axis! !!B3DRotation methodsFor: 'accessing'!axis
	| sinAngle |
	sinAngle := self a arcCos sin.
	sinAngle isZero ifTrue:[^B3DVector3 zero].
	^B3DVector3 
		x: (self b / sinAngle)
		y: (self c / sinAngle)
		z: (self d / sinAngle)! !!B3DRotation methodsFor: 'accessing'!axis: newAxis
	self angle: self angle axis: newAxis! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!b
	^self at: 2! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!b: aFloat
	self at: 2 put: aFloat! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!c
	^self at: 3! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!c: aFloat
	self at: 3 put: aFloat! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:59'!d
	^self at: 4! !!B3DRotation methodsFor: 'accessing' stamp: 'ar 2/1/1999 22:00'!d: aFloat
	self at: 4 put: aFloat! !!B3DRotation methodsFor: 'arithmetic' stamp: 'ar 2/1/1999 22:05'!* aRotation
	"Multiplying two rotations is the same as concatenating the two rotations."
	| v1 v2 v3 vv |
	v1 := self bcd * aRotation a.
	v2 := aRotation bcd * self a.
	v3 := aRotation bcd cross: self bcd.
	vv := v1 + v2 + v3.
	^B3DRotation
		a: (self a * aRotation a) - (self bcd dot: aRotation bcd)
		b: vv x
		c: vv y
		d: vv z! !!B3DRotation methodsFor: 'arithmetic' stamp: 'ar 2/1/1999 22:05'!dot: aRotation
	^(self a * aRotation a) + (self b * aRotation b) + (self c * aRotation c) + (self d * aRotation d)! !!B3DRotation methodsFor: 'arithmetic' stamp: 'ar 2/1/1999 22:06'!negated
	"Negating a quaternion is the same as reversing the angle of rotation"
	^B3DRotation
		a: self a negated
		b: self b
		c: self c
		d: self d! !!B3DRotation methodsFor: 'arithmetic'!normalize
	"Normalize the receiver. Note that the actual angle (a) determining the amount of 
	rotation is fixed, since we do not want to modify angles. This leads to:
		a^2 + b^2 + c^2 + d^2 = 1.
		b^2 + c^2 + d^2 = 1 - a^2.
	Note also that the angle (a) can not exceed 1.0 (due its creation by cosine) and
	if it is 1.0 we have exactly the unit quaternion ( 1, [ 0, 0, 0]).
	"
	| oneMinusASquared length |
	oneMinusASquared := 1.0 - (self a squared).
	(oneMinusASquared < 1.0e-10) ifTrue:[^self setIdentity].
	length := ((self b squared + self c squared + self d squared) / oneMinusASquared) sqrt.
	self b: self b / length.
	self c: self c / length.
	self d: self d / length.
! !!B3DRotation methodsFor: 'converting'!asMatrix4x4
	"Given a quaternion q = (a, [ b, c , d]) the rotation matrix can be calculated as
			|	1 -	2(cc+dd),		2(bc-da),		2(db+ca)	|
		m =	|		2(bc+da),	1 - 	2(bb+dd),		2(cd-ba)		|
			|		2(db-ca),		2(cd+ba),	1 -	2(bb+cc)	|
	"
	| a b c d m bb cc dd bc cd db ba ca da |
	a _ self a. b _ self b. c _ self c. d _ self d.
	bb := (b * b).	cc := (c * c).	dd := (d * d).
	bc := (b * c).	cd := (c * d).	db := (d * b).
	ba := (b * a).	ca := (c * a).	da := (d * a).
	m := self matrixClass identity.
	m 
		a11: 1.0 - (cc + dd * 2.0);a12: (bc - da * 2.0); 		a13: (db + ca * 2.0);
		a21: (bc + da * 2.0);		a22: 1.0 - (bb + dd * 2.0);a23: (cd - ba * 2.0);
		a31: (db - ca * 2.0);		a32: (cd + ba * 2.0);		a33: 1.0 - (bb + cc * 2.0).
	^m
! !!B3DRotation methodsFor: 'converting' stamp: 'ar 2/1/1999 22:08'!normalized
	^self copy normalize! !!B3DRotation methodsFor: 'interpolating' stamp: 'ar 2/1/1999 22:08'!slerpTo: aRotation at: t
	"Spherical linear interpolation (slerp) from the receiver to aQuaternion"
	^self slerpTo: aRotation at: t extraSpins: 0! !!B3DRotation methodsFor: 'interpolating' stamp: 'ar 2/1/1999 22:09'!slerpTo: aRotation at: t extraSpins: spin
	"Sperical Linear Interpolation (slerp).
	Calculate the new quaternion when applying slerp from the receiver (t = 0.0)
	to aRotation (t = 1.0). spin indicates the number of extra rotations to be added.
	The code shown below is from Graphics Gems III"
	| cosT alpha beta flip theta phi sinT |
	alpha := t.
	flip := false.
	"calculate the cosine of the two quaternions on the 4d sphere"
	cosT := self dot: aRotation.
	"if aQuaternion is on the opposite hemisphere reverse the direction
	(note that in quaternion space two points describe the same rotation)"
	cosT < 0.0 ifTrue:[
		flip := true.
		cosT := cosT negated].
	"If the aQuaternion is nearly the same as I am use linear interpolation"
	cosT > 0.99999 ifTrue:[
		"Linear Interpolation"
		beta := 1.0d - alpha
	] ifFalse:[
		"Spherical Interpolation"
		theta := cosT arcCos.
		phi := (spin * Float pi) + theta.
		sinT := theta sin.
		beta := (theta - (alpha * phi)) sin / sinT.
		alpha := (alpha * phi) sin / sinT].

	flip ifTrue:[alpha := alpha negated].
	^B3DRotation 
		a: (alpha * aRotation a) + (beta * self a)
		b: (alpha * aRotation b) + (beta * self b)
		c: (alpha * aRotation c) + (beta * self c)
		d: (alpha * aRotation d) + (beta * self d)! !!B3DRotation methodsFor: 'printing' stamp: 'ar 2/1/1999 22:09'!printOn: aStream

	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: self angle;
		nextPut: Character space;
		print: self axis;
		nextPut:$).! !!B3DRotation methodsFor: 'private'!bcd
	^B3DVector3 x: self b y: self c z: self d! !!B3DRotation methodsFor: 'private' stamp: 'ar 2/1/1999 22:10'!matrixClass
	^B3DMatrix4x4! !!B3DRotation class methodsFor: 'instance creation'!a: aValue b: bValue c: cValue d: dValue
	^self new a: aValue b: bValue c: cValue d: dValue! !!B3DRotation class methodsFor: 'instance creation'!angle: anAngle axis: aVector3
	^self new angle: anAngle axis: aVector3! !!B3DRotation class methodsFor: 'instance creation'!axis: aVector3 angle: anAngle
	^self angle: anAngle axis: aVector3! !!B3DRotation class methodsFor: 'instance creation'!from: startVector to: endVector
	^self new from: startVector to: endVector! !!B3DRotation class methodsFor: 'instance creation'!identity
	^self new setIdentity! !!B3DRotation class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:32'!numElements
	^4! !!B3DRotation class methodsFor: 'instance creation'!radiansAngle: anAngle axis: aVector3
	^self new radiansAngle: anAngle axis: aVector3! !!B3DRotation class methodsFor: 'instance creation'!x: xValue y: yValue z: zValue a: anAngle
	^self new x: xValue y: yValue z: zValue a: anAngle! !!B3DRotation class methodsFor: 'class initialization'!initialize
	"B3DRotation initialize"
	B3DIdentityRotation _ self new.
	B3DIdentityRotation floatAt: 1 put: 1.0.! !!B3DSqueakFormRasterizer methodsFor: 'initialize' stamp: 'ar 2/4/1999 20:17'!canvas: aCanvas	"Set the 2D output device"	canvas _ aCanvas.! !!B3DSqueakFormRasterizer methodsFor: 'initialize'!initialize
	self on: Display.
	dotForm _ Form extent: 3@3 depth: 32.
	dotForm fillBlack.
	lineForm _ Form extent: 1@1 depth: 32.
	lineForm fillBlack.! !!B3DSqueakFormRasterizer methodsFor: 'initialize'!on: aForm
	bb _ BitBlt toForm: aForm.
	bb combinationRule: Form over.
	viewport ifNil:[self viewport: aForm boundingBox].! !!B3DSqueakFormRasterizer methodsFor: 'private'!installForm: aForm on: aBitBlt
	aBitBlt
		sourceForm: aForm;
		sourceOrigin: 0@0;
		width: aForm width;
		height: aForm height;
		colorMap: (aForm colormapIfNeededForDepth: aBitBlt destForm depth).! !!B3DSqueakFormRasterizer methodsFor: 'testing'!needsClip
	^false "Maybe we'll change this later on"! !!B3DSqueakFormRasterizer methodsFor: 'processing' stamp: 'ar 2/4/1999 06:07'!processLineLoop: vb
	"Draw a closed line defined by the vertex buffer"
	| vtxArray firstPoint lastPoint nextPoint |
	vb vertexCount = 0 ifTrue:[^self].
	vtxArray _ vb vertexArray.
	self installForm: lineForm on: bb.
	firstPoint _ lastPoint _ viewport mapVertex4: (vtxArray at: 1) rasterPos.
	2 to: vb vertexCount do:[:i|
		nextPoint _ viewport mapVertex4: (vtxArray at: i) rasterPos.
		bb drawFrom: lastPoint to: nextPoint withFirstPoint: false.
		lastPoint _ nextPoint.
	].
	bb drawFrom: lastPoint to: firstPoint withFirstPoint: false.
! !!B3DSqueakFormRasterizer methodsFor: 'processing' stamp: 'ar 2/4/1999 06:07'!processLines: vb
	"Draw a series of lines defined by each two points the vertex buffer"
	| vtxArray lastPoint nextPoint |
	vb vertexCount = 0 ifTrue:[^self].
	vtxArray _ vb vertexArray.
	self installForm: lineForm on: bb.
	1 to: vb vertexCount by: 2 do:[:i|
		lastPoint _ viewport mapVertex4: (vtxArray at: i) rasterPos.
		nextPoint _ viewport mapVertex4: (vtxArray at: i+1) rasterPos.
		bb drawFrom: lastPoint to: nextPoint withFirstPoint: true.
	].! !!B3DSqueakFormRasterizer methodsFor: 'processing' stamp: 'ar 2/4/1999 06:07'!processPoints: vb
	"Draw a series of points defined by the vertex buffer"
	| vtxArray |
	vtxArray _ vb vertexArray.
	self installForm: dotForm on: bb.
	1 to: vb vertexCount do:[:i|
		bb destOrigin: (viewport mapVertex4: (vtxArray at: i) rasterPos).
		bb copyBits.
	].! !!B3DSqueakFormRasterizer methodsFor: 'processing' stamp: 'ar 2/4/1999 20:23'!processPolygon: vb
	| out vtxArray |
	vb vertexCount = 0 ifTrue:[^self].	canvas ifNil:[	
	canvas _ BalloonCanvas on: bb destForm.
		canvas aaLevel: 1].
	vtxArray _ vb vertexArray.
	out _ Array new: vb vertexCount+1.
	1 to: vb vertexCount do:[:i|
		out at: i put: (viewport mapVertex4: (vtxArray at: i) rasterPos).
	].
	out at: vb vertexCount+1 put: out first.
	canvas drawPolygon: out color: vtxArray first color borderWidth: 1 borderColor: Color black.! !!B3DSqueakFormRasterizer methodsFor: 'processing' stamp: 'ar 2/4/1999 04:34'!processVertexBuffer: vb
	false
		ifTrue:[self processLineLoop: vb]
		ifFalse:[self processPolygon: vb]! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!x
	^self at: 1! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!x: aFloat
	self at: 1 put: aFloat! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!y
	^self at: 2! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!y: aFloat
	self at: 2 put: aFloat! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!z
	^self at: 3! !!B3DVector3 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:24'!z: aFloat
	self at: 3 put: aFloat! !!B3DVector3 methodsFor: 'vector functions'!cross: aVector 
	"calculate the cross product from the receiver with aVector"
	^self species
		x: self y * aVector z - (aVector y * self z)
		y: self z * aVector x - (aVector z * self x)
		z: self x * aVector y - (aVector x * self y)! !!B3DVector3 methodsFor: 'vector functions'!dot: aVector 
	"return the dot product from the receiver and aVector"

	^self x * aVector x + (self y * aVector y) + (self z * aVector z)! !!B3DVector3 methodsFor: 'vector functions'!length
	^self squaredLength sqrt! !!B3DVector3 methodsFor: 'vector functions'!length: newLength
	self safelyNormalize *= newLength! !!B3DVector3 methodsFor: 'vector functions'!normalize
	self /= self length! !!B3DVector3 methodsFor: 'vector functions'!normalized
	^self / self length! !!B3DVector3 methodsFor: 'vector functions'!safelyNormalize
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	| length |
	length _ self length.
	length = 0.0 ifFalse:[self /= length].! !!B3DVector3 methodsFor: 'vector functions'!safelyNormalized
	"Safely normalize the receiver, e.g. check if the length is non-zero"
	^self copy safelyNormalize! !!B3DVector3 methodsFor: 'vector functions'!squaredLength
	^self dot: self! !!B3DVector3 methodsFor: 'vector functions'!squaredLength: newLength
	self length: newLength sqrt! !!B3DVector3 methodsFor: 'private'!privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z.! !!B3DVector3 methodsFor: 'initialize'!x: x y: y z: z
	self x: x.
	self y: y.
	self z: z.! !!B3DVector3 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:23'!numElements
	^3! !!B3DVector3 class methodsFor: 'instance creation'!x: x y: y z: z
	^self new x: x y: y z: z! !!B3DVector3 class methodsFor: 'instance creation'!zero
	^self new! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!w
	^self at: 4! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!w: aFloat
	self at: 4 put: aFloat! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!x
	^self at: 1! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!x: aFloat
	self at: 1 put: aFloat! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!y
	^self at: 2! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!y: aFloat
	self at: 2 put: aFloat! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:16'!z
	^self at: 3! !!B3DVector4 methodsFor: 'accessing' stamp: 'ar 2/1/1999 21:17'!z: aFloat
	self at: 3 put: aFloat! !!B3DVector4 methodsFor: 'private'!privateLoadFrom: srcObject
	self x: srcObject x y: srcObject y z: srcObject z w: srcObject w.! !!B3DVector4 methodsFor: 'initialize'!x: x y: y z: z w: w
	self x: x.
	self y: y.
	self z: z.
	self w: w.! !!B3DVector4 class methodsFor: 'instance creation' stamp: 'ar 2/1/1999 21:21'!numElements
	^4! !!B3DVector4 class methodsFor: 'instance creation'!x: x y: y z: z
	^self x: x y: y z: z w: 1.0! !!B3DVector4 class methodsFor: 'instance creation'!x: x y: y z: z w: w
	^self new x: x y: y z: z w: w! !!B3DVector4 class methodsFor: 'instance creation'!zero
	^self new! !!B3DVertexBuffer methodsFor: 'accessing'!vertexArray
	^vertexArray! !!B3DVertexBuffer methodsFor: 'accessing'!vertexCount
	^vertexCount! !!B3DVertexBuffer methodsFor: 'initialize'!initialize
	vertexArray _ B3DPrimitiveVertexArray new: 100.
	vertexCount _ 0.
	current _ B3DPrimitiveVertex new.
	primitive _ nil.
	bounds _ nil.! !!B3DVertexBuffer methodsFor: 'initialize'!reset
	vertexCount _ 0.
	bounds _ nil.! !!B3DVertexBuffer methodsFor: 'attributes'!color
	^current color! !!B3DVertexBuffer methodsFor: 'attributes'!color: aColor
	current color: aColor! !!B3DVertexBuffer methodsFor: 'attributes'!normal
	^current normal! !!B3DVertexBuffer methodsFor: 'attributes'!normal: aVector
	current normal: aVector! !!B3DVertexBuffer methodsFor: 'attributes'!texCoords
	^current texCoords! !!B3DVertexBuffer methodsFor: 'attributes'!texCoords: aVector
	current texCoords: aVector! !!B3DVertexBuffer methodsFor: 'attributes'!vertex
	^current position! !!B3DVertexBuffer methodsFor: 'attributes'!vertex: aVector
	current position: aVector.
	self privateAddVertex.! !!B3DVertexBuffer methodsFor: 'private'!privateAddVertex
	"Add the current vertex to the list of vertices processed"
	| newVtxArray |
	vertexCount >= vertexArray size ifTrue:[
		newVtxArray _ vertexArray species new: vertexCount * 3 // 2 + 20.
		vertexArray _ newVtxArray.
	].
	vertexArray at: (vertexCount _ vertexCount + 1) put: current.
	bounds _ nil.! !!B3DVertexBuffer class methodsFor: 'instance creation'!new
	^super new initialize! !!B3DVertexShader methodsFor: 'initialize'!initialize
	super initialize.
	lights _ OrderedCollection new.! !!B3DVertexShader methodsFor: 'shading' stamp: 'ar 2/4/1999 20:22'!processVertexBuffer: vb
	| colors vtxArray vtx |	true ifTrue:[^self]. "Currently no shading available"
	colors _ B3DColor4Array new: vb vertexCount.
	colors fillWith: 0. "r = g = b = a = 0.0"
	lights do:[:light|
		light shadeVertexBuffer: vb with: material into: colors.
	].
	colors clampAllFrom: 0.0 to: 1.0.
	vtxArray _ vb vertexArray.
	1 to: vb vertexCount do:[:i|
		vtx _ vtxArray at: i.
		vtx color: (colors at: i).
		vtxArray at: i put: vtx].
! !!B3DVertexTransformer methodsFor: 'initialize' stamp: 'ar 2/2/1999 18:58'!initialize
	modelMatrix := B3DMatrix4x4 identity.
	viewMatrix := B3DMatrix4x4 identity.
	textureMatrix := B3DMatrix4x4 identity.
	currentMatrix := modelMatrix.
	matrixStack := OrderedCollection new: 30.
	needsUpdate := false.! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:58'!currentMatrix
	^currentMatrix! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:58'!matrixMode
	currentMatrix == modelMatrix ifTrue:[^#modelView].
	currentMatrix == viewMatrix ifTrue:[^#projection].
	currentMatrix == textureMatrix ifTrue:[^#texture].
	self error:'Bad matrix state'.! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:58'!matrixMode: aSymbol
	aSymbol == #modelView ifTrue:[currentMatrix := modelMatrix. ^self].
	aSymbol == #projection ifTrue:[currentMatrix := viewMatrix. ^self].
	aSymbol == #texture ifTrue:[currentMatrix := textureMatrix. ^self].
	self error:'Bad matrix mode'.! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:58'!modelViewMatrix
	^modelMatrix! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:59'!popMatrix
	"Pop the current matrix from the stack"
	matrixStack isEmpty ifTrue:[^self error:'Empty matrix stack'].
	currentMatrix loadFrom: matrixStack removeLast.! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:59'!projectionMatrix
	^viewMatrix! !!B3DVertexTransformer methodsFor: 'accessing' stamp: 'ar 2/2/1999 18:59'!pushMatrix
	"Push the current matrix"
	| theMatrix |
	theMatrix := B3DMatrix4x4 new.
	theMatrix loadFrom: currentMatrix.
	matrixStack addLast: theMatrix.! !!B3DVertexTransformer methodsFor: 'modifying' stamp: 'ar 2/2/1999 19:00'!loadIdentity
	currentMatrix setIdentity.
	needsUpdate := true.! !!B3DVertexTransformer methodsFor: 'modifying' stamp: 'ar 2/2/1999 19:00'!loadMatrix: aMatrix
	currentMatrix loadFrom: aMatrix.
	needsUpdate := true.! !!B3DVertexTransformer methodsFor: 'modifying'!lookFrom: position to: target up: upDirection
	"create a matrix such that we look from eyePoint to centerPoint using upDirection"
	| xDir yDir zDir m |
	"calculate z vector"
	zDir _ target - position.
	zDir safelyNormalize.
	"calculate x vector"
	xDir _ upDirection cross: zDir.
	xDir safelyNormalize.
	"recalc y vector"
	yDir _ zDir cross: xDir.
	yDir safelyNormalize.
	m := B3DMatrix4x4 new.
	m	a11: xDir x;		a12: xDir y;		a13: xDir z;		a14: 0.0;
		a21: yDir x;		a22: yDir y;		a23: yDir z;		a24: 0.0;
		a31: zDir x;		a32: zDir y;		a33: zDir z;		a34: 0.0;
		a41: 0.0;			a42: 0.0;		a43: 0.0;		a44: 1.0.
	self translateBy: position negated.
	self transformBy: m.! !!B3DVertexTransformer methodsFor: 'modifying' stamp: 'ar 2/2/1999 19:00'!multiplyMatrix: aMatrix
	"Multiply aMatrix with the current matrix"
	currentMatrix *= aMatrix! !!B3DVertexTransformer methodsFor: 'modifying'!rotateBy: aRotation
	self transformBy: aRotation asMatrix4x4.! !!B3DVertexTransformer methodsFor: 'modifying'!scaleBy: aVector
	self transformBy: (B3DMatrix4x4 identity setScale: aVector)! !!B3DVertexTransformer methodsFor: 'modifying' stamp: 'ar 2/2/1999 19:01'!scaleByX: x y: y z: z
	currentMatrix scaleByX: x y: y z: z.
	needsUpdate := true.! !!B3DVertexTransformer methodsFor: 'modifying'!transformBy: aTransformation
	self privateTransformMatrix: currentMatrix 
			with: aTransformation asMatrix4x4 
			into: currentMatrix.
	needsUpdate := true.! !!B3DVertexTransformer methodsFor: 'modifying'!translateBy: aVector
	"Add the translation defined by aVector to the current matrix"
	self transformBy: (B3DMatrix4x4 identity setTranslation: aVector).! !!B3DVertexTransformer methodsFor: 'modifying' stamp: 'ar 2/2/1999 19:01'!translateByX: x y: y z: z
	"Add the translation defined by aVector to the current matrix"
	currentMatrix translateByX: x y: y z: z.
	needsUpdate := true.! !!B3DVertexTransformer methodsFor: 'transforming' stamp: 'ar 2/4/1999 06:00'!privateTransformPrimitiveVertex: primitiveVertex byModelView: aMatrix
	| x y z rx ry rz rw oneOverW |
	x _ primitiveVertex positionX.
	y _ primitiveVertex positionY.
	z _ primitiveVertex positionZ.
	rx := (x * aMatrix a11) + (y * aMatrix a12) + (z * aMatrix a13) + aMatrix a14.
	ry := (x * aMatrix a21) + (y * aMatrix a22) + (z * aMatrix a23) + aMatrix a24.
	rz := (x * aMatrix a31) + (y * aMatrix a32) + (z * aMatrix a33) + aMatrix a34.
	rw := (x * aMatrix a41) + (y * aMatrix a42) + (z * aMatrix a43) + aMatrix a44.
	rw = 1.0 ifTrue:[
		primitiveVertex positionX: rx.
		primitiveVertex positionY: ry.
		primitiveVertex positionZ: rz.
	] ifFalse:[
		rw = 0.0 
			ifTrue:[oneOverW _ 0.0]
			ifFalse:[oneOverW _ 1.0 / rw].
		primitiveVertex positionX: rx * oneOverW.
		primitiveVertex positionY: ry * oneOverW.
		primitiveVertex positionZ: rz * oneOverW.
	].
! !!B3DVertexTransformer methodsFor: 'transforming' stamp: 'ar 2/4/1999 03:52'!privateTransformPrimitiveVertex: primitiveVertex byProjection: aMatrix
	| x y z rx ry rz rw |
	x _ primitiveVertex positionX.
	y _ primitiveVertex positionY.
	z _ primitiveVertex positionZ.
	rx := (x * aMatrix a11) + (y * aMatrix a12) + (z * aMatrix a13) + aMatrix a14.
	ry := (x * aMatrix a21) + (y * aMatrix a22) + (z * aMatrix a23) + aMatrix a24.
	rz := (x * aMatrix a31) + (y * aMatrix a32) + (z * aMatrix a33) + aMatrix a34.
	rw := (x * aMatrix a41) + (y * aMatrix a42) + (z * aMatrix a43) + aMatrix a44.
	primitiveVertex rasterPosX: rx.
	primitiveVertex rasterPosY: ry.
	primitiveVertex rasterPosZ: rz.
	primitiveVertex rasterPosW: rw.! !!B3DVertexTransformer methodsFor: 'transforming'!privateTransformVB: vertexArray count: vertexCount modelViewMatrix: modelViewMatrix projectionMatrix: projectionMatrix
	| primitiveVertex |
	1 to: vertexCount do:[:i|
		primitiveVertex _ vertexArray at: i.
		self privateTransformPrimitiveVertex: primitiveVertex
			byModelView: modelViewMatrix.
		self privateTransformPrimitiveVertex: primitiveVertex
			byProjection: projectionMatrix.
		vertexArray at: i put: primitiveVertex.
	].! !!B3DVertexTransformer methodsFor: 'transforming' stamp: 'ar 2/4/1999 04:25'!processVertexBuffer: vb
	self privateTransformVB: vb vertexArray 
			count: vb vertexCount
			modelViewMatrix: self modelViewMatrix
			projectionMatrix: self projectionMatrix.! !!B3DVertexTransformer methodsFor: 'view transformation'!ortho: aFrustum
	viewMatrix _ aFrustum asFrustum asOrthoMatrix.
	needsUpdate _ true.! !!B3DVertexTransformer methodsFor: 'view transformation'!perspective: aPerspectiveOrFrustum
	viewMatrix _ aPerspectiveOrFrustum asFrustum asPerspectiveMatrix.
	needsUpdate _ true.! !!B3DVertexTransformer methodsFor: 'private-transforming'!privateTransformMatrix: m1 with: m2 into: m3
	"Perform a 4x4 matrix multiplication
		m2 * m1 = m3
	being equal to first transforming points by m2 and then by m1.
	Note that m1 may be identical to m3."
	| c1 c2 c3 c4 |
	m2 == m3 ifTrue:[^self error:'Argument and result matrix identical'].
	c1 _ ((m1 a11 * m2 a11) + (m1 a12 * m2 a21) + 
				(m1 a13 * m2 a31) + (m1 a14 * m2 a41)).
	c2 _ ((m1 a11 * m2 a12) + (m1 a12 * m2 a22) + 
				(m1 a13 * m2 a32) + (m1 a14 * m2 a42)).
	c3 _ ((m1 a11 * m2 a13) + (m1 a12 * m2 a23) + 
				(m1 a13 * m2 a33) + (m1 a14 * m2 a43)).
	c4 _ ((m1 a11 * m2 a14) + (m1 a12 * m2 a24) + 
				(m1 a13 * m2 a34) + (m1 a14 * m2 a44)).

	m3 a11: c1; a12: c2; a13: c3; a14: c4.

	c1 _ ((m1 a21 * m2 a11) + (m1 a22 * m2 a21) + 
				(m1 a23 * m2 a31) + (m1 a24 * m2 a41)).
	c2 _ ((m1 a21 * m2 a12) + (m1 a22 * m2 a22) + 
				(m1 a23 * m2 a32) + (m1 a24 * m2 a42)).
	c3 _ ((m1 a21 * m2 a13) + (m1 a22 * m2 a23) + 
				(m1 a23 * m2 a33) + (m1 a24 * m2 a43)).
	c4 _ ((m1 a21 * m2 a14) + (m1 a22 * m2 a24) + 
				(m1 a23 * m2 a34) + (m1 a24 * m2 a44)).

	m3 a21: c1; a22: c2; a23: c3; a24: c4.

	c1 _ ((m1 a31 * m2 a11) + (m1 a32 * m2 a21) + 
				(m1 a33 * m2 a31) + (m1 a34 * m2 a41)).
	c2 _ ((m1 a31 * m2 a12) + (m1 a32 * m2 a22) + 
				(m1 a33 * m2 a32) + (m1 a34 * m2 a42)).
	c3 _ ((m1 a31 * m2 a13) + (m1 a32 * m2 a23) + 
				(m1 a33 * m2 a33) + (m1 a34 * m2 a43)).
	c4 _ ((m1 a31 * m2 a14) + (m1 a32 * m2 a24) + 
				(m1 a33 * m2 a34) + (m1 a34 * m2 a44)).

	m3 a31: c1; a32: c2; a33: c3; a34: c4.

	c1 _ ((m1 a41 * m2 a11) + (m1 a42 * m2 a21) + 
				(m1 a43 * m2 a31) + (m1 a44 * m2 a41)).
	c2 _ ((m1 a41 * m2 a12) + (m1 a42 * m2 a22) + 
				(m1 a43 * m2 a32) + (m1 a44 * m2 a42)).
	c3 _ ((m1 a41 * m2 a13) + (m1 a42 * m2 a23) + 
				(m1 a43 * m2 a33) + (m1 a44 * m2 a43)).
	c4 _ ((m1 a41 * m2 a14) + (m1 a42 * m2 a24) + 
				(m1 a43 * m2 a34) + (m1 a44 * m2 a44)).

	m3 a41: c1; a42: c2; a43: c3; a44: c4.

	^m3! !!B3DViewingFrustum commentStamp: '<historical>' prior: 0!I represent a viewing frustum, defined by the following values:

	typedef struct B3DViewingFrustum {
		float left;
		float right;
		float top;
		float bottom;
		float near;
		float far;
	} B3DViewingFrustum;

The frustum can be converted into either a ortho matrix (having no perspective distortion) or a perspective matrix for use in the Balloon 3D render engine.!!B3DViewingFrustum methodsFor: 'private'!asOrthoMatrixInto: aB3DMatrix4x4
	| x y z tx ty tz dx dy dz |
	(self near <= 0.0 or:[self far <= 0.0]) ifTrue: [^self error:'Clipping plane error'].
	dx := self right - self left.
	dy := self top - self bottom.
	dz := self far - self near.
	x := dx * 0.5.
	y := dy * 0.5.
	z := dz * -0.5.
	tx := (self left + self right) / dx.
	ty := (self top + self bottom) / dy.
	tz := (self near + self far) / dz.
	aB3DMatrix4x4
		a11: x;		a12: 0.0;		a13: 0.0;		a14: tx;
		a21: 0.0;		a22: y;		a23: 0.0;	a24: ty;
		a31: 0.0;		a32: 0.0;	a33: z;		a34: tz;
		a41: 0.0;		a42: 0.0;	a43: 0.0;	a44: 1.0.
	^aB3DMatrix4x4! !!B3DViewingFrustum methodsFor: 'private'!asPerspectiveMatrixInto: aB3DMatrix4x4
	| x y a b c d dx dy dz z2 |
	(self near <= 0.0 or:[self far <= 0.0]) ifTrue: [^self error:'Clipping plane error'].
	dx := self right - self left.
	dy := self top - self bottom.
	dz := self far - self near.
	z2 := 2.0 * self near.
	x := z2 / dx.
	y := z2 / dy.
	a := (self left + self right) / dx.
	b := (self top + self bottom) / dy.
	c := (self near + self far) negated / dz.
	d := (-2.0 * self near * self far) / dz.
	aB3DMatrix4x4
		a11: x;		a12: 0.0;		a13: a;		a14: 0.0;
		a21: 0.0;		a22: y;		a23: b;		a24: 0.0;
		a31: 0.0;		a32: 0.0;	a33: c;		a34: d;
		a41: 0.0;		a42: 0.0;	a43: -1;		a44: 0.0.
	^aB3DMatrix4x4! !!B3DViewingFrustum methodsFor: 'private'!computeFromNear: nearDistance far: farDistance fov: fieldOfView aspect: aspectRatio
	"Compute the viewing frustum from the given values"
	| top bottom |
	top := nearDistance * fieldOfView degreesToRadians tan.
	bottom := top negated.
	self left: bottom * aspectRatio.
	self right: top * aspectRatio.	
	self top: top.
	self bottom: bottom.
	self near: nearDistance.
	self far: farDistance.! !!B3DViewingFrustum methodsFor: 'accessing'!bottom
	^self floatAt: 4.! !!B3DViewingFrustum methodsFor: 'accessing'!bottom: aFloat
	self floatAt: 4 put: aFloat! !!B3DViewingFrustum methodsFor: 'accessing'!far
	^self floatAt: 6! !!B3DViewingFrustum methodsFor: 'accessing'!far: aFloat
	self floatAt: 6 put: aFloat! !!B3DViewingFrustum methodsFor: 'accessing'!left
	^self floatAt: 1! !!B3DViewingFrustum methodsFor: 'accessing'!left: aFloat
	self floatAt: 1 put: aFloat! !!B3DViewingFrustum methodsFor: 'accessing'!near
	^self floatAt: 5! !!B3DViewingFrustum methodsFor: 'accessing'!near: aFloat
	self floatAt: 5 put: aFloat! !!B3DViewingFrustum methodsFor: 'accessing'!right
	^self floatAt: 2! !!B3DViewingFrustum methodsFor: 'accessing'!right: aFloat
	self floatAt: 2 put: aFloat! !!B3DViewingFrustum methodsFor: 'accessing'!top
	^self floatAt: 3! !!B3DViewingFrustum methodsFor: 'accessing'!top: aFloat
	self floatAt: 3 put: aFloat! !!B3DViewingFrustum methodsFor: 'converting'!asFrustum
	^self! !!B3DViewingFrustum methodsFor: 'converting'!asOrthoMatrix
	^self asOrthoMatrixInto: B3DMatrix4x4 new! !!B3DViewingFrustum methodsFor: 'converting'!asPerspectiveMatrix
	^self asPerspectiveMatrixInto: B3DMatrix4x4 new! !!B3DViewingFrustum class methodsFor: 'instance creation'!near: nearDistance far: farDistance fov: fieldOfView aspect: aspectRatio
	^self new computeFromNear: nearDistance far: farDistance fov: fieldOfView aspect: aspectRatio! !!B3DViewingFrustum class methodsFor: 'instance creation'!numElements
	^6! !!B3DViewport methodsFor: 'mapping'!asMatrixTransform2x3
	^(MatrixTransform2x3 withScale: scale) offset: center! !!B3DViewport methodsFor: 'mapping'!mapVertex4: aVector
	| w x y oneOverW |
	w _ aVector w.
	w = 1.0 ifTrue:[
		x _ aVector x.
		y _ aVector y.
	] ifFalse:[
		w = 0.0
			ifTrue:[oneOverW _ 0.0]
			ifFalse:[oneOverW _ 1.0 / w].
		x _ aVector x * oneOverW.
		y _ aVector y * oneOverW.
	].
	^((x@y) * scale + center) truncated! !!B3DViewport methodsFor: 'private'!setOrigin: topLeft corner: bottomRight
	super setOrigin: topLeft corner: bottomRight.
	center _ (self origin + self corner) / 2.0.
	scale _ corner - center.! !!BalloonCanvas methodsFor: 'balloon drawing' stamp: 'ar 2/4/1999 20:16'!render: anObject	| b3d |	b3d _ B3DRenderEngine new.	b3d canvas: self.	anObject renderOn: b3d.! !!Color methodsFor: 'conversions' stamp: 'ar 2/1/1999 22:11'!asB3DColor
	"Convert the receiver into a color"
	^B3DColor4
		r: self red
		g: self green
		b: self blue
		a: self alpha! !!FillStyle methodsFor: 'converting' stamp: 'ar 2/1/1999 22:12'!asB3DColor
	^self asColor asB3DColor! !!Number methodsFor: 'testing'!isZero
	^self = 0! !!Float methodsFor: 'testing'!isZero
	^self = 0.0! !!Point methodsFor: 'converting'!@ aNumber
	^B3DVector3 x: x y: y z: aNumber! !B3DBox initialize!B3DMatrix4x4 initialize!B3DPrimitiveVertexArray initialize!B3DRotation initialize!