'From Squeak 2.4b of April 23, 1999 on 3 June 1999 at 1:37:56 pm'!"Change Set:		WnldMorphicTextures-arDate:			3 June 1999Author:			Andreas RaabThis change set adds 'active' textures to WonderlandActors. To try out an active texture in a Wonderland execute the following code from a Wonderland script editor (LINE BY LINE not all at once):	w makePlaneNamed:'plane'.	plane move: up distance: 0.5.	texture _ w makeActiveTexture.	plane setActiveTexture: texture.	plane initializeMorphicReactions.Then, embedd a polygon into the texture morph. You can now edit the the polygon in either 2D or 3D."!B3DRenderEngine subclass: #B3DPickerEngine	instanceVariableNames: 'pickMatrix pickList objects maxVtx '	classVariableNames: ''	poolDictionaries: ''	category: 'Balloon-3D-Engine'!WonderlandMorph subclass: #WonderlandCameraMorph	instanceVariableNames: 'myCamera myControls eventFocus lastCursorPoint mouseUpButton firstPersonControls '	classVariableNames: ''	poolDictionaries: 'WonderlandConstants '	category: 'Wonderland-Morphs'!Object subclass: #WonderlandEvent	instanceVariableNames: 'actor camera cursorPoint cursorDelta key morph vertex morphicEvent '	classVariableNames: ''	poolDictionaries: 'WonderlandConstants '	category: 'Wonderland-Misc'!HandMorph subclass: #WonderlandHandMorph	instanceVariableNames: 'weakTexture events '	classVariableNames: ''	poolDictionaries: 'WonderlandConstants '	category: 'Wonderland-Morphs'!BorderedMorph subclass: #WonderlandTextureMorph	instanceVariableNames: 'myTexture changeFlag myWonderland myHand '	classVariableNames: ''	poolDictionaries: ''	category: 'Wonderland-Morphs'!!B3DPickerPlugin methodsFor: 'primitives' stamp: 'ar 6/2/1999 11:59'!b3dComputeMinIndexZ	"Primitive. Compute and return the index for the minimal z value of all objects in the vertex buffer."	| idxSize vtxSize primType vtxArray idxArray minIndex |	self export: true.	self inline: false.	self var: #vtxArray declareC:'float *vtxArray'.	self var: #idxArray declareC:'int *idxArray'.	interpreterProxy methodArgumentCount = 5		ifFalse:[^interpreterProxy primitiveFail].	idxSize _ interpreterProxy stackIntegerValue: 0.	vtxSize _ interpreterProxy stackIntegerValue: 2.	primType _ interpreterProxy stackIntegerValue: 4.	interpreterProxy failed ifTrue:[^nil].	vtxArray _ self stackPrimitiveVertexArray: 3 ofSize: vtxSize.	idxArray _ self stackPrimitiveIndexArray: 1 ofSize: idxSize validate: true forVertexSize: vtxSize.	(vtxArray == nil or:[idxArray == nil or:[interpreterProxy failed]])		ifTrue:[^interpreterProxy primitiveFail].	(primType < 1 or:[primType > 6])		ifTrue:[^interpreterProxy primitiveFail].	primType <= 3 ifTrue:[		minIndex _ self processNonIndexedIDX: vtxArray ofSize: vtxSize.	] ifFalse:[		minIndex _ self processIndexedIDX: vtxArray ofSize: vtxSize idxArray: idxArray idxSize: idxSize.	].	interpreterProxy failed ifFalse:[		interpreterProxy pop: 6. "nArgs+rcvr"		interpreterProxy pushInteger: minIndex.	].! !!B3DPickerPlugin methodsFor: 'processing' stamp: 'ar 6/2/1999 12:00'!processIndexedIDX: vtxArray ofSize: vtxSize idxArray: idxArray idxSize: idxSize	| vtxPtr zValue wValue minZ minIndex index |	self var: #vtxArray declareC:'float *vtxArray'.	self var: #vtxPtr declareC:'float *vtxPtr'.	self var: #idxArray declareC:'int *idxArray'.	self var: #wValue declareC:'double wValue'.	self var: #zValue declareC:'double zValue'.	self var: #minZ declareC:'double minZ'.	minZ _ 10.0.	minIndex _ 0.	1 to: idxSize do:[:i|		index _ idxArray at: i.		index > 0 ifTrue:[			vtxPtr _ vtxArray + (index-1 * PrimVertexSize).			zValue _ vtxPtr at: PrimVtxRasterPosZ.			wValue _ vtxPtr at: PrimVtxRasterPosW.			wValue = 0.0 ifFalse:[zValue _ zValue / wValue].			(minIndex = 0 or:[zValue < minZ]) ifTrue:[				minIndex _ i.				minZ _ zValue].		].	].	^minIndex! !!B3DPickerPlugin methodsFor: 'processing' stamp: 'ar 6/2/1999 12:00'!processNonIndexedIDX: vtxArray ofSize: vtxSize	| vtxPtr zValue wValue minZ minIndex |	self var: #vtxArray declareC:'float *vtxArray'.	self var: #vtxPtr declareC:'float *vtxPtr'.	self var: #wValue declareC:'double wValue'.	self var: #zValue declareC:'double zValue'.	self var: #minZ declareC:'double minZ'.	minZ _ 10.0.	minIndex _ 0.	vtxPtr _ vtxArray.	1 to: vtxSize do:[:i|		zValue _ vtxPtr at: PrimVtxRasterPosZ.		wValue _ vtxPtr at: PrimVtxRasterPosW.		wValue = 0.0 ifFalse:[zValue _ zValue / wValue].		(minIndex = 0 or:[zValue < minZ]) ifTrue:[			minIndex _ i.			minZ _ zValue].	].	^minIndex! !!B3DRenderEngine methodsFor: 'attributes' stamp: 'ar 6/2/1999 14:00'!texture: anObject	"Note: For convenience; the object can be anything that understands #asTexture"	^rasterizer texture: anObject asTexture! !!B3DPickerEngine methodsFor: 'initialize' stamp: 'ar 6/2/1999 12:08'!initialize	"Do not call super initialize here. We get our components directly by the creating engine."	pickList _ SortedCollection new: 100.	pickList sortBlock:[:a1 :a2| a1 value rasterPosZ < a2 value rasterPosZ].	objects _ OrderedCollection new: 100.	objects resetTo: 1.	maxVtx _ B3DPrimitiveVertex new.	maxVtx rasterPosZ: 1.0e30.	maxVtx rasterPosW: 1.0.! !!B3DPickerEngine methodsFor: 'picking' stamp: 'ar 6/2/1999 12:03'!render: anObject	| assoc |	assoc _ Association key: anObject value: maxVtx.	objects addLast: assoc.	anObject renderOn: self.	(objects removeLast == assoc) ifFalse:[^self error:'Object stack is confused'].	assoc value rasterPosZ > 2.0 ifFalse:[pickList add: assoc].! !!B3DPickerEngine methodsFor: 'picking' stamp: 'ar 6/2/1999 12:08'!topMostVertex	"Return the top most primitive vertex of all picked objects.	Note: Except from the z value the vertex is *not* normalized yet 		(e.g., there was no division by w)"	^pickList isEmpty		ifTrue:[nil]		ifFalse:[pickList first value]! !!B3DPickerEngine methodsFor: 'private-rendering' stamp: 'ar 6/2/1999 11:54'!primComputeMinIndexZ: primType vtxArray: vtxArray vtxSize: vtxSize idxArray: idxArray idxSize: idxSize	"<primitive: 'b3dComputeMinIndexZ' module: 'Squeak3D'>"	^nil "Indicates failure"! !!B3DPickerEngine methodsFor: 'private-rendering' stamp: 'ar 6/2/1999 11:53'!processIndexed: vb	| idxArray vtxArray index vtx zValue minIndex minZ wValue |	idxArray _ vb indexArray.	vtxArray _ vb vertexArray.	minZ _ 10.0.	minIndex _ 0.	1 to: vb indexCount do:[:i|		index _ idxArray at: i.		index = 0 ifFalse:[			vtx _ vtxArray at: index.			zValue _ vtx rasterPosZ.			wValue _ vtx rasterPosW.			wValue = 0.0 ifFalse:[zValue _ zValue / wValue].			(minIndex = 0 or:[zValue < minZ]) ifTrue:[				minIndex _ i.				minZ _ zValue].		].	].	^minIndex! !!B3DPickerEngine methodsFor: 'private-rendering' stamp: 'ar 6/2/1999 11:54'!processNonIndexed: vb	| vtxArray vtx zValue minZ minIndex wValue |	vtxArray _ vb vertexArray.	minZ _ 10.0.	minIndex _ 0.	1 to: vb vertexCount do:[:i|		vtx _ vtxArray at: i.		zValue _ vtx rasterPosZ.		wValue _ vtx rasterPosW.		wValue = 0.0 ifFalse:[zValue _ zValue / wValue].		(minIndex = 0 or:[zValue < minZ]) ifTrue:[			minIndex _ i.			minZ _ zValue].	].	^minIndex! !!B3DPickerEngine methodsFor: 'private-rendering' stamp: 'ar 6/2/1999 13:17'!processVertexBuffer: vb	| minIndex minVertex |	minIndex _ self primComputeMinIndexZ: vb primitive vtxArray: vb vertexArray vtxSize: vb vertexCount idxArray: vb indexArray idxSize: vb indexCount.	minIndex == nil ifTrue:[minIndex _ super processVertexBuffer: vb].	minIndex = 0 ifTrue:[^maxVtx].	minVertex _ vb vertexArray at: minIndex.	minVertex rasterPosZ: minVertex rasterPosZ / minVertex rasterPosW.	^minVertex! !!B3DPickerEngine methodsFor: 'private-rendering' stamp: 'ar 6/2/1999 11:47'!renderPrimitive	"This is the main rendering loop for all operations"	| visible minVertex |	"Step 1: Check if the mesh is visible at all"	visible _ self privateVisibleVB: vertexBuffer.	visible == false ifTrue:[^self].	"Step 2: Transform vertices, normals, texture coords of the mesh"	self privateTransformVB: vertexBuffer.	"Step 3: Clip the mesh if necessary"	visible _ self privateClipVB: vertexBuffer.	visible == false ifTrue:[^self].	"Step 4: Collect the minimal/maximal distances for the current object."	minVertex _ self processVertexBuffer: vertexBuffer.	objects isEmpty ifFalse:[		objects last value rasterPosZ > minVertex rasterPosZ 			ifTrue:[objects last value: minVertex].	].! !!B3DTexture methodsFor: 'converting' stamp: 'ar 5/27/1999 17:49'!asTexture	^self! !!MorphicEvent methodsFor: 'private' stamp: 'ar 6/2/1999 14:35'!setCursorPoint: aPoint	cursorPoint _ aPoint.! !!Wonderland methodsFor: 'creating' stamp: 'ar 5/27/1999 16:17'!createSimpleActor: aMesh named: aString parent: parentActor	| newActor |	newActor _ self makeActorNamed: aString.	aMesh ifNotNil:[newActor setMesh: aMesh].	parentActor ifNotNil:[		newActor reparentTo: parentActor.		newActor becomePart].	^newActor! !!Wonderland methodsFor: 'creating' stamp: 'ar 5/27/1999 17:58'!makeActiveTexture	| tex |	tex _ WonderlandTextureMorph new.	World primaryHand attachMorph: tex.	^tex! !!Wonderland methodsFor: 'creating' stamp: 'ar 5/27/1999 16:14'!makeActor	"Creates a new actor without any geometry"	^self makeActorNamed:'unnamed'! !!Wonderland methodsFor: 'creating' stamp: 'ar 5/27/1999 16:13'!makeActorNamed: aString	"Creates a new actor without any geometry"	| newClass newActor name |	newClass _ WonderlandActor newUniqueClassInstVars: '' classInstVars: ''.	newActor _ (newClass createFor: self).	actorClassList addLast: newClass.	scriptEditor ifNotNil: [ 				name _ self uniqueNameFrom: aString.				newActor setName: name.				myNamespace at: name put: newActor.				scriptEditor updateActorBrowser.						].	"Add an undo item to undo the creation of this object"	myUndoStack push: (UndoAction new: [ newActor removeFromScene.											myNamespace removeKey: name ifAbsent: [].						 					scriptEditor updateActorBrowser. ]).	^ newActor.! !!Wonderland methodsFor: 'creating' stamp: 'ar 6/3/1999 12:35'!makePlaneNamed: aString	"Create a simple plane"	| tex vertices faces mesh |	tex _ B3DTexture2Array new: 4.	tex at: 1 put: (0@0); at: 2 put: (1@0); at: 3 put: (1@1); at: 4 put: (0@1).	vertices _ B3DVector3Array new: 4.	vertices  at: 1 put: -1@1@-1; at: 2 put: 1@1@-1; at: 3 put: 1@-1@-1; at: 4 put: -1@-1@-1.	vertices do:[:vtx| vtx *= 0.5].	faces _ B3DIndexedTriangleArray new: 2.	faces at: 1 put: (B3DIndexedTriangle with: 1 with: 2 with: 3).	faces at: 2 put: (B3DIndexedTriangle with: 3 with: 4 with: 1).	mesh _ B3DIndexedTriangleMesh new.	mesh vertices: vertices.	mesh faces: faces.	mesh texCoords: tex.	self createSimpleActor: mesh named: aString parent: nil.! !!WonderlandActor methodsFor: 'initialize' stamp: 'ar 6/2/1999 14:15'!initializeDefaultReactions	"Set up our default reactions"	myReactions _ Dictionary new.	self respondWith: [:event | self onLeftMouseDown: event] to: leftMouseDown.	self respondWith: [:event | self onLeftMouseUp: event] to: leftMouseUp.! !!WonderlandActor methodsFor: 'initialize' stamp: 'ar 6/2/1999 14:16'!initializeFor: aWonderland	"Initialize the instance variables for the WonderlandActor"	super initialize.	myName _ 'Unnamed'.	myWonderland _ aWonderland.	myParent _ aWonderland getScene.	myParent addChild: self.	"Initialize our material"	myMaterial _ B3DMaterial new.	myMaterial ambientPart: Color white.	myMaterial diffusePart: Color white.	myMaterial specularPart: Color white.	"Set up our default properties"	myColor _ B3DColor4 r: 1.0 g: 1.0 b: 1.0 a: 1.0.	composite _ B3DMatrix4x4 identity.	scaleMatrix _ B3DMatrix4x4 identity.	"Setup the default reactions"	self initializeDefaultReactions.	hidden _ false.	firstClass _ true.! !!WonderlandActor methodsFor: 'initialize' stamp: 'ar 6/3/1999 12:29'!initializeMorphicReactions	"Create a set of default reactions for using active textures"	myReactions _ Dictionary new.	self respondWith: [:event | self morphicLeftMouseDown: event] to: leftMouseDown.	self respondWith: [:event | self morphicLeftMouseUp: event] to: leftMouseUp.	self respondWith: [:event | self morphicKeyPress: event] to: keyPress.	self respondWith: [:event | self morphicMouseMove: event] to: mouseMove.! !!WonderlandActor methodsFor: 'drawing' stamp: 'ar 5/27/1999 16:34'!renderOn: aRenderer	"Draw the actor."	"Save the old transformation matrix"	aRenderer pushMatrix.	"Modify the matrix using our composite matrix for position and orientation"	aRenderer transformBy: composite.	"Save the new transformation matrix"	aRenderer pushMatrix.	"Modify the matrix using our scale matrix - we do this seperately to avoid scaling space"	aRenderer transformBy: scaleMatrix.	"Draw our mesh if the object is not hidden"	(hidden) ifFalse: [ self drawMesh: aRenderer ].	"Remove the scaling matrix"	aRenderer popMatrix.	"Set the default texture for our children"	myTexture ifNotNil:[aRenderer pushTexture; texture: myTexture].	"Draw our children.	Note: For correct picking it is important to use B3DRenderEngine>>render: here."	myChildren do: [:child | aRenderer render: child].	"Restore the old texture"	myTexture ifNotNil:[aRenderer popTexture].	"Restore the old matrix"	aRenderer popMatrix.! !!WonderlandActor methodsFor: 'get property' stamp: 'ar 6/2/1999 15:31'!getActiveTexture	^myTexture isMorph		ifTrue:[myTexture]		ifFalse:[nil]! !!WonderlandActor methodsFor: 'set property' stamp: 'ar 6/3/1999 13:21'!setActiveTexture: aTextureMorph	"Sets the object's texture"	self setTexturePointer: aTextureMorph! !!WonderlandActor methodsFor: 'set property' stamp: 'ar 5/27/1999 16:49'!setTextureFromUser	"Set the texture from the user"	| form |	form _ Form fromUser.	self setTexturePointer: form asTexture.! !!WonderlandActor methodsFor: 'private' stamp: 'ar 6/2/1999 14:05'!setTexturePointer: texture	"Sets the object's texture"	myTexture _ texture.	self partsDo:[:part| part setTexturePointer: texture].! !!WonderlandActor methodsFor: 'morphic reactions' stamp: 'ar 6/2/1999 14:39'!morphicKeyPress: event	"Handle the given event"	myTexture isMorph ifTrue:[myTexture hand addEvent: event].! !!WonderlandActor methodsFor: 'morphic reactions' stamp: 'ar 6/3/1999 12:29'!morphicLeftMouseDown: event	"Handle the given event"	"self respondWith: [:anEvent | self morphicMouseMove: anEvent] to: mouseMove."	myTexture isMorph ifTrue:[myTexture hand addEvent: event].! !!WonderlandActor methodsFor: 'morphic reactions' stamp: 'ar 6/3/1999 13:33'!morphicLeftMouseUp: event	"Handle the given event"	myTexture isMorph ifTrue:[myTexture hand addEvent: event].! !!WonderlandActor methodsFor: 'morphic reactions' stamp: 'ar 6/2/1999 14:39'!morphicMouseMove: event	"Handle the given event"	myTexture isMorph ifTrue:[myTexture hand addEvent: event].! !!WonderlandActor methodsFor: 'enumerating' stamp: 'ar 6/2/1999 14:04'!allPartsDo: aBlock	"Recursively evaluate aBlock with all elements of the receiver that are a part of its parent"	self partsDo:[:child|		aBlock value: child.		child allPartsDo: aBlock]! !!WonderlandActor methodsFor: 'enumerating' stamp: 'ar 6/2/1999 14:03'!partsDo: aBlock	"Evaluate aBlock with all elements of the receiver that are a part of it"	myChildren do:[:child| child isPart ifTrue:[aBlock value: child]].! !!WonderlandActor methodsFor: 'testing' stamp: 'ar 6/2/1999 14:08'!hasParts	"Return true if the receiver has any children that are parts"	self partsDo:[:child| ^true].	^false! !!WonderlandCamera methodsFor: 'rendering' stamp: 'ar 6/2/1999 12:13'!pickObjectAndVertexAt: aPoint	"Return an association with the top object and the primitive vertex at the given point or nil"	^self render: (B3DRenderEngine defaultForPlatformOn: nil) new 		pickingAt: aPoint		withPrimitiveVertex: true.! !!WonderlandCamera methodsFor: 'rendering' stamp: 'ar 6/2/1999 13:20'!render: aRenderEngine pickingAt: aPointOrNil	"Render one frame of the Wonderland using this camera.	If aPointOrNil is not nil then return the top most object at this point.	Note: If picking, no objects are actually drawn."	^self render: aRenderEngine pickingAt: aPointOrNil withPrimitiveVertex: false! !!WonderlandCamera methodsFor: 'rendering' stamp: 'ar 6/2/1999 13:20'!render: aRenderEngine pickingAt: aPointOrNil withPrimitiveVertex: aBoolean	"Render one frame of the Wonderland using this camera.	If aPointOrNil is not nil then pick the top most object at this point.	For picking only: If aBoolean is true return an association 	object -> B3DPrimitiveVertex; otherwise simply return the top most object.	Note: If picking, no objects are actually drawn."	| aRenderer pickedObject |	aRenderer _ aRenderEngine. "A couple of things must be set before we can pick!!"	aRenderer viewport: (myMorph bounds insetBy: 1@1).	(drawSceneBackground)		ifTrue: [ aRenderer clearViewport: (myWonderland getScene) getColorObject. ]		ifFalse: [ ].	aRenderer clearDepthBuffer.	aRenderer loadIdentity.	"Add any existing lights to the renderer for this camera"	(myWonderland getLights) do: [:light | aRenderer addLight: light ].	"Calculate our view matrix by inverting the camera's composite matrix and hand it	to the renderer - note that this will eventually have to walk up the tree"	viewMatrix _ self getMatrixToRoot.	aRenderer transformBy: viewMatrix.	aRenderer perspective: perspective.	"Initialize picking if necessary"	aPointOrNil ifNotNil:[aRenderer _ aRenderer asPickerAt: aPointOrNil].	"Now render the scene"	myWonderland renderWonderland: aRenderer.	"Force the renderer to draw to the screen"	aRenderer finish.	"Fetch the picked object"	aPointOrNil ifNotNil:[		aBoolean			ifTrue:[pickedObject _ aRenderer topMostObject -> aRenderer topMostVertex]			ifFalse:[pickedObject _ aRenderer topMostObject]].	aRenderer destroy.	^pickedObject "Will be nil if not picking"! !!WonderlandCameraMorph methodsFor: 'initialization' stamp: 'ar 6/2/1999 13:43'!initialize	super initialize.	lastCursorPoint _ 0@0.	Project current addGuard: self.! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 14:33'!convertEvent: evt	"Create a Wonderland event that will be dispatched later on from the given MorphicEvent."	| newEvent temp clickedActor clickedVertex |	eventFocus 		ifNil:[ "Find the clicked actor under the cursor"			temp _ myCamera pickObjectAndVertexAt: evt cursorPoint.			clickedActor _ temp key.			clickedVertex _ temp value]		ifNotNil: [ "Route all events to the object having the focus"			clickedActor _ eventFocus.			clickedVertex _ nil].	clickedActor ifNotNil:[		newEvent _ WonderlandEvent new.		newEvent setMorphicEvent: evt.		newEvent setCameraMorph: self.		newEvent setActor: clickedActor.		newEvent setCamera: myCamera.		newEvent setCursorDelta: ((evt cursorPoint) - lastCursorPoint).		newEvent setCursorPoint: (lastCursorPoint _ evt cursorPoint).		newEvent setVertex: clickedVertex.		evt isKeystroke ifTrue:[newEvent setKey: (evt keyCharacter)].	].	^newEvent! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 5/27/1999 15:44'!doFirstPersonControl: aBoolean	firstPersonControls _ aBoolean! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 5/27/1999 15:51'!firstPersonKeystroke: evt	"Handle a keyboard event"	| keyValue |	lastCursorPoint _ evt cursorPoint.	keyValue _ evt keyValue.	keyValue = 28 ifTrue:[^myCamera turn: #left].	keyValue = 29 ifTrue:[^myCamera turn: #right].	keyValue = 30 ifTrue:[^myCamera move: #forward].	keyValue = 31 ifTrue:[^myCamera move: #backward].! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 13:45'!getEventFocus	"Return the actor that all events should go to. If not explicitly specified, choose the actor under the mouse cursor.  If no actor is under the mouse cursor, return nil.	ar 6/2/1999: This method is more or less obsolete. For event handling, #convertEvent: should be used."	self flag: #obsolete.	eventFocus ifNotNil: [ ^ eventFocus ].	^ myCamera pickAt: (Sensor cursorPoint).! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 13:39'!keyStroke: evt	"Handle a keyboard event"	| newEvent reactions |	firstPersonControls == true "For existing camera morphs"		ifTrue:[^self firstPersonKeystroke: evt].	newEvent _ self convertEvent: evt.	newEvent ifNil:[^self].	reactions _ newEvent getActor getReactionsTo: keyPress.	reactions ifNotNil:[ 		reactions do: [:aReaction | aReaction reactTo: newEvent ]].! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 13:39'!mouseDown: evt	"When the user clicks in a camera window, determine which actor the user clicked on and have that actor respond to the event"	| newEvent reactions |	newEvent _ self convertEvent: evt.	newEvent ifNil:[^self].	(evt redButtonPressed)		ifTrue: [reactions _ newEvent getActor getReactionsTo: leftMouseDown.				mouseUpButton _ leftMouseUp. ]		ifFalse: [(evt yellowButtonPressed)			ifTrue: [reactions _ newEvent getActor getReactionsTo: rightMouseDown.					mouseUpButton _ rightMouseUp. ]			ifFalse: [ reactions _ nil ]].	reactions ifNotNil:[ 		reactions do: [:aReaction | aReaction reactTo: newEvent ]].! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 13:43'!mouseMove: evt	"When the user clicks in a camera window, determine which actor the user clicked on and have that actor respond to the event"	| newEvent reactions |	newEvent _ self convertEvent: evt.	newEvent ifNil:[^self].	reactions _ newEvent getActor getReactionsTo: mouseMove.	reactions ifNotNil:[ 		reactions do: [:aReaction | aReaction reactTo: newEvent]].! !!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 13:41'!mouseUp: evt	"When the user clicks in a camera window, determine which actor the user clicked on and have that actor respond to the event"	| newEvent reactions |	newEvent _ self convertEvent: evt.	newEvent ifNil:[^self].	reactions _ newEvent getActor getReactionsTo: mouseUpButton.	reactions ifNotNil:[ 		reactions do: [:aReaction | aReaction reactTo: newEvent ]].! !!WonderlandEvent methodsFor: 'accessing' stamp: 'ar 6/2/1999 14:32'!getMorphicEvent	^morphicEvent! !!WonderlandEvent methodsFor: 'accessing' stamp: 'ar 6/2/1999 13:23'!getVertex	"Return the primitive vertex from picking operations"	^vertex! !!WonderlandEvent methodsFor: 'accessing' stamp: 'ar 6/2/1999 14:32'!setMorphicEvent: aMorphicEvent	morphicEvent _ aMorphicEvent! !!WonderlandEvent methodsFor: 'accessing' stamp: 'ar 6/2/1999 13:23'!setVertex: aB3DPrimitiveVertex	"Set the primitive vertex from picking operations"	vertex _ aB3DPrimitiveVertex.! !!WonderlandHandMorph methodsFor: 'initialization' stamp: 'ar 6/2/1999 17:47'!delete	| world |	world _ self world.	world ifNotNil:[world removeHand: self].	super delete.! !!WonderlandHandMorph methodsFor: 'initialization' stamp: 'ar 6/2/1999 15:19'!initialize	super initialize.	events _ OrderedCollection new.! !!WonderlandHandMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 15:19'!addEvent: aWonderlandEvent	| evt |	evt _ aWonderlandEvent getMorphicEvent copy.	evt setHand: self.	evt setCursorPoint: (self mapPrimitiveVertex: aWonderlandEvent getVertex).	events add: evt.! !!WonderlandHandMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 17:48'!processEvents	(weakTexture at: 1) ifNil:[^self delete].	[events isEmpty] whileFalse:		[self handleEvent: events removeFirst].! !!WonderlandHandMorph methodsFor: 'private' stamp: 'ar 6/2/1999 17:44'!mapPrimitiveVertex: aPrimitiveVertex	"Map the vertex into 2D space. This is the core of using a WonderlandHandMorph."	| pt myTexture |	myTexture _ weakTexture at: 1.	myTexture ifNil:[^-1@-1].	pt _ aPrimitiveVertex texCoordS @ aPrimitiveVertex texCoordT.	^(myTexture extent * pt) asIntegerPoint + myTexture position.! !!WonderlandHandMorph methodsFor: 'private' stamp: 'ar 6/2/1999 17:43'!setTexture: aMorph	weakTexture _ WeakArray with: aMorph.! !!WonderlandTextureMorph commentStamp: 'ar 6/2/1999 13:55' prior: 0!A Wonderland texture morph is a morph that provides an 'active' texture for all morph it contains. In the future, this will be a full world, but for now we have to use some more complicated mapping operations.!!WonderlandTextureMorph methodsFor: 'initialization' stamp: 'ar 6/3/1999 13:15'!initialize	super initialize.	changeFlag _ false.! !!WonderlandTextureMorph methodsFor: 'initialization' stamp: 'ar 5/27/1999 17:55'!initializeWith: aWonderland	myWonderland _ aWonderland.! !!WonderlandTextureMorph methodsFor: 'accessing' stamp: 'ar 6/2/1999 13:59'!asTexture	"Return a Balloon 3D Texture"	(myTexture == nil or:[changeFlag])		ifTrue:[self updateContents].	^myTexture asTexture! !!WonderlandTextureMorph methodsFor: 'accessing' stamp: 'ar 6/2/1999 14:26'!assuredHand	myHand == nil ifTrue:[		myHand _ WonderlandHandMorph new.		myHand setTexture: self.		self world addHand: myHand].	^myHand! !!WonderlandTextureMorph methodsFor: 'accessing' stamp: 'ar 5/27/1999 17:45'!fullBounds	"Clip my children"	^self bounds! !!WonderlandTextureMorph methodsFor: 'accessing' stamp: 'ar 6/2/1999 14:40'!hand	^myHand ifNil:[self assuredHand]! !!WonderlandTextureMorph methodsFor: 'accessing' stamp: 'ar 5/27/1999 18:00'!myTexture	^myTexture! !!WonderlandTextureMorph methodsFor: 'drawing' stamp: 'ar 5/27/1999 17:46'!canvasForSubmorphs: aCanvas	^aCanvas copyClipRect: self bounds! !!WonderlandTextureMorph methodsFor: 'drawing' stamp: 'ar 5/27/1999 18:11'!changed	changeFlag _ true.	super changed.! !!WonderlandTextureMorph methodsFor: 'drawing' stamp: 'ar 5/27/1999 17:47'!fullDrawOn: aCanvas	changeFlag ifTrue:[self updateContents].	^super fullDrawOn: aCanvas! !!WonderlandTextureMorph methodsFor: 'dependents' stamp: 'ar 5/27/1999 17:41'!invalidRect: aRectangle	"Some of my morphs have changed"	changeFlag _ true.	^super invalidRect: aRectangle! !!WonderlandTextureMorph methodsFor: 'private' stamp: 'ar 6/2/1999 15:32'!resetHand	myHand delete.	myHand _ nil.! !!WonderlandTextureMorph methodsFor: 'private' stamp: 'ar 6/2/1999 15:30'!resetTexture	myHand delete.	myHand _ nil.! !!WonderlandTextureMorph methodsFor: 'private' stamp: 'ar 6/3/1999 13:15'!updateContents	"Update the texture"	| canvas |	changeFlag _ false.	(myTexture == nil or:[myTexture extent ~= self fullBounds extent]) ifTrue:[		myTexture _ B3DTexture extent: self extent depth: 32.		myTexture interpolate: false.		myTexture wrap: false.		myTexture envMode: 0.		^self].	canvas _ FormCanvas on: myTexture.	canvas _ canvas copyOffset: self fullBounds topLeft negated.	self fullDrawOn: canvas.! !!WonderlandTextureMorph methodsFor: 'event handling' stamp: 'ar 6/2/1999 15:23'!handlesMouseDown: evt	"This is to prevent moving the entire texture in 2D when clicked on it in 3D"	^true! !