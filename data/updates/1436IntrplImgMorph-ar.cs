'From Squeak 2.5 of August 6, 1999 on 10 September 1999 at 3:11:37 pm'!"Change Set:		IntrplImgMorph-arDate:			10 September 1999Author:			Andreas RaabAdds an InterpolatedImageMorph to Morphic-Demos (using bi-linear interpolation from the Balloon 3D engine) and fixes a pretty big bug in Balloon3D itself..."!ImageMorph subclass: #InterpolatingImageMorph	instanceVariableNames: 'cachedImage '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Demo'!!B3DIndexedQuadMesh methodsFor: 'private' stamp: 'ar 9/10/1999 15:05'!plainTextureRect	"Create a new plain rectangle w/ texture coords"	vertices _ B3DVector3Array new: 4.	vertices at: 1 put: (-1@-1@0).	vertices at: 2 put: (1@-1@0).	vertices at: 3 put: (1@1@0).	vertices at: 4 put: (-1@1@0).	vtxTexCoords _ B3DTexture2Array new: 4.	vtxTexCoords at: 1 put: (0@1).	vtxTexCoords at: 2 put: (1@1).	vtxTexCoords at: 3 put: (1@0).	vtxTexCoords at: 4 put: (0@0).	faces _ B3DIndexedQuadArray new: 1.	faces at: 1 put: (B3DIndexedQuad with: 1 with: 2 with: 3 with: 4).! !!B3DPrimitiveRasterizer methodsFor: 'processing' stamp: 'ar 9/10/1999 14:58'!processIndexedQuads: vb	"Process an indexed quad set"	| objSize |	objSize _ self primObjectSize + (vb vertexCount + 1 * PrimVertexSize) + (vb indexCount).	self addPrimitiveObject: vb ofSize: objSize.! !!B3DPrimitiveRasterizer methodsFor: 'processing' stamp: 'ar 9/10/1999 14:59'!processIndexedTriangles: vb	| objSize |	objSize _ self primObjectSize + (vb vertexCount + 1 * PrimVertexSize) + (vb indexCount).	self addPrimitiveObject: vb ofSize: objSize.! !!InterpolatingImageMorph commentStamp: 'ar 9/10/1999 14:36' prior: 0!InterpolatingImageMorphs use bi-linear interpolation as provided by the Balloon 3D engine.!!InterpolatingImageMorph reorganize!('accessing' image:)('drawing' cachedImageForDepth: drawImage:on: drawOn:)('geometry' extent:)!!InterpolatingImageMorph methodsFor: 'accessing' stamp: 'ar 9/10/1999 15:01'!image: newImage	cachedImage _ nil.	super image: newImage! !!InterpolatingImageMorph methodsFor: 'drawing' stamp: 'ar 9/10/1999 14:40'!cachedImageForDepth: aDepth	(cachedImage ~~ nil and:[cachedImage depth = aDepth])		ifTrue:[^cachedImage].	cachedImage _ nil. "In case we need some space"	cachedImage _ Form extent: self bounds extent depth: aDepth.	self drawImage: image on: (FormCanvas on: cachedImage).	^cachedImage! !!InterpolatingImageMorph methodsFor: 'drawing' stamp: 'ar 9/10/1999 14:51'!drawImage: aForm on: aCanvas	"Draw the given form onto the canvas using the Balloon 3D engine"	| engine |	engine _ (B3DRenderEngine defaultForPlatformOn: aCanvas form) new.	engine == nil ifTrue:[^self].	"Setup the engine"	engine canvas: aCanvas.	engine viewport: aCanvas form boundingBox.	"Install the material to be used (using a plain white emission color)"	engine material: (B3DMaterial new emission: Color white).	"Install the texture"	engine texture: aForm.	"Draw the mesh"	engine render: (B3DIndexedQuadMesh new plainTextureRect).	"and finish"	engine finish.! !!InterpolatingImageMorph methodsFor: 'drawing' stamp: 'ar 9/10/1999 14:50'!drawOn: aCanvas	aCanvas image: (self cachedImageForDepth: aCanvas depth) at: self topLeft! !!InterpolatingImageMorph methodsFor: 'geometry' stamp: 'ar 9/10/1999 14:38'!extent: extentPoint	bounds extent = extentPoint ifFalse: [		cachedImage _ nil.		self changed.		bounds _ bounds topLeft extent: extentPoint.		self layoutChanged.		self changed].! !