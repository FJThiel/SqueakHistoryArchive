'From Squeak3.1alpha of 28 February 2001 [latest update: #4004] on 10 May 2001 at 4:01:39 pm'!"Change Set:		B3DTfmFixes-arDate:			10 May 2001Author:			Andreas RaabA few fixes for transforming meshes."!!B3DIndexedMesh methodsFor: 'modifying' stamp: 'ar 5/10/2001 15:23'!transformBy: aMatrix	"Modify the mesh by transforming it using a matrix; this allows us to change the insertion point of the mesh"	| transformer primVtx |	transformer _ B3DPrimitiveTransformer new.	primVtx _ B3DPrimitiveVertex new.	vertices do:[:vtx|		primVtx position: vtx.		transformer privateTransformPrimitiveVertex: primVtx byModelView: aMatrix.		vtx loadFrom: primVtx position].	vtxNormals ifNotNil:[		vtxNormals do:[:nrm|			primVtx normal: nrm.			transformer privateTransformPrimitiveNormal: primVtx byMatrix: aMatrix rescale: true.			nrm loadFrom: primVtx normal]].	bBox ifNotNil: [self computeBoundingBox].	vtxNormals ifNil:[self computeVertexNormals].! !!B3DMatrix4x4 class methodsFor: 'instance creation' stamp: 'ar 5/10/2001 15:27'!withRotation: angle around: axis	^self new rotation: angle around: axis! !!B3DMultiMesh methodsFor: 'modifying' stamp: 'ar 5/10/2001 15:23'!transformBy: aMatrix	meshes do:[:m| m transformBy: aMatrix].! !