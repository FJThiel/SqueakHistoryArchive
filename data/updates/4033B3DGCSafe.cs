'From Squeak3.1alpha of 28 February 2001 [latest update: #4032] on 16 May 2001 at 5:48:08 pm'!"Change Set:		B3DGCSafeDate:			16 May 2001Author:			Andreas RaabFix a few primitives that may cause GCs."!!B3DAcceleratorPlugin methodsFor: 'primitives-textures' stamp: 'ar 5/16/2001 17:46'!primitiveTextureGetColorMasks	| handle result masks array renderer arrayOop |	self export: true.	self var: #masks declareC:'int masks[4]'.	interpreterProxy methodArgumentCount = 3		ifFalse:[^interpreterProxy primitiveFail].	array _ interpreterProxy stackObjectValue: 0.	handle _ interpreterProxy stackIntegerValue: 1.	renderer _ interpreterProxy stackIntegerValue: 2.	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy fetchClassOf: array) = interpreterProxy classArray		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy slotSizeOf: array) = 4		ifFalse:[^interpreterProxy primitiveFail].	result _ self cCode:'b3dxTextureColorMasks(renderer, handle, masks)' inSmalltalk:[false].	result ifFalse:[^interpreterProxy primitiveFail].	arrayOop _ array.	0 to: 3 do:[:i|		interpreterProxy pushRemappableOop: arrayOop.		result _ interpreterProxy positive32BitIntegerFor: (masks at: i).		arrayOop _ interpreterProxy popRemappableOop.		interpreterProxy storePointer: i ofObject: arrayOop withValue: result].	^interpreterProxy pop: 3. "pop args return receiver"! !!B3DAcceleratorPlugin methodsFor: 'primitives-renderer' stamp: 'ar 5/16/2001 17:46'!primitiveGetRendererColorMasks	| handle result masks array arrayOop |	self export: true.	self var: #masks declareC:'int masks[4]'.	interpreterProxy methodArgumentCount = 2		ifFalse:[^interpreterProxy primitiveFail].	array _ interpreterProxy stackObjectValue: 0.	handle _ interpreterProxy stackIntegerValue: 1.	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy fetchClassOf: array) = interpreterProxy classArray		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy slotSizeOf: array) = 4		ifFalse:[^interpreterProxy primitiveFail].	result _ self cCode:'b3dxGetRendererColorMasks(handle, masks)' inSmalltalk:[false].	result ifFalse:[^interpreterProxy primitiveFail].	arrayOop _ array.	0 to: 3 do:[:i|		interpreterProxy pushRemappableOop: arrayOop.		result _ interpreterProxy positive32BitIntegerFor: (masks at: i).		arrayOop _ interpreterProxy popRemappableOop.		interpreterProxy storePointer: i ofObject: arrayOop withValue: result].	^interpreterProxy pop: 2. "pop args return receiver"! !!B3DClipperPlugin methodsFor: 'clipping' stamp: 'ar 5/16/2001 17:44'!mapVB: vtxArray ofSize: vtxCount into: boxArray	| left right top bottom vtxPtr flags w x y oop floatOop |	self var: #vtxPtr declareC:'float *vtxPtr'.	self var: #vtxArray declareC:'void *vtxArray'.	self var: #x declareC:'double x'.	self var: #y declareC:'double y'.	self var: #w declareC:'double w'.	self var: #left declareC:'double left'.	self var: #right declareC:'double right'.	self var: #top declareC:'double top'.	self var: #bottom declareC:'double bottom'.	vtxPtr _ self cCoerce: vtxArray to: 'float *'.	1 to: vtxCount do:[:i|		flags _ (self cCoerce: vtxPtr to:'int *') at: PrimVtxClipFlags.		w _ vtxPtr at: PrimVtxRasterPosW.		w = 0.0 ifFalse:[w _ 1.0 / w].		(flags bitAnd: OutLeftBit) ~= 0			ifTrue:[x _ -1.0]			ifFalse:[(flags bitAnd: OutRightBit) ~= 0				ifTrue:[x _ 1.0]				ifFalse:[x _ (vtxPtr at: PrimVtxRasterPosX) * w]].		(flags bitAnd: OutTopBit) ~= 0			ifTrue:[y _ -1.0]			ifFalse:[(flags bitAnd: OutBottomBit) ~= 0				ifTrue:[y _ 1.0]				ifFalse:[y _ (vtxPtr at: PrimVtxRasterPosY) * w]].		i = 1 ifTrue:[			left _ right _ x.			top _ bottom _ y.		].		x < left ifTrue:[left _ x].		x > right ifTrue:[right _ x].		y < top ifTrue:[top _ y].		y > bottom ifTrue:[bottom _ y].		vtxPtr _ vtxPtr + PrimVertexSize.	].	oop _ boxArray.	interpreterProxy pushRemappableOop: oop.	floatOop _ interpreterProxy floatObjectOf: left.	oop _ interpreterProxy popRemappableOop.	interpreterProxy storePointer: 0 ofObject: oop withValue: floatOop.	interpreterProxy pushRemappableOop: oop.	floatOop _ interpreterProxy floatObjectOf: top.	oop _ interpreterProxy popRemappableOop.	interpreterProxy storePointer: 0 ofObject: oop withValue: floatOop.	interpreterProxy pushRemappableOop: oop.	floatOop _ interpreterProxy floatObjectOf: right.	oop _ interpreterProxy popRemappableOop.	interpreterProxy storePointer: 0 ofObject: oop withValue: floatOop.	interpreterProxy pushRemappableOop: oop.	floatOop _ interpreterProxy floatObjectOf: bottom.	oop _ interpreterProxy popRemappableOop.	interpreterProxy storePointer: 0 ofObject: oop withValue: floatOop.! !