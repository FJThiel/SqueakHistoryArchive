'From Squeak3.2 of 12 January 2002 [latest update: #4956] on 5 September 2002 at 9:45:08 pm'!"Change Set:		B3DRendererFlagsDate:			5 September 2002Author:			Andreas RaabAdds renderer creation flags for creating HWA renderers. Besides using software/hardware renderers (which are now handled through the flags and no longer through individual booleans) a new flag is introduced to indicate the request for stencil buffering."B3DHardwareEngine addClassVarName:'B3DHardwareRenderer'.B3DHardwareEngine addClassVarName:'B3DSoftwareRenderer'.B3DHardwareEngine addClassVarName:'B3DStencilBuffer'.!!B3DAcceleratorPlugin methodsFor: 'primitives-renderer' stamp: 'ar 9/5/2002 16:53'!primitiveCreateRenderer	"NOTE: This primitive is obsolete but should be supported for older images"	| h w y x result allowHardware allowSoftware |	self export: true.	interpreterProxy methodArgumentCount = 6		ifFalse:[^interpreterProxy primitiveFail].	h _ interpreterProxy stackIntegerValue: 0.	w _ interpreterProxy stackIntegerValue: 1.	y _ interpreterProxy stackIntegerValue: 2.	x _ interpreterProxy stackIntegerValue: 3.	allowHardware _ interpreterProxy booleanValueOf: (interpreterProxy stackValue: 4).	allowSoftware _ interpreterProxy booleanValueOf: (interpreterProxy stackValue: 5).	interpreterProxy failed ifTrue:[^nil].	result _ self cCode:'b3dxCreateRenderer(allowSoftware, allowHardware, x, y, w, h)'.	result < 0 ifTrue:[^interpreterProxy primitiveFail].	interpreterProxy pop: 7.	^interpreterProxy pushInteger: result.! !!B3DAcceleratorPlugin methodsFor: 'primitives-renderer' stamp: 'ar 9/5/2002 16:52'!primitiveCreateRendererFlags	| flags h w y x result  |	self export: true.	interpreterProxy methodArgumentCount = 5		ifFalse:[^interpreterProxy primitiveFail].	h _ interpreterProxy stackIntegerValue: 0.	w _ interpreterProxy stackIntegerValue: 1.	y _ interpreterProxy stackIntegerValue: 2.	x _ interpreterProxy stackIntegerValue: 3.	flags _ interpreterProxy stackIntegerValue: 4.	interpreterProxy failed ifTrue:[^nil].	result _ self cCode:'b3dxCreateRendererFlags(x, y, w, h, flags)'.	result < 0 ifTrue:[^interpreterProxy primitiveFail].	interpreterProxy pop: 6.	^interpreterProxy pushInteger: result.! !!B3DHardwareEngine methodsFor: 'initialize' stamp: 'ar 9/5/2002 16:51'!defaultFlags	"Answer the flags to use by default when creating a new renderer.	By default we request either software or hardware renderer with no specific flags."	^B3DHardwareRenderer + B3DSoftwareRenderer! !!B3DHardwareEngine methodsFor: 'initialize' stamp: 'ar 9/5/2002 16:50'!initializeIn: bounds	handle _ self primCreateRenderer: self defaultFlags 		x: bounds left y: bounds top w: bounds width h: bounds height.	bufRect _ bounds.	handle ifNil:[^nil].	self initializeTarget.	^self! !!B3DHardwareEngine methodsFor: 'primitives-renderer' stamp: 'ar 9/5/2002 21:40'!primCreateRenderer: flags x: x y: y w: w h: h	<primitive: 'primitiveCreateRendererFlags' module:'B3DAcceleratorPlugin'>	"If the above fails, retry with the old interface"	^self primCreateRendererSW: (flags anyMask: B3DSoftwareRenderer)		hw: (flags anyMask: B3DHardwareRenderer)		x: x y: y w: w h: h! !!B3DHardwareEngine class methodsFor: 'class initialization' stamp: 'ar 9/5/2002 21:41'!initialize	"B3DHardwareEngine initialize"	B3DSoftwareRenderer := 1.	B3DHardwareRenderer := 2.	B3DStencilBuffer := 4.! !B3DHardwareEngine initialize!