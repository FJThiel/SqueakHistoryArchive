'From Squeak2.8alpha of 16 February 2000 [latest update: #2217] on 28 May 2000 at 1:09:37 pm'!"Change Set:		ExternalGraphics-arDate:			28 May 2000Author:			Andreas RaabThe basics of external graphics support. Provides us with a different kind of DisplayScreen allowing allocation of forms and textures directly. Also provides the base for accelerated fills and blits from and to the display. Acceleration of external forms is currently not supported."!Form subclass: #ExternalForm	instanceVariableNames: 'display argbMap '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-External'!!ExternalForm commentStamp: '<historical>' prior: 0!An ExternalForm is just like any other form. It's only difference is that it is allocated on a specific display and can be used for accelerated blts on the particular display.!ExternalForm class	instanceVariableNames: ''!WeakKeyDictionary subclass: #ExternalFormRegistry	instanceVariableNames: 'lockFlag '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-External'!!ExternalFormRegistry commentStamp: '<historical>' prior: 0!The ExternalFormRegistry needs to be synchronized with rendering to prevent forms from being destroyed during rendering. Only at certain points (that is after a rendering cycle is completed) the texture registry may be cleaned up.!DisplayScreen subclass: #ExternalScreen	instanceVariableNames: 'argbMap allocatedForms '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-External'!!ExternalScreen commentStamp: '<historical>' prior: 0!I represent a DisplayScreen that is not part of the Squeak universe. Subclasses must implement the appropriate primitives for creating, destroying and allocating the appropriate external objects.Note: It is assumed that all external display surfaces are accessible by FXBlt, meaning that any support code must register the surfaces with the surface plugin. This requires that the support code will have a way of accessing the bits of the surface. Although this can be terribly expensive (such as on X where a roundtrip to the server might be required or for an OpenGL display where glReadPixels usually is slow as hell) the appropriate methods should be implemented. This allows for a gradual transition to less expensive model (such as implementing an X11Canvas supporting the drawing primitives of X) and is therefore the preferred solution.In the eventual case that it's known that BitBlt/FXBlt will *never* be used in conjunction with a particular drawing surface, the support code should return a handle that is a) not a SmallInteger (these are used by the surface plugin) and b) not of the 'bitsSize' of a Form. One possible representation for such a handle would be a ByteArray of a non-integral word size (e.g., a ByteArray of size 5,6, or 7). In this case, all attempts to use FXBlt with the drawing surface will simply fail.!B3DTexture subclass: #ExternalTexture	instanceVariableNames: 'display argbMap '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-External'!!ExternalTexture commentStamp: '<historical>' prior: 0!An ExternalTexture is a B3DTexture which is allocated on a specific (hopefully hardware accelerated) display.!ExternalTexture class	instanceVariableNames: ''!!ExternalForm methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 17:42'!destroy	"Destroy the receiver"	^self displayScreen destroyForm: self! !!ExternalForm methodsFor: 'accessing' stamp: 'ar 5/27/2000 20:13'!colormapFromARGB	"Return a ColorMap mapping from canonical ARGB pixel values into the receiver"	^argbMap ifNil:[argbMap _ ColorMap mappingFromARGB: self rgbaBitMasks].! !!ExternalForm methodsFor: 'accessing' stamp: 'ar 5/27/2000 16:56'!displayScreen	"Return the display screen the receiver is allocated on."	^display! !!ExternalForm methodsFor: 'accessing' stamp: 'ar 5/27/2000 18:02'!rgbaBitMasks	"Return the masks for specifying the R,G,B, and A components in the receiver"	^self displayScreen rgbaBitMasksOfForm: self! !!ExternalForm methodsFor: 'testing' stamp: 'ar 5/27/2000 16:54'!isExternalForm	^true! !!ExternalForm methodsFor: 'private' stamp: 'ar 5/27/2000 16:36'!getExternalHandle	"Private. Return the virtual handle used to represent the receiver"	^bits! !!ExternalForm methodsFor: 'private' stamp: 'ar 5/27/2000 16:38'!initializeFrom: aForm	"Private. Initialize the receiver from aForm"	width _ aForm width.	height _ aForm height.	depth _ aForm depth.! !!ExternalForm methodsFor: 'private' stamp: 'ar 5/27/2000 16:52'!setExternalHandle: aHandle on: aDisplay	"Initialize the receiver from the given external handle"	display _ aDisplay.	bits _ aHandle.! !!ExternalForm class methodsFor: 'instance creation' stamp: 'ar 5/27/2000 20:49'!initializeFrom: anotherForm	^self basicNew initializeFrom: anotherForm! !!ExternalFormRegistry methodsFor: 'accessing' stamp: 'ar 5/26/2000 00:40'!lock	lockFlag _ true! !!ExternalFormRegistry methodsFor: 'accessing' stamp: 'ar 5/26/2000 00:40'!unlock	lockFlag _ false.! !!ExternalFormRegistry methodsFor: 'finalization' stamp: 'ar 5/27/2000 00:56'!finalizeValues	"This message is sent when an element has gone away."	lockFlag == true ifTrue:[^self].	self forceFinalization.! !!ExternalFormRegistry methodsFor: 'finalization' stamp: 'ar 5/27/2000 00:55'!forceFinalization	self associationsDo:[:assoc|		assoc key isNil ifTrue:[assoc value destroy].	].	super finalizeValues.! !!ExternalScreen methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 20:16'!destroy	"Destroy the receiver"	allocatedForms ifNotNil:[		allocatedForms lock. "Make sure we don't get interrupted"		allocatedForms associationsDo:[:assoc|			false ifTrue:["Copy the external contents back in?!!"				(FXBlt toForm: assoc key)					sourceForm: assoc value;					combinationRule: 3;					copyBits].			assoc key: nil].		allocatedForms forceFinalization. "actual cleanup done there"		allocatedForms _ nil.	].	bits ifNotNil:[self primDestroyDisplaySurface: bits].	bits _ nil.! !!ExternalScreen methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 01:30'!finish	"Flush the receiver"	self primFinish: bits.	"Now is the time to do some cleanup"	allocatedForms unlock.	allocatedForms finalizeValues.! !!ExternalScreen methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 01:30'!flush	"Flush the receiver"	self primFlush: bits.! !!ExternalScreen methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 20:16'!release	"I am no longer Display. Release any resources if necessary"	self destroy! !!ExternalScreen methodsFor: 'initialize-release' stamp: 'ar 5/28/2000 11:24'!shutDown 	"Minimize Display memory saved in image"	self destroy.! !!ExternalScreen methodsFor: 'accessing' stamp: 'ar 5/27/2000 20:18'!colormapFromARGB	"Return a ColorMap mapping from canonical ARGB pixel values into the receiver"	^argbMap ifNil:[argbMap _ ColorMap mappingFromARGB: self rgbaBitMasks].! !!ExternalScreen methodsFor: 'accessing' stamp: 'ar 5/26/2000 19:44'!defaultBitBltClass	^FXBlt! !!ExternalScreen methodsFor: 'accessing' stamp: 'ar 5/26/2000 19:45'!defaultCanvasClass	^FXFormCanvas! !!ExternalScreen methodsFor: 'accessing' stamp: 'ar 5/26/2000 19:45'!defaultWarpBltClass	^FXBlt! !!ExternalScreen methodsFor: 'accessing' stamp: 'ar 5/27/2000 20:18'!rgbaBitMasks	"Return the masks for specifying the R,G,B, and A components in the receiver"	| rgbaBitMasks |	rgbaBitMasks _ Array new: 4.	self primDisplay: bits colorMasksInto: rgbaBitMasks.	^rgbaBitMasks! !!ExternalScreen methodsFor: 'testing' stamp: 'ar 5/27/2000 20:17'!isExternalForm	"Sorta. Kinda."	^true! !!ExternalScreen methodsFor: 'form support' stamp: 'ar 5/28/2000 00:54'!allocateForm: aForm	"Allocate a new form for the given (Squeak internal) form.	NOTE: The size/depth of the form allocated can not differ	from aForm. If there's no more VRAM/AGP space for the form	or the format is not supported the primitive will fail.		This is contrary to textures which have special requirements	for size/depth and may be allocated at will."	| formHandle |	formHandle _ self primAllocateForm: aForm depth width: aForm width height: aForm height.	formHandle = nil ifTrue:[^nil].	^(ExternalForm initializeFrom: aForm) setExternalHandle: formHandle on: self! !!ExternalScreen methodsFor: 'form support' stamp: 'ar 5/28/2000 01:12'!allocateOrRecycleForm: aForm	"If a form for the given one has already been allocated return it. 	If not, allocate a new form."	| form |	allocatedForms lock. "Rendering may begin any time"	^allocatedForms at: aForm ifAbsent:[		form _ self allocateForm: aForm.		form ifNotNil:[			allocatedForms at: aForm put: form.			aForm hasBeenModified: true].		form]! !!ExternalScreen methodsFor: 'form support' stamp: 'ar 5/27/2000 17:43'!destroyForm: anExternalForm	"Destroy the given external form"	self primDestroyForm: anExternalForm getExternalHandle.	anExternalForm setExternalHandle: nil on: nil! !!ExternalScreen methodsFor: 'form support' stamp: 'ar 5/28/2000 00:48'!formHandleOf: aForm	| displayForm |	aForm ifNil:[^-1].	displayForm _ self allocateOrRecycleForm: aForm.	displayForm ifNil:[^-1].	"Update textureForm if aTexture is dirty"	aForm hasBeenModified ifTrue:[		(FXBlt toForm: displayForm)			sourceForm: aForm destRect: (0@0 extent: displayForm extent);			combinationRule: 3;			copyBits.		aForm hasBeenModified: false].	^displayForm getExternalHandle! !!ExternalScreen methodsFor: 'form support' stamp: 'ar 5/27/2000 18:03'!rgbaBitMasksOfForm: anExternalForm	| rgbaBitMasks |	rgbaBitMasks _ Array new: 4.	self primForm: anExternalForm getExternalHandle colorMasksInto: rgbaBitMasks.	^rgbaBitMasks! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/28/2000 01:12'!allocateOrRecycleTexture: aB3DTexture	"If a texture for the given one has already been allocated return it. If not, allocate a new texture."	| texture |	allocatedForms lock. "Rendering may begin any time"	^allocatedForms at: aB3DTexture ifAbsent:[		texture _ self allocateTexture: aB3DTexture.		texture ifNotNil:[			allocatedForms at: aB3DTexture put: texture.			aB3DTexture hasBeenModified: true].		texture]! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/28/2000 00:26'!allocateTexture: aB3DTexture	"Allocate a new texture for the given (Squeak internal) form.	NOTE: The size/depth of the texture allocated can differ. Right now	there's an implicit strategy in the primitive code for choosing the	right tradeoff between speed and space. In the optimal case this	will result in a texture which is 'good enough' for what we have	but if that can't be achieved anything might come back.		Also, textures might be subject to certain restrictions. Some 	graphics cards have minimum/maximum sizes of textures (some 	older even require squared textures) and this needs to be taken	into account by the primitive.		One thing that's currently not handled is if insufficient	memory is encountered. This can happen if there's just not enough	VRAM or AGP memory. A good idea would be to free up some of	the already allocated textures but it's not quite sure if that'll do	the trick and it would require flushing the renderer. Tricky."	| textureHandle |	textureHandle _ self primAllocateTexture: aB3DTexture depth width: aB3DTexture width height: aB3DTexture height.	textureHandle = nil ifTrue:[^nil].	"And return the allocated texture.	Note: #setExternalHandle: will automatically check for w/h/d"	^(ExternalTexture initializeFrom: aB3DTexture) setExternalHandle: textureHandle on: self! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/27/2000 17:04'!depthOfTexture: anExternalTexture	"Return the actual height of the given external texture"	^self primGetTextureDepth: anExternalTexture getExternalHandle! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/27/2000 17:44'!destroyTexture: anExternalTexture	"Destroy the given external form"	self primDestroyTexture: anExternalTexture getExternalHandle.	anExternalTexture setExternalHandle: nil on: nil.! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/27/2000 17:04'!heightOfTexture: anExternalTexture	"Return the actual height of the given external texture"	^self primGetTextureHeight: anExternalTexture getExternalHandle! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/27/2000 18:04'!rgbaBitMasksOfTexture: anExternalTexture	| rgbaBitMasks |	rgbaBitMasks _ Array new: 4.	self primTexture: anExternalTexture getExternalHandle colorMasksInto: rgbaBitMasks.	^rgbaBitMasks! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/28/2000 00:49'!textureHandleOf: aTexture	| textureForm |	aTexture ifNil:[^-1].	textureForm _ self allocateOrRecycleTexture: aTexture.	textureForm ifNil:[^-1].	"Update textureForm if aTexture is dirty"	aTexture hasBeenModified ifTrue:[		(FXBlt toForm: textureForm)			sourceForm: aTexture destRect: (0@0 extent: textureForm extent);			combinationRule: 3;			copyBits.		aTexture hasBeenModified: false].	^textureForm getExternalHandle! !!ExternalScreen methodsFor: 'texture support' stamp: 'ar 5/27/2000 17:03'!widthOfTexture: anExternalTexture	"Return the actual width of the given external texture"	^self primGetTextureWidth: anExternalTexture getExternalHandle! !!ExternalScreen methodsFor: 'blitting support' stamp: 'ar 5/27/2000 21:15'!copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: hf map: map	"Attempt to accelerate blts to the receiver"	| r |	((self isBltAccelerated: rule for: sourceForm) "must support blt"		and:[map == nil and:[hf == nil "no color map/fill color support for now"			and:[(allocatedForms includesKey: sourceForm) "must be pre-allocated"]]]) ifTrue:[		"Try an accelerated blt"		r _ (destOrigin extent: sourceRect extent) intersect: (clipRect intersect: clippingBox).		r area <= 0 ifTrue:[^self].		(self primBltFast: bits from: (self formHandleOf: sourceForm)			at: r origin from: sourceRect origin			extent: r extent) ifNotNil:[^self].	].	^super copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: hf map: map! !!ExternalScreen methodsFor: 'blitting support' stamp: 'ar 5/28/2000 02:06'!displayOn: destForm at: destOrigin clippingBox: clipRect rule: rule fillColor: hf	"Attempt to accelerate blts to aDisplayMedium"	| map sourceRect |	map _ self colormapIfNeededFor: destForm.	(rule = Form over and:[map = nil and:[hf = nil "plain blt, no color map, no fillColor"		and:[allocatedForms includesKey: destForm]]]) ifTrue:[		"Try an accelerated blt"		sourceRect _ (clipRect translateBy: destOrigin negated) intersect: clippingBox.		(self primBltFast: bits to: (self formHandleOf: destForm)			at: 0@0 from: sourceRect origin			extent: sourceRect extent) ifNotNil:[^self]].	destForm copyBits: self boundingBox		from: self		at: destOrigin + self offset		clippingBox: clipRect		rule: rule		fillColor: hf		map: map.! !!ExternalScreen methodsFor: 'blitting support' stamp: 'ar 5/27/2000 21:09'!fill: aRectangle rule: anInteger fillColor: aColor 	"Replace a rectangular area of the receiver with the pattern described by aForm 	according to the rule anInteger."	| rect |	(self isFillAccelerated: anInteger for: aColor) ifTrue:[		rect _ aRectangle intersect: clippingBox.		(self primFill: bits			color: (self pixelWordFor: aColor)			x: rect left			y: rect top			w: rect width			h: rect height) ifNotNil:[^self]].	^super fill: aRectangle rule: anInteger fillColor: aColor! !!ExternalScreen methodsFor: 'blitting support' stamp: 'ar 5/28/2000 00:53'!isBltAccelerated: ruleInteger for: aForm	"Return true if the receiver can perform accelerated blt operations by itself.	It is assumed that blts at the same depth using Form>>over may be accelerated.	Although some hardware may allow source-key blts (that is, Form>>paint or similar)	this is usually questionable and the additional effort for allocating and	maintaining the OS form doesn't quite seem worth the effort."	^aForm depth = self depth and:[ruleInteger = Form over]! !!ExternalScreen methodsFor: 'blitting support' stamp: 'ar 5/27/2000 16:09'!isFillAccelerated: ruleInteger for: aColor	"Return true if the receiver can perform accelerated fill operations by itself.	It is assumed that the hardware can accelerate plain color fill operations."	^ruleInteger = Form over and:[aColor isColor]! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:18'!primBltFast: displayHandle from: sourceHandle at: destOrigin from: sourceOrigin extent: extent	"Primitive. Perform a fast blt operation. Return the receiver if successful."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/28/2000 01:46'!primBltFast: displayHandle to: dstHandle at: destOrigin from: sourceOrigin extent: extent	"Primitive. Perform a fast blt operation. Return the receiver if successful."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:18'!primCreateDisplaySurface: d width: w height: h	"Primitive. Create a new external display surface. Return the handle used to identify the receiver. Fail if the surface cannot be created."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:18'!primDestroyDisplaySurface: aHandle	"Primitive. Destroy the display surface associated with the given handle."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:19'!primDisplay: aHandle colorMasksInto: anArray	"Primitive. Store the bit masks for each color into the given array."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:19'!primFill: handle color: pixelWord x: x y: y w: w h: h	"Primitive. Perform an accelerated fill operation on the receiver."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:20'!primFinish: aHandle	"Primitive. Finish all rendering operations on the receiver.	Do not return before all rendering operations have taken effect."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:21'!primFlush: aHandle	"Primitive. If any rendering operations are pending, force them to be executed.	Do not wait until they have taken effect."	^nil! !!ExternalScreen methodsFor: 'primitives-display' stamp: 'ar 5/27/2000 17:21'!supportsDisplayDepth: pixelDepth	"Return true if this pixel depth is supported on the current host platform."	^false! !!ExternalScreen methodsFor: 'primitives-forms' stamp: 'ar 5/27/2000 17:22'!primAllocateForm: d width: w height: h	"Primitive. Allocate a form with the given parameters"	^nil! !!ExternalScreen methodsFor: 'primitives-forms' stamp: 'ar 5/27/2000 17:22'!primDestroyForm: aHandle	"Primitive. Destroy the form associated with the given handle."	^nil! !!ExternalScreen methodsFor: 'primitives-forms' stamp: 'ar 5/27/2000 17:22'!primForm: aHandle colorMasksInto: anArray	"Primitive. Store the bit masks for each color into the given array."	^nil! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:23'!primAllocateTexture: d width: w height: h	"Primitive. Allocate a texture with the given dimensions.	Note: The texture allocated may *not* match the specified	values here."	^nil! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:23'!primDestroyTexture: aHandle	"Primitive. Destroy the texture associated with the given handle."	^nil! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:23'!primGetTextureDepth: aHandle	"Primitive. Return the actual depth of the texture with the given handle"	^self primitiveFailed! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:24'!primGetTextureHeight: aHandle	"Primitive. Return the actual height of the texture with the given handle"	^self primitiveFailed! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:24'!primGetTextureWidth: aHandle	"Primitive. Return the actual width of the texture with the given handle"	^self primitiveFailed! !!ExternalScreen methodsFor: 'primitives-textures' stamp: 'ar 5/27/2000 17:24'!primTexture: aHandle colorMasksInto: anArray	"Primitive. Store the bit masks for each color into the given array."	^nil! !!ExternalScreen methodsFor: 'private' stamp: 'ar 5/27/2000 22:03'!setExtent: aPoint depth: bitsPerPixel	"Create a 3D accelerated display screen"	| screen |	(depth == bitsPerPixel and: [aPoint = self extent and: 					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [		bits ifNotNil:[self primDestroyDisplaySurface: bits].		bits _ nil.  "Free up old bitmap in case space is low"		DisplayChangeSignature _ (DisplayChangeSignature ifNil: [0]) + 1.		(self supportsDisplayDepth: bitsPerPixel)			ifTrue:[depth _ bitsPerPixel]			ifFalse:["Search for a suitable depth"					depth _ self findAnyDisplayDepthIfNone:[nil]].		depth == nil ifFalse:[			bits _ self primCreateDisplaySurface: depth 					width: aPoint x height: aPoint y].		"Bail out if surface could not be created"		(bits == nil) ifTrue:[			screen _ DisplayScreen extent: aPoint depth: bitsPerPixel.			self == Display ifTrue:[				Display _ screen.				Display beDisplay].			^screen].		width _ aPoint x.		height _ aPoint y.	].	clippingBox _ super boundingBox.	allocatedForms ifNil:[		allocatedForms _ ExternalFormRegistry new.		WeakArray addWeakDependent: allocatedForms].! !!ExternalTexture methodsFor: 'initialize-release' stamp: 'ar 5/27/2000 16:58'!destroy	"Destroy the receiver"	^display destroyTexture: self! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 17:10'!actualDepth	"Return the actual depth of the receiver"	^self displayScreen depthOfTexture: self! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 17:09'!actualHeight	"Return the actual height of the receiver"	^self displayScreen heightOfTexture: self! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 17:09'!actualWidth	"Return the actual width of the receiver"	^self displayScreen widthOfTexture: self! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 20:19'!colormapFromARGB	"Return a ColorMap mapping from canonical ARGB pixel values into the receiver"	^argbMap ifNil:[argbMap _ ColorMap mappingFromARGB: self rgbaBitMasks].! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 16:58'!displayScreen	"Return the display screen the receiver is allocated on."	^display! !!ExternalTexture methodsFor: 'accessing' stamp: 'ar 5/27/2000 20:20'!rgbaBitMasks	"Return the masks for specifying the R,G,B, and A components in the receiver"	^self displayScreen rgbaBitMasksOfTexture: self! !!ExternalTexture methodsFor: 'testing' stamp: 'ar 5/27/2000 16:58'!isExternalForm	"Sorta. Kinda. But not really..."	^true! !!ExternalTexture methodsFor: 'testing' stamp: 'ar 5/27/2000 17:11'!isExternalTexture	^true! !!ExternalTexture methodsFor: 'private' stamp: 'ar 5/27/2000 16:59'!getExternalHandle	"Private. Return the virtual handle used to represent the receiver"	^bits! !!ExternalTexture methodsFor: 'private' stamp: 'ar 5/27/2000 17:01'!initializeFrom: aTexture	"Private. Initialize the receiver from aTexture.	Note: width/height/depth are not set here since textures	are subject to restricted allocation and need to be handled	specially."	wrap _ aTexture wrap.	envMode _ aTexture envMode.	interpolate _ aTexture interpolate.! !!ExternalTexture methodsFor: 'private' stamp: 'ar 5/27/2000 17:43'!setExternalHandle: aHandle on: aDisplay	"Initialize the receiver from the given external handle"	display _ aDisplay.	bits _ aHandle.	(display notNil and:[bits notNil]) ifTrue:[		"Now we can find out what the format of the receiver is"		width _ self actualWidth.		height _ self actualHeight.		depth _ self actualDepth.	].! !!ExternalTexture class methodsFor: 'instance creation' stamp: 'ar 5/27/2000 20:49'!initializeFrom: anotherForm	^self basicNew initializeFrom: anotherForm! !