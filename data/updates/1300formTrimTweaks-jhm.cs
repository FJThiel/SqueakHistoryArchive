'From Squeak 2.4c of May 10, 1999 on 30 June 1999 at 2:02:28 pm'!"Change Set:		formTrimTweaks-jhmDate:			2 July 1999Author:			John MaloneyFaster trimming of Forms. Also removes some unused and unneeded Formmanipulation methods, adds a handy instance creation method forUpdatingStringMorph, and makes ADPCM compressAndDecompress: workfor any of the supported bits-per-sample."!!DisplayMedium methodsFor: 'coloring' stamp: 'jm 6/18/1999 19:01'!reverse	"Change all the bits in the receiver that are white to black, and the ones 	that are black to white."	self fill: self boundingBox rule: Form reverse fillColor: (Color quickHighLight: self depth)! !!DisplayMedium methodsFor: 'coloring' stamp: 'jm 6/18/1999 19:00'!reverse: aRectangle	"Change all the bits in the receiver's area that intersects with aRectangle 	that are white to black, and the ones that are black to white."	self fill: aRectangle rule: Form reverse fillColor: (Color quickHighLight: self depth)! !!Form methodsFor: 'image manipulation' stamp: 'jm 6/18/1999 19:04'!rectangleEnclosingPixelsNotOfColor: aColor	"Answer the smallest rectangle enclosing all the pixels of me that are different from the given color. Useful for extracting a foreground graphic from its background."	| cm slice copyBlt countBlt top bottom newH left right |	"map the specified color to 1 and all others to 0"	cm _ Bitmap new: (1 bitShift: (depth min: 15)).	cm primFill: 1.	cm at: (aColor indexInMap: cm) put: 0.	"build a 1-pixel high horizontal slice and BitBlts for counting pixels of interest"	slice _ Form extent: width@1 depth: 1.	copyBlt _ (BitBlt toForm: slice)		sourceForm: self;		combinationRule: Form over;		destX: 0 destY: 0 width: width height: 1;		colorMap: cm.	countBlt _ (BitBlt toForm: slice)		fillColor: (Bitmap with: 0);		destRect: (0@0 extent: slice extent);		combinationRule: 32.	"scan in from top and bottom"	top _ (0 to: height)		detect: [:y |			copyBlt sourceOrigin: 0@y; copyBits.			countBlt copyBits > 0]		ifNone: [^ 0@0 extent: 0@0].	bottom _ (height - 1 to: top by: -1)		detect: [:y |			copyBlt sourceOrigin: 0@y; copyBits.			countBlt copyBits > 0].	"build a 1-pixel wide vertical slice and BitBlts for counting pixels of interest"	newH _ bottom - top + 1.	slice _ Form extent: 1@newH depth: 1.	copyBlt _ (BitBlt toForm: slice)		sourceForm: self;		combinationRule: Form over;		destX: 0 destY: 0 width: 1 height: newH;		colorMap: cm.	countBlt _ (BitBlt toForm: slice)		fillColor: (Bitmap with: 0);		destRect: (0@0 extent: slice extent);		combinationRule: 32.	"scan in from left and right"	left _ (0 to: width)		detect: [:x |			copyBlt sourceOrigin: x@top; copyBits.			countBlt copyBits > 0].	right _ (width - 1 to: left by: -1)		detect: [:x |			copyBlt sourceOrigin: x@top; copyBits.			countBlt copyBits > 0].	^ left@top corner: (right + 1)@(bottom + 1)! !!Form methodsFor: 'image manipulation' stamp: 'jm 6/18/1999 18:41'!tallyPixelValues	"Answer a Bitmap whose elements contain the number of pixels in this Form with the pixel value corresponding to their index. Note that the pixels of multiple Forms can be tallied together using tallyPixelValuesInRect:into:."	^ self tallyPixelValuesInRect: self boundingBox		into: (Bitmap new: (1 bitShift: (self depth min: 15)))"Move a little rectangle around the screen and print its tallies... | r tallies nonZero |Cursor blank showWhile: [[Sensor anyButtonPressed] whileFalse:	[r _ Sensor cursorPoint extent: 10@10.	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.	tallies _ (Display copy: r) tallyPixelValues.	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]			thenCollect: [:i | (tallies at: i) -> (i-1)].	nonZero printString , '          ' displayAt: 0@0.	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]"! !!Form methodsFor: 'image manipulation' stamp: 'jm 6/18/1999 18:47'!tallyPixelValuesInRect: destRect into: valueTable	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."	(BitBlt toForm: self)		sourceForm: self;  "src must be given for color map ops"		sourceOrigin: 0@0;		colorMap: valueTable;		combinationRule: 33;		destRect: destRect;		copyBits.	^ valueTable"Move a little rectangle around the screen and print its tallies... | r tallies nonZero |Cursor blank showWhile: [[Sensor anyButtonPressed] whileFalse:	[r _ Sensor cursorPoint extent: 10@10.	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.	tallies _ (Display copy: r) tallyPixelValues.	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]			thenCollect: [:i | (tallies at: i) -> (i-1)].	nonZero printString , '          ' displayAt: 0@0.	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]"! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'jm 6/18/1999 18:58'!init4	"Just a record of how Ted loaded in the paintbox button images, Feb 98"| bb im pp newImage pic24Bit picNewBit blt |"self loadoffImage: 'roundedPalette3.bmp'."pic24Bit _ GIFReadWriter formFromServerFile: 'updates/137roundedPalette3.bmp'.picNewBit _ Form extent: pic24Bit extent depth: 16.pic24Bit displayOn: picNewBit.OriginalBounds _ picNewBit boundingBox.AllOffImage _ Form extent: OriginalBounds extent depth: 16.blt _ BitBlt toForm: AllOffImage.blt sourceForm: picNewBit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.AllOffImage mapColor: Color transparent to: Color black.self image: AllOffImage.self invalidRect: bounds.self submorphsDo: [:button | button position: button position + (10@10)].(im _ submorphs at: 28) class == ImageMorph ifTrue: [	im position: im position + (2@0)].	"color picker""exercise it once"(bb _ self findButton: #keep:) position: bb position + (0@25).(bb _ self findButton: #toss:) position: bb position + (0@25).(bb _ self findButton: #undo:) position: bb position + (0@-25).(bb _ self findButton: #clear:) position: bb position + (0@-25).(bb _ self findButton: #undo:) position: bb position + (0@-69).(bb _ self findButton: #clear:) position: bb position + (0@-69).self submorphsDo: [:button | 	button class == AlignmentMorph ifTrue: [		button position: button position + (0@25)].	(button printString includesSubString: 'stamp:') ifTrue: [		button position: button position + (0@25)]].(bb _ self findButton: #prevStamp:) position: bb position + (0@25).(bb _ self findButton: #nextStamp:) position: bb position + (0@25).bb _ self findButton: #keep:.newImage _ bb pressedImage copy: (0@4 corner: (bb pressedImage boundingBox extent)).bb onImage: newImage.  bb pressedImage: newImage.  bb extent: newImage extent.bb position: bb position + (4@1).pp _ (bb _ self findButton: #toss:) pressedImage.newImage _ pp copy: (0@4 corner: (bb pressedImage extent - (3@0))).bb onImage: newImage.  bb pressedImage: newImage.  bb extent: newImage extent.bb position: bb position + (3@1).pp _ (bb _ self findButton: #undo:) pressedImage.newImage _ pp copy: (0@0 corner: (bb pressedImage extent - (3@5))).bb onImage: newImage.  bb pressedImage: newImage.  bb extent: newImage extent.bb position: bb position + (3@-1).pp _ (bb _ self findButton: #clear:) pressedImage.newImage _ pp copy: (0@0 corner: (bb pressedImage extent - (0@5))).bb onImage: newImage.  bb pressedImage: newImage.  bb extent: newImage extent.bb position: bb position + (3@-1).pic24Bit _ GIFReadWriter formFromServerFile: 'updates/137pencil.bmp'.picNewBit _ Form extent: pic24Bit extent depth: 16.pic24Bit displayOn: picNewBit.newImage _ picNewBit as8BitColorForm.newImage transparentColor: (Color r: 0 g: 0 b: 0).(bb _ self findButton: #erase:) pressedImage: newImage; onImage: newImage;	extent: newImage extent.bb position: bb position + (-11@-1).! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'jm 6/18/1999 18:58'!loadColorChooser	"Load Forms for ColorMemoryMorph."	| doc closedForm openForm |	doc _ Utilities objectStrmFromUpdates: 'colorPalClosed.obj'.	closedForm _ doc fileInObjectAndCode mapColor: Color transparent to: Color black.	doc _ Utilities objectStrmFromUpdates: 'colorPalOpen.obj'.	openForm _ doc fileInObjectAndCode mapColor: Color transparent to: Color black.	colorMemoryThin image: closedForm.	colorMemoryThin position: self position + (0@140).	colorMemory delete.	"delete old one"	colorMemory _ PaintBoxColorPicker new image: openForm.! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'jm 6/18/1999 18:58'!loadOnImage: fileName	"Read in and convert the image for the paintBox with the buttonson.  A .bmp 24-bit image.  For each button, cut that chunk out and save it."	"	self loadOnImage: 'NoSh_on.bmp'.		AllOnImage _ nil.	'save space'.	"	| pic16Bit blt aa on type |	type _ 'gif'.  "   gif or bmp  "type = 'gif' ifTrue: [	pic16Bit "really 8" _ GIFReadWriter formFromFileNamed: fileName.	pic16Bit display.	aa _ AllOnImage _ Form extent: OriginalBounds extent depth: 8.	blt _ BitBlt toForm: aa.	blt sourceForm: pic16Bit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.	].type = 'bmp' ifTrue: [	pic16Bit _ (Form fromBMPFileNamed: fileName) asFormOfDepth: 16.	pic16Bit display.	aa _ AllOnImage _ Form extent: OriginalBounds extent depth: 16.	blt _ BitBlt toForm: aa.	blt sourceForm: pic16Bit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.	aa mapColor: Color transparent to: Color black.	].	"Collect all the images for the buttons in the on state"	self allMorphsDo: [:button |		(button isKindOf: ThreePhaseButtonMorph) ifTrue: [			type = 'gif' ifTrue: [on _ ColorForm extent: button extent depth: 8.					 on colors: pic16Bit colors]				ifFalse: [on _ Form extent: button extent depth: 16].			on copy: (0@0 extent: button extent)				from: (button topLeft - self topLeft) in: aa rule: Form over.			button onImage: on]].	self invalidRect: bounds.	! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'jm 6/18/1999 18:58'!loadPressedImage: fileName	"Read in and convert the image for the paintBox with the buttonson.  A .bmp 24-bit image.  For each button, cut that chunk out and save it."	"	self loadPressedImage: 'NoSh_on.bmp'.		AllPressedImage _ nil.	'save space'.	"	| pic16Bit blt aa on type |	type _ 'gif'.  "   gif or bmp  "type = 'gif' ifTrue: [	pic16Bit "really 8" _ GIFReadWriter formFromFileNamed: fileName.	pic16Bit display.	aa _ AllPressedImage _ Form extent: OriginalBounds extent depth: 8.	blt _ BitBlt toForm: aa.	blt sourceForm: pic16Bit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.	].type = 'bmp' ifTrue: [	pic16Bit _ (Form fromBMPFileNamed: fileName) asFormOfDepth: 16.	pic16Bit display.	aa _ AllPressedImage _ Form extent: OriginalBounds extent depth: 16.	blt _ BitBlt toForm: aa.	blt sourceForm: pic16Bit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.	aa mapColor: Color transparent to: Color black.	].	"Collect all the images for the buttons in the on state"	self allMorphsDo: [:button |		(button isKindOf: ThreePhaseButtonMorph) ifTrue: [			type = 'gif' ifTrue: [on _ ColorForm extent: button extent depth: 8.					 on colors: pic16Bit colors]				ifFalse: [on _ Form extent: button extent depth: 16].			on copy: (0@0 extent: button extent)				from: (button topLeft - self topLeft) in: aa rule: Form over.			button pressedImage: on]].	self invalidRect: bounds.	! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'jm 6/18/1999 18:58'!loadoffImage: fileName	"Read in and convert the background image for the paintBox.  Allbuttons off.  A .bmp 24-bit image."	"	Prototype loadoffImage: 'roundedPalette3.bmp'	"	| pic16Bit blt type getBounds |	type _ 'bmp'.  " gif or bmp  "	getBounds _ 'fromPic'.	"fromUser = draw out rect of paintbox on image"		"fromOB = just read in new bits, keep same size and place as last time."		"fromPic = picture is just the PaintBox, use its bounds"type = 'gif' ifTrue: [	pic16Bit "really 8" _ GIFReadWriter formFromFileNamed: fileName.	getBounds = 'fromUser' ifTrue: ["Just first time, collect the bounds"			pic16Bit display.			OriginalBounds _ Rectangle fromUser].	getBounds = 'fromPic' ifTrue: [OriginalBounds _ pic16Bit boundingBox].	].		"Use OriginalBounds as it was last time".type = 'bmp' ifTrue: [	pic16Bit _ (Form fromBMPFileNamed: fileName) asFormOfDepth: 16.	getBounds = 'fromUser' ifTrue: ["Just first time, collect the bounds"			pic16Bit display.			OriginalBounds _ Rectangle fromUser].		"Use OriginalBounds as it was last time".	(getBounds = 'fromPic') ifTrue: [OriginalBounds _ pic16Bit boundingBox].	AllOffImage _ Form extent: OriginalBounds extent depth: 16.	].type = 'gif' ifTrue: [	AllOffImage _ ColorForm extent: OriginalBounds extent depth: 8.	AllOffImage colors: pic16Bit colors].	blt _ BitBlt toForm: AllOffImage.	blt sourceForm: pic16Bit; combinationRule: Form over;		sourceRect: OriginalBounds; destOrigin: 0@0; copyBits.type = 'bmp' ifTrue: [AllOffImage mapColor: Color transparent to: Color black].	self image: AllOffImage.	self invalidRect: bounds.	! !!SketchEditorMorph methodsFor: 'start & finish' stamp: 'jm 6/18/1999 17:03'!deliverPainting: result	"Done painting.  May come from resume, or from original call.  Execute user's post painting instructions in the block.  Always use this standard one.  4/21/97 tk"	| newBox newForm |	action == #areaFill: ifTrue: [palette setCurrentBrush: palette brush3a].	palette ifNotNil: "nil happens" [palette setAction: #paint:].	"Get out of odd modes"	"rot _ palette getRotations."	"rotate with heading, or turn to and fro"	"palette setRotation: #normal."	result == #cancel ifTrue: [^ self cancelOutOfPainting].	"for Morphic"	"hostView rotationStyle: rot."		"rotate with heading, or turn to and fro"	newBox _ paintingForm rectangleEnclosingPixelsNotOfColor: Color transparent.	registrationPoint ifNotNil:		[registrationPoint _ registrationPoint - newBox origin]. "relative to newForm origin"	newForm _ 	Form extent: newBox extent depth: paintingForm depth.	newForm copyBits: newBox from: paintingForm at: 0@0 		clippingBox: newForm boundingBox rule: Form over fillColor: nil.	newForm isAllWhite ifTrue: [		(self valueOfProperty: #background) == true 			ifFalse: [^ self cancelOutOfPainting]].	self delete.	"so won't find me again"	dimForm delete.	newPicBlock value: newForm value: (newBox copy translateBy: bounds origin).! !!SketchEditorMorph methodsFor: 'actions & preps' stamp: 'jm 6/18/1999 17:03'!brushAlphaFromGray	"Get currentNib again, (a gray-scale Form) and transform it into an alpha brush.  3/15/97 tk"	| d alphaMap this alpha colorMaker newBox smallNib |	currentNib _ palette getNib.	newBox _ currentNib rectangleEnclosingPixelsNotOfColor: Color transparent.	"minimum size"	smallNib _ Form extent: newBox extent depth: currentNib depth.	smallNib copyBits: newBox from: currentNib at: 0@0 		clippingBox: smallNib boundingBox rule: Form over fillColor: nil."smallNib display.  newBox printString displayAt: 0@50."	d _ currentNib depth.	"usually 8"	alphaMap _ (Color cachedColormapFrom: d to: 32) copy.	"force a map to be there"	1 to: alphaMap size do: [:pixVal |		this _ Color colorFromPixelValue: pixVal-1 depth: d.		alpha _ 1.0 - this brightness.	"based on brightness"		"alpha _ alpha * 0.14 - 0.01."	"Adjust sensitivity for buffer depth"		"alpha _ alpha raisedTo: 2.0."	"Adjust sensitivity for buffer depth"		alphaMap at: pixVal 				put: ((currentColor alpha: alpha) pixelWordForDepth: 32)].		brush _ Form extent: smallNib extent depth: 32.	"brush offset: smallNib offset."	colorMaker _ BitBlt toForm: brush.	colorMaker sourceForm: smallNib; colorMap: alphaMap.	colorMaker sourceOrigin: 0@0; destOrigin: 0@0; combinationRule: Form over;		width: brush width; height: brush height; copyBits.	^ brush	! !!SketchEditorMorph methodsFor: 'actions & preps' stamp: 'jm 6/18/1999 17:03'!rotateScalePrep	"Make a source that is the paintingForm.  Work from that.  3/26/97 tk"	| newBox |	action == #scaleOrRotate ifTrue: [^ self].	"Already doing it"	paintingForm width > 120 		ifTrue: [newBox _ paintingForm rectangleEnclosingPixelsNotOfColor: Color transparent.			"minimum size"			newBox _ newBox insetBy: 				((18 - newBox width max: 0)//2) @ ((18 - newBox height max: 0)//2) * -1]		ifFalse: [newBox _ paintingForm boundingBox].	newBox _ newBox expandBy: 1.	buff _ Form extent: newBox extent depth: paintingForm depth.	buff offset: newBox center - paintingForm center.	buff copyBits: newBox from: paintingForm at: 0@0 		clippingBox: buff boundingBox rule: Form over fillColor: nil.	"Could just run up owner chain asking colorUsed, but may not be embedded"	cumRot _ 0.0.  cumMag _ 1.0.	"start over"	action _ #scaleOrRotate.		"Only changed by mouseDown with tool in paint area"! !!SketchMorph methodsFor: 'menu' stamp: 'jm 6/18/1999 17:00'!erasePixelsOfColor: evt	| c r |	c _ evt hand chooseColor.	originalForm mapColor: c to: Color transparent.	r _ originalForm rectangleEnclosingPixelsNotOfColor: Color transparent.	self form: (originalForm copy: r).! !!Form methodsFor: 'image manipulation' stamp: 'jm 6/30/1999 15:36'!trimBordersOfColor: aColor	"Answer a copy of this Form with each edge trimmed in to the first pixel that is not of the given color. (That is, border strips of the given color are removed)."	| r |	r _ self rectangleEnclosingPixelsNotOfColor: aColor.	^ self copy: r! !!FormEditor methodsFor: 'editing tools' stamp: 'jm 6/30/1999 15:46'!newSourceForm	"Allow the user to define a new source form for the FormEditor. Copying 	the source form onto the display is the primary graphical operation. 	Resets the tool to be repeatCopy."	| dForm interiorPoint interiorColor |	dForm _ Form fromUser: grid.	"sourceForm must be only 1 bit deep"	interiorPoint _ dForm extent // 2.	interiorColor _ dForm colorAt: interiorPoint.	form _ (dForm makeBWForm: interiorColor) reverse				findShapeAroundSeedBlock:					[:f | f pixelValueAt: interiorPoint put: 1].	form _ form trimBordersOfColor: Color white.	tool _ previousTool! !!Paragraph methodsFor: 'selecting' stamp: 'jm 7/1/1999 12:31'!hiliteRect: rect	| highlightColor |	highlightColor _ Color quickHighLight: destinationForm depth.	rect ifNotNil: [		destinationForm			fill: rect			rule: Form reverse			fillColor: highlightColor.		"destinationForm			fill: (rect translateBy: 1@1)			rule: Form reverse			fillColor: highlightColor" ].! !!UpdatingStringMorph class methodsFor: 'instance creation' stamp: 'jm 5/31/1999 21:27'!on: targetObject selector: aSymbol	^ self new		target: targetObject;		getSelector: aSymbol! !!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 7/2/1999 13:29'!compressAndDecompress: aSound	"Compress and decompress the given sound. Overridden to use same bits per sample for both compressing and decompressing."	| compressed decoder |	compressed _ self compressSound: aSound.	decoder _ self class new		initializeForBitsPerSample: bitsPerSample		samplesPerFrame: 0.	^ decoder decompressSound: compressed! !FlashMorph removeSelector: #imageFormAALevel:color:!Form removeSelector: #as32LevelGrayScale!Form removeSelector: #highLight!Form removeSelector: #trimRectForBackgroundColor:!Form removeSelector: #peripheralColor!Form removeSelector: #removeZeroPixelsFromForm!Form removeSelector: #trimToPixelValue:orNot:!Form removeSelector: #tallyPixelValuesPrimitive:into:!