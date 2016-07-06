'From Squeak 2.5 of August 6, 1999 on 3 September 1999 at 7:08:14 pm'!"Change Set:		FlashFixesPart4-arDate:			3 September 1999Author:			Andreas RaabThe 4th set of fixes (and the last one for today - I'm getting tired of this). Dealing with morphed shapes. Not completely working but hopefully enough for Florida."!FlashMorph subclass: #FlashCharacterMorph	instanceVariableNames: 'id stepTime frame renderTime vData mData dData cmData rData '	classVariableNames: ''	poolDictionaries: ''	category: 'MM-Flash-Morphs'!FlashFileReader subclass: #FlashMorphReader	instanceVariableNames: 'location fillStyles lineStyles shapes fonts forms sounds buttons lineSequence currentShape player spriteOwners stepTime frameRate frame activeMorphs passiveMorphs activeFont textOffset textHeight textMorph canCompressPoints pointList compressionBounds fillIndex0 fillIndex1 lineStyleIndex leftFillList rightFillList lineStyleList streamingSound morphedFillStyles morphedLineStyles '	classVariableNames: ''	poolDictionaries: ''	category: 'MM-Flash-Import'!FlashCharacterMorph subclass: #FlashMorphingMorph	instanceVariableNames: 'srcShapes dstShapes morphShapes '	classVariableNames: ''	poolDictionaries: ''	category: 'MM-Flash-Morphs'!!CompressedBoundaryShape methodsFor: 'morphing' stamp: 'ar 9/3/1999 17:19'!morphFrom: srcShape to: dstShape at: ratio	| scale unscale srcPoints dstPoints pt1 pt2 x y |	scale _ (ratio * 1024) asInteger.	scale < 0 ifTrue:[scale _ 0].	scale > 1024 ifTrue:[scale _ 1024].	unscale _ 1024 - scale.	srcPoints _ srcShape points.	dstPoints _ dstShape points.	1 to: points size do:[:i|		pt1 _ srcPoints at: i.		pt2 _ dstPoints at: i.		x _ ((pt1 x * unscale) + (pt2 x * scale)) bitShift: -10.		y _ ((pt1 y * unscale) + (pt2 y * scale)) bitShift: -10.		points at: i put: x@y].! !!FlashCharacterMorph methodsFor: 'initialize' stamp: 'ar 9/3/1999 18:03'!initialize	super initialize.	frame _ 1.	self reset.! !!FlashCharacterMorph methodsFor: 'initialize' stamp: 'ar 9/3/1999 18:02'!reset	self removeAllKeyFrameData.	self matrix: MatrixTransform2x3 identity atFrame: 0.	self visible: false atFrame: 0.	self depth: 0 atFrame: 0.	self ratio: 0.0 atFrame: 0.	self visible: true.! !!FlashCharacterMorph methodsFor: 'keyframe data' stamp: 'ar 9/3/1999 15:20'!ratio: aNumber atFrame: frameNumber	^self ratioData at: frameNumber put: aNumber! !!FlashCharacterMorph methodsFor: 'keyframe data' stamp: 'ar 9/3/1999 15:20'!ratioAtFrame: frameNumber	^self ratioData at: frameNumber! !!FlashCharacterMorph methodsFor: 'keyframe data' stamp: 'ar 9/3/1999 15:24'!ratioData	^rData! !!FlashCharacterMorph methodsFor: 'keyframe data' stamp: 'ar 9/3/1999 15:20'!removeAllKeyFrameData	"Remove all of the keyframe data associated with this morph"	self removeColorMatrixData.	self removeDepthData.	self removeMatrixData.	self removeVisibleData.	self removeRatioData.! !!FlashCharacterMorph methodsFor: 'keyframe data' stamp: 'ar 9/3/1999 15:30'!removeRatioData	rData _ FlashKeyframes new.! !!FlashFileReader methodsFor: 'processing shapes' stamp: 'ar 9/3/1999 14:54'!processShapesFrom: data	"Process a new shape"	| id bounds |	"Read shape id and bounding box"	id _ data nextWord.	bounds _ data nextRect.	"Start new shape definition"	self recordShapeStart: id bounds: bounds.	"Read styles for this shape"	self processShapeStylesFrom: data.	"Get number of bits for fill and line styles"	data initBits.	nFillBits _ data nextBits: 4.	nLineBits _ data nextBits: 4.	"Process all records in this shape definition"	[self processShapeRecordFrom: data] whileTrue.	"And mark the end of this shape"	self recordShapeEnd: id.	self recordShapeProperty: id length: data size.! !!FlashFileReader methodsFor: 'processing morphs' stamp: 'ar 9/3/1999 14:41'!processMorphFillStylesFrom: data	| nFills nColors rampIndex rampColor id fillStyleType color1 color2 matrix1 matrix2 ramp1 ramp2 |	nFills _ data nextByte.	nFills = 255 ifTrue:[nFills _ data nextWord].	log ifNotNil:[log crtab; print: nFills; nextPutAll:' New fill styles'].	1 to: nFills do:[:i|		log ifNotNil:[log crtab: 2; print: i; nextPut:$:; tab].		fillStyleType _ data nextByte.		(fillStyleType = 0) ifTrue:["Solid fill"			color1 _ data nextColor: true.			color2 _ data nextColor: true.			self recordMorphFill: i color1: color1 color2: color2.			log ifNotNil:[log nextPutAll:'solid color '; print: color1; nextPutAll:' -- '; print: color2].		].		(fillStyleType anyMask: 16) ifTrue:["Gradient fill"			"Read gradient matrix"			matrix1 _ data nextMatrix.			matrix2 _ data nextMatrix.			"Read color ramp data"			nColors _ data nextByte.			ramp1 _ Array new: nColors.			ramp2 _ Array new: nColors.			log ifNotNil:[log nextPutAll:'Gradient fill with '; print: nColors; nextPutAll:' colors'].			1 to: nColors do:[:j|				rampIndex _ data nextByte.				rampColor _ data nextColor: true.				ramp1 at: j put: (rampIndex -> rampColor).				rampIndex _ data nextByte.				rampColor _ data nextColor.				ramp2 at: j put: (rampIndex -> rampColor)].			self recordMorphFill: i matrix1: matrix1 ramp1: ramp1 matrix2: matrix2 ramp2: ramp2 linear: (fillStyleType = 16).			fillStyleType = 0].		(fillStyleType anyMask: 16r40) ifTrue:["Bit fill"			"Read bitmap id"			id _ data nextWord.			"Read bitmap matrix"			matrix1 _ data nextMatrix.			matrix2 _ data nextMatrix.			log ifNotNil:[log nextPutAll:'Bitmap fill id='; print: id].			self recordMorphFill: i matrix1: matrix1 matrix2: matrix2 id: id clipped: (fillStyleType anyMask: 1).			fillStyleType = 0].		fillStyleType = 0 ifFalse:[self error:'Unknown fill style: ',fillStyleType printString].		self flushLog.	].! !!FlashFileReader methodsFor: 'processing morphs' stamp: 'ar 9/3/1999 14:40'!processMorphLineStylesFrom: data	| nStyles styles lineWidth1 lineWidth2 lineColor1 lineColor2 |	nStyles _ data nextByte.	nStyles = 255 ifTrue:[nStyles _ data nextWord].	log ifNotNil:[log crtab; print: nStyles; nextPutAll:' New line styles'].	styles _ Array new: nStyles.	1 to: nStyles do:[:i|		lineWidth1 _ data nextWord.		lineWidth2 _ data nextWord.		lineColor1 _ data nextColor: true.		lineColor2 _ data nextColor: true.		self recordMorphLineStyle: i width1: lineWidth1 width2: lineWidth2 color1: lineColor1 color2: lineColor2.		log ifNotNil:[log crtab: 2; print: i; nextPut:$:; tab; 						print: lineWidth1; tab; print: lineColor1; tab;						print: lineWidth2; tab; print: lineColor2; tab]].	self flushLog.	^styles! !!FlashFileReader methodsFor: 'processing morphs' stamp: 'ar 9/3/1999 19:08'!processMorphShapeFrom: data	"Process a new morph shape"	| id bounds1 bounds2 edgeOffset |	"Read shape id and bounding box"	id _ data nextWord.	bounds1 _ data nextRect.	bounds2 _ data nextRect.	edgeOffset _ data nextULong. "edge offset"	edgeOffset _ edgeOffset + data position.	"Start new shape definition"	self recordMorphShapeStart: id srcBounds: bounds1 dstBounds: bounds2.	"Read fill styles for this shape"	self processMorphFillStylesFrom: data.	"Read line styles for this shape"	self processMorphLineStylesFrom: data.	"Get number of bits for fill and line styles"	data initBits.	nFillBits _ data nextBits: 4.	nLineBits _ data nextBits: 4.	"Process all records in this shape definition"	[self processShapeRecordFrom: data] whileTrue.	self recordMorphBoundary: id.	data position: edgeOffset.	data initBits.	nFillBits _ data nextBits: 4.	nLineBits _ data nextBits: 4.	[self processShapeRecordFrom: data] whileTrue.	"And mark the end of this shape"	self recordMorphShapeEnd: id.	self recordShapeProperty: id length: data size.! !!FlashFileReader methodsFor: 'processing tags' stamp: 'ar 9/3/1999 14:45'!processDefineMorphShape: data	self processMorphShapeFrom: data.	^true! !!FlashFileReader methodsFor: 'processing tags' stamp: 'ar 9/3/1999 15:23'!processPlaceObject2: data	| id flags depth matrix cxForm ratio name move |	flags _ data nextByte.	depth _ data nextWord.	move _ (flags anyMask: 1).	(flags anyMask: 2) ifTrue:[id _ data nextWord].	(flags anyMask: 4) ifTrue:[matrix _ data nextMatrix].	(flags anyMask: 8) ifTrue:[cxForm _ data nextColorMatrix: version >= 3].	self flag: #checkThis.	(flags anyMask: 16) ifTrue:["self halt." ratio _ data nextWord / 65536.0].	(flags anyMask: 32) ifTrue:["self halt." name _ data nextString].	(flags anyMask: 64) ifTrue:["self halt:'Clip shape encountered'." ^true].	log ifNotNil:[		log nextPutAll:' (id = ', id printString,' name = ', name printString,' depth = ', depth printString, ' move: ', move printString, ')'.		self flushLog].	move 		ifTrue:[self recordMoveObject: id name: name depth: depth matrix: matrix colorMatrix: cxForm ratio: ratio]		ifFalse:[self recordPlaceObject: id name: name depth: depth matrix: matrix colorMatrix: cxForm ratio: ratio].	^true! !!FlashFileReader methodsFor: 'processing tags' stamp: 'ar 9/3/1999 15:12'!processPlaceObject: data	| id depth matrix colorMatrix |	id _ data nextWord.	depth _ data nextWord.	matrix _ data nextMatrix.	log ifNotNil:[		log nextPutAll:' (id = ', id printString,' depth = ', depth printString, ')'.		self flushLog].	data atEnd ifFalse:[colorMatrix _ data nextColorMatrix].	self recordPlaceObject: id name: nil depth: depth matrix: matrix colorMatrix: colorMatrix ratio: nil.	^true! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:43'!recordMorphBoundary: id! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:06'!recordMorphFill: i color1: color1 color2: color2! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:07'!recordMorphFill: id matrix1: matrix1 matrix2: matrix2 id: bmId clipped: aBool! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:07'!recordMorphFill: id matrix1: matrix1 ramp1: ramp1 matrix2: matrix2 ramp2: ramp2 linear: isLinear! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:13'!recordMorphLineStyle: i width1: lineWidth1 width2: lineWidth2 color1: lineColor1 color2: lineColor2! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:18'!recordMorphShapeEnd: id! !!FlashFileReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 14:06'!recordMorphShapeStart: id srcBounds: bounds1 dstBounds: bounds2! !!FlashFileReader methodsFor: 'misc' stamp: 'ar 9/3/1999 15:10'!recordMorph: id depth: depth ratio: ratio! !!FlashFileStream methodsFor: 'reading data' stamp: 'ar 9/3/1999 14:40'!nextColor: usingAlpha	| r g b baseColor |	r _ self nextByte / 255.0.	g _ self nextByte / 255.0.	b _ self nextByte / 255.0.	baseColor _ Color r: r g: g b: b.	^usingAlpha 		ifTrue:[baseColor alpha: self nextByte / 255.0]		ifFalse:[baseColor]! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:48'!recordMorphBoundary: id	self recordShapeEnd: id.	morphedLineStyles keysAndValuesDo:[:idx :val| lineStyles at: idx put: val].	morphedFillStyles keysAndValuesDo:[:idx :val| fillStyles at: idx put: val].	location _ 0@0.	self beginShape.! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:43'!recordMorphFill: id color1: color1 color2: color2	self recordSolidFill: id color: color2.	morphedFillStyles at: id put: (fillStyles at: id).	self recordSolidFill: id color: color1.! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:43'!recordMorphFill: id matrix1: matrix1 matrix2: matrix2 id: bmId clipped: aBool	self recordBitmapFill: id matrix: matrix2 id: bmId clipped: aBool.	morphedFillStyles at: id put: (fillStyles at: id).	self recordBitmapFill: id matrix: matrix1 id: bmId clipped: aBool.! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:44'!recordMorphFill: id matrix1: matrix1 ramp1: ramp1 matrix2: matrix2 ramp2: ramp2 linear: isLinear	self recordGradientFill: id matrix: matrix2 ramp: ramp2 linear: isLinear.	morphedFillStyles at: id put: (fillStyles at: id).	self recordGradientFill: id matrix: matrix1 ramp: ramp1 linear: isLinear.	! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:45'!recordMorphLineStyle: id width1: lineWidth1 width2: lineWidth2 color1: lineColor1 color2: lineColor2	self recordLineStyle: id width: lineWidth2 color: lineColor2.	morphedLineStyles at: id put: (lineStyles at: id).	self recordLineStyle: id width: lineWidth1 color: lineColor1.! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:46'!recordMorphShapeEnd: id	| startShape endShape morphShape |	startShape _ shapes at: id.	self recordShapeEnd: id.	endShape _ shapes at: id.	morphShape _ FlashMorphingMorph from: startShape to: endShape.	morphShape id: id.	morphShape stepTime: stepTime.	shapes at: id put: morphShape.	morphedLineStyles _ morphedFillStyles _ nil.! !!FlashMorphReader methodsFor: 'composing morphs' stamp: 'ar 9/3/1999 18:42'!recordMorphShapeStart: shapeId srcBounds: bounds1 dstBounds: bounds2	morphedFillStyles _ Dictionary new.	morphedLineStyles _ Dictionary new.	location _ 0@0.	self logShapes ifFalse:[log _ nil].	self beginShape.! !!FlashMorphReader methodsFor: 'misc' stamp: 'ar 9/3/1999 15:10'!recordMorph: id depth: depth ratio: ratio! !!FlashMorphReader methodsFor: 'misc' stamp: 'ar 9/3/1999 15:50'!recordMoveObject: objectIndex name: aString depth: depth matrix: matrix colorMatrix: colorMatrix ratio: ratio	| index oldObj mat |	index _ nil.	activeMorphs do:[:list|		list do:[:morph|			((morph visibleAtFrame: frame-1) and:[				(morph depthAtFrame: frame-1) = depth])					ifTrue:[index _ morph id]]].	oldObj _ self recordRemoveObject: index depth: depth.	oldObj isNil ifTrue:[^self].	objectIndex isNil ifFalse:[index _ objectIndex].	matrix isNil 		ifTrue:[mat _ oldObj matrixAtFrame: frame]		ifFalse:[mat _ matrix].	self recordPlaceObject: index name: aString depth: depth matrix: mat colorMatrix: colorMatrix ratio: ratio.! !!FlashMorphReader methodsFor: 'misc' stamp: 'ar 9/3/1999 15:50'!recordPlaceObject: objectIndex name: aString depth: depth matrix: matrix colorMatrix: colorTransform ratio: ratio	| cached active doLoad |	cached _ passiveMorphs at: objectIndex ifAbsent:[#()].	cached size >= 1 		ifTrue:["Got an old morph. Re-use it"				doLoad _ false.				active _ cached first.				passiveMorphs at: objectIndex put: (cached copyWithout: active)]		ifFalse:["Need a new morph"				doLoad _ true.				active _ self newMorphFromShape: objectIndex.				active isNil ifTrue:[^self].				active reset.				active visible: false atFrame: frame - 1].	active isNil ifTrue:[^self].	active visible: true atFrame: frame.	active depth: depth atFrame: frame.	active matrix:  matrix atFrame: frame.	active colorTransform: colorTransform atFrame: frame.	doLoad ifTrue:[		active loadInitialFrame.		player addMorph: active].	cached _ (activeMorphs at: objectIndex ifAbsent:[#()]) copyWith: active.	activeMorphs at: objectIndex put: cached.	aString ifNotNil:[active setNameTo: aString].	ratio ifNotNil:[active ratio: ratio atFrame: frame].! !!FlashMorphReader methodsFor: 'private' stamp: 'ar 9/3/1999 18:39'!newMorphFromShape: objectIndex	"Return a new character morph from the given object index.	If the character morph at objectIndex is already used, then create and return a full copy of it"	| prototype |	prototype _ self oldMorphFromShape: objectIndex.	prototype isNil ifTrue:[^nil].	^(prototype owner notNil) 		ifTrue:[prototype fullCopy]		ifFalse:[prototype].! !!FlashMorphingMorph methodsFor: 'initialize' stamp: 'ar 9/3/1999 16:58'!extractShapesFrom: aMorph	| shapes |	shapes _ WriteStream on: Array new.	aMorph allMorphsDo:[:m|		(m isFlashMorph and:[m isFlashShape])			ifTrue:[shapes nextPut: m shape].	].	^shapes contents.! !!FlashMorphingMorph methodsFor: 'initialize' stamp: 'ar 9/3/1999 18:35'!from: srcMorph to: dstMorph	| shape |	"Note: Add srcMorph and dstMorph to the receiver so the damned bounds will be correct."	self addMorphBack: srcMorph.	self addMorphBack: dstMorph.	self computeBounds.	srcShapes _ self extractShapesFrom: srcMorph.	dstShapes _ self extractShapesFrom: dstMorph.	srcShapes size = dstShapes size ifFalse:[^self error:'Shape size mismatch'].	1 to: srcShapes size do:[:i|		(srcShapes at: i) numSegments = (dstShapes at: i) numSegments			ifFalse:[^self error:'Edge size mismatch']].	morphShapes _ WriteStream on: Array new.	srcShapes do:[:s|		shape _ FlashBoundaryShape					points: s points copy					leftFills: s leftFills					rightFills: s rightFills					fillStyles: s fillStyles					lineWidths: s lineWidths					lineFills: s lineFills.		morphShapes nextPut: shape.		self addMorphFront: (FlashShapeMorph shape: shape)].	morphShapes _ morphShapes contents.	srcMorph visible: false.	dstMorph visible: false.! !!FlashMorphingMorph methodsFor: 'stepping' stamp: 'ar 9/3/1999 18:50'!morphTo: ratio	| srcShape dstShape morphShape |	1 to: morphShapes size do:[:i|		srcShape _ srcShapes at: i.		dstShape _ dstShapes at: i.		morphShape _ morphShapes at: i.		morphShape morphFrom: srcShape to: dstShape at: ratio].		! !!FlashMorphingMorph methodsFor: 'stepping' stamp: 'ar 9/3/1999 18:38'!stepToFrame: frameNumber	| ratio |	super stepToFrame: frameNumber.	self visible ifTrue:[		ratio _ self ratioAtFrame: frame.		self morphTo: ratio.		self changed].! !!FlashMorphingMorph methodsFor: 'copying' stamp: 'ar 9/3/1999 18:39'!updateReferencesUsing: aDictionary	| srcMorph dstMorph |	super updateReferencesUsing: aDictionary.	srcMorph _ (submorphs at: submorphs size-1).	dstMorph _ (submorphs at: submorphs size).	self removeAllMorphs.	self from: srcMorph to: dstMorph.! !!FlashMorphingMorph class methodsFor: 'instance creation' stamp: 'ar 9/3/1999 16:53'!from: srcMorph to: dstMorph	^self new from: srcMorph to: dstMorph! !FlashFileReader removeSelector: #processMorphShapesFrom:!FlashFileReader removeSelector: #processMorphShapeRecordFrom:!FlashMorphReader removeSelector: #recordMoveObject:name:depth:matrix:colorMatrix:!FlashMorphReader removeSelector: #recordPlaceObject:name:depth:matrix:colorMatrix:!FlashMorphingMorph removeSelector: #ratio:atFrame:!FlashMorphingMorph removeSelector: #ratioAtFrame:!FlashMorphingMorph removeSelector: #initialize!FlashMorphingMorph removeSelector: #removeAllKeyFrameData!FlashMorphingMorph removeSelector: #extractMorphShapes!FlashMorphingMorph removeSelector: #ratioData!FlashMorphingMorph removeSelector: #removeRatioData!FlashMorphingMorph removeSelector: #fullCopy!FlashMorphingMorph removeSelector: #reset!FlashMorphingMorph class removeSelector: #withAll:!