'From Squeak 2.4c of May 10, 1999 on 26 May 1999 at 4:32:16 pm'!"Change Set:		subGameTweaks-jhmDate:			26 May 1999Author:			John MaloneyVarious tweaks to the baseline to support the subgame."!StringMorph subclass: #UpdatingStringMorph	instanceVariableNames: 'format target lastValue getSelector putSelector floatPrecision growable stepTime '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Widgets'!!ArrayedCollection methodsFor: 'sorting'!mergeFirst: first middle: middle last: last into: dst by: aBlock	"Private!! Merge the sorted ranges [first..middle] and [middle+1..last] of the receiver into the range [first..last] of dst."	| i1 i2 val1 val2 out |	i1 _ first.	i2 _ middle + 1.	val1 _ self at: i1.	val2 _ self at: i2.	out _ first - 1.  "will be pre-incremented"	"select 'lower' half of the elements based on comparator"	[(i1 <= middle) and: [i2 <= last]] whileTrue: [		(aBlock value: val1 value: val2)			ifTrue: [				dst at: (out _ out + 1) put: val1.				val1 _ self at: (i1 _ i1 + 1)]			ifFalse: [				dst at: (out _ out + 1) put: val2.				i2 _ i2 + 1.				i2 <= last ifTrue: [val2 _ self at: i2]]].	"copy the remaining elements"	i1 <= middle		ifTrue: [			dst replaceFrom: out + 1 to: last with: self startingAt: i1]		ifFalse: [			dst replaceFrom: out + 1 to: last with: self startingAt: i2].! !!Color class methodsFor: 'colormaps' stamp: 'jm 5/2/1999 07:24'!cachedColormapFrom: sourceDepth to: destDepth	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."	| srcIndex map |	CachedColormaps class == Array ifFalse: [CachedColormaps _ (1 to: 9) collect: [:i | Array new: 32]].	srcIndex _ sourceDepth.	sourceDepth > 8 ifTrue: [srcIndex _ 9].	(map _ (CachedColormaps at: srcIndex) at: destDepth) ~~ nil ifTrue: [^ map].	map _ self computeColormapFrom: sourceDepth to: destDepth.	(CachedColormaps at: srcIndex) at: destDepth put: map.	^ map! !!Form methodsFor: 'other' stamp: 'jm 4/5/1999 19:20'!colorReduced	"Return a color-reduced ColorForm version of the receiver, if possible, or the receiver itself if not."	| tally tallyDepth colorCount newForm cm oldPixelValues newFormColors nextColorIndex c |	tally _ self tallyPixelValues asArray.	tallyDepth _ (tally size log: 2) asInteger.	colorCount _ 0.	tally do: [:n | n > 0 ifTrue: [colorCount _ colorCount + 1]].	(tally at: 1) = 0 ifTrue: [colorCount _ colorCount + 1].  "include transparent"	colorCount > 256 ifTrue: [^ self].  "cannot reduce"	newForm _ self formForColorCount: colorCount.	"build an array of just the colors used, and a color map to translate	 old pixel values to their indices into this color array"	cm _ Bitmap new: tally size.	oldPixelValues _ self colormapIfNeededForDepth: 32.	newFormColors _ Array new: colorCount.	newFormColors at: 1 put: Color transparent.	nextColorIndex _ 2.	2 to: cm size do: [:i |		(tally at: i) > 0 ifTrue: [			oldPixelValues = nil				ifTrue: [c _ Color colorFromPixelValue: i - 1 depth: tallyDepth]				ifFalse: [c _ Color colorFromPixelValue: (oldPixelValues at: i) depth: 32].			newFormColors at: nextColorIndex put: c.			cm at: i put: nextColorIndex - 1.  "pixel values are zero-based indices"			nextColorIndex _ nextColorIndex + 1]].	"copy pixels into new ColorForm, mapping to new pixel values"	newForm copyBits: self boundingBox		from: self		at: 0@0		clippingBox: self boundingBox		rule: Form over		fillColor: nil		map: cm.	newForm colors: newFormColors.	newForm offset: offset.	^ newForm! !!ColorForm methodsFor: 'private' stamp: 'jm 4/5/1999 10:11'!setColors: colorArray cachedColormap: aBitmap depth: anInteger	"Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations."	colors _ colorArray.	cachedDepth _ anInteger.	cachedColormap _ aBitmap.! !!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 5/5/1999 20:59'!fromAIFFFileNamed: fileName mergeIfStereo: mergeFlag	"Initialize this sound from the data in the given AIFF file. If mergeFlag is true and the file is stereo, its left and right channels are mixed together to produce a mono sampled sound."	| aiffFileReader |	aiffFileReader _ AIFFFileReader new.	aiffFileReader readFromFile: fileName		mergeIfStereo: mergeFlag		skipDataChunk: false.	aiffFileReader isLooped		ifTrue: [			self samples: aiffFileReader leftSamples				loopEnd: aiffFileReader loopEnd				loopLength: aiffFileReader loopLength				pitch: aiffFileReader pitch				samplingRate: aiffFileReader samplingRate]		ifFalse: [			self unloopedSamples: aiffFileReader leftSamples				pitch: aiffFileReader pitch				samplingRate: aiffFileReader samplingRate].	"the following must be done second, since the initialization above sets	 leftSamples and rightSamples to the same sample data"	aiffFileReader isStereo		ifTrue: [rightSamples _ aiffFileReader rightSamples].	initialCount _ (leftSamples size * self samplingRate) // originalSamplingRate.	self loudness: 1.0.	self addReleaseEnvelope.! !!Preferences class methodsFor: 'preferences panel' stamp: 'jm 5/26/1999 16:17'!openPreferencesControlPanel        "Preferences openPreferencesControlPanel"        | aPanel aWindow aRow wrapper but aList odd aColor w width1 width2 spacer |        Smalltalk verifyMorphicAvailability ifFalse: [^ self beep].        aPanel _ AlignmentMorph newColumn.        aPanel beSticky.        aList _ OrderedCollection new.        FlagDictionary associationsDo: [:assoc | aList add: (Array                                with: assoc key                                with: assoc value                                with: (self helpMessageForPreference: assoc key))].        odd _ false.        width1 _ 172.        spacer _ 4.        width2 _ 14.        (aList asSortedCollection: [:a :b | a first < b first])                do:                         [:triplet |                         aPanel addMorphBack: (aRow _ AlignmentMorph newRow).                        aRow color: (aColor _ odd                                                        ifTrue: [Color green muchLighter]                                                        ifFalse: [Color red veryMuchLighter]).                        odd _ odd not.                        aRow addMorph: (wrapper _ Morph new color: aColor).                        wrapper setBalloonText: triplet third.                        wrapper extent: width1 @ 15.                        wrapper addMorph: (StringMorph new contents: triplet first).                        aRow addMorphBack: (Morph new color: aColor; extent: (spacer @ 15)).                        aRow addMorphBack: (wrapper _ Morph new color: aColor).                        wrapper extent: width2 @ 15.                        wrapper addMorphBack: (but _ UpdatingBooleanStringMorph new contents: triplet second printString).                        but getSelector: triplet first;                        putSelector: #setPreference:toValue:;                        stepTime: 1800;                         target: self].                wrapper _ ScrollPane new.                wrapper scroller addMorph: aPanel.        Smalltalk isMorphic                ifTrue:                        [aWindow _ SystemWindow new model: self.                        aWindow addMorph: wrapper frame: (0 @ 0 extent: 1 @ 1).                        aWindow setLabel: 'Preferences'.                        aWindow openInWorld]                ifFalse:                        [w _ WorldMorph new addMorph: wrapper.                        wrapper                                retractable: false;                                extent: self initialExtent + (wrapper scrollbarWidth @ 0).                        w startSteppingSubmorphsOf: wrapper.                        MorphWorldView openOn: w                                label: 'Preferences'                                extent: w fullBounds extent]! !!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jm 5/26/1999 16:23'!initialize	super initialize.	format _ #default.  "formats: #string, #default"	target _ getSelector _ putSelector _ nil.	floatPrecision _ 1.	growable _ true.	stepTime _ 50.! !!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jm 5/26/1999 16:22'!floatPrecision	floatPrecision ifNil: [floatPrecision _ 1].	^ floatPrecision! !!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jm 5/26/1999 16:22'!floatPrecision: aNumber	floatPrecision _ aNumber.! !!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jm 5/26/1999 16:21'!growable	^ growable ~~ false! !!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jm 5/26/1999 16:22'!growable: aBoolean	growable _ aBoolean.! !!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jm 5/26/1999 16:17'!stepTime	^ stepTime ifNil: [50]! !!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jm 5/26/1999 16:23'!stepTime: mSecsPerStep	stepTime _ mSecsPerStep truncated.! !!UpdatingStringMorph methodsFor: 'copying' stamp: 'jm 5/26/1999 16:18'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."	super veryDeepInner: deepCopier.	format _ format.				"Weakly copied"	target _ target.					"Weakly copied"	lastValue _ lastValue veryDeepCopyWith: deepCopier.	getSelector _ getSelector.			"Weakly copied"	putSelector _ putSelector.		"Weakly copied"	floatPrecision _ floatPrecision.	"Weakly copied"	growable _ growable.			"Weakly copied"	stepTime _ stepTime.				"Weakly copied"! !!Utilities class methodsFor: 'flaps' stamp: 'jm 5/26/1999 16:17'!addSystemStatusLinesTo: aPlayfield	"Add three lines of system status info beneath the other items on aPlayfield"	| aString |	aString _ UpdatingStringMorph new target: Smalltalk.	aString useStringFormat; color: Color blue; stepTime: 3000; getSelector: #version.	aString setBalloonText: 'Indicates the official Squeak release code of the current image.'.	aPlayfield addCenteredAtBottom: aString offset: 8.	aString left: (aPlayfield left + 6).	aString _ aString fullCopy getSelector: #lastUpdateString.	aString setBalloonText: 'Indicates the update number of the last official update present in the image.'.	aPlayfield addCenteredAtBottom: aString offset: 6.	aString left: (aPlayfield left + 6).	aString _ aString fullCopy getSelector: #currentChangeSetString.	aString setBalloonText: 'Indicates the name of the current change set.'.	aPlayfield addCenteredAtBottom: aString offset: 6.	aString left: (aPlayfield left + 6)! !!WarpBlt methodsFor: 'primitives' stamp: 'jm 5/2/1999 07:09'!sourceForm: srcForm destRect: dstRectangle	"Set up a WarpBlt from the entire source Form to the given destination rectangle."	| w h |	sourceForm _ srcForm.	sourceX _ sourceY _ 0.	destX _ dstRectangle left.	destY _ dstRectangle top.	width _ dstRectangle width.	height _ dstRectangle height.	w _ 16384 * (srcForm width - 1).	h _ 16384 * (srcForm height - 1).	p1x _ 0.	p2x _ 0.	p3x _ w.	p4x _ w.	p1y _ 0.	p2y _ h.	p3y _ h.	p4y _ 0.	p1z _ p2z _ p3z _ p4z _ 16384.  "z-warp ignored for now"! !!WorldMorph methodsFor: 'install / exit' stamp: 'jm 5/26/1999 16:31'!install	self viewBox: Display boundingBox.	hands do: [:h | h initForEvents].	Preferences useGlobalFlaps ifTrue: [self installFlaps].	SystemWindow noteTopWindowIn: self.	self displayWorldSafely.! !UpdatingStringMorph removeSelector: #stepFrequency:!