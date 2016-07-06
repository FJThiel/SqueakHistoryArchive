'From Squeak3.7beta of ''1 April 2004'' [latest update: #5905] on 29 April 2004 at 7:18:49 pm'!"Change Set:		PSPointSizeFix-nk (v3)Date:				13 April 2004Author:			Ned KonzPostscript output from Squeak has been in pixel units. However, we're outputting requests for fonts in point units, making the fonts appear 3/4 as big as they should on output.This change set fixes that problem by also requesting fonts in pixel units.v3 (13 April): fixed discrepancy between (ascender+descender) and (unitsPerEm) in TTCFont. This will rename existing point sizes but keep glyphs the same size.v2 (25 March): improved decoding of font names and attributes.Factored out knowledge of font naming to TextStyle class.""Repair bad sizes *before* installing new code (see TTCFont class>>repairBadSizes)"	| description computedScale cached desiredScale newPointSize repaired |	repaired _ OrderedCollection new.	TTCFont allInstancesDo: [ :font |		cached := (font cache copyFrom: $A asciiValue + 1 to: $z asciiValue + 1)			detect: [ :f | f notNil ] ifNone: [].		cached := cached ifNil: [  font formOf: $A ] ifNotNil: [ cached value ].		description _ font ttcDescription.		desiredScale _ cached height asFloat / (description ascender - description descender).		computedScale _ font pixelSize asFloat / font ttcDescription unitsPerEm.		(((computedScale / desiredScale) - 1.0 * cached height) abs < 1.0) ifFalse: [			newPointSize _ (font pointSize * desiredScale / computedScale) rounded.			font pointSize: newPointSize; flushCache.			repaired add: font.			font derivativeFonts do: [ :df | df ifNotNil: [				df pointSize: newPointSize; flushCache.				repaired add: df. ]].		].	].!Canvas subclass: #PostscriptCanvas	instanceVariableNames: 'origin clipRect currentColor currentFont morphLevel gstateStack fontMap usedFonts psBounds topLevelMorph initialScale savedMorphExtent currentTransformation printSpecs pages shadowColor '	classVariableNames: 'FontMap '	poolDictionaries: ''	category: 'Morphic-Postscript Canvases'!!TTCFont commentStamp: 'nk 4/2/2004 11:32' prior: 0!I represent a font that uses TrueType derived glyph.  Upon a request for glyph for a character through a call to #formOf: (or #widthOf:), I first search corresponding glyph in the cache.  If there is not, it creates a 32bit depth form with the glyph.  The cache is weakly held.  The entries are zapped at full GC.Structure: ttcDescription	TTFontDescription -- The Squeak data structure for a TrueType font data file. pointSize		Number -- Nominal Em size in points. Conversion to pixel sizes depends on the definition of TextStyle class>>pixelsPerInch. foregroundColor	Color -- So far, this font need to know the glyph color in cache. cache			WeakArray of <Color -> <Array(256) of glyph>> derivatives		Array -- stores the fonts in the same family but different emphasis.!!TextStyle commentStamp: '<historical>' prior: 0!A TextStyle comprises the formatting information for composing and displaying a unit (usually a paragraph) of text.  Typically one makes a copy of a master TextStyle (such as TextStyle default), and then that copy may get altered in the process of editing.  Bad things can happen if you do not copy first.Each of my instances consists of...	fontArray		An array of StrikeFonts or other fonts	fontFamilySize	unused	lineGrid			An integer; default line spacing for paragraphs	baseline			An integer; default baseline (dist from line top to bottom of an 'a')	alignment		An integer; text alignment, see TextStyle alignment:	firstIndent		An integer; indent of first line in pixels	restIndent		An integer; indent of remaining lines in pixels	rightIndent		An integer; indent of right margin rel to section	tabsArray		An array of integers giving tab offsets in pixels	marginTabsArray	An array of margin tabs	leading			An integer giving default vertical line separationFor a concrete example, look at TextStyle default copy inspect!]style[(367 10 226 20 381 30)f1,f1LStrikeFont Comment;,f1,f1LTextStyle alignment:;,f1,f1dTextStyle default copy inspect;;!!AbstractFont methodsFor: 'accessing' stamp: 'nk 4/2/2004 11:06'!baseKern	^0! !!AbstractFont methodsFor: 'accessing' stamp: 'nk 4/1/2004 10:51'!height	"Answer the height of the receiver, total of maximum extents of 	characters above and below the baseline."	^self ascent + self descent! !!AbstractFont methodsFor: 'accessing' stamp: 'nk 4/2/2004 11:07'!lineGrid	"Answer the relative space between lines"	^ self ascent + self descent! !!AbstractFont methodsFor: 'accessing' stamp: 'nk 4/2/2004 11:33'!pixelSize	"Make sure that we don't return a Fraction"	^ TextStyle pointsToPixels: self pointSize! !!AbstractFont methodsFor: 'accessing' stamp: 'nk 4/1/2004 10:48'!pointSize	self subclassResponsibility.! !!AbstractFont methodsFor: 'accessing' stamp: 'nk 3/22/2004 15:15'!textStyleName	"Answer the name to be used as a key in the TextConstants dictionary."	^self familyName! !!AbstractFont methodsFor: 'notifications' stamp: 'nk 4/2/2004 11:25'!pixelsPerInchChanged	"The definition of TextStyle class>>pixelsPerInch has changed. Do whatever is necessary."! !!PostscriptCanvas methodsFor: 'accessing' stamp: 'nk 4/1/2004 19:08'!isShadowDrawing	^shadowColor notNil! !!PostscriptCanvas methodsFor: 'accessing' stamp: 'nk 4/1/2004 19:06'!shadowColor	^shadowColor! !!PostscriptCanvas methodsFor: 'accessing' stamp: 'nk 4/1/2004 19:02'!shadowColor: aColor	shadowColor _ aColor.! !!PostscriptCanvas methodsFor: 'balloon compatibility' stamp: 'nk 4/1/2004 20:34'!drawGeneralBezierShape: shapeArray color: color borderWidth: borderWidth borderColor: borderColor 	"shapeArray is an array of: 	arrays of points, each of which must have 	a multiple of 3 points in it. 	This method tries to sort the provided triplets so that curves that 	start and end at the same point are together."	| where triplets groups g2 fillC |	fillC := self shadowColor				ifNil: [color].	shapeArray isEmpty		ifTrue: [^ self].	where := nil.	groups := OrderedCollection new.	triplets := OrderedCollection new.	shapeArray		do: [:arr | arr				groupsOf: 3				atATimeDo: [:bez | 					| rounded | 					rounded := bez roundTo: 0.001.					(where isNil							or: [where = rounded first])						ifFalse: [groups addLast: triplets.							triplets := OrderedCollection new].					triplets addLast: rounded.					where := rounded last]].	groups addLast: triplets.	triplets := OrderedCollection new.	"now try to merge stray groups"	groups copy		do: [:g1 | g1 first first = g1 last last				ifFalse: ["not closed"					g2 := groups								detect: [:g | g ~~ g1										and: [g1 last last = g first first]]								ifNone: [].					g2						ifNotNil: [groups remove: g2.							groups add: g2 after: g1]]].	groups		do: [:g | triplets addAll: g].	where := nil.	self		definePathProcIn: [ :cvs |			triplets do: [:shape | 					where ~= shape first						ifTrue: [where								ifNotNil: [cvs closepath].							cvs moveto: shape first].					where := cvs outlineQuadraticBezierShape: shape]]		during: [ :cvs |			cvs clip.			cvs setLinewidth: borderWidth "*2";				 fill: fillC andStroke: borderColor]! !!PostscriptCanvas methodsFor: 'balloon compatibility' stamp: 'nk 4/1/2004 19:14'!drawOval: r color: c borderWidth: borderWidth borderColor: borderColor	| fillC |	fillC _ self shadowColor ifNil:[c].	^ self fillOval: r color: fillC borderWidth: borderWidth borderColor: borderColor			! !!PostscriptCanvas methodsFor: 'balloon compatibility' stamp: 'nk 4/1/2004 19:16'!drawRectangle: r color: color borderWidth: borderWidth borderColor: borderColor	| fillC |	fillC := self shadowColor				ifNil: [color].	^ self		frameAndFillRectangle: r		fillColor: fillC		borderWidth: borderWidth		borderColor: borderColor! !!PostscriptCanvas methodsFor: 'drawing-polygons' stamp: 'nk 4/1/2004 19:15'!drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc 	| fillC |	fillC _ self shadowColor ifNil:[aColor].	self		preserveStateDuring: [:pc | pc			 outlinePolygon: vertices;				 setLinewidth: bw;								fill: fillC				andStroke: ((bc isKindOf: Symbol)						ifTrue: [Color gray]						ifFalse: [bc])]! !!PostscriptCanvas methodsFor: 'drawing-support' stamp: 'nk 4/1/2004 20:46'!definePathProcIn: pathBlock during: duringBlock 	"Bracket the output of pathBlock (which is passed the receiver) in 	gsave 		newpath 			<pathBlock> 		closepath 		<duringBlock> 	grestore 	"	| retval |	self		preserveStateDuring: [:tgt | 			self comment: 'begin pathProc path block'.			target newpath.			pathBlock value: tgt.			target closepath.			self comment: 'begin pathProc during block'.			retval := duringBlock value: tgt.			self comment: 'end pathProc'].	^ retval! !!PostscriptCanvas methodsFor: 'drawing-support' stamp: 'nk 4/1/2004 19:52'!preserveStateDuring: aBlock	| retval saveClip saveTransform |	target preserveStateDuring: [ :innerTarget |		saveClip _ clipRect.		saveTransform _ currentTransformation.		gstateStack addLast: currentFont.		gstateStack addLast: currentColor.		gstateStack addLast: shadowColor.		retval _ aBlock value: self.		shadowColor _ gstateStack removeLast.		currentColor _ gstateStack removeLast.		currentFont _ gstateStack removeLast.		clipRect _ saveClip.		currentTransformation _ saveTransform.	].	^ retval! !!PostscriptCanvas methodsFor: 'drawing-support' stamp: 'nk 4/1/2004 19:48'!transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize 	| retval oldShadow |	oldShadow := shadowColor.	self comment: 'drawing clipped ' with: aClipRect.	self comment: 'drawing transformed ' with: aDisplayTransform.	self		preserveStateDuring: [:inner | 			currentTransformation				ifNil: [currentTransformation := aDisplayTransform]				ifNotNil: [currentTransformation := currentTransformation composedWithLocal: aDisplayTransform].			aClipRect				ifNotNil: [clipRect := aDisplayTransform								globalBoundsToLocal: (clipRect intersect: aClipRect).					inner rect: aClipRect;						 clip].			inner transformBy: aDisplayTransform.			retval := aBlock value: inner].	self comment: 'end of drawing clipped ' with: aClipRect.	shadowColor := oldShadow.	^ retval! !!PostscriptCanvas methodsFor: 'drawing-support' stamp: 'nk 4/1/2004 19:41'!translateBy: delta during: aBlock	"Set a translation only during the execution of aBlock."	| result oldShadow |	oldShadow := shadowColor.	self translate: delta.	result _ aBlock value: self.	self translate: delta negated.	shadowColor := oldShadow.	^ result! !!PostscriptCanvas methodsFor: 'drawing-text' stamp: 'nk 4/1/2004 19:28'!drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c 	| fillC oldC |	fillC := self shadowColor		ifNil: [c].	self setFont: (fontOrNil				ifNil: [self defaultFont]).	self comment: ' text color: ' , c printString.	oldC := currentColor.	self setColor: fillC.	self comment: ' boundsrect origin ' , boundsRect origin printString.	self comment: '  origin ' , origin printString.	self moveto: boundsRect origin.	target print: ' (';		 print: (s asString copyFrom: firstIndex to: lastIndex) asPostscript;		 print: ') show';		 cr.	self setColor: oldC.! !!PostscriptCanvas methodsFor: 'initialization' stamp: 'nk 4/1/2004 19:09'!reset	super reset.	origin := 0 @ 0.				"origin of the top-left corner of this canvas"	clipRect := 0 @ 0 corner: 10000 @ 10000.		"default clipping rectangle"	currentTransformation := nil.	morphLevel := 0.	pages := 0.	gstateStack := OrderedCollection new.	usedFonts := Dictionary new.	initialScale := 1.0.	shadowColor := nil.	currentColor := nil! !!PostscriptCanvas methodsFor: 'private' stamp: 'nk 4/1/2004 20:36'!drawGradient: fillColor 	self comment: 'not-solid fill ' with: fillColor.	self comment: ' origin ' with: fillColor origin.	self comment: ' direction ' with: fillColor direction.	self fill: fillColor asColor! !!PostscriptCanvas methodsFor: 'private' stamp: 'nk 12/28/2003 21:08'!fill: fillColor	fillColor isSolidFill		ifTrue: [self paint: fillColor asColor operation: #eofill]		ifFalse: [self preserveStateDuring: [:inner | inner clip; drawGradient: fillColor]]! !!PostscriptCanvas methodsFor: 'private' stamp: 'nk 3/25/2004 15:36'!setFont:aFont	| fInfo |	aFont = currentFont ifTrue: [^self].	currentFont _ aFont.	self defineFont: aFont.	fInfo _ self class postscriptFontInfoForFont: aFont.	target 		selectflippedfont: fInfo first		size: (aFont pixelSize * fInfo second)		ascent: aFont ascent.! !!PostscriptCanvas methodsFor: 'private' stamp: 'nk 4/1/2004 19:28'!text: s at: point font: fontOrNil color: c spacePad: pad 	| fillC oldC |	fillC := self shadowColor				ifNil: [c].	self		setFont: (fontOrNil				ifNil: [self defaultFont]).	self comment: ' text color: ' , c printString.	oldC := currentColor.	self setColor: fillC.	self comment: '  origin ' , origin printString.	self moveto: point.	target write: pad;		 print: ' 0 32 (';		 print: s asPostscript;		 print: ') widthshow';		 cr.	self setColor: oldC.! !!PostscriptCanvas methodsFor: 'private' stamp: 'nk 4/1/2004 19:27'!textStyled: s at: ignored0 font: ignored1 color: c justified: justify parwidth: parwidth 	| fillC oldC |	fillC := c.	self shadowColor		ifNotNilDo: [:sc | 			self comment: ' shadow color: ' , sc printString.			fillC := sc].	self comment: ' text color: ' , c printString.	oldC := currentColor.	self setColor: fillC.	self comment: '  origin ' , origin printString.	"self moveto: point."	"now done by sender"	target print: ' (';		 print: s asPostscript;		 print: ') '.	justify		ifTrue: [target write: parwidth;				 print: ' jshow';				 cr]		ifFalse: [target print: 'show'].	target cr.	self setColor: oldC.! !!PostscriptCanvas class methodsFor: 'font mapping' stamp: 'nk 3/25/2004 16:06'!fontSampler	"Produces a Postscript .eps file on disk, returns a Morph."	"PostscriptCanvas fontSampler"	"PostscriptCanvas fontSampler openInWorld"	| morph file |	morph _ Morph new		layoutPolicy: TableLayout new;		listDirection: #topToBottom;		wrapDirection: #leftToRight;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		color: Color white.	TextStyle actualTextStyles keysAndValuesDo: [ :styleName :style |		{ style fontArray first. style fontArray last } do: [ :baseFont | | info |			0 to: 2 do: [ :i | | font string string2 textMorph row |				font _ baseFont emphasized: i.				(i isZero or: [ font ~~ baseFont ]) ifTrue: [					string _ font fontNameWithPointSize.					row _ Morph new						layoutPolicy: TableLayout new;						listDirection: #topToBottom;						hResizing: #shrinkWrap;						vResizing: #shrinkWrap;						cellSpacing: 20@0;						color: Color white.							textMorph _ TextMorph new hResizing: #spaceFill; backgroundColor: Color white; beAllFont: font; contentsAsIs: string.					row addMorphBack: (textMorph imageForm asMorph).					info _ self postscriptFontInfoForFont: font.					string2 _ String streamContents: [ :stream |						stream nextPutAll: info first; space; print: (font pixelSize * info second) rounded.					].					textMorph _ TextMorph new hResizing: #spaceFill; backgroundColor: Color white; beAllFont: font; contentsAsIs: string2.					row addMorphBack: textMorph.										morph addMorphBack: row.				]			]		]	].	morph bounds: World bounds.	morph layoutChanged; fullBounds.	file _ (FileDirectory default newFileNamed: 'PSFontSampler.eps').	Cursor wait showWhile: [ 		file nextPutAll: (EPSCanvas morphAsPostscript: morph) ].	^morph! !!PostscriptCanvas class methodsFor: 'font mapping' stamp: 'nk 3/25/2004 20:13'!postscriptFontInfoForFont: font	| fontName decoded desired mask decodedName keys match |	fontName _ font textStyleName asString.	decoded _ TextStyle decodeStyleName: fontName.	decodedName _ decoded second.	keys _ self fontMap keys asArray sort: [ :a :b | a size > b size ].	match _ keys select: [ :k | decoded first = k or: [ fontName = k ] ].	match do: [ :key | | subD |		subD := self fontMap at: key.		desired _ font emphasis.		mask _ 31.		[			desired _ desired bitAnd: mask.			subD at: desired ifPresent: [ :answer | ^answer].			mask _ mask bitShift: -1.			desired > 0		] whileTrue.	].	"No explicit lookup found; try to convert the style name into the canonical Postscript name.	This name will probably still be wrong."	fontName _ String streamContents: [ :s |		s nextPutAll: decodedName.		decoded third do: [ :nm | s nextPut: $-; nextPutAll: nm ].		(font emphasis == 0 and: [ (decoded last includes: 0) not ])			ifTrue: [ s nextPutAll:  '-Regular' ].		(font emphasis == 1 and: [ (decoded first anyMask: 1) not ])			ifTrue: [ s nextPutAll:  '-Bold' ].		(font emphasis == 2 and: [ (decoded first anyMask: 2) not ])			ifTrue: [ s nextPutAll:  '-Italic' ].		(font emphasis == 3 and: [ (decoded first anyMask: 3) not ])			ifTrue: [ s nextPutAll:  '-BoldItalic' ].	].	^ {fontName. 1.0}! !!PostscriptCanvas class methodsFor: 'font mapping' stamp: 'nk 3/25/2004 15:55'!postscriptFontMappingSummary	"	Transcript nextPutAll: 	PostscriptCanvas postscriptFontMappingSummary	; endEntry	"	| stream |	stream _ WriteStream on: (String new: 1000).	TextStyle actualTextStyles keysAndValuesDo: [ :styleName :style |		stream nextPutAll: styleName; cr.		style fontArray do: [ :baseFont | | info |			0 to: 3 do: [ :i | | font |				font _ baseFont emphasized: i.				font emphasis = i ifTrue: [					stream tab; nextPutAll: font fontNameWithPointSize; tab.					info _ self postscriptFontInfoForFont: font.					stream nextPutAll: info first; space; print: (font pixelSize * info second) rounded.					stream cr.				]			]		]	].	^stream contents! !!PostscriptEncoder methodsFor: 'Postscript generation' stamp: 'nk 4/1/2004 20:16'!clip	self print: 'clip'; cr.! !!PostscriptEncoder methodsFor: 'Postscript generation' stamp: 'nk 4/1/2004 20:16'!newpath	self print: 'newpath'; cr.! !!TTCFont methodsFor: 'accessing' stamp: 'nk 4/1/2004 09:17'!ascent	^ (ttcDescription ascender * self scale) ceiling! !!TTCFont methodsFor: 'accessing' stamp: 'nk 4/1/2004 09:18'!descent	^ (ttcDescription descender * self scale * -1) ceiling! !!TTCFont methodsFor: 'accessing' stamp: 'nk 4/2/2004 11:27'!pixelSize	"Make sure that we don't return a Fraction"	^ TextStyle pointsToPixels: pointSize! !!TTCFont methodsFor: 'accessing' stamp: 'nk 4/2/2004 11:27'!pixelSize: aNumber	"Make sure that we don't return a Fraction"	self pointSize: (TextStyle pixelsToPoints: aNumber) rounded.! !!TTCFont methodsFor: 'accessing' stamp: 'nk 4/1/2004 09:50'!pointSize: aNumber	pointSize _ aNumber.	self flushCache.	derivatives ifNotNil: [ derivatives do: [ :f | f ifNotNil: [ f pointSize: aNumber ]]].! !!TTCFont methodsFor: 'objects from disk' stamp: 'nk 4/2/2004 11:29'!convertToCurrentVersion: varDict refStream: smartRefStrm	"If we're reading in an old version with a pixelSize instance variable, convert it to a point size."	"Deal with the change from pixelSize to pointSize, assuming the current monitor dpi."	varDict at: 'pixelSize' ifPresent: [ :x | 		pointSize _ (TextStyle pixelsToPoints: x) rounded.	].	^super convertToCurrentVersion: varDict refStream: smartRefStrm.! !!TTCFont methodsFor: 'private' stamp: 'nk 4/1/2004 09:19'!computeForm: char	| ttGlyph scale |	scale _ self scale.	ttGlyph _ ttcDescription at: char.	^ ttGlyph asFormWithScale: scale ascender: ttcDescription ascender descender: ttcDescription descender fgColor: foregroundColor bgColor: Color transparent depth: self depth.! !!TTCFont methodsFor: 'private' stamp: 'nk 3/25/2004 17:01'!indexOfSubfamilyName: aName	| decoded |	"decodeStyleName will consume all the modifiers and leave nothing if everything was recognized."	decoded := TextStyle decodeStyleName: aName.	decoded second isEmpty ifTrue: [ ^decoded first ].	"If you get a halt here - please add the missing synonym to the lookup table in TextStyle>>decodeStyleName: ."		self error: 'please add the missing synonym ', aName, ' to the lookup table in TextStyle>>decodeStyleName:'.	^0.! !!TTCFont methodsFor: 'private' stamp: 'nk 4/1/2004 09:15'!scale	^ self pixelSize / ttcDescription unitsPerEm! !!TTCFont class methodsFor: 'other' stamp: 'nk 4/13/2004 17:56'!repairBadSizes	"There was a bug that would cause the TTCFonts to generate incorrectly sized glyphs.	By looking at the dimensions of cached forms,	we can tell whether the incorrect height logic was used.	If it was, change the point size of the font and its derivatives.		Note that this is probably pointless to call after the new code has been loaded; it's here for documentation (it should be called from the CS preamble instead)."	"TTCFont repairBadSizes"	| description computedScale cached desiredScale newPointSize repaired |	repaired _ OrderedCollection new.	TTCFont allInstancesDo: [ :font |		cached := (font cache copyFrom: $A asciiValue + 1 to: $z asciiValue + 1)			detect: [ :f | f notNil ] ifNone: [].		cached := cached ifNil: [  font formOf: $A ] ifNotNil: [ cached value ].		description _ font ttcDescription.		desiredScale _ cached height asFloat / (description ascender - description descender).		computedScale _ font pixelSize asFloat / font ttcDescription unitsPerEm.		(((computedScale / desiredScale) - 1.0 * cached height) abs < 1.0) ifFalse: [			newPointSize _ (font pointSize * desiredScale / computedScale) rounded.			font pointSize: newPointSize; flushCache.			repaired add: font.			font derivativeFonts do: [ :df | df ifNotNil: [				df pointSize: newPointSize; flushCache.				repaired add: df. ]].		].	].	repaired isEmpty ifFalse: [ repaired asArray inspect ].! !!TTFontReader class methodsFor: 'instance creation' stamp: 'nk 4/1/2004 09:05'!installTTF: ttfFileName asTextStyle: textStyleName sizes: sizeArray	"Sizes are in pixels."	"TTFontReader		installTTF: 'F:\fonts\amazon__.TTF' 		asTextStyle: #Amazon		sizes: #(24 60)"	| ttf fontArray |	ttf _ self parseFileNamed: ttfFileName.	fontArray _ sizeArray collect:		[:each |		(ttf asStrikeFontScale: each / ttf unitsPerEm)			name: textStyleName;			pixelSize: each].	TextConstants at: textStyleName asSymbol put: (TextStyle fontArray: fontArray)! !!TextPrinter methodsFor: 'initialize' stamp: 'nk 4/2/2004 11:32'!defaultResolution	"Return the default resolution (DPI) for printing"	^TextStyle pixelsPerInch asPoint! !!TextStyle class methodsFor: 'class initialization' stamp: 'nk 3/25/2004 17:51'!initialize	self initializeStyleDecoder.! !!TextStyle class methodsFor: 'class initialization' stamp: 'nk 3/25/2004 17:53'!initializeStyleDecoder	TextConstants at: #StyleDecoder put: nil.	self styleDecoder.! !!TextStyle class methodsFor: 'class initialization' stamp: 'nk 3/25/2004 17:57'!styleDecoder	TextConstants at: #StyleDecoder ifPresent: [ :dict | dict ifNotNil: [ ^dict ]].	^TextConstants at: #StyleDecoder put: (		Dictionary new at: 'Regular' put: 0;				 at: 'Roman' put: 0;				 at: 'Medium' put: 0;				 at: 'Light' put: 0;				 at: 'Normal' put: 0;				 at: 'Plain' put: 0;				 at: 'Book' put: 0;				 at: 'Demi' put: 0;				 at: 'Demibold' put: 0;				 at: 'Semibold' put: 0;				 at: 'SemiBold' put: 0;				 at: 'ExtraBold' put: 1;				 at: 'SuperBold' put: 1;				 at: 'B' put: 1;				 at: 'I' put: 2;				 at: 'U' put: 4;				 at: 'X' put: 16;				 at: 'N' put: 8;				 at: 'Bold' put: 1;				 at: 'Italic' put: 2;				 at: 'Oblique' put: 2;				 at: 'Narrow' put: 8;				 at: 'Condensed' put: 8;				 at: 'Underlined' put: 4;				 yourself )! !!TextStyle class methodsFor: 'utilities' stamp: 'nk 3/25/2004 17:55'!decodeStyleName: styleName 	"Given a string styleName, return a collection with: 	 	* [1] the probable Squeak emphasis code, which is a bit combination of: 	1	bold 	2	italic 	4	underlined 	8	narrow 	16	strikeout 	 	* [2] the base style name without the modifiers (can be empty)	* [3] the modifiers in the order they were found 	* [4] the codes for those modifiers, in the same order	"	| decoder keys modifiers modifierCodes baseName styleCode matchedKey |	decoder := self styleDecoder.	modifiers := OrderedCollection new.	modifierCodes := OrderedCollection new.	keys := decoder keys asArray				sort: [:a :b | a size > b size].	styleCode := 0.	baseName := styleName asString.	[matchedKey := keys				detect: [:k | baseName endsWith: k]				ifNone: [].	matchedKey notNil]		whileTrue: [| last code | 			last := baseName size - matchedKey size.			last > 0				ifTrue: [('- ' includes: (baseName at: last))						ifTrue: [last := last - 1]].			baseName := baseName copyFrom: 1 to: last.			code := decoder at: matchedKey.			styleCode := styleCode + code.			modifiers addFirst: matchedKey.			modifierCodes addFirst: code.	].	^ {styleCode. baseName. modifiers. modifierCodes }! !!TextStyle class methodsFor: 'utilities' stamp: 'nk 4/2/2004 11:26'!pixelsPerInch	"Answer the nominal resolution of the screen."	^TextConstants at: #pixelsPerInch ifAbsentPut: [ 96.0 ].! !!TextStyle class methodsFor: 'utilities' stamp: 'nk 4/2/2004 11:24'!pixelsPerInch: aNumber	"Set the nominal number of pixels per inch to aNumber."	TextConstants at: #pixelsPerInch put: aNumber asFloat.	AbstractFont allSubInstancesDo: [ :font | font pixelsPerInchChanged ].! !!TextStyle class methodsFor: 'utilities' stamp: 'nk 4/2/2004 11:23'!pixelsToPoints: pixels	^pixels * 72.0 / self pixelsPerInch! !!TextStyle class methodsFor: 'utilities' stamp: 'nk 4/2/2004 11:22'!pointsToPixels: points	^points * self pixelsPerInch / 72.0! !TextStyle initialize!!TextStyle class reorganize!('TextConstants access' actualTextStyles fontArrayForStyle: fontPointSizesFor: fontSizesFor: fontWidthsFor: knownTextStyles knownTextStylesWithoutDefault pointSizesFor:)('class initialization' initialize initializeStyleDecoder styleDecoder)('constants' default defaultFont named:)('instance creation' changeDefaultFontSizeBy: fontArray: initDefaultFontsAndStyle new)('mime file in/out' collectionFromCompressedMIMEString: looseFontsFromFamily: replaceFontsIn:with: replaceStyle:with: writeSF2FamilyNamed:inDirectory:toChangeSet: writeStyle:named:toChangeSet:)('user interface' chooseTTCFontSize: fontMenuForStyle:target:selector: fontMenuForStyle:target:selector:highlight: fontSizeSummary modalMVCStyleSelectorWithTitle: modalStyleSelectorWithTitle: mvcPromptForFont:andSendTo:withSelector: promptForFont:andSendTo:withSelector: promptForFont:andSendTo:withSelector:highlight:)('utilities' decodeStyleName: pixelsPerInch pixelsPerInch: pixelsToPoints: pointsToPixels:)!TTCFont removeSelector: #baseKern!TTCFont removeSelector: #height!!TTCFont reorganize!('accessing' ascent descent descentKern emphasis emphasis: emphasized: familyName familySizeFace fontNameWithPointSize lineGrid maxAscii minAscii name pixelSize pixelSize: pointSize pointSize: textStyle)('copying' copy deepCopy objectForDataStream: veryDeepCopyWith:)('file in/out' encodedForRemoteCanvas)('friend' cache derivativeFont: derivativeFonts displayString:on:from:to:at:kern: flushCache initialize installOn:foregroundColor:backgroundColor: recreateCache ttcDescription ttcDescription:)('objects from disk' convertToCurrentVersion:refStream:)('printing' printOn:)('public' depth foregroundColor size widthOf:)('testing' isRegular isTTCFont)('private' at:put: computeForm: formOf: indexOfSubfamilyName: scale)('caching' releaseCachedState)!PostscriptEncoder removeSelector: #definePathProcIn:during:!PostscriptEncoder removeSelector: #executePathProc!Canvas subclass: #PostscriptCanvas	instanceVariableNames: 'origin clipRect currentColor shadowColor currentFont morphLevel gstateStack fontMap usedFonts psBounds topLevelMorph initialScale savedMorphExtent currentTransformation printSpecs pages'	classVariableNames: 'FontMap'	poolDictionaries: ''	category: 'Morphic-Postscript Canvases'!!AbstractFont reorganize!('accessing' baseKern characterToGlyphMap derivativeFonts height isRegular lineGrid pixelSize pointSize textStyleName xTable)('displaying' displayString:on:from:to:at:kern: installOn:foregroundColor:backgroundColor:)('measuring' approxWidthOfText: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)('testing' isTTCFont)('notifications' pixelsPerInchChanged)('caching' releaseCachedState)!