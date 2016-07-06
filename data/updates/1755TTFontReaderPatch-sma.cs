'From Squeak2.6 of 11 October 1999 [latest update: #1578] on 1 January 2000 at 7:34:58 pm'!"Change Set:		TTFontReader-PatchDate:			1 January 2000Author:			Stefan Matthias Aust (sma@3plus4.de)This change set adds support for cmap format 4 and 6 encoding to the TTFontReader class (format 4 is actually recommended by Microsoft). This allows to read in most existing true type fonts without errors - actually, of some 30 freeware and shareware fonts I tested all worked.It also corrects the format 0 encoding and fixes a problem, that the reader was preferring an missing mac encoding over an existing windows encoding.  Furthermore, a bug in TTSampleFontMorph's smoothing menu has fixed. I also reformatted a few methods."!!TTFontReader methodsFor: 'processing' stamp: 'sma 1/1/2000 17:03'!processCharMap: assoc	"Process the given character map"	| charTable glyph cmap |	cmap _ assoc value.	charTable _ Array new: 256 withAll: glyphs first. "Initialize with default glyph"	assoc key = 1 ifTrue: "Mac encoded table"		[1 to: (cmap size min: charTable size) do:			[:i |			glyph _ glyphs at: (cmap at: i) + 1.			charTable at: i put: glyph]].	assoc key = 3 ifTrue: "Win encoded table"		[1 to: (cmap size min: charTable size) do:			[:i |			glyph _ glyphs at: (cmap at: i) + 1.			charTable at: (WinToMacTable at: i) put: glyph]].	^ charTable! !!TTFontReader methodsFor: 'processing' stamp: 'sma 1/1/2000 17:01'!processCharacterMappingTable: entry	"Read the font's character to glyph index mapping table.	If an appropriate mapping can be found then return an association	with the format identifier and the contents of the table"	| copy initialOffset nSubTables pID sID offset cmap assoc |	initialOffset _ entry offset.	entry skip: 2. "Skip table version"	nSubTables _ entry nextUShort.	1 to: nSubTables do:[:i|		pID _ entry nextUShort.		sID _ entry nextUShort.		offset _ entry nextULong.		"Check if this is either a Macintosh encoded table		or a Windows encoded table"		(pID = 1 or:[pID = 3]) ifTrue:[			"Go to the beginning of the table"			copy _ entry copy.			copy offset: initialOffset + offset.			cmap _ self decodeCmapFmtTable: copy.			(pID = 1 and: [cmap notNil]) "Prefer Macintosh encoding over everything else"				ifTrue: [^ pID -> cmap].			assoc _ pID -> cmap. "Keep it in case we don't find a Mac encoded table"		].	].	^assoc! !!TTFontReader methodsFor: 'private' stamp: 'sma 1/1/2000 19:17'!decodeCmapFmtTable: entry	| cmapFmt length cmap firstCode entryCount segCount segments offset code |	cmapFmt _ entry nextUShort.	length _ entry nextUShort.	entry skip: 2. "skip version"	cmapFmt = 0 ifTrue: "byte encoded table"		[length _ length - 6. 		"should be always 256"		length <= 0 ifTrue: [^ nil].	"but sometimes, this table is empty"		cmap _ Array new: length.		entry nextBytes: length into: cmap startingAt: entry offset.		^ cmap].	cmapFmt = 4 ifTrue: "segment mapping to deltavalues"		[segCount _ entry nextUShort // 2.		entry skip: 6. "skip searchRange, entrySelector, rangeShift"		segments _ Array new: segCount.		segments _ (1 to: segCount) collect: [:e | Array new: 4].		1 to: segCount do: [:i | (segments at: i) at: 2 put: entry nextUShort]. "endCount"		entry skip: 2. "skip reservedPad"		1 to: segCount do: [:i | (segments at: i) at: 1 put: entry nextUShort]. "startCount"		1 to: segCount do: [:i | (segments at: i) at: 3 put: entry nextShort]. "idDelta"		offset _ entry offset.		1 to: segCount do: [:i | (segments at: i) at: 4 put: entry nextUShort]. "idRangeOffset"		cmap _ Array new: 256 withAll: 0. "could be larger, but Squeak can't handle that"		segments withIndexDo:			[:seg :si |			seg first to: seg second do:				[:i |				i < 256 ifTrue:					[seg last > 0 ifTrue:						["offset to glypthIdArray - this is really C-magic!!"						entry offset: i - seg first - 1 * 2 + seg last + si + si + offset. 						code _ entry nextUShort.						code > 0 ifTrue: [code _ code + seg third]]					ifFalse:						["simple offset"						code _ i + seg third].					cmap at: i + 1 put: code]]].		^ cmap].	cmapFmt = 6 ifTrue: "trimmed table"		[firstCode _ entry nextUShort.		entryCount _ entry nextUShort.		cmap _ Array new: entryCount + firstCode withAll: 0.		entryCount timesRepeat:			[cmap at: (firstCode _ firstCode + 1) put: entry nextUShort].		^ cmap].	^ nil! !!TTSampleFontMorph methodsFor: 'accessing' stamp: 'sma 1/1/2000 18:08'!font	^ font! !!TTSampleFontMorph methodsFor: 'accessing' stamp: 'sma 1/1/2000 17:53'!smoothing	^ smoothing! !!TTSampleFontMorph methodsFor: 'drawing' stamp: 'sma 1/1/2000 17:56'!drawCharactersOn: aCanvas	| glyph origin r offset cy m |	0 to: 255 do:[:i|		glyph _ font at: i.		origin _ font bounds extent * ((i \\ 16) @ (i // 16)).		r _ origin extent: font bounds extent.		offset _ r center - glyph bounds center.		cy _ glyph bounds center y.		m _ MatrixTransform2x3 withOffset: 0@cy.		m _ m composedWithLocal: (MatrixTransform2x3 withScale: 1@-1).		m _ m composedWithLocal: (MatrixTransform2x3 withOffset: 0@cy negated).		m _ m composedWithGlobal: (MatrixTransform2x3 withOffset: offset).		aCanvas asBalloonCanvas preserveStateDuring:[:balloonCanvas|			balloonCanvas transformBy: m.			balloonCanvas drawGeneralBezierShape: glyph contours					color: color					borderWidth: 0					borderColor: Color black.		].	].! !!TTSampleFontMorph methodsFor: 'menu' stamp: 'sma 1/1/2000 17:51'!nextSmoothingLevel	smoothing = 1		ifTrue: [smoothing _ 2]		ifFalse: [smoothing = 2			ifTrue: [smoothing _ 4]			ifFalse: [smoothing = 4				ifTrue: [smoothing _ 1]]].	self changed! !!TTSampleFontMorph methodsFor: 'private' stamp: 'sma 1/1/2000 17:59'!changed	self invalidRect: (self fullBounds expandBy: 1)! !!TTSampleStringMorph methodsFor: 'initialize' stamp: 'sma 1/1/2000 18:05'!initialize	super initialize.	borderWidth _ 0.	borderColor _ Color black.	color _ {Color magenta. Color yellow. Color orange. Color lightGray} atRandom.	smoothing _ 4! !!TTSampleStringMorph methodsFor: 'initialize' stamp: 'sma 1/1/2000 18:08'!initializeString	| xStart char glyph |	(font isNil or: [string isNil]) ifTrue: [^ self].	xStart _ 0.	ttBounds _ 0@0 corner: 0@0.	1 to: string size do:		[:i |		char _ string at: i.		glyph _ font at: char.		ttBounds _ ttBounds quickMerge: (glyph bounds translateBy: xStart@0).		xStart _ xStart + glyph advanceWidth.	].	self extent: ttBounds extent // 40.	borderWidth _ ttBounds height // 40! !!TTSampleStringMorph methodsFor: 'accessing' stamp: 'sma 1/1/2000 18:08'!string	^ string! !!TTSampleStringMorph methodsFor: 'geometry' stamp: 'sma 1/1/2000 18:10'!containsPoint: aPoint	"^ super containsPoint: aPoint"  "so much faster..."	| picker |	(self bounds containsPoint: aPoint) ifFalse:[^false].	picker _ BalloonCanvas on: (Form extent: 1@1 depth: 32).	picker transformBy: (MatrixTransform2x3 withOffset: aPoint negated).	self drawOn: picker.	^(picker form bits at: 1) ~= 0! !