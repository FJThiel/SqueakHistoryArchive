'From Squeak 2.3 beta of Nov 25, 1998 on 8 December 1998 at 9:43:04 pm'!!JPEGReadWriter methodsFor: 'public access' stamp: 'ar 12/8/1998 21:40'!nextImageDitheredToDepth: depth	| form xStep yStep x y |	ditherMask _ DitherMasks		at: depth		ifAbsent: [self error: 'can only dither to display depths'].	redResidual _ greenResidual _ blueResidual _ 0.	sosSeen _ false.	self parseFirstMarker.	[sosSeen] whileFalse: [self parseNextMarker].	form _ Form extent: (width @ height) depth: 32.	xStep _ mcuWidth * DCTSize.	yStep _ mcuHeight * DCTSize.	y _ 0.	1 to: mcuRowsInScan do:		[:row |		x _ 0.		1 to: mcusPerRow do:			[:col |			self decodeMCU.			self idctMCU.			self colorConvertMCU.			mcuImageBuffer displayOn: form at: (x @ y).			x _ x + xStep].		y _ y + yStep].	"Fixup the alpha channel in the 32bit form"	(BitBlt toForm: form)		destRect: form boundingBox;		fillColor: (Bitmap with: 16rFF000000);		combinationRule: 7; "bitOr:with:"		copyBits.			^ form! !