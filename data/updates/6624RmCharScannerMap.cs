'From Squeak3.8-Nihongo of 2 March 2005 [latest update: #24] on 11 March 2005 at 3:04:01 pm'!"Change Set:		RemoveCharacterScannerMapDate:			12 March 2005Author:			Ohshima Yoshiki, Takashi YamamiyaCleaning up some instance variables of character scannerbecause Strikefont >> characterToGlyphMap is obsolete today."!Object subclass: #CharacterScanner	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel wantsColumnBreaks '	classVariableNames: 'DefaultStopConditions NilCondition PaddedSpaceCondition SpaceCondition '	poolDictionaries: 'TextConstants'	category: 'Graphics-Text'!Object subclass: #MultiCharacterScanner	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel wantsColumnBreaks presentation presentationLine numOfComposition baselineY '	classVariableNames: 'DefaultStopConditions NilCondition PaddedSpaceCondition SpaceCondition '	poolDictionaries: 'TextConstants'	category: 'Multilingual-Scanning'!!BitBlt methodsFor: 'private' stamp: 'yo 3/11/2005 14:49'!primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta	| ascii |	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>	startIndex to: stopIndex do:[:charIndex|		ascii _ (aString at: charIndex) asciiValue.		sourceX _ xTable at: ascii + 1.		width _ (xTable at: ascii + 2) - sourceX.		self copyBits.		destX _ destX + width + kernDelta.	].! !!CharacterScanner methodsFor: 'private' stamp: 'tak 3/12/2005 00:43'!setFont	| priorFont |	"Set the font and other emphasis."	priorFont _ font.	text == nil ifFalse:[		emphasisCode _ 0.		kern _ 0.		indentationLevel _ 0.		alignment _ textStyle alignment.		font _ nil.		(text attributesAt: lastIndex forStyle: textStyle)			do: [:att | att emphasizeScanner: self]].	font == nil ifTrue:		[self setFont: textStyle defaultFontIndex].	font _ font emphasized: emphasisCode.	priorFont ifNotNil: [destX _ destX + priorFont descentKern].	destX _ destX - font descentKern.	"NOTE: next statement should be removed when clipping works"	leftMargin ifNotNil: [destX _ destX max: leftMargin].	kern _ kern - font baseKern.	"Install various parameters from the font."	spaceWidth _ font widthOf: Space.	xTable _ font xTable.	stopConditions _ DefaultStopConditions.! !!CharacterScanner methodsFor: 'scanning' stamp: 'tak 3/12/2005 00:43'!measureString: aString inFont: aFont from: startIndex to: stopIndex	"WARNING: In order to use this method the receiver has to be set up using #initializeStringMeasurer"	destX _ destY _ lastIndex _ 0.	xTable _ aFont xTable.	self scanCharactersFrom: startIndex to: stopIndex in: aString rightX: 999999 stopConditions: stopConditions kern: 0.	^destX! !!StrikeFont methodsFor: 'private' stamp: 'yo 3/11/2005 07:38'!createCharacterToGlyphMap        "Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."        maxAscii < 256 ifTrue: [^ (1 to: 256) collect: [:i | i - 1]].        ^ nil.! !StrikeFontSet removeSelector: #characterToGlyphMap!Object subclass: #MultiCharacterScanner	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel wantsColumnBreaks presentation presentationLine numOfComposition baselineY'	classVariableNames: 'DefaultStopConditions NilCondition PaddedSpaceCondition SpaceCondition'	poolDictionaries: 'TextConstants'	category: 'Multilingual-Scanning'!Object subclass: #CharacterScanner	instanceVariableNames: 'destX lastIndex xTable destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel wantsColumnBreaks'	classVariableNames: 'DefaultStopConditions NilCondition PaddedSpaceCondition SpaceCondition'	poolDictionaries: 'TextConstants'	category: 'Graphics-Text'!"Postscript:"StrikeFont allInstancesDo: [:t | t instVarNamed: 'characterToGlyphMap' put: nil].!