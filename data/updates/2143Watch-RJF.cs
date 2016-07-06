'From Squeak2.8alpha of 12 January 2000 [latest update: #2139] on 16 May 2000 at 11:40:12 am'!"Change Set:		WatchDate:			10 May 2000Author:			Ricardo J. FerreiraIt is a representation of a watch. One can change its labels' font to any system font.When resizing the watch, the labels' sizes are increased or decreased.Open the morph's menu to change the watch's font, color, center color and hands' color.Evaluate:	WatchMorph new openInWorldto create and show the watch.Tweaked by Dan I. for simplicity, performance, and roman numerals."!Morph subclass: #WatchMorph	instanceVariableNames: 'fontSizes currentSize fontName cColor handsColor romanNumerals '	classVariableNames: 'Labels Angles '	poolDictionaries: ''	category: 'Morphic-Demo'!!WatchMorph commentStamp: '<historical>' prior: 0!This class is a representation of a watch.The labels' font is changeble. Labels' font size increase or decrease when resizing me.WatchMorph new openInWorld(WatchMorph fontName: 'ComicPlain') openInWorld		" transparent watch "(WatchMorph fontName: 'ComicBold' bgColor: Color white centerColor: Color black) openInWorldStructure: fontSizes		Dictionary -- map each font size to an interval of my extent currentSize		Integer -- current font size fontName		String -- the labels' font name cColor			Color -- center color handsColor		Color!WatchMorph class	instanceVariableNames: ''!!WatchMorph methodsFor: 'accessing' stamp: 'rlf 3/17/2000 02:20'!cColor: aColor	cColor _ aColor! !!WatchMorph methodsFor: 'accessing' stamp: 'rlf 3/17/2000 02:13'!centerColor: aColor	cColor _ aColor! !!WatchMorph methodsFor: 'accessing' stamp: 'rlf 3/17/2000 02:19'!fontName: aString	fontName _ aString! !!WatchMorph methodsFor: 'accessing' stamp: 'rjf 5/11/2000 00:03'!handsColor: aColor	handsColor _ aColor! !!WatchMorph methodsFor: 'drawing' stamp: 'di 5/16/2000 11:23'!drawOn: aCanvas	| h m s pHour pMin pSec time hWidth hHeight |	time _ Time now.	h _ time hours-1\\12 + 1.	m _ time minutes/5 asFloat.	s _ time seconds/5 asFloat.	hWidth _ self extent x/2.	hHeight _ self extent y/2.	pHour _ ((self center x)+(((Angles atWrap: h) - (m*2.5)) degreesToRadians cos*(hWidth*0.6)))@			 ((self center y)-(((Angles atWrap: h) - (m*2.5)) degreesToRadians sin*(hHeight*0.6))).	pMin _ ((self center x)+(((Angles atWrap: m asInteger) - (30*m fractionPart)) degreesToRadians cos*(hWidth*0.72)))@			((self center y)-(((Angles atWrap: m asInteger) - (30*m fractionPart)) degreesToRadians sin*(hHeight*0.72))).	pSec _ ((self center x)+(((Angles atWrap: s asInteger) - (30*s fractionPart)) degreesToRadians cos*(hWidth*0.8)))@		    ((self center y)-(((Angles atWrap: s asInteger) - (30*s fractionPart)) degreesToRadians sin*(hHeight*0.8))).	aCanvas fillOval: bounds color: color;			 fillOval: (bounds insetBy: (hWidth*0.7)@(hHeight*0.7)) color: cColor;			 frameOval: bounds width: 1 color: Color gray;			 line: self center to: pHour width: 3 color: handsColor;			 line: self center to: pMin width: 2 color: handsColor;			 line: self center to: pSec width: 1 color: handsColor.! !!WatchMorph methodsFor: 'initialization' stamp: 'di 5/16/2000 11:18'!extent: newExtent	super extent: newExtent.	self alignLabels! !!WatchMorph methodsFor: 'initialization' stamp: 'di 5/16/2000 11:35'!initialize	super initialize.	color ifNil: [self color: self defaultColor].	handsColor _ self defaultHandsColor.	cColor ifNil: [cColor _ self defaultCenterColor].	romanNumerals _ false.	fontName _ 'NewYork'.	self createLabels.	self extent: self defaultExtent.	self start.! !!WatchMorph methodsFor: 'labels' stamp: 'di 5/16/2000 11:32'!alignLabels	"Align the labels. If the size has changed, update it."	| font |	font _ StrikeFont familyName: fontName size: self height//8 emphasized: 1 "bold".	self submorphs withIndexDo: [:m :hour | m font: font.		m center: (self center x + ((Angles at: hour) degreesToRadians cos*((self extent x/2) - (10*font height/12))) rounded)@				  (self center y - ((Angles at: hour) degreesToRadians sin*((self extent y/2) - (10*font height/12))) rounded)].! !!WatchMorph methodsFor: 'labels' stamp: 'di 5/16/2000 11:21'!createLabels	| numeral |	self removeAllMorphs.	Angles withIndexDo:		[:angle :hour |		numeral _ romanNumerals			ifTrue: [#('I' 'II' 'III' 'IV' 'V' 'VI' 'VII' 'VIII' 'IX' 'X' 'XI' 'XII') at: hour]			ifFalse: [hour asString].		self addMorphBack: ((StringMorph contents: numeral font: (TextStyle default fontAt: 1) emphasis: 1)			center: (self center x + (angle degreesToRadians cos*(self radius - 10)))@				    (self center y - (angle degreesToRadians sin*(self radius - 10)))) lock].! !!WatchMorph methodsFor: 'menus' stamp: 'di 5/16/2000 10:58'!addCustomMenuItems: aCustomMenu hand: aHandMorph	"Add morph-specific items to the given menu which was invoked by the given hand."	aCustomMenu add: 'change font...' action: #changeFont.	romanNumerals		ifTrue: [aCustomMenu add: 'use latin numerals' action: #toggleRoman]		ifFalse: [aCustomMenu add: 'use roman numerals' action: #toggleRoman].	aCustomMenu add: 'change hands color...' action: #changeHandsColor.	aCustomMenu add: 'change center color...' action: #changeCenterColor! !!WatchMorph methodsFor: 'menus' stamp: 'rlf 3/17/2000 02:12'!changeCenterColor	ColorPickerMorph new		sourceHand: self activeHand;		target: self;		selector: #centerColor:;		originalColor: self color;		addToWorld: self world			near: self fullBounds! !!WatchMorph methodsFor: 'menus' stamp: 'di 5/16/2000 10:40'!changeFont	fontName _ (SelectionMenu labelList: StrikeFont familyNames selections: StrikeFont familyNames) startUp ifNil: [^self].	self alignLabels.! !!WatchMorph methodsFor: 'menus' stamp: 'rjf 5/11/2000 00:03'!changeHandsColor	ColorPickerMorph new		sourceHand: self activeHand;		target: self;		selector: #handsColor:;		originalColor: self color;		addToWorld: self world			near: self fullBounds! !!WatchMorph methodsFor: 'menus' stamp: 'di 5/16/2000 11:13'!toggleRoman	romanNumerals _ romanNumerals not.	self createLabels; alignLabels! !!WatchMorph methodsFor: 'private' stamp: 'rlf 2/9/2000 13:32'!radius	^self extent x/2! !!WatchMorph methodsFor: 'stepping' stamp: 'rlf 3/17/2000 12:59'!step	self changed.! !!WatchMorph methodsFor: 'visual properties' stamp: 'rlf 3/17/2000 04:00'!color: aColor	super color: aColor.	color _ aColor.! !!WatchMorph methodsFor: 'visual properties' stamp: 'rlf 3/17/2000 02:49'!defaultCenterColor ^Color gray	"^(Color r: 0.0 g: 0.6 b: 1.0)" "this is fine also"! !!WatchMorph methodsFor: 'visual properties' stamp: 'rlf 3/17/2000 01:47'!defaultColor ^Color green! !!WatchMorph methodsFor: 'visual properties' stamp: 'rjf 5/7/2000 15:48'!defaultExtent ^130@130! !!WatchMorph methodsFor: 'visual properties' stamp: 'rjf 5/10/2000 22:51'!defaultHandsColor	^Color red! !!WatchMorph class methodsFor: 'as yet unclassified' stamp: 'rlf 3/17/2000 03:51'!fontName: aString	^self fontName: aString bgColor: nil centerColor: nil! !!WatchMorph class methodsFor: 'as yet unclassified' stamp: 'rlf 3/17/2000 09:53'!fontName: aString bgColor: aColor centerColor: otherColor	^super new		fontName: aString;		color: aColor;		cColor: otherColor! !!WatchMorph class methodsFor: 'as yet unclassified' stamp: 'di 5/16/2000 11:20'!initialize  "WatchMorph initialize"	Angles _ (1 to: 12) collect: [:i | 3-i*30 \\ 360] "(60 30 0 330 300 270 240 210 180 150 120 90 )"! !WatchMorph class removeSelector: #createLabelsDictionary!WatchMorph initialize!WatchMorph removeSelector: #fontSizeMustChange!WatchMorph removeSelector: #sizeForCurrentExtent!WatchMorph removeSelector: #updateFont!WatchMorph removeSelector: #updateFontSizes!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."!