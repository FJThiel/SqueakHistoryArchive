'From Squeak2.9alpha of 13 June 2000 [latest update: #3175] on 15 December 2000 at 12:23:24 am'!"Change Set:		MonthTweaks-arDate:			15 December 2000Author:			Andreas RaabRemove the (plain broken) implementation of MonthMorph>>extent: and make it rather use layouts in the right way."!!MonthMorph methodsFor: 'initialization' stamp: 'ar 12/15/2000 00:10'!initialize	super initialize.	tileRect _ 0@0 extent: 23@19.	self layoutInset: 1;		color: Color red;		listDirection: #topToBottom;		vResizing: #shrinkWrap;		hResizing: #shrinkWrap;		month: Date today month.	self rubberBandCells: false.	self extent: 160@130.! !!MonthMorph methodsFor: 'initialization' stamp: 'ar 12/15/2000 00:17'!initializeHeader	| title sep  frame button monthName |	title _ (self findA: WeekMorph) title.	title hResizing: #spaceFill.	"should be done by WeekMorph but isn't"	title submorphsDo:[:m| m hResizing: #spaceFill].	monthName _ month name.	self width < 160 ifTrue:		[(#(6 7 9) includes: month index)			ifTrue: [monthName _ monthName copyFrom: 1 to: 4]			ifFalse: [monthName _ monthName copyFrom: 1 to: 3]].	sep _ Morph new color: Color transparent; extent: title width @ 1.	self		addMorph: sep;		addMorph: title;		addMorph: sep copy.	button _ SimpleButtonMorph new				target: self;				actWhen: #whilePressed;				color: (Color r: 0.8 g: 0.8 b: 0.8).	frame _ AlignmentMorph new		color: Color transparent;		listDirection: #leftToRight;		hResizing: #spaceFill;		vResizing: #shrinkWrap;		layoutInset: 0.	frame		addMorph:			(button				label: '>>';				actionSelector: #nextYear;				width: 15);		addMorph:			(button copy				label: '>';				actionSelector: #next;				width: 15);		addMorph:			((AlignmentMorph new				color: Color transparent;				listDirection: #topToBottom;				"hResizing: #shrinkWrap;"				wrapCentering: #center; cellPositioning: #topCenter;				extent: (title fullBounds width - (button width * 3)) @ title height)				addMorph:					(StringMorph new						contents:							monthName, ' ', month year printString));		addMorph:			(button copy				label: '<';				actionSelector: #previous;				width: 15);		addMorph:			(button copy				label: '<<';				actionSelector: #previousYear;				width: 15).	self addMorph: frame! !!MonthMorph methodsFor: 'initialization' stamp: 'ar 12/15/2000 00:08'!initializeWeeks	| weeks firstWeek lastWeek |	self removeAllMorphs.	weeks _ OrderedCollection new.	(firstWeek _ month firstDate week) asDate dayOfMonth = 1 ifTrue:		["If the entire first week is this month, then insert prior week"		weeks add: (WeekMorph newWeek: firstWeek previous month: month tileRect: tileRect model: model)].	month eachWeekDo:		[:each |		weeks add: (WeekMorph newWeek: (lastWeek _ each) month: month tileRect: tileRect model: model)].	weeks size < 6 ifTrue:		["If there's room at the bottom, add another week of next month."		weeks add: (WeekMorph newWeek: lastWeek next month: month tileRect: tileRect model: model)].	weeks reverseDo: [:each | 		each hResizing: #spaceFill; vResizing: #spaceFill.		"should be done by WeekMorph but isn't"		each submorphsDo:[:m| m hResizing: #spaceFill; vResizing: #spaceFill].		self addMorph: each].	self initializeHeader.	self highlightToday! !MonthMorph removeSelector: #extent:!