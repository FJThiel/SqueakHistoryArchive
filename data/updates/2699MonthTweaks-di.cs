'From Squeak2.9alpha of 12 June 2000 [latest update: #2705] on 24 September 2000 at 11:26:27 pm'!"Change Set:		MonthTweaksDate:			24 September 2000Author:			Dan IngallsA couple of changes to work with new highlighting in MonthMorph."!!MonthMorph methodsFor: 'initialization' stamp: 'di 9/24/2000 22:46'!initializeWeeks	| weeks firstWeek lastWeek |	self removeAllMorphs.	weeks _ OrderedCollection new.	(firstWeek _ month firstDate week) asDate dayOfMonth = 1 ifTrue:		["If the entire first week is this month, then insert prior week"		weeks add: (WeekMorph newWeek: firstWeek previous month: month tileRect: tileRect model: model)].	month eachWeekDo:		[:each |		weeks add: (WeekMorph newWeek: (lastWeek _ each) month: month tileRect: tileRect model: model)].	weeks size < 6 ifTrue:		["If there's room at the bottom, add another week of next month."		weeks add: (WeekMorph newWeek: lastWeek next month: month tileRect: tileRect model: model)].	weeks reverseDo: [:each | self addMorph: each].	self initializeHeader.	self highlightToday! !!MonthMorph methodsFor: 'geometry' stamp: 'di 9/24/2000 23:26'!extent: newExtent	| selectedDates |	selectedDates _ self selectedDates.	tileRect _ 0@0 extent: (newExtent-(4.0@25.0) / 7.0 max: 18.0@12.0).	self initializeWeeks.	self allMorphsDo:  "Restore selection state"		[:m | (m isKindOf: SimpleSwitchMorph) ifTrue:				[(m arguments isEmpty not and: [selectedDates includes: m arguments first])					ifTrue: [m setSwitchState: true]]].! !!MonthMorph methodsFor: 'controls' stamp: 'di 9/24/2000 22:46'!month: aMonth	month _ aMonth.	model ifNotNil: [model setDate: nil fromButton: nil down: false].	self initializeWeeks! !!PDA methodsFor: 'date' stamp: 'di 9/24/2000 22:23'!setDate: aDate fromButton: aButton down: down	dateButtonPressed ifNotNil: [dateButtonPressed setSwitchState: false].	down ifTrue: [self selectDate: aDate.				dateButtonPressed _ aButton]		ifFalse: [self selectDate: nil.				dateButtonPressed _ nil].	self currentItem: nil.	aButton ifNotNil: [aButton owner owner highlightToday] "ugly hack to restore highlight for today"! !!PDA methodsFor: 'currentItem' stamp: 'di 9/24/2000 23:22'!currentItemMenu: aMenu	| donorMenu labels |	aMenu add: 'save database' target: self selector: #saveDatabase.	aMenu add: 'load database from file...' target: self selector: #loadDatabase.	aMenu add: 'spawn entire month' target: self selector: #openMonthView.	viewDescriptionOnly		ifTrue: [aMenu add: 'view entire records' target: self selector: #toggleDescriptionMode]		ifFalse: [aMenu add: 'view descriptions only' target: self selector: #toggleDescriptionMode].	aMenu addLine.	donorMenu _ ParagraphEditor yellowButtonMenu.	labels _ donorMenu labelString findTokens: String cr.	aMenu labels: (labels allButLast: 2) lines: donorMenu lineArray selections: donorMenu selections.	^ aMenu! !Undeclared removeUnreferencedKeys!