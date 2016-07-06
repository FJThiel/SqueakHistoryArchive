'From Squeak2.9alpha of 12 June 2000 [latest update: #3185] on 21 December 2000 at 3:49:51 pm'!"Change Set:		PDAFixDate:			21 December 2000Author:			Dan IngallsFixes MonthMorph highlight of today, and linkage to PDA model, both of which were recently broken."!!MonthMorph methodsFor: 'stepping' stamp: 'di 12/21/2000 13:41'!step	todayCache = Date today		ifFalse: [self highlightToday  "Only happens once a day"]! !!MonthMorph class methodsFor: 'as yet unclassified' stamp: 'di 12/21/2000 15:19'!newWithModel: aModel	^ (self basicNew model: aModel) initialize! !!PDA methodsFor: 'initialization' stamp: 'di 12/21/2000 15:18'!openAsMorphIn: window  "PDA new openAsMorph openInWorld"	"Create a pluggable version of all the morphs for a Browser in Morphic"	| dragNDropFlag paneColor chooser |	window color: Color black.	paneColor _ (Color r: 0.6 g: 1.0 b: 0.0).	window model: self.	dragNDropFlag _ Preferences browseWithDragNDrop.	window addMorph: ((PluggableListMorph on: self list: #peopleListItems			selected: #peopleListIndex changeSelected: #peopleListIndex:			menu: #peopleMenu: keystroke: #peopleListKey:from:) enableDragNDrop: dragNDropFlag)		frame: (0@0 corner: 0.3@0.25).	window addMorph: ((chooser _ PDAChoiceMorph new color: paneColor) contentsClipped: 'all';			target: self; actionSelector: #chooseFrom:categoryItem:; arguments: {chooser};			getItemsSelector: #categoryChoices)		frame: (0@0.25 corner: 0.3@0.3).	window addMorph: ((MonthMorph newWithModel: self) color: paneColor; extent: 148@109)		frame: (0.3@0 corner: 0.7@0.3).	window addMorph: (PDAClockMorph new color: paneColor;						faceColor: (Color r: 0.4 g: 0.8 b: 0.6))  "To match monthMorph"		frame: (0.7@0 corner: 1.0@0.3).	window addMorph: ((PluggableListMorph on: self list: #toDoListItems			selected: #toDoListIndex changeSelected: #toDoListIndex:			menu: #toDoMenu: keystroke: #toDoListKey:from:) enableDragNDrop: dragNDropFlag)		frame: (0@0.3 corner: 0.3@0.7).	window addMorph: ((PluggableListMorph on: self list: #scheduleListItems			selected: #scheduleListIndex changeSelected: #scheduleListIndex:			menu: #scheduleMenu: keystroke: #scheduleListKey:from:) enableDragNDrop: dragNDropFlag)		frame: (0.3@0.3 corner: 0.7@0.7).	window addMorph: ((PluggableListMorph on: self list: #notesListItems			selected: #notesListIndex changeSelected: #notesListIndex:			menu: #notesMenu: keystroke: #notesListKey:from:) enableDragNDrop: dragNDropFlag)		frame: (0.7@0.3 corner: 1@0.7).	window addMorph: (PluggableTextMorph on: self			text: #currentItemText accept: #acceptCurrentItemText:			readSelection: #currentItemSelection menu: #currentItemMenu:)		frame: (0@0.7 corner: 1@1).	window firstSubmorph color: paneColor.	window updatePaneColors.	window step.	^ window! !PDAMorph removeSelector: #stepAt:!