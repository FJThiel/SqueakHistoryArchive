'From Squeak3.7beta of ''1 April 2004'' [latest update: #5967] on 29 June 2004 at 3:01:43 pm'!"Change Set:		ListMorphArrowKeyFix-nkDate:			29 June 2004Author:			Ned KonzEven though SimpleHierarchicalListMorphs default to having a keystrokeActionSelector: of #arrowKey:from:, if this is called on a typical Model it will not work.This is because of the differences between the PluggableListMorph, the PluggableListView and the SimpleHierarchicalListMorph:* In the PLM, the model is NEVER called for any un-modified arrow keys, just for keys with modifiers (shift, alt, command, control?). The model never hears about keys whose asciiValue is < 32.* In the PLV, the model is ALWAYS called if it has a keystrokeActionSelector. Its code, however, calls back to the view and does the same thing that the PLM's specialKeyPressed: method does.* In the SHLM, the model is ALWAYS called if it has a keystrokeActionSelector.This change set makes the SHLM and the PLV work the same as the PLM. It moves the contents of Model>>arrowKey:from: into PLV, which is the only place that the code was actually being used for anything.This also fixes the problem with the 5934InspectorExplorer-rhi and arrow keys."!!Model methodsFor: 'keyboard' stamp: 'nk 6/29/2004 14:46'!arrowKey: aChar from: view	"backstop; all the PluggableList* classes actually handle arrow keys, and the models handle other keys."	^false! !!PluggableListView methodsFor: 'model access' stamp: 'nk 6/29/2004 14:45'!handleKeystroke: aChar	"Answer the menu for this list view."	| args aSpecialKey |	aSpecialKey _ aChar asciiValue.	aSpecialKey < 32 ifTrue: [ self specialKeyPressed: aSpecialKey. ^nil ].	keystrokeActionSelector ifNil: [^ nil].	controller controlTerminate.	(args _ keystrokeActionSelector numArgs) = 1		ifTrue: [model perform: keystrokeActionSelector with: aChar.				^ controller controlInitialize].	args = 2		ifTrue: [model perform: keystrokeActionSelector with: aChar with: self.				^ controller controlInitialize].	^ self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !!PluggableListView methodsFor: 'model access' stamp: 'nk 6/29/2004 14:42'!specialKeyPressed: keyEvent	"Process the up and down arrows in a list pane."     | oldSelection nextSelection max min howMany |	(#(1 4 11 12 30 31) includes: keyEvent) ifFalse: [ ^ false ].     oldSelection := self getCurrentSelectionIndex.     nextSelection := oldSelection.     max := self maximumSelection.     min := self minimumSelection.     howMany := self numSelectionsInView.	"get this exactly??"     keyEvent == 31 ifTrue:		["down-arrow; move down one, wrapping to top if needed"		nextSelection := oldSelection + 1.		nextSelection > max ifTrue: [nextSelection _ 1]].     keyEvent == 30 ifTrue:		["up arrow; move up one, wrapping to bottom if needed"		nextSelection := oldSelection - 1.		nextSelection < 1 ifTrue: [nextSelection _ max]].     keyEvent == 1  ifTrue: [nextSelection := 1].  "home"     keyEvent == 4  ifTrue: [nextSelection := max].   "end"     keyEvent == 11 ifTrue: [nextSelection := min max: (oldSelection - howMany)].  "page up"     keyEvent == 12  ifTrue: [nextSelection := (oldSelection + howMany) min: max].  "page down"     nextSelection = oldSelection  ifFalse:		[model okToChange			ifTrue:				[self changeModelSelection: nextSelection.				"self controller moveMarker"]].		^true			! !!SimpleHierarchicalListMorph methodsFor: 'event handling' stamp: 'nk 6/29/2004 14:48'!keyStroke: event 	"Process potential command keys"	| args aCharacter |	(self scrollByKeyboard: event) ifTrue: [^self].	aCharacter := event keyCharacter.	(self arrowKey: aCharacter) ifTrue: [^true].	keystrokeActionSelector isNil ifTrue: [^false].	(args := keystrokeActionSelector numArgs) = 1 		ifTrue: [^model perform: keystrokeActionSelector with: aCharacter].	args = 2 		ifTrue: 			[^model 				perform: keystrokeActionSelector				with: aCharacter				with: self].	^self 		error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !