'From Squeak 2.0 BETA of May 8, 1998 on 13 May 1998 at 2:32:34 pm'!!Debugger methodsFor: 'private' stamp: 'di 5/13/1998 14:07'!contextStackIndex: anInteger oldContextWas: oldContext	| newMethod |	contextStackIndex _ anInteger.	anInteger = 0		ifTrue:			[tempNames _ sourceMap _ contents _ nil.			self changed: #contextStackIndex.			self changed: #contents.			contextVariablesInspector object: nil.			receiverInspector object: self receiver.			^self].	(newMethod _ oldContext == nil or:		[oldContext method ~~ self selectedContext method])		ifTrue:			[tempNames _ sourceMap _ nil.			contents _ self selectedContext sourceCode.			self changed: #contents.			self pcRange "will compute tempNamesunless noFrills"].	self changed: #contextStackIndex.	tempNames == nil		ifTrue: [tempNames _ 					self selectedClassOrMetaClass parserClass new parseArgsAndTemps: contents notifying: nil].	contextVariablesInspector object: self selectedContext.	receiverInspector object: self receiver.	newMethod ifFalse: [self changed: #contentsSelection]! !!PluggableTextController methodsFor: 'transcript' stamp: 'di 5/13/1998 14:27'!appendEntry	"Append the text in the model's writeStream to the editable text. "	view topView isCollapsed ifTrue:		[^ paragraph text replaceFrom: 1 to: paragraph text size				with: model contents asText].	self deselect.	paragraph text size > model characterLimit ifTrue:		["Knock off first half of text"		self selectInvisiblyFrom: 1 to: paragraph text size // 2.		self replaceSelectionWith: Text new].	self selectInvisiblyFrom: paragraph text size + 1 to: paragraph text size.	self replaceSelectionWith: model contents asText.	self selectInvisiblyFrom: paragraph text size + 1 to: paragraph text size! !!PluggableTextController methodsFor: 'transcript' stamp: 'di 5/13/1998 14:16'!changeText: aText	"The paragraph to be edited is changed to aText."	paragraph text: aText.	self resetState.	self selectInvisiblyFrom: paragraph text size + 1 to: paragraph text size.	self selectAndScroll.	self deselect! !!PluggableTextMorph methodsFor: 'transcript' stamp: 'di 5/13/1998 14:29'!appendEntry	"Append the text in the model's writeStream to the editable text. "	textMorph asText size > model characterLimit ifTrue:		["Knock off first half of text"		self selectInvisiblyFrom: 1 to: textMorph asText size // 2.		self replaceSelectionWith: Text new].	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size.	self replaceSelectionWith: model contents asText.	self selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size! !!TranscriptStream methodsFor: 'all' stamp: 'di 5/13/1998 14:31'!characterLimit	"Tell the views how much to retain on screen"	^ 3000! !ParagraphEditor removeSelector: #selectInvisiblyAt:!