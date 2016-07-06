'From Squeak 2.0 of May 22, 1998 on 3 June 1998 at 8:56:38 pm'!"Change Set:		MenuAndTransFixes-diDate:			3 June 1998Author:			Dan IngallsSeveral fixes to 2.0:Fixed bug in multi-segment menu display that caused an infinite	recursion for certain display sizes.Fixed Transcript display so it behaves properly when collapsed	Doesn't write on screen.	Keeps text up to date as though open.Fixed Transcript so the more... menu works."!!PluggableTextController methodsFor: 'transcript' stamp: 'di 6/3/1998 20:46'!appendEntry	"Append the text in the model's writeStream to the editable text. "	self deselect.	paragraph text size > model characterLimit ifTrue:		["Knock off first half of text"		self selectInvisiblyFrom: 1 to: paragraph text size // 2.		self replaceSelectionWith: Text new].	self selectInvisiblyFrom: paragraph text size + 1 to: paragraph text size.	self replaceSelectionWith: model contents asText.	self selectInvisiblyFrom: paragraph text size + 1 to: paragraph text size! !!PluggableTextController methodsFor: 'transcript' stamp: 'di 6/3/1998 20:42'!doOccluded: actionBlock	| paneRect rectSet bottomStrip |	paneRect _ paragraph clippingRectangle.	paragraph withClippingRectangle: (paneRect withHeight: 0)		do: [actionBlock value.			self scrollIn: paneRect].	view topView isCollapsed ifTrue: [^ self].	rectSet _ self visibleAreas.	bottomStrip _ paneRect withTop: paragraph compositionRectangle bottom + 1.	rectSet do:		[:rect |		(bottomStrip intersects: rect) ifTrue:			["The subsequent displayOn should clear this strip but it doesnt"			Display fill: (bottomStrip intersect: rect)					fillColor: paragraph backgroundColor].		paragraph withClippingRectangle: rect				do: [paragraph displayOn: Display]]! !!PopUpMenu methodsFor: 'basic control sequence' stamp: 'di 6/3/1998 16:43'!startUpSegmented: segmentHeight withCaption: captionOrNil at: location	"This menu is too big to fit comfortably on the screen.	Break it up into smaller chunks, and manage the relative indices.	Inspired by a special-case solution by Reinier van Loon.""(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; cr]. s skip: -1])		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'."	| nLines nLinesPer allLabels from to subset subLines index |	nLines _ (frame height - 4) // marker height.	allLabels := labelString findTokens: Character cr asString.	lineArray ifNil: [lineArray _ Array new].	nLinesPer _ segmentHeight // marker height - 3.	from := 1.	[ true ] whileTrue:		[to := (from + nLinesPer) min: nLines.		subset := allLabels copyFrom: from to: to.		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])			before: subset first.		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].		subLines _ (Array with: 1) , subLines.		index := (PopUpMenu labels: subset asStringWithCr lines: subLines) startUpWithCaption: captionOrNil at: location.		index = 1			ifTrue: [from := to + 1.					from > nLines ifTrue: [ from := 1 ]]			ifFalse: [index = 0 ifTrue: [^ 0].					^ from + index - 2]]! !!TranscriptStream methodsFor: 'all' stamp: 'di 6/3/1998 20:50'!doMenuItem: selector paneID: textSelector from: aController	"Copied from Model" 	aController perform: selector! !