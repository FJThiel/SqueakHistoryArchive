'From Squeak3.6 of ''6 October 2003'' [latest update: #5424] on 1 December 2003 at 12:23:07 pm'!"Change Set:		TextMorphBugletDate:			1 December 2003Author:			Bijan ParsiaAnother in a series of microfixes.This showed up when using the codeProvenance button to toggle colorPrinting from prettyPrinting. Since the actually characters didn't change, the display wouldn't update until you went to another method, or cycled through some other display mode. This fix follows the advice in Text>>=. I expect it'll slow somethings down, especially for complex texts (e.g., with embedded morphs). It may be better to change how this test logic works, but now, at least, the behavior is correct."!!TextMorph methodsFor: 'accessing' stamp: 'BJP 12/1/2003 00:19'!newContents: stringOrText 	"Accept new text contents."	| newText embeddedMorphs |	"If my text is all the same font, use the font for my new contents"	newText _ stringOrText isString ifTrue: [ | textSize |		(text notNil		  and: [ (textSize _ text size) > 0		    and: [ (text runLengthFor: 1) = textSize ]]) ifTrue: [ | attribs |			attribs _ text attributesAt: 1 forStyle: textStyle.			Text string: stringOrText copy attributes: attribs.		]		ifFalse: [ Text fromString: stringOrText copy ]	]	ifFalse: [ stringOrText copy asText.	"should be veryDeepCopy?" ].	(text = newText and: [text runs = newText runs]) ifTrue: [^ self].	"No substantive change"	text ifNotNil: [(embeddedMorphs _ text embeddedMorphs)			ifNotNil: 				[self removeAllMorphsIn: embeddedMorphs.				embeddedMorphs do: [:m | m delete]]].	text _ newText.	"add all morphs off the visible region; they'll be moved into the right 	place when they become visible. (this can make the scrollable area too 	large, though)"	newText embeddedMorphs do: 		[:m | 		self addMorph: m.		m position: -1000 @ 0].	self releaseParagraph.	"update the paragraph cache"	self paragraph.	"re-instantiate to set bounds"	self world ifNotNil: [self world startSteppingSubmorphsOf: self]! !