'From Vancouver1.0 of 28 September 2004 [latest update: #345] on 13 November 2004 at 3:51:40 am'!"Change Set:		FixJustifiedTextDate:			18 October 2004Author:			Andreas RaabFix a subtle typo which broke justified text."!!CanvasCharacterScanner methodsFor: 'stop conditions' stamp: 'ar 10/18/2004 14:31'!setStopConditions	"Set the font and the stop conditions for the current run."		self setFont.	self setConditionArray: (alignment = Justified ifTrue: [#paddedSpace]).! !!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'ar 10/18/2004 14:30'!setStopConditions	"Set the font and the stop conditions for the current run."		self setFont.	self setConditionArray: (alignment = Justified ifTrue: [#paddedSpace]).! !!MultiCharacterBlockScanner methodsFor: 'stop conditions' stamp: 'ar 10/18/2004 14:31'!setStopConditions	"Set the font and the stop conditions for the current run."		self setFont.	self setConditionArray: (alignment = Justified ifTrue: [#paddedSpace]).! !