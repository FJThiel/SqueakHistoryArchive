'From Squeak2.9alpha of 16 June 2000 [latest update: #3303] on 25 January 2001 at 5:49:54 pm'!"Change Set:		SyntaxMorph-tkDate:			25 January 2001Author:			Ted KaehlerFixes a bug in CascadeNodes that shows up when you run Smalltalk macroBenchmarks."!!MessageNode methodsFor: 'tiles' stamp: 'tk 1/25/2001 17:19'!asMorphicSyntaxIn: parent	| printer substitute row sel |	sel _ #message.	((parent nodeClassIs: CascadeNode) and: [parent parseNode receiver ~~ self]) ifTrue: [		sel _ #keyword2.		receiver ifNotNil: [self inform: 'receiver should be nil']].	row _ parent addRow: sel on: self.	special > 0 ifTrue: [printer _ MacroPrinters at: special].	substitute _ self as: TileMessageNode.	(printer == #printCaseOn:indent:) ifTrue: [		self asMorphicCaseOn: row indent: nil.		^ parent].	(special > 0)		ifTrue: 			[substitute perform: printer with: row with: nil]		ifFalse: 			[substitute 				printKeywords: selector key				arguments: arguments				on: row				indent: nil].	^ row addTransparentSpacerOfSize: 3@10.! !