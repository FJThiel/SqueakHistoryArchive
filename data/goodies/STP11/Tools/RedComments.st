"RedComments.st
Simple changes to the Browser to make the selector of a method appear in bold blue and make comments red (or italics, your choice) so that they stick out more; edit to taste.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 18, 1996"!


!BrowserCodeView methodsFor: 'updating'!
updateDisplayContents 
	"Refer to the comment in StringHolderView|updateDisplayContents."

	| contents |
	contents _ model contents.
	displayContents asString ~= contents
		ifTrue: 
			[model messageListIndex ~= 0
				ifTrue: [contents _ (contents asText
								makeSelectorBoldIn: model selectedClassOrMetaClass)
								makeCommentsItalics].
			self editString: contents.
			self displayView.
			model editSelection == #newMessage ifTrue:
				[controller selectFrom: 1 to: contents size]]! !

!Text methodsFor: 'emphasis'!
makeColor: color from: start to: stop

	^ self addAttribute: (TextColor color: color) from: start to: stop!

makeCommentsItalics
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that looks like a comment to be italicized."

	| start stop size |
	string size = 0 ifTrue: [^self].
	size := string size.
	stop := 0.
	[true] whileTrue:
		[start := self indexOf: $" startingAt: stop + 1 ifAbsent: [nil].
		start = nil ifTrue: [^self].
		stop := self indexOf: $" startingAt: start + 1 ifAbsent: [nil].
		stop = nil ifTrue: [^self].
"		self addAttribute: TextEmphasis italic from: start to: stop.
"		self makeColor: Color red from: start "+ 1" to: stop "- 1"]!

makeSelectorBoldIn: aClass
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."
	| parser |
	string size = 0 ifTrue: [^self].
	(parser _ aClass parserClass new) parseSelector: string.
	self makeBoldFrom: 1 to: (parser endOfLastToken min: string size).
	self makeColor: Color blue from: 1 to: (parser endOfLastToken min: string size).
! !

