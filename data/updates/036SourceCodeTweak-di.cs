'From Squeak 2.0 BETA of May 8, 1998 on 17 May 1998 at 12:05:17 pm'!!ChangeList methodsFor: 'scanning' stamp: 'di 5/17/1998 12:01'!scanVersionsOf: method class: class meta: meta category: category selector: selector	| position prevPos prevFileIndex preamble tokens sourceFilesCopy stamp |	changeList _ OrderedCollection new.	list _ OrderedCollection new.	listIndex _ 0.	position _ method filePosition.	sourceFilesCopy _ SourceFiles collect:		[:x | x isNil ifTrue: [ nil ]				ifFalse: [x readOnlyCopy]].	method fileIndex == 0 ifTrue: [self inform: 'Not Logged, no versions'.								^ nil].	file _ sourceFilesCopy at: method fileIndex.	[position notNil & file notNil]		whileTrue:		[file position: (0 max: position-150).  "Skip back to before the preamble"		[file position < (position-1)]  "then pick it up from the front"			whileTrue: [preamble _ file nextChunk].		"Preamble is likely a linked method preamble, if we're in			a changes file (not the sources file).  Try to parse it			for prior source position and file index"		prevPos _ nil.		stamp _ ''.		(preamble findString: 'methodsFor:' startingAt: 1) > 0			ifTrue: [tokens _ Scanner new scanTokens: preamble]			ifFalse: [tokens _ Array new  "ie cant be back ref"].		((tokens size between: 7 and: 8)			and: [(tokens at: tokens size-5) = #methodsFor:])			ifTrue:				[(tokens at: tokens size-3) = #stamp:				ifTrue: ["New format gives change stamp and unified prior pointer"						stamp _ tokens at: tokens size-2.						prevPos _ tokens last.						prevFileIndex _ prevPos // 16r1000000.						prevPos _ prevPos \\ 16r1000000]				ifFalse: ["Old format gives no stamp; prior pointer in two parts"						prevPos _ tokens at: tokens size-2.						prevFileIndex _ tokens last].				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos _ nil]].		((tokens size between: 5 and: 6)			and: [(tokens at: tokens size-3) = #methodsFor:])			ifTrue:				[(tokens at: tokens size-1) = #stamp:				ifTrue: ["New format gives change stamp and unified prior pointer"						stamp _ tokens at: tokens size]]. 		self addItem:				(ChangeRecord new file: file position: position type: #method						class: class name category: category meta: meta stamp: stamp)			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.		position _ prevPos.		prevPos notNil ifTrue:			[file _ sourceFilesCopy at: prevFileIndex]].	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].	listSelections _ Array new: list size withAll: false! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'di 5/17/1998 10:40'!fileOutCategory: aString on: aFileStream moveSource: moveSource toFile: fileIndex 	"File a description of the receiver's category, aString, onto aFileStream. If 	moveSource, is true, then set the method source pointer to the new file position.	Note when this method is called with moveSource=true, it is condensing the	.sources file, and should only write one preamble per method category."	aFileStream cr.true ifTrue:	["Overridden to preserve author stamps in sources file regardless"	(self organization listAtCategoryNamed: aString)		do: [:sel | self printMethodChunk: sel withPreamble: true						on: aFileStream moveSource: moveSource toFile: fileIndex].	^ self].	moveSource ifTrue:		["Single header for condensing source files"		self printCategoryChunk: aString on: aFileStream].	(self organization listAtCategoryNamed: aString)		do: [:sel | self printMethodChunk: sel withPreamble: moveSource not						on: aFileStream moveSource: moveSource toFile: fileIndex].	moveSource ifTrue: [aFileStream nextChunkPut: ' ']! !