'From Squeak 1.2 of June 29, 1997 on 16 July 1997 at 11:45:35 pm'!
"Change Set:		FileIn-1.21
Date:			16 July 1997
Author:			Dan Ingalls

Sundry fixes to Squeak 1.2, including...
StrikeFont>>readFromBitFont: fixed to tolerate lowercase hex in Bitfont data files.
Form>>fromBMPFileNamed: supports reading of simple .BMP files.
This fixes two calls on BitBlt that allowed invalid color maps to slip by.
Adds an optimization that saves one send on every color map check.
	(subsumes the file colorMapFix)
InterpreterSimulator>>showDisplayBits fixed so simulator runs again.
SystemDictionary>>abandonSources improved so it can be called repeatedly,
	each time compressing all changes, and resetting the changes file.
Removes MappedCollection and all references to it.  As an alternative,
	defines atAll: and atAll:putAll:, which allow mapped access to any
	sequenceable collection.
Introduces consistent protocol and implementation for do: and collect: with a
	second collection that is either supplied or implied by indexing.
Fixes a long-standing error in the decompiler, and several related methods
	so that the entire system will now decompile without error.
Repairs the ProtocolBrowser that was broken by a change in Text protocol.
	Makes it work for class methods which didn't before.
Makes listPanes support emphasis in text entries (used by ProtocolBrowser).
Fixes a bug in ClassDescription>>renameInstVar:to:.
Restores a lost cr in ReadWriteStream>>timeStamp.
"!

!BitBlt methodsFor: 'copying' stamp: 'di 7/1/97 14:07'!
copyForm: srcForm to: destPt rule: rule
	sourceForm _ srcForm.
	halftoneForm _ nil.
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	colorMap _ srcForm colormapIfNeededForDepth: destForm depth.
	self copyBits.
! !

!BitBlt methodsFor: 'copying' stamp: 'di 7/1/97 14:09'!
copyFrom: sourceRectangle in: srcForm to: destPt
	| sourceOrigin |
	sourceForm _ srcForm.
	halftoneForm _ nil.
	combinationRule _ 3.  "store"
	destX _ destPt x.
	destY _ destPt y.
	sourceOrigin _ sourceRectangle origin.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ sourceRectangle width.
	height _ sourceRectangle height.
	colorMap _ srcForm colormapIfNeededForDepth: destForm depth.
	self copyBits! !


!Browser methodsFor: 'class functions' stamp: 'di 7/13/97 16:43'!
spawnProtocol
        "Create and schedule a new protocol browser on the currently selected class or meta."
        classListIndex = 0 ifTrue: [^ self].
        ProtocolBrowser openSubProtocolForClass: self selectedClassOrMetaClass  ! !

!Browser methodsFor: 'message functions' stamp: 'di 7/13/97 11:14'!
defineMessage: aString notifying: aController 
	"Compile the expressions in aString. Notify aController if a syntax error 
	occurs. Install the compiled method in the selected class classified under 
	the currently selected message category name. Answer true if 
	compilation succeeds, false otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	contents _ nil.
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aController.
	selector == nil ifTrue: [^ false].
	contents _ aString copy.
	selector ~~ selectedMessageName
		ifTrue: 
			[category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageListChanged].
			self messageListIndex: (self messageList indexOf: selector)].
	^ true! !


!ChangeSorter methodsFor: 'code pane' stamp: 'di 7/13/97 11:15'!
contents: aString notifying: aController 
	"Compile the code in aString. Notify aController of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |
	messageList listIndex = 0 ifTrue: [^ false].
	class _ self selectedClassOrMetaClass.
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aController.
	selector == nil ifTrue: [^false].
	selector == oldSelector ifFalse: [self changed: #message].
	^ true! !


!ClassDescription methodsFor: 'instance variables' stamp: 'di 7/15/97 00:04'!
renameInstVar: oldName to: newName
	| i oldCode newCode parser header body sels |
	(i _ instanceVariables indexOf: oldName) = 0 ifTrue:
		[self error: oldName , ' is not defined in ', self name].
	self allSuperclasses , self withAllSubclasses asOrderedCollection do:
		[:cls | (cls instVarNames includes: newName) ifTrue:
			[self error: newName , ' is already used in ', cls name]].
	(self confirm: 'WARNING: Renaming of instance variables
is subject to substitution ambiguities.
Do you still wish to attempt it?') ifFalse: [self halt].

	"...In other words, this does a dumb text search-and-replace,
	which might improperly alter, eg, a literal string.  As long as
	the oldName is unique, everything should work jes' fine. - di"
	instanceVariables replaceFrom: i to: i with: (Array with: newName).
	self withAllSubclasses do:
		[:cls | sels _ cls selectors.
		sels removeAllFoundIn: #(DoIt DoItIn:).
		sels do:
			[:sel |
			oldCode _ cls sourceCodeAt: sel.
			"Don't make changes in the method header"
			(parser _ cls parserClass new) parseSelector: oldCode.
			header _ oldCode copyFrom: 1 to: (parser endOfLastToken min: oldCode size).
			body _ header size > oldCode size
					ifTrue: ['']
					ifFalse: [oldCode copyFrom: header size+1 to: oldCode size].
			newCode _ header , (body copyReplaceTokens: oldName with: newName).
			newCode ~= oldCode ifTrue:
				[cls compile: newCode
					classified: (cls organization categoryOfElement: sel)
					notifying: nil]].
			cls isMeta ifFalse:
				[oldCode _ cls comment.
				newCode _ oldCode copyReplaceTokens: oldName with: newName.
				newCode ~= oldCode ifTrue:
					[cls comment: newCode]]]! !


!Collection methodsFor: 'enumerating' stamp: 'di 7/5/97 14:56'!
sum
	"Return the sum of all my elements."
	| sum |  sum _ 0.
	self do: [:each | sum _ sum + each].  
	^ sum! !


!FontSet class methodsFor: 'as yet unclassified' stamp: 'di 7/2/97 07:42'!
fileOut
	"FileOut and then change the properties of the file so that it won't be
	treated as text by, eg, email attachment facilities"
	super fileOut.
	(FileStream oldFileNamed: self name , '.st') setFileTypeToObject; close! !


!Form methodsFor: 'displaying' stamp: 'di 7/1/97 14:06'!
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	depth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"
	^ Color colorMapIfNeededFrom: depth to: destDepth
! !

!Form methodsFor: 'other' stamp: 'jm 6/30/97 18:47'!
removeZeroPixelsFrom16BitForm

	| cm |
	depth = 16 ifFalse: [self error: 'this method is only for 16-bit forms'].
	cm _ Bitmap new: (1 bitShift: 15).
	1 to: cm size do: [:i | cm at: i put: i - 1].
	cm at: 1 put: 1.
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form paint;
		destX: 0 destY: 0 width: width height: height;
		colorMap: cm;
		copyBits! !


!Form class methodsFor: 'instance creation' stamp: 'jm 6/30/97 18:18'!
fromBMPFileNamed: fileName
	"Form fromBMPFileNamed: 'FulS.bmp'"

	| f bfType bfSize bfReserved bfOffBits biSize biWidth biHeight
	  biPlanes biBitCount biCompression biSizeImage
	  biXPelsPerMeter biYPelsPerMeter biClrUsed biClrImportant
	   form bytesPerLine pixelLine pixIndex rgb |
	f _ (FileStream oldFileNamed: fileName) binary.
	bfType _ f nextLitteEndianNumber: 2.
	bfSize _ f nextLitteEndianNumber: 4.
	bfReserved _ f nextLitteEndianNumber: 4.
	bfOffBits _ f nextLitteEndianNumber: 4.
	biSize _ f nextLitteEndianNumber: 4.
	biWidth _ f nextLitteEndianNumber: 4.
	biHeight _ f nextLitteEndianNumber: 4.
	biPlanes _ f nextLitteEndianNumber: 2.
	biBitCount _ f nextLitteEndianNumber: 2.
	biCompression _ f nextLitteEndianNumber: 4.
	biSizeImage _ f nextLitteEndianNumber: 4.
	biXPelsPerMeter _ f nextLitteEndianNumber: 4.
	biYPelsPerMeter _ f nextLitteEndianNumber: 4.
	biClrUsed _ f nextLitteEndianNumber: 4.
	biClrImportant _ f nextLitteEndianNumber: 4.

	"reference vars to avoid compiler warnings"
	biSizeImage + biXPelsPerMeter + biYPelsPerMeter + biClrUsed + biClrImportant.

	((bfType = 19778) & (bfReserved = 0) & (biPlanes = 1) &
	 (biSize = 40) & (bfSize = f size))
		ifFalse: [self error: 'Bad BMP file header'].

	((biBitCount = 24) & (biCompression = 0) & (bfOffBits = 54))
		ifFalse: [self error: 'Can currently only read 24-bit BMP files'].

	form _ Form extent: biWidth@biHeight depth: 32.
	bytesPerLine _ (((biWidth * 3) + 3) // 4) * 4.
	1 to: biHeight do: [:i |
		pixelLine _ f next: bytesPerLine.
		pixIndex _ 1.
		1 to: biWidth do: [:j |
			rgb _ (pixelLine at: pixIndex) +
				   ((pixelLine at: pixIndex + 1) bitShift: 8) +
				   ((pixelLine at: pixIndex + 2) bitShift: 16).
			form bits at: ((biHeight - i) * biWidth) + j put: rgb.
			pixIndex _ pixIndex + 3]].

	f close.
	^ form
! !

!Form class methodsFor: 'examples' stamp: 'di 7/13/97 11:39'!
toothpaste: diam		"Display restoreAfter: [Form toothpaste: 30]"
	"Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| facade ball filter point queue port color q colors colr colr2 |
	colors _ Display depth = 1
		ifTrue: [Array with: Color black]
		ifFalse: [Color red wheel: 20].
	facade _ Form extent: diam@diam offset: (diam//-2) asPoint.
	(Form dotOfSize: diam) displayOn: facade
			at: (diam//2) asPoint clippingBox: facade boundingBox
			rule: Form under fillColor: Color white.
	#(1 2 3) do:
		[:x |  "simulate facade by circles of gray"
		(Form dotOfSize: x*diam//5) displayOn: facade
			at: (diam*2//5) asPoint clippingBox: facade boundingBox
			rule: Form under
			fillColor: (Color perform: 
					(#(black gray lightGray) at: x)).
		"facade displayAt: 50*x@50"].
	ball _ Form dotOfSize: diam.
	color _ 1.
	[ true ] whileTrue:
		[port _ BitBlt toForm: Display.
		"Expand 1-bit forms to any pixel depth"
		port colorMap: (Bitmap with: 0 with: 16rFFFFFFFF).
		queue _ SharedQueue new: 32.
		16 timesRepeat: [queue nextPut: -20@-20].
		Sensor waitButton.
		Sensor yellowButtonPressed ifTrue: [^ self].
		filter _ Sensor cursorPoint.
		colr _ colors atWrap: color*9.
		colr2 _ colr mixed: 0.3 with: Color white.
		[Sensor redButtonPressed or: [queue size > 0]] whileTrue:
			[filter _ filter * 4 + Sensor cursorPoint // 5.
			point _ Sensor redButtonPressed
				ifTrue: [filter] ifFalse: [-20@-20].
			port copyForm: ball to: point rule: Form paint fillColor: colr.
			(q _ queue next) == nil ifTrue: [^ self].	"exit"
			Display depth = 1
				ifTrue: [port copyForm: facade to: q rule: Form erase]
				ifFalse: [port copyForm: facade to: q rule: Form paint fillColor: colr2].
			Sensor redButtonPressed ifTrue: [queue nextPut: point]].
		color _ color + 1]! !


!InterpreterSimulator methodsFor: 'I/O primitives' stamp: 'di 7/2/97 14:07'!
showDisplayBits
	| displayObj destBits raster destDepth pixPerWord simDisp realDisp top bottom rect |
	displayObj _ self splObj: TheDisplay.
	self targetForm = displayObj ifFalse: [^ self].
	destBits _ self fetchPointer: 0 ofObject: displayObj.
	destDepth _ self fetchInteger: 3 ofObject: displayObj.
	pixPerWord _ 32 // destDepth.
	raster _ displayForm width + (pixPerWord-1) // pixPerWord.
	simDisp _ Form new hackBits: memory.
	realDisp _ Form new hackBits: displayForm bits.
	top _ myBitBlt affectedTop.
	bottom _ myBitBlt affectedBottom.
	realDisp copy: (0@(top*raster) extent: 1@(bottom - top * raster))
		from: 0@(destBits + 4 //4 + (top * raster))
		in: simDisp rule: Form over.
	rect _ 0@top corner: displayForm width@bottom.
	Display copy: (rect translateBy: self displayLocation)
		from: rect topLeft
		in: displayForm rule: Form over! !


!ListParagraph methodsFor: 'private' stamp: 'di 7/13/97 16:56'!
withArray: anArray 
	"Modifies self to contain the list of strings in anArray"
	| startOfLine endOfLine lineIndex aString |
	lines _ Array new: 20.
	lastLine _ 0.
	startOfLine _ 1.
	endOfLine _ 1.
	lineIndex _ 0.
	anArray do: 
		[:item | 
		endOfLine _ startOfLine + item size.		"this computation allows for a cr after each line..."
												"...but later we will adjust for no cr after last line"
		lineIndex _ lineIndex + 1.
		self lineAt: lineIndex put:
			((TextLineInterval start: startOfLine stop: endOfLine
				internalSpaces: 0 paddingWidth: 0)
				lineHeight: textStyle lineGrid baseline: textStyle baseline).
		startOfLine _ endOfLine + 1].
	endOfLine _ endOfLine - 1.		"endOfLine is now the total size of the text"
	self trimLinesTo: lineIndex.
	aString _ String new: endOfLine.
	anArray with: lines do: 
		[:item :interval | 
		aString
			replaceFrom: interval first
			to: interval last - 1
			with: item asString
			startingAt: 1.
		interval last <= endOfLine ifTrue: [aString at: interval last put: Character cr]].
	lineIndex > 0 ifTrue: [(lines at: lineIndex) stop: endOfLine].	"adjust for no cr after last line"
	self text: aString asText.
	anArray with: lines do: 
		[:item :interval |  item isText ifTrue:
			[text replaceFrom: interval first to: interval last - 1 with: item]].
	self updateCompositionHeight! !


!MessageNode methodsFor: 'macro transformations' stamp: 'di 7/13/97 10:48'!
toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
		and: [(initStmt isMemberOf: AssignmentNode) and:
				[initStmt variable isTemp]])
		ifFalse: [^ nil].
	body _ arguments last statements.
	variable _ initStmt variable.
	increment _ body last toDoIncrement: variable.
	(increment == nil or: [receiver statements size ~= 1])
		ifTrue: [^ nil].
	test _ receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	((test isMemberOf: MessageNode)
		and: [(limit _ test toDoLimit: variable) notNil])
		ifFalse: [^ nil].
	toDoBlock _ BlockNode new
			statements: body allButLast
			returns: false.
	toDoBlock arguments: (Array with: variable).
	^ MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence! !

!MessageNode methodsFor: 'printing' stamp: 'di 7/13/97 14:50'!
printCaseOn: aStream indent: level
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"

	| braceNode otherwise extra |
	braceNode _ arguments first.
	otherwise _ arguments last.
	((arguments size = 1) or: [otherwise isJustCaseError])
		ifTrue: [otherwise _ nil].
	receiver printOn: aStream indent: level precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printOn: aStream indent: level]
		ifFalse:
	[aStream nextPutAll: '{'; crtab: level+1.
	braceNode casesForwardDo:
		[:keyNode :valueNode :last |
		keyNode printOn: aStream indent: level+1.
	 	aStream nextPutAll: ' -> '.
		extra _ valueNode isComplex ifTrue: [aStream crtab: level+2. 1] ifFalse: [0].
	 	valueNode printOn: aStream indent: level+1+extra.
	 	last ifTrue: [aStream nextPut: $}] ifFalse: [aStream nextPut: $.; crtab: level+1]]].
	otherwise isNil
		ifFalse:
			[aStream crtab: level+1; nextPutAll: 'otherwise: '.
			 extra _ otherwise isComplex ifTrue: [aStream crtab: level+2. 1] ifFalse: [0].
			 otherwise printOn: aStream indent: level+1+extra]! !


!MessageNode class methodsFor: 'class initialization' stamp: 'di 7/13/97 10:31'!
initialize		"MessageNode initialize"
	MacroSelectors _ 
		#(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise: as: ).
	MacroTransformers _ 
		#(transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase: transformAs: ).
	MacroEmitters _ 
		#(emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value:
			emitIf:on:value: emitIf:on:value:
			emitWhile:on:value: emitWhile:on:value: emitWhile:on:value: emitWhile:on:value:
			emitToDo:on:value: emitToDo:on:value:
			emitCase:on:value: emitCase:on:value: emitAs:on:value: ).
	MacroSizers _ 
		#(sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value:
			sizeIf:value: sizeIf:value:
			sizeWhile:value: sizeWhile:value: sizeWhile:value: sizeWhile:value:
			sizeToDo:value: sizeToDo:value:
			sizeCase:value: sizeCase:value: sizeAs:value: ).
	MacroPrinters _ 
		#(printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent: printAsOn:indent: )! !


!MessageSet methodsFor: 'contents' stamp: 'di 7/13/97 11:15'!
contents: aString notifying: aController 
	"Compile the code in aString. Notify aController of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector |
	messageListIndex = 0 ifTrue: [^ false].
	self setClassAndSelectorIn: [:class :oldSelector].
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aController.
	selector == nil ifTrue: [^false].
	selector == oldSelector ifFalse: [self messageListIndex: 0].
	^ true! !


!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:33'!
getList
	"Answer the receiver's message list."
	^ messageList! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:33'!
list
	"Answer the receiver's message list."
	^ messageList! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:35'!
selectedClass
	"Answer the receiver's selected class."
	^ selectedClass! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:35'!
selectedClass: aClass
	"Set the receiver's selected class to be the argument."
	selectedClass := aClass! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:35'!
selector
	"Answer the receiver's selected selector."
	^ selectedSelector! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:35'!
selector: aString
	"Set the currently selected message selector to be aString."
	selectedSelector := aString.
	self changed: #selector! !

!ProtocolBrowser methodsFor: 'accessing' stamp: 'di 7/13/97 16:35'!
setSelector: aString
	"Set the currently selected message selector to be aString."
	selectedSelector := aString! !

!ProtocolBrowser methodsFor: 'private' stamp: 'di 7/13/97 16:23'!
initListFrom: selectorCollection highlighting: aClass 
	"Make up the messageList with items from aClass in boldface."
	| defClass item |
	messageList := OrderedCollection new.
	selectorCollection do: 
		[:selector |  defClass := aClass whichClassIncludesSelector: selector.
		item _ selector, '     (' , defClass name , ')'.
		messageList add: (defClass == aClass ifTrue:[item asText allBold] ifFalse:[item])]! !

!ProtocolBrowser methodsFor: 'private' stamp: 'di 7/13/97 16:26'!
on: aClass 
	"Initialize with the entire protocol for the class, aClass."
	self initListFrom: aClass allSelectors asSortedCollection
		highlighting: aClass! !

!ProtocolBrowser methodsFor: 'private' stamp: 'di 7/13/97 16:29'!
onSubProtocolOf: aClass 
	"Initialize with the entire protocol for the class, aClass,
		but excluding those inherited from Object."
	| selectors |
	selectors := Set new.
	(aClass withAllSuperclasses copyWithout: Object) do:
		[:each | selectors addAll: each selectors].
	self initListFrom: selectors asSortedCollection
		highlighting: aClass! !

!ProtocolBrowser methodsFor: 'private' stamp: 'di 7/13/97 16:51'!
parse: messageString toClassAndSelector: csBlock
	"Decode strings of the form <selectorName>   (<className> [class])  "
	| tuple cl |
	tuple _ messageString asString findTokens: ' '.
	cl _ tuple at: 2.
	cl _ cl copyWithoutAll: '()'.  "Strip parens"
	cl _ tuple size = 2
		ifTrue: [Smalltalk at: cl asSymbol]
		ifFalse: [(Smalltalk at: cl asSymbol) class].
	self selectedClass: cl.
	self setSelector: tuple first.
	^ csBlock value: cl value: tuple first asSymbol! !

!ProtocolBrowser methodsFor: 'private' stamp: 'di 7/13/97 16:37'!
setClassAndSelectorIn: csBlock
	"Decode strings of the form <selectorName>   (<className> [class])  "
	^ self parse: self selection toClassAndSelector: csBlock! !


!ProtocolBrowser class methodsFor: 'instance creation' stamp: 'di 7/13/97 15:15'!
openFullProtocolForClass: aClass 
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openFullProtocolForClass: ProtocolBrowser."
	| aPBrowser label |
	aPBrowser := ProtocolBrowser new on: aClass.
	label := 'Entire protocol of: ', aClass name.
	self open: aPBrowser name: label! !

!ProtocolBrowser class methodsFor: 'instance creation' stamp: 'di 7/13/97 15:15'!
openSubProtocolForClass: aClass 
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openSubProtocolForClass: ProtocolBrowser."
	| aPBrowser label |
	aPBrowser := ProtocolBrowser new onSubProtocolOf: aClass.
	label := 'Sub-protocol of: ', aClass name.
	self open: aPBrowser name: label! !


!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'di 7/14/97 22:57'!
timeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !!s"
		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.
	self cr! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'di 7/13/97 09:49'!
atAll: indexArray
	"Return the selected elements in order"
	^ indexArray collect: [:i | self at: i]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'di 7/13/97 09:50'!
atAll: indexArray putAll: valueArray
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."
	indexArray with: valueArray do:
		[:i :x | self at: i put: x]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:44'!
collectWithIndex: elementAndIndexBlock
	"Use the new version with consistent naming"
	^ self withIndexCollect: elementAndIndexBlock! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:43'!
doWithIndex: elementAndIndexBlock
	"Use the new version with consistent naming"
	^ self withIndexDo: elementAndIndexBlock! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:30'!
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:32'!
with: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:35'!
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:35'!
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]! !

!SequenceableCollection methodsFor: 'converting' stamp: 'di 7/7/97 09:51'!
reversed
	"Answer a copy of the receiver with element order reversed."
	| reversal strm |
	reversal _ self species new: self size.
	strm _ WriteStream on: reversal.
	self reverseDo: [:elem | strm nextPut: elem].
	^ reversal
" 'frog' reversed "! !


!SketchMorph methodsFor: 'other' stamp: 'jm 6/30/97 15:30'!
colorUnder
	"Return the color of under the receiver's reference position."

	| w |
	w _ self world.
	w == nil
		ifTrue: [^ self color]
		ifFalse: [^ w colorAt: self referencePosition belowMorph: self].
! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'di 7/14/97 23:15'!
upTo: delim 
	"Fast version to speed up nextChunk"
	| pos buffer count |
	pos _ self position.
	buffer _ self next: 2000.
	(count _ buffer indexOf: delim) > 0 ifTrue: 
		["Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upTo: delim)! !


!StrikeFont methodsFor: 'file in/out' stamp: 'di 6/30/97 17:27'!
readFromBitFont: fileName
	"This builds a StrikeFont instance by reading the data file format
	produced by BitFont, a widely available font conversion utility
	written by Peter DiCamillo at Brown University"
	"StrikeFont new readFromBitFont: 'Palatino10.BF' "
	| f lastAscii charLine width ascii charForm line missingForm tempGlyphs s nn iRect |
	f _ FileStream readOnlyFileNamed: fileName.
	self readBFHeaderFrom: f.
	tempGlyphs _ Form extent: (maxWidth*257) @ self height.
	xTable _ (Array new: 258) atAllPut: 0.
	xTable at: 1 put: 0.

	self restOfLine: 'Extent information for entire font' from: f.
	"Parse the foloowing line (including mispelling!!)"
	"Image rectange: left = -2, right = 8, bottom = -2, top = 7"
	s _ ReadStream on: (self restOfLine: 'Image rect' from: f).
	s upTo: $:.
	nn _ (1 to: 4) collect:
		[:i | s upTo: $=; skipSeparators. Number readFrom: (s upTo: $,)].
	iRect _ Rectangle left: (nn at: 1) right: (nn at: 2)
				top: (nn at: 3) bottom: (nn at: 4).
	
	"Read character forms and blt into tempGlyphs"
	lastAscii _ -1.
	[charLine _ self restOfLine: 'Character: ' from: f.
	charLine == nil ifFalse:
		[width_ (self restOfLine: 'Width (final pen position) = ' from: f) asNumber.
		(charLine beginsWith: 'Missing character') ifTrue: [ascii _ 256].
		('x''*' match: charLine) ifTrue:
			[ascii _ Number readFrom: (charLine copyFrom: 3 to: 4) asUppercase base: 16].
		charForm _ Form extent: width@self height.
		('*[all blank]' match: charLine) ifFalse:
			[self restOfLine: '  +' from: f.
			1 to: self height do:
				[:y | line _ f upTo: Character cr.
				4 to: line size+iRect left-1 do:
					[:x | (line at: x-iRect left)=$* ifTrue: [charForm pixelValueAt: (x-4)@(y-1) put: 1]]]]].
	charLine == nil]
		whileFalse:
			[self displayChar: ascii form: charForm.
			ascii = 256
				ifTrue: [missingForm _ charForm deepCopy]
				ifFalse:
				[minAscii _ minAscii min: ascii.
				maxAscii _ maxAscii max: ascii.
				lastAscii+1 to: ascii-1 do: [:a | xTable at: a+2 put: (xTable at: a+1)].
				tempGlyphs copy: ((xTable at: ascii+1)@0
										extent: charForm extent)
							from: 0@0 in: charForm rule: Form over.
				xTable at: ascii+2 put: (xTable at: ascii+1) + width.
				lastAscii _ ascii]].
	f close.
	lastAscii+1 to: maxAscii+1 do: [:a | xTable at: a+2 put: (xTable at: a+1)].
	missingForm == nil ifFalse:
		[tempGlyphs copy: missingForm boundingBox from: missingForm
				to: (xTable at: maxAscii+2)@0 rule: Form over.
		xTable at: maxAscii+3 put: (xTable at: maxAscii+2) + missingForm width].
	glyphs _ Form extent: (xTable at: maxAscii+3) @ self height.
	glyphs copy: glyphs boundingBox from: 0@0 in: tempGlyphs rule: Form over.
	xTable _ xTable copyFrom: 1 to: maxAscii+3.

	self setStopConditions! !


!SwitchController methodsFor: 'basic control sequence' stamp: 'di 7/13/97 11:16'!
sendMessage
	"The receiver consists of a selector and possibly of arguments that should 
	be used to create a message to send to the receiver's model."

	arguments size = 0
		ifTrue: [model perform: selector]
		ifFalse: [model perform: selector withArguments: arguments]! !


!SwitchView methodsFor: 'selector' stamp: 'di 7/13/97 11:17'!
interrogateModel
	"Answer the result of sending the receiver's model the message created 
	from the receiver's selector and arguments."

	arguments size = 0
		ifTrue: [^ model perform: selector]
		ifFalse: [^ model perform: selector withArguments: arguments]! !


!SystemDictionary methodsFor: 'shrinking'!
abandonSources    "Smalltalk abandonSources"
	"Replaces every method by a copy with the 4-byte source pointer 
	replaced by a string of all arg and temp names, followed by its length.
	These names can then be used to inform the decompiler.  See stats below"
	 | oldCodeString argsAndTemps bTotal bCount oldMethods newMethods m |
	(self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, hit Yes.
If you have any doubts, hit No,
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	Smalltalk forgetDoIts.
	oldMethods _ OrderedCollection new: CompiledMethod instanceCount.
	newMethods _ OrderedCollection new: CompiledMethod instanceCount.
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Saving temp names for better decompilation...'
	displayProgressAt: Sensor cursorPoint
	from: 0 to: bTotal
	during: [:bar |
	Smalltalk allBehaviorsDo:    "for test:  (Array with: Arc with: Arc class) do: "
		[:cl |  bar value: (bCount _ bCount + 1).
		cl selectors do:
			[:selector |
			oldCodeString _ cl sourceCodeAt: selector.
			argsAndTemps _ (cl compilerClass new
				parse: oldCodeString in: cl notifying: nil)
				tempNames.
			m _ cl compiledMethodAt: selector.
			oldMethods addLast: m.
			newMethods addLast: (m copyWithTempNames: argsAndTemps)]]].
	oldMethods asArray elementsExchangeIdentityWith: newMethods asArray.
	Smalltalk allClassesDo: [:c | c comment: ''].
	Smalltalk condenseChanges
"
In a system with 7780 methods, we got 83k of temp names, or around 100k with spaces between.  The order of letter frequency was eatrnoislcmdgpSub, with about 60k falling in the first 11.  This suggests that we could encode in 4 bits, with 0-11 beng most common chars, and 12-15 contributing 2 bits to the next nibble for 6 bits, enough to cover all alphaNumeric with upper and lower case.  If we get 3/4 in 4 bits and 1/4 in 8, then we get 5 bits per char, or about 38% savings (=38k in this case).

Summary: about 13 bytes of temp names per method, or 8 with simple compression, plus 1 for the size.  This would be 5 bytes more than the current 4-byte trailer.
"! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'di 7/13/97 09:46'!
majorShrink    "Smalltalk majorShrink; abandonSources; lastRemoval"
	"This method throws out lots of the system that is not needed for, eg, operation in a hand-held PC.  The shrink process is being improved and, in conjunction with removeAllUnSentMessages, yields an image under 800k in size."

	"Remove references to a few classes to be deleted, so that they won't leave obsolete versions around."
	FormView compile: 'defaultControllerClass 
	^  NoController' classified: 'controller access'.
	FileModel removeSelector: #fileIntoNewChangeSet.
	Form removeSelector: #edit.
	ChangeSet class compile: 'defaultName
		^ ''Changes'' ' classified: 'initialization'.
	ScreenController removeSelector: #openChangeManager.
	ScreenController removeSelector: #exitProject.
	ScreenController removeSelector: #openProject.
	ScreenController removeSelector: #viewGIFImports.

	"Now delete lots of classes.."
	(Smalltalk includesKey: #CCodeGenerator) ifTrue:
		[(Smalltalk at: #CCodeGenerator) removeCompilerMethods].
	SystemOrganization removeSystemCategory: 'Squeak Interpreter'.
	SystemOrganization removeSystemCategory: 'Translation to C'.
	(SystemOrganization categories select: [:c | 'Morphic*' match: c]) reverseDo:
		[:c | SystemOrganization removeSystemCategory: c].
	SystemOrganization removeSystemCategory: 'System-Network'.
	SystemOrganization removeSystemCategory: 'Graphics-Symbols'.
	SystemOrganization removeSystemCategory: 'Graphics-Files'.
	SystemOrganization removeSystemCategory: 'Interface-Pluggable'.
	SystemOrganization removeSystemCategory: 'Object Storage'.
	SystemOrganization removeSystemCategory: 'System-Sound'.

	FormEditor removeFromSystem.
	FormEditorView removeFromSystem.
	FormMenuView removeFromSystem.
	FormMenuController removeFromSystem.
	FormButtonCache removeFromSystem.

	CurveFitter removeFromSystem.
	LinearFit removeFromSystem.
	Spline removeFromSystem.

	ParagraphEditor removeSelector: #recognizeCharacters.
	ParagraphEditor removeSelector: #recognizer:.
	ParagraphEditor removeSelector: #recognizeCharactersWhileMouseIn:.
	CharRecog removeFromSystem.
	Array2D removeFromSystem.
	FFT removeFromSystem.

	ChangeSorter removeFromSystem.
	DualChangeSorter removeFromSystem.
	CngsClassList removeFromSystem.
	CngsMsgList removeFromSystem.
	TriggerController removeFromSystem.
	Project removeFromSystem.
	ProjectView removeFromSystem.
	ProjectController removeFromSystem.
	MessageTally removeFromSystem.
	BitEditor removeFromSystem.

	StringHolder class removeSelector: #originalWorkspaceContents.
	StringHolder systemWorkspaceContents: ''.
	TextConstants removeKey: #ClairVaux.  "Gets rid of a couple of fonts"

	FormHolderView removeFromSystem.
	FormInspectView removeFromSystem.
	GeneralListView removeFromSystem.
	GeneralListController removeFromSystem.
	HierarchicalMenu removeFromSystem.
	EmphasizedMenu removeFromSystem.
	ObjectViewer removeFromSystem.
	ObjectTracer removeFromSystem.
	SystemBuilder removeFromSystem.
	HtmlFileStream removeFromSystem.
	ConciseInspector removeFromSystem.

	Smalltalk noChanges.
	ChangeSorter classPool at: #AllChangeSets put: (OrderedCollection with: Smalltalk changes).

	[self removeAllUnSentMessages > 0] whileTrue.
	Smalltalk allClassesDo: [:c | c zapOrganization].
	Symbol rehash.
! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 7/13/97 11:56'!
testDecompiler    "Smalltalk testDecompiler"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	 | methodNode oldMethod newMethod badOnes oldCodeString |
	badOnes _ OrderedCollection new.
	Smalltalk forgetDoIts.
	Smalltalk allBehaviorsDo:
		[:cls |  Transcript cr; show: cls name.
		cls selectors do:
			[:selector |
			oldMethod _ cls compiledMethodAt: selector.
			oldCodeString _ (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: [].
			newMethod _ methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [Transcript cr; show: '***' , cls name , ' ' , selector.
											badOnes add: cls name , ' ' , selector]]].
	^ badOnes! !

!ClassDescription methodsFor: 'as yet unclassified' stamp: 'di 7/17/97 00:06'!
whichCategoryIncludesSelector: aSelector 
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	(self includesSelector: aSelector)
		ifTrue: [^ self organization categoryOfElement: aSelector]
		ifFalse: [^nil]! !

ProtocolBrowser removeSelector: #classDictionary:!
ProtocolBrowser removeSelector: #classDictionary!
ProtocolBrowser removeSelector: #selectorList:!
ProtocolBrowser removeSelector: #selectorList!
ProtocolBrowser class removeSelector: #openForClass:!

MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'selectedClass selectedSelector '
	classVariableNames: 'TextMenu '
	poolDictionaries: ''
	category: 'Interface-Browser'!

ClassDescription removeSelector: #checkForPerform:in:!
MessageNode removeSelector: #printAs:indent:!
MessageNode initialize!
SequenceableCollection removeSelector: #mappedBy:!
OrderedCollection removeSelector: #collectWithIndex:!
MappedCollection removeFromSystem!
TylerEToyDemo removeFromSystem!

!Socket class methodsFor: 'examples' stamp: 'tk 7/11/97 16:05'!
httpGet: url
	"	Socket httpShowPage: 'http://www.altavista.digital.com/index.html'	 "
	"	Socket httpShowPage: 'www.webPage.com/~kaehler2/ab.html'	 "
	"	Socket httpShowPage: 'www.exploratorium.edu/index.html'	 "
	"	Socket httpShowPage: 'www.apple.com/default.html'	 "
	"	Socket httpShowPage: 'www.altavista.digital.com/'	 "
	"	Socket httpShowPage: 'jumbo/tedk/ab.html'	 "
"Return the exact contents of a web page or other web object.  Parsed header is save.  Use proxy server if one has been stored.  tk 6/19/97 09:28"

	| serverName serverAddr s header length bare page list firstData aStream newURL |
	Socket initializeNetwork: 0.
	bare _ (url asLowercase beginsWith: 'http://') 
		ifTrue: [url copyFrom: 8 to: url size]
		ifFalse: [url].
	"For now, may not put :80 or other port number in a url.  Use setHTTPPort:"
	serverName _ bare copyUpTo: $/.
	page _ bare copyFrom: serverName size + 1 to: bare size.
	page size = 0 ifTrue: [page _ '/'].
	HTTPProxy ifNotNil: [
		page _ 'http://', serverName, page.		"put back together"
		serverName _ HTTPProxy].
	
	self retry: [serverAddr _ NetNameResolver addressForName: serverName timeout: 20.
				serverAddr ~~ nil] 
		asking: 'Trouble resolving server name.  Keep trying?'
		ifGiveUp: ["^ nil"
			self error: 'Could not find the address for ', serverName].

	s _ SocketWithHeader new.
	s connectTo: serverAddr port: HTTPPort.  "80 is normal"
	s waitForConnectionUntil: self standardDeadline.
	Transcript cr; show: serverName; cr.
	s sendCommand: 'GET ', page, ' HTTP/1.0', CrLf, 
		HTTPBlabEmail,	"may be empty"
		'User-Agent: Squeak 1.19', 
		CrLf.	"blank line"
	list _ s getResponseUpTo: CrLfCrLf.	"list = header, CrLfCrLf, beginningOfData"
	header _ list at: 1.
	Transcript show: page; cr; show: header; cr.
	firstData _ list at: 3.

	"Find the length"
	length _ s contentsLength: header.	"saves the headerTokens"
	length ifNil: [
		(newURL _ s redirect) ifNotNil: [^ self httpGet: newURL].
		Transcript cr; show: 'Some kind of Error'.
		s destroy.   ^ header].
	
	aStream _ s getRestOfBuffer: firstData totalLength: length.
	s destroy.	"Always OK to destroy!!"

	^ aStream	"String with just the data"! !

'From Squeak 1.2 of June 29, 1997 on 17 July 1997 at 10:35:58 am'!
"Change Set:		SecondChanges
Date:			17 July 1997
Author:			Dan Ingalls

Last-minute fixes for a couple of morphic demos
"!


!BitBlt methodsFor: 'copying' stamp: 'di 7/17/97 10:04'!
copyForm: srcForm to: destPt rule: rule
	^ self copyForm: srcForm to: destPt rule: rule
		colorMap: (srcForm colormapIfNeededForDepth: destForm depth)! !

!BitBlt methodsFor: 'copying' stamp: 'di 7/17/97 10:04'!
copyForm: srcForm to: destPt rule: rule colorMap: map
	sourceForm _ srcForm.
	halftoneForm _ nil.
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	colorMap _ map.
	self copyBits! !


!ColorForm methodsFor: 'displaying' stamp: 'di 7/17/97 10:04'!
displayOnPort: port at: location

	port copyForm: self to: location rule: Form paint! !


!MorphicModel methodsFor: 'initialization' stamp: 'di 7/17/97 10:32'!
initialize
	super initialize.
	open _ false.
	bounds _ 0@0 corner: 200@100.
	self color: Color transparent;
		setBorderWidth: 2 borderColor: Color yellow! !

!ScreeningMorph methodsFor: 'drawing' stamp: 'di 7/17/97 10:09'!
fullDrawOn: aCanvas
	| mergeForm |
	submorphs size = 2 ifFalse: [^ super fullDrawOn: aCanvas].
	(aCanvas isVisible: self fullBounds) ifFalse: [^ self].
	"self drawOn: aCanvas."
	displayMode == #showScreenOnly ifTrue:
		[self screenMorph fullDrawOn: aCanvas].
	displayMode == #showSourceOnly ifTrue:
		[self sourceMorph fullDrawOn: aCanvas].
	screenForm ifNil:
		[self mapColor: Color black to: 16rFFFFFFFF othersTo: 0].
	displayMode == #showScreenOverSource ifTrue:
		[self sourceMorph fullDrawOn: aCanvas.
		aCanvas image: screenForm at: self position].
	displayMode == #showScreened ifTrue:
		[mergeForm _ self sourceMorph imageFormForRectangle: self bounds.
		(BitBlt toForm: mergeForm) copyForm: screenForm to: 0@0 rule: Form and
			colorMap: (Bitmap with: 0 with: 16rFFFFFFFF).
		aCanvas image: mergeForm at: self position]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'di 7/17/97 10:40'!
version
	"Answer the version of this release."

	^ 'Squeak 1.21 of July 17, 1997'! !

SystemWindow allSubInstances do: [:m | m closeToEdits]!
Undeclared removeUnreferencedKeys!
