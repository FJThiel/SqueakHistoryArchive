'From Squeak 2.1 of June 30, 1998 on 11 September 1998 at 1:02:16 pm'!!MethodNode methodsFor: 'printing' stamp: 'di 8/24/1998 12:13'!printOn: aStream 	| args |	precedence = 1		ifTrue: 			[aStream nextPutAll: self selector]		ifFalse: 			[args _ ReadStream on: arguments.			self selector keywords do: 				[:s | 				aStream nextPutAll: s; space.				aStream withAttribute: (TextColor color: Color green)					do: [aStream nextPutAll: args next key].				aStream space]].	comment == nil ifFalse: 			[aStream crtab: 1.			self printCommentOn: aStream indent: 1].	temporaries size > 0 ifTrue: 			[aStream crtab: 1.			aStream nextPutAll: '| '.			aStream withAttribute: (TextColor color: Color green)				do: [temporaries do: 					[:temp | 					aStream nextPutAll: temp key.					aStream space]].			aStream nextPut: $|].	primitive > 0 ifTrue:			[(primitive between: 256 and: 519) ifFalse:  " Dont decompile <prim> for, eg, ^ self "				[aStream crtab: 1.				self printPrimitiveOn: aStream]].	aStream crtab: 1.	^block printStatementsOn: aStream indent: 0! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 8/24/1998 09:56'!prettyPrint	"Reformat the contents of the receiver's view, formatted, if the view is unlocked. "	| selectedClass aCompiler newText |	model messageListIndex = 0 ifTrue: [^view flash].	selectedClass _ model selectedClassOrMetaClass.		aCompiler _ selectedClass compilerClass new.		self deselect; selectInvisiblyFrom: 1 to: paragraph text size.		newText _ aCompiler			format: model contents			in: selectedClass			notifying: self.		newText == nil ifFalse: 			[self replaceSelectionWith:				(newText asText makeSelectorBoldIn: selectedClass).			self selectAt: 1].! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 8/24/1998 09:56'!shiftedYellowButtonMessages	"Answer the set of messages that go with the shifted menu.  Inconvenient to have it here in this separate method; when/if we consolidate via a class variable, as for unshifted, the problem will go away.  1/17/96 sw	 3/7/96 sw: added methodSourceContainingIt	 3/13/96 sw: merged ParagraphEditor and StringHolderController versions into ParagraphEditor, and deleted the StringHolderController versions	 5/27/96 sw: added offerFontMenu	 8/20/96 sw: makeover"	^ #(offerFontMenu changeStyle explain prettyPrint fileItIn recognizeCharacters spawn browseIt sendersOfIt implementorsOfIt referencesToIt  methodNamesContainingIt methodStringsContainingit methodSourceContainingIt  presentSpecialMenu unshiftedYellowButtonActivity)"set font... (k)set style... (K)explainformatfile it inrecognizer (r)spawn (o)browse it (b)senders of it (n)implementors of it (m)references to it (N)selectors containing it (W)method strings with itmethod source with itspecial menu...more..."! !!ParagraphEditor methodsFor: 'editing keys' stamp: 'di 8/24/1998 09:57'!prettyPrint: characterStream 	"Triggered by Cmd-shift-P; reformat this code by prettyPrinting the parse tree"	sensor keyboard.		"flush character"	self prettyPrint.	^ true! !!ParagraphEditor class methodsFor: 'keyboard shortcut tables' stamp: 'di 8/24/1998 09:58'!initializeShiftCmdKeyShortcuts	"Initialize the shift-command-key (or control-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"	| cmdMap cmds |	"shift-command and control shortcuts"	cmdMap _ Array new: 256.  "use temp in case of a crash"	cmdMap atAllPut: #noop:.	cmdMap at: ( 1 + 1) put: #cursorHome:.			"home key"	cmdMap at: ( 4 + 1) put: #cursorEnd:.			"end key"	cmdMap at: ( 8 + 1) put: #forwardDelete:.			"ctrl-H or delete key"	cmdMap at: (13 + 1) put: #crWithIndent:.			"ctrl-Return"	cmdMap at: (27 + 1) put: #selectCurrentTypeIn:.	"escape key"	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"	cmdMap at: (30 + 1) put: #cursorUp:.				"up arrow key"	cmdMap at: (31 + 1) put: #cursorDown:.			"down arrow key"	cmdMap at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"	cmdMap at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"	"Note: Command key overrides shift key, so, for example, cmd-shift-9 produces $9 not $("	'9[,''' do: [ :char | cmdMap at: (char asciiValue + 1) put: #shiftEnclose: ].	"({< and double-quote"	"Note: Must use cmd-9 or ctrl-9 to get '()' since cmd-shift-9 is a Mac FKey command."	cmdMap at: (27 + 1) put: #shiftEnclose:.	"ctrl-["	cmds _ #(		$a	argAdvance:		$b	browseItHere:		$c	compareToClipboard:		$d	duplicate:		$e	methodStringsContainingIt:		$f	displayIfFalse:		$j	doAgainMany:		$k	changeStyle:		$n	referencesToIt:		$p	prettyPrint:		$r	indent:		$l	outdent:		$s	search:		$t	displayIfTrue:		$u	changeLfToCr:		$v	pasteInitials:		$w	methodNamesContainingIt:		$x	makeLowercase:		$y	makeUppercase:		$z	makeCapitalized:	).	1 to: cmds size by: 2 do: [ :i |		cmdMap at: ((cmds at: i) asciiValue + 1)			put: (cmds at: i + 1).		cmdMap at: (((cmds at: i) asciiValue - 96) + 1)	put: (cmds at: i + 1).	].	ShiftCmdActions _ cmdMap.! !!PluggableTextController class methodsFor: 'as yet unclassified' stamp: 'di 8/24/1998 09:57'!shiftedYellowButtonMessages	"Answer the set of messages that go with the shifted menu.  "	^ #(offerFontMenu changeStyle explain prettyPrint fileItIn recognizeCharacters spawn browseIt sendersOfIt implementorsOfIt referencesToIt  methodNamesContainingIt methodStringsContainingit methodSourceContainingIt  presentSpecialMenu yellowButtonActivity "<-note change")"set font... (k)set style... (K)explainformatfile it inrecognizer (r)spawn (o)browse it (b)senders of it (n)implementors of it (m)references to it (N)selectors containing it (W)method strings with itmethod source with itspecial menu...more..."! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'di 8/24/1998 09:57'!prettyPrint	self handleEdit: [textMorph editor prettyPrint]! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 8/23/1998 14:35'!testDecompiler    "Smalltalk testDecompiler"	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."	 | methodNode oldMethod newMethod badOnes oldCodeString n |	badOnes _ OrderedCollection new.	Smalltalk forgetDoIts.'Decompiling all classes...'displayProgressAt: Sensor cursorPointfrom: 0 to: CompiledMethod instanceCountduring: [:bar | n _ 0.	Smalltalk allBehaviorsDo:		[:cls | 		"Transcript cr; show: cls name."		cls selectors do:			[:selector | (n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].			oldMethod _ cls compiledMethodAt: selector.			oldCodeString _ (cls decompilerClass new								decompile: selector in: cls method: oldMethod)							decompileString.			methodNode _ cls compilerClass new						compile: oldCodeString						in: cls notifying: nil ifFail: [].			newMethod _ methodNode generate: #(0 0 0 0).			oldCodeString = (cls decompilerClass new								decompile: selector in: cls method: newMethod)							decompileString ifFalse: [Transcript cr; show: '***' , cls name , ' ' , selector.											badOnes add: cls name , ' ' , selector]]].].	Smalltalk browseMessageList: badOnes asSortedCollection name: 'Decompiler Discrepancies'! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 8/23/1998 14:35'!testFormatter    "Smalltalk testFormatter"	"Reformats the source for every method in the system, and then	compiles that source and verifies that it generates identical code"	 | newCodeString methodNode oldMethod newMethod badOnes n |	badOnes _ OrderedCollection new.	Smalltalk forgetDoIts.'Formatting all classes...'displayProgressAt: Sensor cursorPointfrom: 0 to: CompiledMethod instanceCountduring: [:bar | n _ 0.	Smalltalk allBehaviorsDo:		[:cls | 		"Transcript cr; show: cls name."		cls selectors do:			[:selector | (n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].			newCodeString _ (cls compilerClass new)				format: (cls sourceCodeAt: selector)				in: cls notifying: nil.			methodNode _ cls compilerClass new						compile: newCodeString						in: cls notifying: nil ifFail: [].			newMethod _ methodNode generate: #(0 0 0 0).			oldMethod _ cls compiledMethodAt: selector.			oldMethod = newMethod ifFalse: [Transcript cr; show: '***' , cls name , ' ' , selector.											badOnes add: cls name , ' ' , selector]]].].	Smalltalk browseMessageList: badOnes asSortedCollection name: 'Formatter Discrepancies'! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 8/28/1998 06:06'!testFormatter2    "Smalltalk testFormatter2"	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged"	 | newCodeString badOnes n oldCodeString oldTokens newTokens |	badOnes _ OrderedCollection new.	Smalltalk forgetDoIts.'Formatting all classes...'displayProgressAt: Sensor cursorPointfrom: 0 to: CompiledMethod instanceCountduring: [:bar | n _ 0.	Smalltalk allBehaviorsDo:		[:cls | 		"Transcript cr; show: cls name."		cls selectors do:			[:selector | (n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].			oldCodeString _ (cls sourceCodeAt: selector) asString.			newCodeString _ (cls compilerClass new)				format: oldCodeString				in: cls notifying: nil.			oldTokens _ oldCodeString findTokens: Character separators.			newTokens _ newCodeString findTokens: Character separators.			oldTokens = newTokens ifFalse:					[Transcript cr; show: '***' , cls name , ' ' , selector.					badOnes add: cls name , ' ' , selector]]].].	Smalltalk browseMessageList: badOnes asSortedCollection name: 'Formatter Discrepancies'! !ParagraphEditor removeSelector: #format!PluggableTextMorph removeSelector: #format!"Postscript:Re-link super-send links via shared associationsExecutable statements after this comment quote..."  CompiledMethod allInstancesDo:	[:m | lits _ m literals.	1 to: lits size do:		[:i | ((assn _ lits at: i) isKindOf: Association)			ifTrue: [(assn key == nil and: [Smalltalk includesKey: assn value name]) ifTrue:					[m literalAt: i put: (Smalltalk associationAt: assn value name)]]]].!