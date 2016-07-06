'From Squeak3.4 of 1 March 2003 [latest update: #5170] on 8 April 2003 at 1:10:49 pm'!"Change Set:		instrPrinterDate:			8 April 2003Author:			Marcus DenkerA refactoring for InstructionPrinter,AbstractInstructionPrinter and InstVarRefLocator:AbstractInstructionPrinter: moved the instVar and a method into InstVarRefLocator, now this class canbe used as a Abstract Superclass forboth InstructionPrinter and InstVarRefLocatorRenamed to InstructionClient (c.f. VisualWorks).InstructionPrinter: refactored to be asubclass of InstructionClient"!Smalltalk renameClassNamed: #AbstractInstructionPrinter as: #InstructionClient!Object subclass: #InstructionClient	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Methods'!!InstructionClient commentStamp: 'md 4/8/2003 12:50' prior: 0!My job is to make it easier to implement clients for InstructionStream. See InstVarRefLocatoras an example. !InstructionClient subclass: #InstVarRefLocator	instanceVariableNames: 'bingo '	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Methods'!!InstVarRefLocator commentStamp: 'md 4/8/2003 12:50' prior: 0!My job is to scan bytecodes for instance variable references.BlockContext allInstances collect: [ :x |	{x. x hasInstVarRef}].!InstructionClient subclass: #InstructionPrinter	instanceVariableNames: 'stream oldPC method scanner '	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Methods'!!InstructionPrinter commentStamp: 'md 4/8/2003 12:47' prior: 0!My instances can print the object code of a CompiledMethod in symbolic format. They print into an instance variable, stream, and uses oldPC to determine how many bytes to print in the listing. The variable method  is used to hold the method being printed.!!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!blockReturnTop	"Return Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!doDup	"Duplicate Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!doPop	"Remove Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!jump: offset	"Unconditional Jump bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!jump: offset if: condition 	"Conditional Jump bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!methodReturnConstant: value 	"Return Constant bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:02'!methodReturnReceiver	"Return Self bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!methodReturnTop	"Return Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!popIntoLiteralVariable: anAssociation 	"Remove Top Of Stack And Store Into Literal Variable bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!popIntoReceiverVariable: offset 	"Remove Top Of Stack And Store Into Instance Variable bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!popIntoTemporaryVariable: offset 	"Remove Top Of Stack And Store Into Temporary Variable bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!pushActiveContext	"Push Active Context On Top Of Its Own Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!pushConstant: value	"Push Constant, value, on Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!pushLiteralVariable: anAssociation	"Push Contents Of anAssociation On Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!pushReceiver	"Push Active Context's Receiver on Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:03'!pushReceiverVariable: offset	"Push Contents Of the Receiver's Instance Variable Whose Index 	is the argument, offset, On Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!pushTemporaryVariable: offset	"Push Contents Of Temporary Variable Whose Index Is the 	argument, offset, On Top Of Stack bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!send: selector super: supered numArgs: numberArguments	"Send Message With Selector, selector, bytecode. The argument, 	supered, indicates whether the receiver of the message is specified with 	'super' in the source method. The arguments of the message are found in 	the top numArguments locations on the stack and the receiver just 	below them."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!storeIntoLiteralVariable: anAssociation 	"Store Top Of Stack Into Literal Variable Of Method bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!storeIntoReceiverVariable: offset 	"Store Top Of Stack Into Instance Variable Of Method bytecode."! !!InstructionClient methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 13:04'!storeIntoTemporaryVariable: offset 	"Store Top Of Stack Into Temporary Variable Of Method bytecode."! !!InstVarRefLocator methodsFor: 'initialize-release' stamp: 'md 4/8/2003 11:35'!interpretNextInstructionUsing: aScanner 		bingo _ false.	aScanner interpretNextInstructionFor: self.	^bingo! !!InstructionPrinter methodsFor: 'accessing' stamp: 'md 4/8/2003 11:20'!method	^method.! !!InstructionPrinter methodsFor: 'accessing' stamp: 'md 4/8/2003 11:20'!method: aMethod	method :=  aMethod.! !!InstructionPrinter methodsFor: 'initialize-release' stamp: 'md 4/8/2003 11:19'!printInstructionsOn: aStream 	"Append to the stream, aStream, a description of each bytecode in the 	instruction stream."		| end |	stream _ aStream.	scanner _ InstructionStream on: method.	end _ method endPC.	oldPC _ scanner pc.	[scanner pc <= end]		whileTrue: [scanner interpretNextInstructionFor: self]! !!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 12:14'!doPop	"Print the Remove Top Of Stack bytecode."	self print: 'pop'! !!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 11:13'!jump: offset	"Print the Unconditional Jump bytecode."	self print: 'jumpTo: ' , (scanner pc + offset) printString! !!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'md 4/8/2003 11:13'!jump: offset if: condition 	"Print the Conditional Jump bytecode."	self print: 		(condition			ifTrue: ['jumpTrue: ']			ifFalse: ['jumpFalse: '])			, (scanner pc + offset) printString! !!InstructionPrinter methodsFor: 'printing' stamp: 'md 4/8/2003 12:47'!print: instruction 	"Append to the receiver a description of the bytecode, instruction." 	| code |	stream print: oldPC; space.	stream nextPut: $<.	oldPC to: scanner pc - 1 do: 		[:i | 		code _ (method at: i) radix: 16.		stream nextPut: 			(code size < 5				ifTrue: [$0]				ifFalse: [code at: 4]).		stream nextPut: code last; space].	stream skip: -1.	stream nextPut: $>.	stream space.	stream nextPutAll: instruction.	stream cr.	oldPC _ scanner pc.	"(InstructionPrinter compiledMethodAt: #print:) symbolic."! !!InstructionPrinter class methodsFor: 'printing' stamp: 'md 4/8/2003 11:19'!on: aMethod	^self new method: aMethod.	! !InstructionClient subclass: #InstructionPrinter	instanceVariableNames: 'method scanner stream oldPC '	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Methods'!!InstructionPrinter reorganize!('accessing' method method:)('initialize-release' printInstructionsOn:)('instruction decoding' blockReturnTop doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoTemporaryVariable: pushActiveContext pushConstant: pushLiteralVariable: pushReceiver pushReceiverVariable: pushTemporaryVariable: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoTemporaryVariable:)('printing' print:)!InstVarRefLocator removeSelector: #hasInstVarRef:!!InstVarRefLocator reorganize!('initialize-release' interpretNextInstructionUsing:)('instruction decoding' popIntoReceiverVariable: pushReceiverVariable: storeIntoReceiverVariable:)!InstructionClient removeSelector: #interpretNextInstructionUsing:!