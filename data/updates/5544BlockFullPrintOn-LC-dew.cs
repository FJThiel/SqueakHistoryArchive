'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5526] on 11 November 2003 at 1:30:02 am'!"Change Set:		BlockContext-fullPrintOnDate:			6 January 2002Author:			Leandro Caniglia and Doug Waydew: V2, modifies BlockContext>>printOn: as well so that a truncated decompiled block appears in all inspectors, etc.md: Markus Gaelli digged this up from the past. I have not yet looked at it     at all, but I think something like this would be nice to add. ns is Sheldon      Nicholl, see     http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-January/006947.htmlThis change set implements the BlockContext>>fullPrintOn: method. It is especially useful when combined with the fullPrintOnInspector GOODIE, that replaces the use of printString in inspectors with that of fullPrintString.Full credit to 'ns' (don't remember who's she/he?) for the decompileBlock: method."!!BlockContext methodsFor: 'printing' stamp: 'LC 1/6/2002 11:59'!decompile	^ Decompiler new decompileBlock: self! !!BlockContext methodsFor: 'printing' stamp: 'LC 1/6/2002 13:07'!fullPrintOn: aStream	aStream print: self; cr.	(self decompile ifNil: ['--source missing--']) fullPrintOn: aStream! !!BlockContext methodsFor: 'printing' stamp: 'dew 11/11/2003 01:15'!printOn: aStream	| blockString truncatedBlockString |	home == nil ifTrue: [^aStream nextPutAll: 'a BlockContext with home=nil'].	aStream nextPutAll: '[] in '.	super printOn: aStream.	aStream nextPutAll: ' '.	blockString _ ((self decompile ifNil: ['--source missing--']) printString						replaceAll: Character cr with: Character space)							replaceAll: Character tab with: Character space.	truncatedBlockString _ blockString truncateWithElipsisTo: 80.	truncatedBlockString size < blockString size ifTrue:		[truncatedBlockString _ truncatedBlockString, ']}'].	aStream nextPutAll: truncatedBlockString.! !!Decompiler methodsFor: 'public access' stamp: 'LC 1/6/2002 15:50'!decompileBlock: aBlock 	"Original version timestamp: sn 1/26/98 18:27	(Don't know who's sn?) "	"Decompile aBlock, returning the result as a BlockNode.  	Show temp names from source if available."	"Decompiler new decompileBlock: [3 + 4]"	| startpc end homeClass blockNode tempNames home source |	(home _ aBlock home) ifNil: [^ nil].	method _ home method.	(homeClass _ home who first) == #unknown ifTrue: [^ nil].	constructor _ DecompilerConstructor new.	method fileIndex ~~ 0		ifTrue: ["got any source code?"			source _ [method getSourceFromFile]						on: Error						do: [:ex | ^ nil].			tempNames _ ([homeClass compilerClass new						parse: source						in: homeClass						notifying: nil]						on: (Smalltalk classNamed: 'SyntaxErrorNotification')						do: [:ex | ^ nil]) tempNames.			self withTempNames: tempNames].	self initSymbols: homeClass.	startpc _ aBlock startpc.	end _ (method at: startpc - 2)				\\ 16 - 4 * 256				+ (method at: startpc - 1) + startpc - 1.	stack _ OrderedCollection new: method frameSize.	statements _ OrderedCollection new: 20.	super method: method pc: startpc - 5.	blockNode _ self blockTo: end.	stack isEmpty ifFalse: [self error: 'stack not empty'].	^ blockNode statements first! !!MethodContext methodsFor: 'printing' stamp: 'LC 1/6/2002 11:13'!who	| sel mcls |	self method ifNil: [^ Array with: #unknown with: #unknown].	sel _ self receiver class			selectorAtMethod: self method 			setClass: [:c | mcls _ c].	sel == #? ifTrue: [^ self method who].	^ Array with: mcls with: sel! !