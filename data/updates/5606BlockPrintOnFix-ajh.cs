'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5595] on 11 December 2003 at 11:24:54 pm'!"Change Set:		BlockPrintOnFix-ajhDate:			11 December 2003Author:			Anthony Hannan, Doug WayRevert CompiledMethod>>longPrintOn:indent:, which had been incorrectly overwritten with an old version."!!CompiledMethod methodsFor: 'printing' stamp: 'ar 6/28/2003 00:08'!longPrintOn: aStream indent: tabs	"List of all the byte codes in a method with a short description of each" 	self isQuick ifTrue: 		[self isReturnSpecial ifTrue:			[^ aStream tab: tabs; nextPutAll: 'Quick return ' , 				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].	self primitive = 0 ifFalse: [		aStream tab: tabs.		self printPrimitiveOn: aStream.	].	(InstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.! !