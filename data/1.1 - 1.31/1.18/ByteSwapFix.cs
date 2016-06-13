'From Squeak 1.18 of December 12, 1996 on 14 December 1996 at 7:35:39 am'!

"Change Set:		ByteSwapFix
Date:			14 December 1996
Author:			John Maloney

Fixes an omission in the bytecode swapping code that
supports image portability between machines. Based
on the design by Ian Piumarta. Tested by reading a
windows image on a Mac."!

!Interpreter methodsFor: 'image save/restore'!
byteSwapByteObjects
	"Byte-swap the words of all bytes objects in the image, including Strings, ByteArrays, and CompiledMethods. This returns these objects to their original byte ordering after blindly byte-swapping the entire image. For compiled methods, byte-swap only their bytecodes part."

	| oop fmt wordAddr methodHeader end |
	oop _ self firstObject.
	[oop < endOfMemory] whileTrue: [
		(self isFreeObject: oop) ifFalse: [
			fmt _ self formatOf: oop.
			fmt >= 8 ifTrue: [  "oop contains bytes"
				wordAddr _ oop + BaseHeaderSize.
				fmt >= 12 ifTrue: [
					"compiled method; start after methodHeader and literals"
					methodHeader _ self longAt: oop + BaseHeaderSize.
					wordAddr _ wordAddr + 4 + (((methodHeader >> 10) bitAnd: 16rFF) * 4).
				].
				end _ oop + (self sizeBitsOf: oop).
				[wordAddr < end] whileTrue: [
					self longAt: wordAddr put: (self byteSwapped: (self longAt: wordAddr)).
					wordAddr _ wordAddr + 4.
				].
			].
 		].
		oop _ self objectAfter: oop.
	].
!
reverseBytesInImage
	"Byte-swap all words in memory after reading in the entire image file with bulk read. Contributed by Tim Rowledge."

	| addr lastAddr |
	"First, byte-swap every word in the image. This fixes objects headers."
	addr _ self startOfMemory.
	lastAddr _ endOfMemory.
	[addr < lastAddr] whileTrue: [
		self longAt: addr put: (self byteSwapped: (self longAt: addr)).
		addr _ addr + 4.
	].

	"Second, return the bytes of bytes-type objects to their orginal order."
	self byteSwapByteObjects.! !

