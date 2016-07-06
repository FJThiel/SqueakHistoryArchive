'From Squeak3.6beta of ''4 July 2003'' [latest update: #5373] on 2 August 2003 at 7:22:34 pm'!"Change Set:		BitmapByteAtFix-arDate:			2 August 2003Author:			Andreas RaabFix a bug in Bitmap's #byteAt:put: implementation."!!Bitmap methodsFor: 'accessing' stamp: 'ar 9/21/2001 23:06'!byteAt: byteAddress put: byte	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form pixelAt:put:  7/1/96 tk"	| longWord shift lowBits longAddr |	(byte < 0 or:[byte > 255]) ifTrue:[^self errorImproperStore].	lowBits _ byteAddress - 1 bitAnd: 3.	longWord _ self at: (longAddr _ (byteAddress - 1 - lowBits) // 4 + 1).	shift _ (3 - lowBits) * 8.	longWord _ longWord - (longWord bitAnd: (16rFF bitShift: shift)) 		+ (byte bitShift: shift).	self at: longAddr put: longWord.	^ byte! !