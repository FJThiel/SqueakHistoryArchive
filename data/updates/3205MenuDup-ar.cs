'From Squeak2.9alpha of 13 June 2000 [latest update: #3268] on 15 January 2001 at 8:46:16 pm'!"Change Set:		MenuDup-arDate:			15 January 2001Author:			Andreas RaabFixes the duplication of menu items."!!MenuItemMorph methodsFor: 'grabbing' stamp: 'ar 1/15/2001 20:44'!duplicateMorph: evt	"Make and return a duplicate of the receiver's argument"	| dup menu |	dup _ self duplicate.	menu _ MenuMorph new defaultTarget: nil.	menu addMorphFront: dup.	menu bounds: self bounds.	menu stayUp: true.	evt hand grabMorph: menu from: owner. "duplicate was ownerless so use #grabMorph:from: here"	^menu! !