'From Squeak2.9alpha of 17 July 2000 [latest update: #3206] on 26 December 2000 at 2:32:21 pm'!"Change Set:		fixPartsBinDate:			26 December 2000Author:			Bob ArningFix alignment problem when creating new standard parts bin"!!BookMorph methodsFor: 'other' stamp: 'RAA 12/26/2000 14:31'!wrappedInPartsWindowWithTitle: aTitle	| aWindow |	self fullBounds.	aWindow _ (PartsWindow labelled: aTitle) model: Model new.	aWindow book: self.	^ aWindow! !!PartsWindow methodsFor: 'as yet unclassified' stamp: 'RAA 12/26/2000 14:31'!book: aBook	book _ aBook.	self addMorph: aBook frame: (0@0 extent: 1@1).	book beSticky.	self extent: aBook extent + (0@self labelHeight).	nextButton target: aBook.	prevButton target: aBook! !