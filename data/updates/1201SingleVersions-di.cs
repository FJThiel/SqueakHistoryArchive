'From Squeak 2.4b of April 23, 1999 on 10 May 1999 at 4:26:43 pm'!"Change Set:		SingleVersionsDate:			10 May 1999Author:			Dan IngallsReverts a jarring 'convenience' added to the browse versions command.Once again it behaves consistently, and you can too."!!ChangeList class methodsFor: 'public access' stamp: 'di 5/10/1999 16:22'!browseVersionsOf: method class: class meta: meta category: category selector: selector lostMethodPointer: sourcePointer 	| changeList |	Cursor read showWhile:		[changeList _ self new			scanVersionsOf: method class: class meta: meta			category: category selector: selector].	changeList ifNil: [self inform: 'No versions available'].	sourcePointer ifNotNil:		[changeList setLostMethodPointer: sourcePointer].	self open: changeList name: 'Recent versions of ' ,selector multiSelect: false! !