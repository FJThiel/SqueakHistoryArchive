'From Squeak3.7alpha of 11 September 2003 [latest update: #5816] on 29 March 2004 at 9:18:45 pm'!"Change Set:		RemoveFlexShellDate:			29 March 2004Author:			Masashi UmezawaTo add a flex shell to a morph, you do:	self addFlexShell.However, to remove the shell, you have to:	self isFlexed		ifTrue: [self owner removeFlexShell].It is asymmetrical, awkward, I think. Now you can:	self removeFlexShell."!!Morph methodsFor: 'rotate scale and flex' stamp: 'mu 3/29/2004 17:33'!removeFlexShell	self isFlexed		ifTrue: [self owner removeFlexShell]! !