'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #348] on 15 November 2004 at 10:28:53 am'!"Change Set:		navFix-mirDate:			15 November 2004Author:			Michael RuegerPreliminary patch for fixing the disappearing navigator button problem (507).As this only seems to happen on publishing where the project info dialog does not show first, there must be some deeper problem."!!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'mir 11/15/2004 10:25'!publishStyle: aSymbol forgetURL: aBoolean withRename: renameBoolean	| w saveOwner primaryServer rename |	w _ self world ifNil: [^Beeper beep].	w setProperty: #SuperSwikiPublishOptions toValue: aSymbol.	primaryServer _ w project primaryServerIfNil: [nil].	rename _ ((primaryServer notNil		and: [primaryServer acceptsUploads]) not)		or: [renameBoolean].	w setProperty: #SuperSwikiRename toValue: rename.	saveOwner _ owner.	self delete.	[w project 		storeOnServerShowProgressOn: self 		forgetURL: aBoolean | rename]		ensure: [saveOwner addMorphFront: self]! !