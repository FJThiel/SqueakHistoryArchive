'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #267] on 30 August 2004 at 8:49:32 am'!"Change Set:		CSUniqueNameFix-nkDate:			30 August 2004Author:			Ned KonzAvoids the annoying tendency to add a '1' to the end of change set names on loading when the names are already unique."!!ChangeSet class methodsFor: 'defaults' stamp: 'nk 8/30/2004 08:44'!uniqueNameLike: aString	| try |	(ChangeSorter changeSetNamed: aString) ifNil: [^ aString].	1 to: 999999 do:		[:i | try _ aString , i printString.		(ChangeSorter changeSetNamed: try) ifNil: [^ try]]! !