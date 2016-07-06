'From Squeak3.7alpha of 11 September 2003 [latest update: #5816] on 24 March 2004 at 3:54:11 pm'!"Change Set:		DefaultChangeSetDirFix-nkDate:			24 March 2004Author:			Ned KonzIf you try to set the default change set directory to the full name of the default directory, you get a walkback.Also, if you try to set it to a directory whose name begins with the name of the default directory but is not a subdirectory, the relative path is calculated wrong.This changeset fixes both of those problems."!!ChangeSet class methodsFor: 'defaults' stamp: 'nk 3/24/2004 15:52'!defaultChangeSetDirectory: dirOrName 	"Set the Preference for storing change sets to the given directory or name (possibly relative).	Rewrite directory names below the default directory as relative names.	If dirOrName is an empty string, use the default directory."	"ChangeSet defaultChangeSetDirectory: 'changeSets'"	| dirName defaultFullName |	dirName := dirOrName isString				ifTrue: [FileDirectory default fullNameFor: dirOrName]				ifFalse: [dirOrName fullName].	defaultFullName := FileDirectory default fullName.	dirName = defaultFullName		ifTrue: [dirName := '']		ifFalse: [(dirName beginsWith: defaultFullName , FileDirectory slash)				ifTrue: [dirName := dirName copyFrom: defaultFullName size + 2 to: dirName size]].	Preferences setParameter: #defaultChangeSetDirectoryName to: dirName! !