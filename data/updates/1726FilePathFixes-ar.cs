'From Squeak2.6 of 13 October 1999 [latest update: #1724] on 18 December 1999 at 1:02:42 am'!"Change Set:		FilePathFixesDate:			18 December 1999Author:			Andreas RaabFixes some of the confusion with returned values from #fullPathFor:. Should now consistently return a fully qualified path even for relative path names."!!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 01:01'!fullPathFor: path	^path isEmpty ifTrue:[pathName] ifFalse:[path]! !!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 00:36'!slash	^self class slash! !!FileDirectory methodsFor: 'file name utilities' stamp: 'ar 12/18/1999 00:55'!fullNameFor: fileName	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."	"Details: Note that path relative to a directory, such as '../../foo' are disallowed by this algorithm."	| correctedLocalName prefix |	self class splitName: fileName to:		[:filePath :localName |			correctedLocalName _ self checkName: localName fixErrors: true.			prefix _ self fullPathFor: filePath].	prefix last = self pathNameDelimiter		ifTrue:[^ prefix, correctedLocalName]		ifFalse:[^ prefix, self slash, correctedLocalName]! !!AcornFileDirectory methodsFor: 'file name utilities' stamp: 'ar 12/18/1999 00:47'!fullPathFor: path	path isEmpty ifTrue:[^pathName].	((path includes: $$ ) or:[path includes: $:]) ifTrue:[^path].	^pathName, self slash, path! !!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'ar 12/18/1999 00:52'!fullNameFor: fileName	"Return the fully-qualified path name for the given file. Correct syntax errors in the file name."	(fileName size = 2 and: [fileName first isLetter and: [fileName last = $:]])		ifTrue: [^ fileName].	^super fullNameFor: fileName! !!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'ar 12/18/1999 00:52'!fullPathFor: path	"Return the fully-qualified path name for the given file."	path isEmpty ifTrue:[^pathName].	(path at: 1) = $\ ifTrue:[		(path size >= 2 and:[(path at: 2) = $\]) ifTrue:[^path]. "e.g., \\pipe\"		^(pathName copyFrom: 1 to: 2), path "e.g., \windows\"].	(path size >= 2 and:[(path at: 2) = $: and:[path first isLetter]])		ifTrue:[^path]. "e.g., c:"	^pathName, self slash, path! !!UnixFileDirectory methodsFor: 'file names' stamp: 'ar 12/18/1999 00:46'!fullPathFor: path	"Return the fully-qualified path name for the given file."	path isEmpty ifTrue:[^pathName].	path first = $/ ifTrue:[^path].	^pathName, self slash, path! !AcornFileDirectory removeSelector: #fullNameFor:!UnixFileDirectory removeSelector: #fullNameFor:!