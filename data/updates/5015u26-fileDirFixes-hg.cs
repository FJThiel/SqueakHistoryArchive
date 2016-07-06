'From Squeak3.3alpha of 30 January 2002 [latest update: #4744] on 11 February 2002 at 9:36:24 pm'!"Published to 3.3a as 4775fileDirFixes-hg.cs"!!FileDirectory methodsFor: 'testing' stamp: 'hg 2/2/2002 16:09'!directoryExists: filenameOrPath	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."	"FileDirectory default directoryExists: FileDirectory default pathName"	| fName dir |	FileDirectory splitName: filenameOrPath to:		[:filePath :name |			fName _ name.			filePath isEmpty				ifTrue: [dir _ self]				ifFalse: [dir _ FileDirectory on: filePath]].	^dir exists and: [		self isCaseSensitive 			ifTrue:[dir directoryNames includes: fName]			ifFalse:[dir directoryNames anySatisfy: [:name| name sameAs: fName]]].! !!FileDirectory methodsFor: 'testing' stamp: 'hg 2/2/2002 17:00'!exists	| result |	result _ self primLookupEntryIn: pathName index: 0.	^result notNil and: [result ~= #badDirectoryPath]! !!FileDirectory methodsFor: 'file directory' stamp: 'hg 2/2/2002 16:37'!assureExistence	"Make sure the current directory exists. If necessary, create all parts in between"	self containingDirectory assureExistenceOfPath: self localName! !