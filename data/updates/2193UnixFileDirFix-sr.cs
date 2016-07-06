'From Squeak2.8alpha of 4 February 2000 [latest update: #2158] on 20 May 2000 at 12:26:57 pm'!"Change Set:		070UnixFileDirFix-srDate:			8 May 2000Author:			Stephan RudlofDescription:There is a subtle bug under Unix which is fixed by this changeset.Possible problem:I'm not sure if this is a good manner to fix this problem, or if a deeper refactoring would be better.Under Unix '/' is a real directory, and not just a path delimiter like the corresponding characters/strings in other file systems.The Unix way expressed in terms of other file systems would be:'/' had to be renamed to 'root' and it would be something like 'root/tmp' instead of '/tmp' (this is not a suggestion, just to explain the problem!!).Other opinions?Bug:FileDirectory default fileOrDirectoryExists: '/tmp' trueFileDirectory default fileOrDirectoryExists: '/' falseAfter fix:FileDirectory default fileOrDirectoryExists: '/tmp' trueFileDirectory default fileOrDirectoryExists: '/' true"!!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 12:58'!directoryExists: filenameOrPath	"Handles the special case of testing for the root dir: there isn't a	possibility to express the root dir as full pathname like '/foo'."	^ filenameOrPath = '/' or: [super directoryExists: filenameOrPath]! !!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 13:03'!fileOrDirectoryExists: filenameOrPath 	"Handles the special case of testing for the root dir: there isn't a 	possibility to express the root dir as full pathname like '/foo'."	^ filenameOrPath = '/' or: [super fileOrDirectoryExists: filenameOrPath]! !