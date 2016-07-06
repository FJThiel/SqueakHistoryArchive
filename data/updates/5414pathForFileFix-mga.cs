'From Squeak3.6beta of ''4 July 2003'' [latest update: #5387] on 6 August 2003 at 2:52:39 pm'!"Change Set:		pathForFileFixDate:			6 August 2003Author:			Markus Gaelli'file:///foo/bar' asUrl gives an absolute url.If one asks this for its pathForFile,this path should be prefixed by thepathDelimiter of the OS. Here is the fix."!!FileUrl methodsFor: 'access' stamp: 'mga 8/6/2003 11:30'!pathForFile	"Path using local file system's delimiter.  $\ or $:"	| first |	^String streamContents: [ :s |		first _ self isAbsolute.		self path do: [ :p |			first ifTrue: [ s nextPut: FileDirectory default pathNameDelimiter ].			first _ true.			s nextPutAll: p ] ]! !