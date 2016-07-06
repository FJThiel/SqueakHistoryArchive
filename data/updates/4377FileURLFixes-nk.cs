'From Squeak3.1alpha of 6 February 2001 [latest update: #4347] on 28 September 2001 at 1:12:14 pm'!"Change Set:		FileURLFixes-nkDate:			5 February 2001Author:			Ned KonzFixes some FileURL bugs, so that one slash is prepended instead of two at the appropriate times."!!FileDirectory methodsFor: 'file name utilities' stamp: 'nk 2/2/2001 15:18'!url	"Convert my path into a file:// type url.  Use slash instead of the local delimiter (:), and convert odd characters to %20 notation."	"If slash (/) is not the file system delimiter, encode slashes before converting."	| list |	list _ self pathParts.	^ String streamContents: [:strm |		strm nextPutAll: 'file:'.		list do: [:each | strm nextPut: $/; nextPutAll: each encodeForHTTP].		strm nextPut: $/]! !!FileStream methodsFor: 'file accessing' stamp: 'nk 2/2/2001 15:19'!url	"Convert my path into a file:// type url.  Use slash instead of the local delimiter (:), and convert odd characters to %32 notation."	"If / is not the file system delimiter, encode / before converting."	| list |	list _ self directory pathParts.	^ String streamContents: [:strm |		strm nextPutAll: 'file:'.		list do: [:each | strm nextPut: $/; nextPutAll: each encodeForHTTP].		strm nextPut: $/; nextPutAll: self localName encodeForHTTP]! !!FileUrl methodsFor: 'printing' stamp: 'nk 2/2/2001 16:26'!toText	| s |	s _ WriteStream on: String new.	s nextPutAll: self schemeName.	s nextPut: $:.	s nextPutAll: self pathString.	fragment ifNotNil: [ s nextPut: $#.  s nextPutAll: fragment encodeForHTTP ].	^s contents! !!FileUrl methodsFor: 'access' stamp: 'nk 2/2/2001 16:38'!pathDirString	"Path to directory as url, using slash as delimiter"	^ String streamContents: [ :s |		isAbsolute ifTrue: [ s nextPut: $/ ].		1 to: self path size - 1 do: [ :ii |			s nextPutAll: (path at: ii); nextPut: $/			 ] ]! !!FileUrl methodsFor: 'access' stamp: 'nk 2/2/2001 17:50'!pathForDirectory	"Path using local file system's delimiter.  $\ or $:"	^ String streamContents: [ :s |		isAbsolute ifTrue: [ s nextPut: $/ ].		1 to: self path size - 1 do: [ :ii |			s nextPutAll: (path at: ii); nextPut: FileDirectory default pathNameDelimiter			 ] ]! !