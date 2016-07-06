'From Squeak3.4alpha of ''11 November 2002'' [latest update: #5109] on 15 November 2002 at 11:57:58 am'!!FileDirectory methodsFor: 'testing' stamp: 'nk 11/15/2002 11:57'!exists"Answer whether the directory exists"	| result |	result _ self primLookupEntryIn: pathName index: 0.	^ result notNil and: [result ~= #badDirectoryPath and: [result fourth]]! !!FileDirectoryTests methodsFor: 'create/delete tests' stamp: 'nk 11/13/2002 19:39'!deleteDirectory		(self myDirectory exists) ifTrue:		[self myDirectory containingDirectory deleteDirectory: self myLocalDirectoryName]! !!FileDirectoryTests methodsFor: 'create/delete tests' stamp: 'nk 11/13/2002 20:11'!testDeleteDirectory	"Test deletion of a directory"		self assert: self myAssuredDirectory exists.	self myDirectory containingDirectory deleteDirectory: self myLocalDirectoryName.	self shouldnt: [self myDirectory exists]! !!FileDirectoryTests methodsFor: 'resources' stamp: 'nk 11/13/2002 19:56'!tearDown	[ self deleteDirectory ] on: Error do: [ :ex | ]! !