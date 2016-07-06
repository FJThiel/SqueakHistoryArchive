'From Squeak3.3alpha of 30 January 2002 [latest update: #4744] on 11 February 2002 at 9:36:21 pm'!TestCase subclass: #FileDirectoryTests	instanceVariableNames: ''	classVariableNames: ''	module: #(Squeak Technology Files Tests)!!FileDirectoryTests methodsFor: 'existence tests' stamp: 'hg 2/2/2002 16:45'!testDirectoryExists	self assert: self myAssuredDirectory exists.	self should: [self myDirectory containingDirectory 					directoryExists: self myLocalDirectoryName].	self testDeleteDirectory.	self shouldnt: [self myDirectory containingDirectory 						directoryExists: self myLocalDirectoryName]! !!FileDirectoryTests methodsFor: 'existence tests' stamp: 'hg 2/2/2002 16:44'!testDirectoryNamed	self should: [(self myDirectory containingDirectory 					directoryNamed: self myLocalDirectoryName) pathName 						= self myDirectory pathName]! !!FileDirectoryTests methodsFor: 'existence tests' stamp: 'hg 2/2/2002 16:45'!testExists	self should: [FileDirectory default exists].	self should: [self myAssuredDirectory exists].	self testDeleteDirectory.	self shouldnt: [self myDirectory exists]! !!FileDirectoryTests methodsFor: 'create/delete tests' stamp: 'hg 2/2/2002 16:45'!testDeleteDirectory	self assert: self myAssuredDirectory exists.	self myDirectory containingDirectory deleteDirectory: self myLocalDirectoryName.	self shouldnt: [self myDirectory exists]! !!FileDirectoryTests methodsFor: 'resources' stamp: 'hg 2/2/2002 16:44'!myAssuredDirectory	^self myDirectory assureExistence! !!FileDirectoryTests methodsFor: 'resources' stamp: 'hg 2/2/2002 16:42'!myDirectory	^FileDirectory default directoryNamed: self myLocalDirectoryName! !!FileDirectoryTests methodsFor: 'resources' stamp: 'hg 2/2/2002 16:42'!myLocalDirectoryName	^'zTestDir'! !!FileDirectoryTests methodsFor: 'resources' stamp: 'hg 2/2/2002 19:45'!tearDown	self testDeleteDirectory ! !