'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5420] on 30 September 2003 at 5:51:30 pm'!"Change Set:		KCP-0109-grabScreenDate:			27 September 2003Author:			stephane ducasseMove grabScreenAndSaveOnDisk from Utilities to GIFReaderWriter"!!GIFReadWriter class methodsFor: 'examples' stamp: 'sd 9/27/2003 19:01'!grabScreenAndSaveOnDisk  	"GIFReaderWriter grabScreenAndSaveOnDisk"	| form fileName |	form _ Form fromUser.	form bits size = 0 ifTrue: [^ self beep].	fileName _ FileDirectory default nextNameFor: 'Squeak' extension: 'gif'.	Utilities informUser: 'Writing ' , fileName		during: [GIFReadWriter putForm: form onFileNamed: fileName].! !!Utilities class methodsFor: 'graphical support' stamp: 'sd 9/27/2003 19:03'!grabScreenAndSaveOnDisk  "Utilities grabScreenAndSaveOnDisk"	| form fileName |	self deprecatedExplanation: 'Use GIFReaderWriter grabScreenAndSaveOnDisk'.	form _ Form fromUser.	form bits size = 0 ifTrue: [^ self beep].	fileName _ FileDirectory default nextNameFor: 'Squeak' extension: 'gif'.	Utilities informUser: 'Writing ' , fileName		during: [GIFReadWriter putForm: form onFileNamed: fileName].! !