'From Squeak3.2gamma of 22 February 2002 [latest update: #4867] on 17 May 2002 at 3:17:29 pm'!"Change Set:		pluginFixesDate:			17 May 2002Author:			Michael Rueger- remove prompt for overwriting the thumbnail on local directories- resource cache multiple entries- set the Mac creator file type for project files"!!FileDirectory methodsFor: 'file operations' stamp: 'mir 11/30/2001 16:35'!upLoadProject: projectFile named: destinationFileName resourceUrl: resUrl retry: aBool	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem.  No retrying for local file systems."	| result |	result _ self putFile: projectFile named: destinationFileName.	self		setMacFileNamed: destinationFileName		type: 'SOBJ'		creator: 'FAST'.	^result! !!Project methodsFor: 'file in/out' stamp: 'mir 11/29/2001 16:37'!writeFileNamed: localFileName fromDirectory: localDirectory toServer: primaryServerDirectory	| local resp gifFileName f |	local _ localDirectory oldFileNamed: localFileName.	resp _ primaryServerDirectory upLoadProject: local named: localFileName resourceUrl: self resourceUrl retry: false.	local close.	resp == true ifFalse: [		"abandon resources that would've been stored with the project"		self resourceManager abandonResourcesThat:			[:loc| loc urlString beginsWith: self resourceUrl].		self inform: 'the primary server of this project seems to be down (',							resp printString,')'. 		^ self	].	gifFileName _ self name,'.gif'.	localDirectory deleteFileNamed: gifFileName ifAbsent: [].	local _ localDirectory fileNamed: gifFileName.	thumbnail ifNil: [		(thumbnail _ Form extent: 100@80) fillColor: Color orange	] ifNotNil: [		thumbnail unhibernate.	].	f _ thumbnail colorReduced.  "minimize depth"	f depth > 8 ifTrue: [		f _ thumbnail asFormOfDepth: 8	].	GIFReadWriter putForm: f onStream: local.	local close.	local _ localDirectory oldFileNamed: gifFileName.	(primaryServerDirectory isKindOf: FileDirectory)		ifTrue: [primaryServerDirectory deleteFileNamed: gifFileName ifAbsent: []].	resp _ primaryServerDirectory putFile: local named: gifFileName retry: false.	local close.	primaryServerDirectory updateProjectInfoFor: self.	primaryServerDirectory sleep.	"if ftp, close the connection"! !!ResourceManager class methodsFor: 'private-resources' stamp: 'mir 11/29/2001 16:19'!addCacheLocation: aString for: urlString	| locations |	locations _ CachedResources at: urlString ifAbsentPut: [#()].	(locations includes: aString)		ifFalse: [CachedResources at: urlString put: ({aString} , locations)]! !