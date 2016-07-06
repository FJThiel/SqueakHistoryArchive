'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 27 December 2003 at 2:14:25 pm'!"Change Set:		Dir-isRemoteDirectory-dgdDate:			27 December 2003Author:			Diego Gomez Deck <DiegoGomezDeck@ConsultAr.com>Implementation of FileDirectory>>isRemoteDirectory and ServerDirectory>>isRemoteDirectory and refactoring of sends like '#isKindOf: ServerDirectory' per #isRemoteDirectory.This allows different hierarchies of classes acts like a ServerDirectory"!!FileDirectory methodsFor: 'testing' stamp: 'dgd 12/27/2003 10:46'!isRemoteDirectory	"answer whatever the receiver is a remote directory"	^ false! !!FileList methodsFor: 'initialization' stamp: 'dgd 12/27/2003 12:57'!modelWakeUp	"User has entered or expanded the window -- reopen any remote connection."	(directory notNil and:[directory isRemoteDirectory])		ifTrue: [directory wakeUp] "It would be good to implement a null method wakeUp on the root of directory"! !!FileList methodsFor: 'private' stamp: 'dgd 12/27/2003 12:13'!readContentsBrief: brevityFlag	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."	| f fileSize first5000 |	brevityFlag ifTrue: [		directory isRemoteDirectory ifTrue: [^ self readServerBrief]].	f := directory oldFileOrNoneNamed: self fullName.	f ifNil: [^ 'For some reason, this file cannot be read' translated].	(brevityFlag not or: [(fileSize := f size) <= 100000]) ifTrue:		[contents := f contentsOfEntireFile.		brevityState := #fullFile.   "don't change till actually read"		^ contents].	"if brevityFlag is true, don't display long files when first selected"	first5000 := f next: 5000.	f close.	contents := 'File ''{1}'' is {2} bytes long.You may use the ''get'' command to read the entire file.Here are the first 5000 characters...------------------------------------------{3}------------------------------------------... end of the first 5000 characters.' translated format: {fileName. fileSize. first5000}.	brevityState := #briefFile.   "don't change till actually read"	^ contents.! !!Password methodsFor: 'accessing' stamp: 'dgd 12/27/2003 10:50'!passwordFor: serverDir	"Returned the password from one of many sources.  OK if send in a nil arg."	| sp msg |	cache ifNotNil: [^ cache].	sequence ifNotNil: [		(sp _ self serverPasswords) ifNotNil: [			sequence <= sp size ifTrue: [^ sp at: sequence]]].	msg _ serverDir isRemoteDirectory		ifTrue: [serverDir moniker]		ifFalse: ['this directory'].	(serverDir user = 'anonymous') & (serverDir typeWithDefault == #ftp) ifTrue: [			^ cache _ FillInTheBlank request: 'Please let this anonymous ftp\server know your email address.\This is the polite thing to do.' withCRs			initialAnswer: 'yourName@company.com'].	^ cache _ FillInTheBlank requestPassword: 'Password for ', serverDir user, ' at ', msg, ':'.		"Diff between empty string and abort?"! !!ServerDirectory methodsFor: 'testing' stamp: 'dgd 12/27/2003 10:47'!isRemoteDirectory	"answer whatever the receiver is a remote directory"	^ true! !