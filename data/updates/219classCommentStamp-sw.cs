'From Squeak 2.1 of June 30, 1998 on 8 September 1998 at 5:09:18 pm'!"Change Set:		classCommentStamp-swDate:			8 September 1998Author:			Scott WallaceFinally fixes the family of glitches that resulted in use of the current user's initials and the current date/time as the comment stamp for any class comment filed in or filed out.  This implementation is not ideal (fixing up Ted's nascent ClassCommentReader, from dec 97 would be better) but it is expedient; the class organizer now holds on to the comment stamp, and the right thing is put out onto fileouts, and on fileins there is no longer that absurd prompt for the current authors's initials, etc.  Still missing however are:	(1)	a way to SEE what the comment stamp is	(2) 	a 'versions' mechanisms for class comments.Refer to the class comment in ClassOrganizer"!Object subclass: #ClassOrganizer	instanceVariableNames: 'globalComment categoryArray categoryStops elementArray commentStamp '	classVariableNames: 'Default NullCategory '	poolDictionaries: ''	category: 'Kernel-Classes'!!Browser methodsFor: 'accessing' stamp: 'sw 9/2/1998 14:13'!contents: input notifying: aController 	"The retrieved information has changed and its source must now be 	updated. The information can be a variety of things, depending on the 	list selections (such as templates for class or message definition, methods) 	or the user menu commands (such as definition, comment, hierarchy). 	Answer the result of updating the source."	| aString aText theClass |	aString _ input asString.	aText _ input asText.	editSelection == #editSystemCategories 		ifTrue: [^ self changeSystemCategories: aString].	editSelection == #editClass | (editSelection == #newClass) 		ifTrue: [^ self defineClass: aString notifying: aController].	editSelection == #editComment 		ifTrue: [theClass _ self selectedClass.				theClass ifNil: [PopUpMenu notify: 'You must select a classbefore giving it a comment.'.				^ false].				theClass comment: aText stamp: Utilities changeStamp. ^ true].	editSelection == #hierarchy ifTrue: [^ true].	editSelection == #editMessageCategories 		ifTrue: [^ self changeMessageCategories: aString].	editSelection == #editMessage | (editSelection == #newMessage) 		ifTrue: [^ self defineMessage: aText notifying: aController].	editSelection == #none		ifTrue: [PopUpMenu notify: 'This text cannot be acceptedin this part of the browser.'.				^ false].	self error: 'unacceptable accept'! !!ChangeRecord methodsFor: 'access' stamp: 'sw 8/24/1998 08:16'!fileIn	| methodClass |	Cursor read showWhile:		[(methodClass _ self methodClass) notNil ifTrue:			[methodClass compile: self text classified: category withStamp: stamp notifying: nil].		(type == #doIt) ifTrue:			[Compiler evaluate: self string].		(type == #classComment) ifTrue:			[(Smalltalk at: class asSymbol) comment: self text stamp: stamp]]! !!ChangeSet methodsFor: 'private' stamp: 'sw 9/2/1998 14:24'!fileOutClassModifications: class on: stream 	"Write out class mod-- rename, comment, reorg, remove, on the given stream.  Differs from the superseded fileOutClassChanges:on: in that it does not deal with class definitions, and does not file out entire added classes."	| aClass |	(self atClass: class includes: #rename) ifTrue:		[stream nextChunkPut: 'Smalltalk renameClassNamed: #', (self oldNameFor: class), ' as: #', class name; cr].	(self atClass: class includes: #comment) ifTrue:		[(aClass _ class theNonMetaClass) organization putCommentOnFile: stream numbered: 0 moveSource: false forClass: aClass].	(self atClass: class includes: #reorganize) ifTrue:		[class fileOutOrganizationOn: stream.		stream cr]! !!ClassDescription methodsFor: 'accessing' stamp: 'sw 9/2/1998 14:28'!comment	"Answer the receiver's comment. (If old format, not a Text, unpack the old way.) "	| aString |	aString _ self theNonMetaClass organization classComment.	(aString asString beginsWith: self name, ' comment:\''' withCRs) 		ifFalse: [^ aString]		ifTrue: ["old format"			aString size = 0 ifTrue: [^ ''].			"get string only of classComment, undoubling quotes"			^ String readFromString: aString]! !!ClassDescription methodsFor: 'accessing' stamp: 'sw 9/8/1998 14:43'!comment: aStringOrText stamp: aStamp	"Set the receiver's comment to be the argument, aStringOrText."	self theNonMetaClass classComment: aStringOrText stamp: aStamp.	Smalltalk changes commentClass: self theNonMetaClass! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'sw 9/8/1998 14:44'!classComment: aString	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."	^ self classComment: aString stamp: '<historical>'! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'sw 8/24/1998 12:30'!classComment: aString stamp: aStamp	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."	| ptr header file oldCommentRemoteStr |	(aString isKindOf: RemoteString) ifTrue: [^ organization classComment: aString].	oldCommentRemoteStr _ organization commentRemoteStr.	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ organization classComment: nil].		"never had a class comment, no need to write empty string out"	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].	SourceFiles ifNotNil: [(file _ SourceFiles at: 2) ifNotNil: [		file setToEnd; cr; nextPut: $!!.	"directly"		"Should be saying (file command: 'H3') for HTML, but ignoring it here"		header _ String streamContents: [:strm | strm nextPutAll: self name;			nextPutAll: ' commentStamp: '.			aStamp storeOn: strm.			strm nextPutAll: ' prior: '; nextPutAll: ptr printString].		file nextChunkPut: header]].	aStamp size > 0 ifTrue: [self commentStamp: aStamp].	organization classComment: (RemoteString newString: aString onFileNumber: 2).	Smalltalk changes commentClass: self.	! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'sw 9/2/1998 14:22'!commentStamp: changeStamp	self organization commentStamp: changeStamp.    ^ self commentStamp: changeStamp prior: 0! !!ClassOrganizer methodsFor: 'accessing' stamp: 'sw 8/24/1998 12:29'!commentStamp	^ commentStamp! !!ClassOrganizer methodsFor: 'accessing' stamp: 'sw 8/24/1998 12:29'!commentStamp: aStamp	commentStamp _ aStamp! !!ClassOrganizer methodsFor: 'fileIn/Out' stamp: 'sw 8/24/1998 12:33'!putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass	"Store the comment about the class onto file, aFileStream."	| header |	globalComment ifNotNil:		[aFileStream cr; nextPut: $!!.		header _ String streamContents: [:strm | 				strm nextPutAll: aClass name;				nextPutAll: ' commentStamp: '.				commentStamp ifNil: [commentStamp _ '<historical>'].				commentStamp storeOn: strm.				strm nextPutAll: ' prior: '; nextPutAll: '0'].		aFileStream nextChunkPut: header.		aClass organization fileOutCommentOn: aFileStream				moveSource: moveSource toFile: sourceIndex.		aFileStream cr]! !!ClassOrganizer commentStamp: 'sw 9/8/1998 16:45' prior: 0!I represent method categorization information for classes.  The handling of class comments has gone through a tortuous evolution.   Grandfathered class comments (before late aug 98) have no time stamps, and historically, fileouts of class comments always substituted the timestamp reflecting the author and date/time at the moment of fileout; and historically any timestamps in a filed out class comment were dropped on the floor, with the author & time prevailing at the moment of filein being substituted.   Such grandfathered comments now go out on fileouts with '<historical>' timestamp; class comments created after the 8/98 changes will have their correct timestamps preserved, though there is not yet a decent ui for reading those stamps other than filing out and looking at the file; nor is there yet any ui for browsing and recovering past versions of such comments.  Everything in good time!!!