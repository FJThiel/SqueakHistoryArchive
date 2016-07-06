'From Squeak2.9alpha of 17 July 2000 [latest update: #2666] on 21 September 2000 at 3:51:40 pm'!"Change Set:		kidRenameDate:			21 September 2000Author:			Bob Arning- adds a prompt to rename the Project before Publishing if the name is bad or 'Unnamed...'"!AlignmentMorphBob1 subclass: #EToyProjectRenamerMorph	instanceVariableNames: 'theName theNameMorph actionBlock '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Experimental'!!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:01'!buttonColor	^color darker! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:06'!buttonNamed: aString action: aSymbol color: aColor help: helpString	| f col |	f _ SimpleButtonMorph new		target: self;		label: aString font: self myFont;		color: aColor;		actionSelector: aSymbol;		setBalloonText: helpString.	col _ (self inAColumn: {f}) hResizing: #spaceFill.	^col! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:05'!cancelButton	^self		buttonNamed: 'Cancel' 		action: #doCancel 		color: self buttonColor 		help: 'Cancel this Publish operation.'! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:06'!doCancel	self delete.! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:45'!doOK	| proposed |	proposed _ theNameMorph contents string.	proposed size = 0 ifTrue: [^self inform: 'I do need a name for the project'].	proposed size > 24 ifTrue: [^self inform: 'Please make the name 24 characters or less'].	(Project isBadNameForStoring: proposed) ifTrue: [		^self inform: 'Please remove any funny characters'	].	(ChangeSorter changeSetNamed: proposed) ifNotNil: [		^ Utilities inform: 'Sorry that name is already used'	].	self delete.	actionBlock value: proposed.! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 14:59'!initialize	super initialize.	vResizing _ hResizing _ #shrinkWrap.	color _ Color paleYellow.	borderWidth _ 8.	borderColor _ color darker.	inset _ 4.	self useRoundedCorners.	self rebuild.! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:04'!myFont	^(TextStyle named: #ComicBold) fontOfSize: 16! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:40'!name: aString actionBlock: aBlock	theName _ aString.	actionBlock _ aBlock.	theNameMorph contentsWrapped: theName.! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:21'!okButton	^self		buttonNamed: 'OK' 		action: #doOK 		color: self buttonColor 		help: 'Change my name and continue publishing.'! !!EToyProjectRenamerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:49'!rebuild	self removeAllMorphs.	self addARow: {		(StringMorph contents: 'Please name this project' font: self myFont) lock.	}.	(self addARow: {		(theNameMorph _ TextMorph new			beAllFont: self myFont;			extent: 300@20;			contentsWrapped: 'the old name';			setBalloonText: 'Pick a name 24 characters or less and avoid the following characters: : < > | / \ ? * "'			).	}) color: Color white; borderColor: Color black; borderWidth: 1.	self addARow: {		self okButton.		self cancelButton.	}.! !!EToyProjectRenamerMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:21'!test	| me |"EToyProjectRenamerMorph test"	(me _ self new)		name: 'what it was' 		actionBlock: [ :newName | self inform: 'The name chosen was: "',newName,'"'];		fullBounds;		position: Display extent - me extent // 2;		openInWorld! !!EToyProjectRenamerMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 9/21/2000 15:46'!validate: aString andDo: aBlock	| me |	(me _ self new)		name: aString actionBlock: aBlock;		fullBounds;		position: Display extent - me extent // 2;		openInWorld! !!Project methodsFor: 'file in/out' stamp: 'RAA 9/21/2000 15:30'!hasBadNameForStoring	^Project isBadNameForStoring: self name! !!Project methodsFor: 'file in/out' stamp: 'RAA 9/21/2000 15:34'!storeOnServer	"Save to disk as an Export Segment.  Then put that file on the server I came from, as a new version.  Version is literal piece of file name.  Mime encoded and http encoded."	world setProperty: #optimumExtentFromAuthor toValue: world extent.	self validateProjectNameIfOK: [		self isCurrentProject ifTrue: ["exit, then do the command"			^ self armsLengthCommand: #storeOnServer withDescription: 'Publishing'		].		self storeOnServerWithProgressInfo.	].! !!Project methodsFor: 'file in/out' stamp: 'RAA 9/21/2000 15:34'!storeOnServerShowProgressOn: aMorphOrNil	"Save to disk as an Export Segment.  Then put that file on the server I came from, as a new version.  Version is literal piece of file name.  Mime encoded and http encoded."	world setProperty: #optimumExtentFromAuthor toValue: world extent.	self validateProjectNameIfOK: [		self isCurrentProject ifTrue: ["exit, then do the command"			^ self armsLengthCommand: #storeOnServer withDescription: 'Publishing'		].		self storeOnServerWithProgressInfoOn: aMorphOrNil.	].! !!Project methodsFor: 'file in/out' stamp: 'RAA 9/21/2000 15:44'!validateProjectNameIfOK: aBlock	(self hasBadNameForStoring or: [self name beginsWith: 'Unnamed']) ifTrue: [		EToyProjectRenamerMorph			validate: self name			andDo: [ :newName | 				newName = changeSet name ifFalse: [changeSet name: newName].				aBlock value.			]	] ifFalse: [		aBlock value	]! !!Project class methodsFor: 'squeaklet on server' stamp: 'RAA 9/21/2000 15:29'!isBadNameForStoring: aString	| badChars |	"will the name of this project cause problems when stored on an arbitrary file system?"	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.	^aString size > 24 or: [aString anySatisfy: [ :each | badChars includes: each]]! !Project removeSelector: #validateProjectName!