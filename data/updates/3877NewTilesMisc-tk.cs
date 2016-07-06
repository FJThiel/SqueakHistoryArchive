'From Squeak3.1alpha of 28 February 2001 [latest update: #3888] on 29 March 2001 at 12:54:16 pm'!"Change Set:		NewTilesMisc-tkDate:			10 March 2001Author:			Ted KaehlerDo keep ^ self when it is the only line in the script.Allow dropping on Return tiles.Change comment in typeCheckingInTileScripting to reflect the fact that it is only for testing.Fix bug in Preferences>>valueOfFlag:ifAbsent: that caused an error when dragging a tile in Universal Tiles.Don't unhibernate a scriptEditor when it is just part of an If-Then clause.Inserted extra CR in transcript when putting updates.  Names used to run together."!!Preferences class methodsFor: 'preferences dictionary' stamp: 'tk 3/29/2001 12:39'!valueOfFlag: aFlagName ifAbsent: booleanValuedBlock	"Answer the value of the given flag"	^ (self isProjectPreference: aFlagName)		ifTrue:			[Project current projectPreferenceAt: aFlagName ifAbsent: booleanValuedBlock]		ifFalse:			[FlagDictionary at: aFlagName ifAbsent: booleanValuedBlock]! !!Preferences class methodsFor: 'initial values' stamp: 'tk 3/15/2001 14:58'!initialValuesAdditionscriptingtypeCheckingInTileScripting	^ #(#(#typeCheckingInTileScripting #true #(#scripting) 'Testing only.  Simulates EToyFriendly by applying the EToy viewer''s type checks when dropping tiles in universal tiles.  Per Project.') )! !!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 3/16/2001 11:35'!unhibernate	"Recreate my tiles from my method if I have new universal tiles."	self world		ifNil:			[(playerScripted == nil or: [playerScripted isUniversalTiles not]) ifTrue: [^ self]]		ifNotNil:			[Preferences universalTiles ifFalse: [^ self]].	self topEditor == self ifFalse: [^ self].		"Part of a compound test"	self insertUniversalTiles. 	self showingMethodPane: false."no longer needed as it will already be set up"	"self hResizing: #shrinkWrap;				vResizing: #shrinkWrap;				cellPositioning: #topLeft."! !!ServerDirectory methodsFor: 'updates' stamp: 'tk 3/16/2001 11:15'!putUpdate: fileStrm 	"Put this file out as an Update on the servers of my group.  Each version of the system has its own set of update files.  'updates.list' holds the master list.  Each update is a fileIn whose name begins with a number.  See Utilities class readServerUpdatesThrough:saveLocally:updateImage:."	| myServers updateStrm sequence newName myName response local restOfText seq fileContents |"	(ScheduledControllers scheduledControllers detect: [:each |		each model == Transcript] ifNone: [nil]) ifNil: [			^ self inform: 'Please open a Transcript window, and then start putting out this update again.']."	local _ fileStrm localName.	fileStrm size = 0		ifTrue: [^ self inform: 'That file has zero bytes!!  May have a new name.'].	fileContents _ fileStrm contentsOfEntireFile.	(fileContents includes: Character linefeed)		ifTrue: [self notify: 'That file contains linefeeds.Proceed if you know that this is okay (e.g. the file contains raw binary data).'].	fileStrm reset.	(self checkNames: (Array with: local)) ifFalse: [^ nil].	"illegal characters"	myName _ group ifNil: [self moniker] ifNotNil: [group key].	response _ (PopUpMenu labels: 'Install update\Cancel update' withCRs)		startUpWithCaption: 'Do you really want to broadcast the file ', local, 			'\to every Squeak user who updates from ' withCRs, myName, '?'.	response = 1 ifFalse: [^ nil].	"abort"	self openGroup.	(myServers _ self checkServers) size = 0 ifTrue: [self closeGroup.  ^ self].	updateStrm _ myServers first getFileNamed: 'updates.list'.	sequence _ Utilities lastUpdateNum: updateStrm.	restOfText _ Utilities position: updateStrm 	"sets the postion!!!!"			atVersion: (Smalltalk at: #EToySystem) version.	restOfText size > 0 ifTrue: [		response _ (PopUpMenu labels: 'Make update for my older version\Cancel update' withCRs)			startUpWithCaption: 'This system, ', (Smalltalk at: #EToySystem) version,				' is not the latest version'.		response = 1 ifFalse: [self closeGroup.  ^ nil].	"abort"		sequence _ Utilities olderVersNum: restOfText from: updateStrm default: sequence.		].	"get last number and add 1"	seq _ (sequence+1) printString.	seq size = 1 ifTrue: [seq _ '00', seq].	seq size = 2 ifTrue: [seq _ '0', seq].	newName _ seq, local.	"append name to updates"	(updateStrm skip: -1; next) == Character cr ifFalse: [		updateStrm nextPut: Character cr].	updateStrm nextPutAll: newName; nextPut: Character cr; nextPutAll: restOfText.	myServers do:		[:aServer |		fileStrm reset.	"reopen"		aServer putFile: fileStrm named: newName retry: true.		updateStrm reset.		aServer putFileSavingOldVersion: updateStrm named: 'updates.list'.		Transcript cr; show: 'Update succeeded on server ', aServer moniker; cr].	self closeGroup.			Transcript cr; show: 'Be sure to test your new update!!'; cr.	"rename the file locally (may fail)"	fileStrm directory rename: local toBe: newName.! !!SyntaxMorph methodsFor: 'dropping/grabbing' stamp: 'tk 3/15/2001 13:47'!acceptDroppingMorph: aMorph event: evt	| itNoun old |	"Two cases: 1) a phrase being dropped into a block.  Add a new line.		2) aMorph is replacing self by dropping on it.	For the moment, you have to drop it the right place (the end of a tile if it is complex).  We do not look at enclosing morphs"	itNoun _ aMorph isNoun.	self withAllOwnersDo:		[:m | (m isSyntaxMorph and: [m isBlockNode])				ifTrue: [m stopStepping; removeDropZones]].	self isBlockNode & itNoun ifTrue:		[(aMorph nodeClassIs: TempVariableNode) 			ifTrue:				["If I am a BlockNode, and it is a TempVariableNode, add it into list"				^ (self addBlockArg: aMorph) ifFalse:					["if already declared, start new line of code with it"					self addToBlock: aMorph event: evt]]				ifFalse: ["If I am a BlockNode and it is a noun add it as a new line"				^ self addToBlock: aMorph event: evt]].					self isBlockNode ifTrue: [		 (aMorph nodeClassIs: ReturnNode) ifTrue: [^ self addToBlock: aMorph event: evt]].	"Later add args and keywords.  later allow comments to be dropped"	"Can't put statement, literal, assignment, or cascade into left side of assignment"	(owner isSyntaxMorph) ifTrue:		[(owner nodeClassIs: AssignmentNode) ifTrue:			[(owner submorphIndexOf: self) = 1 ifTrue:				[((aMorph nodeClassIs: TempVariableNode)				or: [aMorph nodeClassIs: VariableNode])  ifFalse: [ ^ self]]]].	aMorph deselect.	(old _ owner) replaceSubmorph: self by: aMorph.	"do the normal replacement"	(old isSyntaxMorph) ifTrue: [old cleanupAfterItDroppedOnMe].	"now owned by no one"! !!SyntaxMorph methodsFor: 'dropping/grabbing' stamp: 'tk 3/10/2001 19:13'!structureMatchWith: aMorph	| meNoun itNoun |	"Return true if the node types would allow aMorph to replace me.  This tests the gross structure of the method only."	"If nodes are of equal class, replace me with new one."	(self nodeClassIs: MessageNode) ifFalse: [		(self nodeClassIs: aMorph parseNode class) ifTrue: [^ true]].	meNoun _ self isNoun.	itNoun _ aMorph isNoun.	"Consider these nouns to be equal:  TempVariableNode, LiteralNode, VariableNode, (MessageNode with receiver), CascadeNode, AssignmentNode"	meNoun & itNoun ifTrue: [^ true].	meNoun & aMorph isBlockNode ifTrue: [^ true].	"If I am a BlockNode, and it is a TempVariableNode, add it into list"	"If I am a BlockNode, and it is a noun, add it as a new line"	self isBlockNode ifTrue:		[itNoun ifTrue: [^ true].		(aMorph nodeClassIs: ReturnNode) ifTrue:			[^ (self submorphs				detect: [:mm | ((mm isSyntaxMorph) and: [mm nodeClassIs: ReturnNode])]				ifNone: [nil]) isNil]].	"none already in this block"				"If I am a BlockNode, and it is a ReturnNode, add to end"	(self isMethodNode) ifTrue: [^ false].	"Later add args and keywords"		"Later allow comments to be dropped in"		"Add MethodTemps by dropping into the main block"	(self nodeClassIs: ReturnNode) & (aMorph parseNode class == MessageNode) 		ifTrue: [^ true].		"Command replace Return"	(self nodeClassIs: MessageNode) & (aMorph parseNode class == ReturnNode) ifTrue: [		(owner submorphs select: [:ss | ss isSyntaxMorph]) last == self			ifTrue: [^ true]].	"Return replace last command"	^ false "otherwise reject"! !!SyntaxMorph methodsFor: 'layout' stamp: 'tk 3/10/2001 18:52'!removeReturnNode	| blk |	"If last line is ^ self, remove it.  I am a methodNode.  Keep if no other tiles in the block."	blk _ submorphs last.	blk submorphs last decompile string = '^self ' ifTrue: [		(blk submorphs count: [:ss | ss isSyntaxMorph]) > 1 ifTrue: [			blk submorphs last delete]].! !"Postscript:Make the preference #typeCheckingInTileScripting be per-project."Preferences flagsHeldByProjects at: #typeCheckingInTileScripting put: false.		"default value. makes it per project" !