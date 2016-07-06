'From Squeak 2.5 of August 6, 1999 on 3 September 1999 at 1:09:21 pm'!"Change Set:		LfSubmissions-arDate:			3 September 1999Author:			Andreas RaabFixes the LF test for updates which contain binary data. Also adds a file list entry for removing LFs from a file."!!FileList methodsFor: 'file list menu' stamp: 'ar 9/3/1999 13:04'!itemsForFileEnding: suffix	| labels lines selectors |	labels _ OrderedCollection new.	lines _ OrderedCollection new.	selectors _ OrderedCollection new.	(suffix = 'bmp') | (suffix = 'gif') | (suffix = 'jpg') | (suffix = 'form') | (suffix = '*') ifTrue:		[labels addAll: #('open image in a window' 'read image into ImageImports'						 'open image as background').		selectors addAll: #(openImageInWindow importImage openAsBackground)].	(suffix = 'morph') | (suffix = 'morphs') | (suffix = 'sp') | (suffix = '*') ifTrue:		[labels add: 'load as morph'.		selectors add: #openMorphFromFile.		labels add: 'load as project'.		selectors add: #openProjectFromFile].	(suffix = 'bo') | (suffix = '*') ifTrue:[		labels add: 'load as book'.		selectors add: #openBookFromFile].	(suffix = 'mid') | (suffix = '*') ifTrue:		[labels add: 'play midi file'.		selectors add: #playMidiFile].	(suffix = 'movie') | (suffix = '*') ifTrue:		[labels add: 'open as movie'.		selectors add: #openAsMovie].	(suffix = 'st') | (suffix = 'cs') | (suffix = '*') ifTrue:		[suffix = '*' ifTrue: [lines add: labels size].		labels addAll: #('fileIn' 'file into new change set' 'browse changes' 'browse code' 'remove line feeds' 'broadcast as update').		lines add: labels size - 1.		selectors addAll: #(fileInSelection fileIntoNewChangeSet browseChanges browseFile removeLinefeeds putUpdate)].	(suffix = 'swf') | (suffix = '*') ifTrue:[		labels add:'open as Flash'.		selectors add: #openAsFlash].	(suffix = 'ttf') | (suffix = '*') ifTrue:[		labels add: 'open true type font'.		selectors add: #openAsTTF].	(suffix = 'gz') | (suffix = '*') ifTrue:[		labels addAll: #('view decompressed' 'decompress to file').		selectors addAll: #(viewGZipContents saveGZipContents)].	(suffix = '3ds') | (suffix = '*') ifTrue:[		labels add: 'Open 3DS file'.		selectors add: #open3DSFile].	(suffix = 'tape') | (suffix = '*') ifTrue:		[labels add: 'open for playback'.		selectors add: #openTapeFromFile].	(suffix = '*') ifTrue:		[labels addAll: #('generate HTML').		lines add: labels size - 1.		selectors addAll: #(renderFile)].	^ Array with: labels with: lines with: selectors! !!FileList methodsFor: 'file list menu' stamp: 'ar 9/3/1999 13:05'!removeLinefeeds	"Remove any line feeds by converting to CRs instead"	| fileContents |	fileContents _ (CrLfFileStream readOnlyFileNamed: self fullName) contentsOfEntireFile.	(StandardFileStream newFileNamed: self fullName) 		nextPutAll: fileContents;		close.! !!ServerDirectory methodsFor: 'updates' stamp: 'ar 9/3/1999 13:03'!putUpdate: fileStrm 	"Put this file out as an Update on the servers of my group.  Each version of the system has its own set of update files.  'updates.list' holds the master list.  Each update is a fileIn whose name begins with a number.  See Utilities class readServerUpdatesThrough:saveLocally:updateImage:."	| myServers updateStrm sequence newName myName response local restOfText seq fileContents |"	(ScheduledControllers scheduledControllers detect: [:each |		each model == Transcript] ifNone: [nil]) ifNil: [			^ self inform: 'Please open a Transcript window, and then start putting out this update again.']."	local _ fileStrm localName.	fileStrm size = 0		ifTrue: [^ self inform: 'That file has zero bytes!!  May have a new name.'].	fileContents _ fileStrm contentsOfEntireFile.	(fileContents includes: Character linefeed)		ifTrue: [self notify: 'That file contains linefeeds.Proceed if you know that this is okay (e.g. the file contains raw binary data).'].	fileStrm reset.	(self checkNames: (Array with: local)) ifFalse: [^ nil].	"illegal characters"	myName _ group ifNil: [self moniker] ifNotNil: [group key].	response _ (PopUpMenu labels: 'Install update\Cancel update' withCRs)		startUpWithCaption: 'Do you really want to broadcast the file ', local, 			'\to every Squeak user who updates from ' withCRs, myName, '?'.	response = 1 ifFalse: [^ nil].	"abort"	self openGroup.	(myServers _ self checkServers) size = 0 ifTrue: [self closeGroup.  ^ self].	updateStrm _ myServers first getFileNamed: 'updates.list'.	"get last number and add 1"	sequence _ Utilities lastUpdateNum: updateStrm.	seq _ (sequence+1) printString.	seq size = 1 ifTrue: [seq _ '00', seq].	seq size = 2 ifTrue: [seq _ '0', seq].	newName _ seq, local.	restOfText _ Utilities position: updateStrm 	"sets the postion!!!!"			atVersion: (Smalltalk at: #EToySystem) version.	restOfText size > 0 ifTrue: [		response _ (PopUpMenu labels: 'Make update for my older version\Cancel update' withCRs)			startUpWithCaption: 'This system, ', (Smalltalk at: #EToySystem) version,				' is not the latest version'.		response = 1 ifFalse: [self closeGroup.  ^ nil].	"abort"		].	"append name to updates"	(updateStrm skip: -1; next) == Character cr ifFalse: [		updateStrm nextPut: Character cr].	updateStrm nextPutAll: newName; nextPut: Character cr; nextPutAll: restOfText.	myServers do: [:aServer |		fileStrm reset.	"reopen"		aServer putFile: fileStrm named: newName retry: true.		updateStrm reset.		aServer putFile: updateStrm named: 'updates.list' retry: true.		Transcript cr; show: 'Update succeeded on server ', aServer moniker].	self closeGroup.			Transcript cr; show: 'Be sure to test your new update!!'; cr.	"rename the file locally (may fail)"	fileStrm directory rename: local toBe: newName.! !