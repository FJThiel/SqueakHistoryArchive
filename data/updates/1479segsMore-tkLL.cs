'From Squeak 2.5 of August 6, 1999 on 24 September 1999 at 4:05 pm'!"Change Set:		segsMore-tkLLDate:			24 September 1999Author:			Ted KaehlerFirst cut at writing Porjects to disk as ImageSegments when you leace one.  There is still a long pause after you go to a new project.  To turn this on: Preferences setPreference:  #projectsSentToDisk  toValue:  true.To write all projects in one long pause (not necessary -- it writes 2 when you leave a project)Project storeAllInSegments."!!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 9/24/1999 11:48'!readFromFile	"Read in a simple segment.  Use folder of this image, even if remembered as previous location of this image"	| ff realName segName |	segName _ FileDirectory localNameFor: fileName.	realName _ self class folder, FileDirectory slash, segName.	ff _ FileStream oldFileNamed: realName.	segment _ ff nextInto: (WordArray new: ff size//4).	ff close.	state _ #active! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 9/24/1999 11:41'!folder	| im |	"Full path name of segments folder.  Be sure to duplicate and rename the folder when you duplicate and rename an image.  Is $_ legal in all file systems?"	im _ Smalltalk imageName.	^ (im copyFrom: 1 to: im size - 6 "'.image' size"), '_segs'! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 9/23/1999 17:50'!shutDown	"Delete segment files that can't be used after this image is saved."	"This is Optional.  (1) How tell if saving image now?  Only do if is.(2) ImageSegmentRootStub allInstancesDo: 	If more than one file, delete all but one we are using now.	Leave files with not stubs (could be out in a segment)	Must forbid two projects from having the same name!!(3) all Projects do:	If project is in, delete all files with its name."	! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 9/24/1999 14:48'!startUp	| choice |	"Minimal thing to assure that a .segs folder is present"(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [	(FileDirectory default includesKey: (FileDirectory localNameFor: self folder)) 		ifFalse: [			choice _ (PopUpMenu labels: 'Create folder\Quit without saving' withCRs)				startUpWithCaption: 					'The folder with segments for this image is missing.\' withCRs,					self folder, '\If you have moved or renamed the image file,\' withCRs,					'please Quit and rename the segments folder in the same way'.			choice = 1 ifTrue: [FileDirectory default createDirectory: self folder].			choice = 2 ifTrue: [Smalltalk snapshot: false andQuit: true]]]	! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 9/24/1999 14:40'!uniqueFileNameFor: segName	"Choose a unique file name for the segment with this name.  If no folder exists, create it in the same directory as the image."| dir segDir fileName fo listOfFiles |dir _ FileDirectory default.fo _ dir class localNameFor: self folder. "ThisImage_segs"(dir includesKey: fo) ifFalse: [	dir createDirectory: fo].	"create the folder"segDir _ dir directoryNamed: fo.listOfFiles _ segDir fileNames.BiggestFileNumber ifNil: [BiggestFileNumber _ 1].BiggestFileNumber > 99 ifTrue: [BiggestFileNumber _ 1].	"wrap"[fileName _ segName, BiggestFileNumber printString, '.seg'. (listOfFiles includes: fileName)] whileTrue: [	BiggestFileNumber _ BiggestFileNumber + 1].	"force a unique file name"^ segDir pathName, FileDirectory slash, fileName! !!Project methodsFor: 'file in/out' stamp: 'tk 9/24/1999 14:53'!storeSegment	"Store my project out on the disk as an ImageSegment.  Keep the outPointers in memory.  Name it <project name>.seg.  *** Caller must be holding (Project alInstances) to keep subprojects from going out. ***"| is response |world == World ifTrue: [^ false]. 	"self inform: 'Can''t send the current world out'."world isInMemory ifFalse: [^ false].  "already done"world isMorph ifFalse: [	self projectParameters at: #isMVC put: true.	^ false].	"Only Morphic projects for now"world ifNil: [^ false].  world presenter ifNil: [^ false]."Do this on project enter"World flapTabs do: [:ft | ft referent adaptToWorld: World].		"keep the Menu flap from pointing at my project.  Already done by enter?"	"Preferences setPreference: #useGlobalFlaps toValue: false."	"Utilities globalFlapTabsIfAny do:		[:aFlapTab | Utilities removeFlapTab: aFlapTab keepInList: false].	Utilities clobberFlapTabList.	"	"project world deleteAllFlapArtifacts."	"self currentWorld deleteAllFlapArtifacts.	"Utilities emptyScrapsBook.World currentHand objectToPaste ifNotNil: [	response _ (PopUpMenu labels: 'Delete\Keep' withCRs)		startUpWithCaption: 'Hand is holding a Morph in its paste buffer:\' withCRs,			World currentHand objectToPaste printString.	response = 1 ifTrue: [World currentHand clearPasteBuffer]].is _ ImageSegment new copyFromRootsLocalFileFor: 	(Array with: world presenter with: world).	"world, and all Players"is segment size < 800 ifTrue: ["debugging" 	Transcript show: self name, ' not enough objects for a Segment.'; cr.	^ false].is extract; writeToFile: (ImageSegment uniqueFileNameFor: self name).^ true! !!Project methodsFor: 'file in/out' stamp: 'tk 9/24/1999 15:37'!storeSomeSegment	| cnt |	"Try all projects to see if any is ready to go out.  Send at most three of them.  Previous one has to wait for a garbage collection before it can go out."	cnt _ 0.	Project allInstances do: [:proj | 		proj storeSegment ifTrue: ["Yes, did send its morphs to the disk"			self beep.			"(PluckedSound pitch: 261.625*4 dur: 1 loudness: 0.1) play."			(cnt _ cnt + 1) >= 2 ifTrue: [				"(PluckedSound pitch: 261.625*2 dur: 0.25 loudness: 0.1) play."				^ self]]].	(PluckedSound pitch: 261.625*4 dur: 0.35 loudness: 0.1) play.	"high = none"! !!SystemDictionary class methodsFor: 'initialization' stamp: 'tk 9/23/1999 17:47'!initialize	"SystemDictionary initialize"	| oldList |	oldList _ StartUpList.	StartUpList _ OrderedCollection new.	"These get processed from the top down..."	Smalltalk addToStartUpList: DisplayScreen.	Smalltalk addToStartUpList: Cursor.	Smalltalk addToStartUpList: InputSensor.	Smalltalk addToStartUpList: ProcessorScheduler.  "Starts low space watcher and bkground."	Smalltalk addToStartUpList: Delay.	Smalltalk addToStartUpList: FileDirectory.  "Enables file stack dump and opens sources."	Smalltalk addToStartUpList: ShortIntegerArray.	Smalltalk addToStartUpList: ShortRunArray.	Smalltalk addToStartUpList: CrLfFileStream.	oldList ifNotNil: [oldList do: [:className | Smalltalk at: className						ifPresent: [:theClass | Smalltalk addToStartUpList: theClass]]].	Smalltalk addToStartUpList: ImageSegment.	Smalltalk addToStartUpList: PasteUpMorph.	Smalltalk addToStartUpList: ControlManager.	oldList _ ShutDownList.	ShutDownList _ OrderedCollection new.	"These get processed from the bottom up..."	Smalltalk addToShutDownList: DisplayScreen.	Smalltalk addToShutDownList: Form.	Smalltalk addToShutDownList: ControlManager.	Smalltalk addToShutDownList: StrikeFont.	Smalltalk addToShutDownList: Color.	Smalltalk addToShutDownList: FileDirectory.	Smalltalk addToShutDownList: Delay.	Smalltalk addToShutDownList: SoundPlayer.	Smalltalk addToShutDownList: HttpUrl.	Smalltalk addToShutDownList: Password.	Smalltalk addToShutDownList: PWS.	Smalltalk addToShutDownList: MailDB.	Smalltalk addToShutDownList: ImageSegment.	oldList ifNotNil: [oldList reverseDo: [:className | Smalltalk at: className						ifPresent: [:theClass | Smalltalk addToShutDownList: theClass]]].! !SystemDictionary initialize!