'From Squeak2.6 of 11 October 1999 [latest update: #1610] on 15 November 1999 at 4:49:29 pm'!!Behavior methodsFor: 'enumerating' stamp: 'tk 11/12/1999 11:36'!allInstancesEverywhereDo: aBlock 	"Evaluate the argument, aBlock, for each of the current instances of the receiver.  Including those in ImageSegments that are out on the disk.  Bring each in briefly."	self ==  UndefinedObject ifTrue: [^ aBlock value: nil].	self allInstancesDo: aBlock.	"Now iterate over instances in segments that are out on the disk."	ImageSegment allSubInstancesDo: [:seg |		seg allInstancesOf: self do: aBlock].! !!FileList methodsFor: 'file list menu' stamp: 'tk 11/12/1999 15:53'!itemsForFileEnding: suffix	| labels lines selectors |	labels _ OrderedCollection new.	lines _ OrderedCollection new.	selectors _ OrderedCollection new.	(suffix = 'bmp') | (suffix = 'gif') | (suffix = 'jpg') | (suffix = 'form') | (suffix = '*') ifTrue:		[labels addAll: #('open image in a window' 'read image into ImageImports'						 'open image as background').		selectors addAll: #(openImageInWindow importImage openAsBackground)].	(suffix = 'morph') | (suffix = 'morphs') | (suffix = 'sp') | (suffix = '*') ifTrue:		[labels add: 'load as morph'.		selectors add: #openMorphFromFile.		labels add: 'load as project'.		selectors add: #openProjectFromFile].	(suffix = 'extseg') ifTrue:		[labels add: 'load as project'.		selectors add: #openProjectFromFile].	(suffix = 'bo') | (suffix = '*') ifTrue:[		labels add: 'load as book'.		selectors add: #openBookFromFile].	(suffix = 'mid') | (suffix = '*') ifTrue:		[labels add: 'play midi file'.		selectors add: #playMidiFile].	(suffix = 'movie') | (suffix = '*') ifTrue:		[labels add: 'open as movie'.		selectors add: #openAsMovie].	(suffix = 'st') | (suffix = 'cs') | (suffix = '*') ifTrue:		[suffix = '*' ifTrue: [lines add: labels size].		labels addAll: #('fileIn' 'file into new change set' 'browse changes' 'browse code' 'remove line feeds' 'broadcast as update').		lines add: labels size - 1.		selectors addAll: #(fileInSelection fileIntoNewChangeSet browseChanges browseFile removeLinefeeds putUpdate)].	(suffix = 'swf') | (suffix = '*') ifTrue:[		labels add:'open as Flash'.		selectors add: #openAsFlash].	(suffix = 'ttf') | (suffix = '*') ifTrue:[		labels add: 'open true type font'.		selectors add: #openAsTTF].	(suffix = 'gz') | (suffix = '*') ifTrue:[		labels addAll: #('view decompressed' 'decompress to file').		selectors addAll: #(viewGZipContents saveGZipContents)].	(suffix = '3ds') | (suffix = '*') ifTrue:[		labels add: 'Open 3DS file'.		selectors add: #open3DSFile].	(suffix = 'tape') | (suffix = '*') ifTrue:		[labels add: 'open for playback'.		selectors add: #openTapeFromFile].	(suffix = 'wrl') | (suffix = '*') ifTrue:		[labels add: 'open in Wonderland'.		selectors add: #openVRMLFile].	(suffix = '*') ifTrue:		[labels addAll: #('generate HTML').		lines add: labels size - 1.		selectors addAll: #(renderFile)].	^ Array with: labels with: lines with: selectors! !!FileList methodsFor: 'file list menu' stamp: 'tk 11/12/1999 15:55'!openProjectFromFile	"Reconstitute a Morph from the selected file, presumed to be represent a Morph saved via the SmartRefStream mechanism, and open it in an appropriate Morphic world" 	| aFileStream morphOrList window proj |	Smalltalk verifyMorphicAvailability ifFalse: [^ self].	World ifNil: [^ self inform: 'Later, allow jumping from MVC to Morphic Projects.'].	aFileStream _ directory oldFileNamed: self fullName.	morphOrList _ aFileStream fileInObjectAndCode.	(morphOrList isKindOf: ImageSegment) ifTrue: [		proj _ morphOrList arrayOfRoots detect: [:mm | mm class == Project] 					ifNone: [nil]..		"rename the project if it conflicts?"		proj ifNotNil: [			^ World currentHand attachMorph: (ProjectViewMorph on: proj)]].	(morphOrList isKindOf: SqueakPage) ifTrue: [		morphOrList _ morphOrList contentsMorph].	(morphOrList isKindOf: PasteUpMorph) ifFalse: [		^ self inform: 'This is not a PasteUpMorph or exported Project.'].	(window _ ProjectViewMorph newMorphicProjectOn: morphOrList) openInWorld.	window model enter! !!ImageSegment methodsFor: 'instance remapping' stamp: 'tk 11/12/1999 11:35'!allInstancesOf: aClass do: aBlock	| withSymbols oldInstances |	"Bring me in, locate instances of aClass and submit them to the block.  Write me out again."	(state = #onFile or: [state = #onFileWithSymbols]) ifFalse: [^ self].	withSymbols _ state = #onFileWithSymbols.	(outPointers includes: aClass) ifFalse: [^ self].		"If has instances, they point out at the class"	self install.	oldInstances _ OrderedCollection new.	self allObjectsDo: [:obj | obj class == aClass ifTrue: [		oldInstances add: obj]].	oldInstances do: [:inst | aBlock value: inst].	"do the work"	self copyFromRoots: arrayOfRoots.	self extract.	withSymbols 		ifTrue: [self writeToFileWithSymbols]		ifFalse: [self writeToFile].! !!Metaclass methodsFor: 'enumerating' stamp: 'tk 11/12/1999 11:45'!allInstancesEverywhereDo: aBlock	"There should be only one"	thisClass class == self ifTrue:[^ aBlock value: thisClass].	^ super allInstancesEverywhereDo: aBlock! !!SmartRefStream methodsFor: 'import image segment' stamp: 'tk 11/15/1999 16:41'!mapClass: newClass installIn: mapFakeClassesToReal	"aClass is has already been mapped!!  Make a fake class for the old shape.  Write it into the dictionary mapping Fake classes to Real classes."	| newName className oldInstVars fakeClass |	newClass isMeta ifTrue: [^ nil].	newName _ newClass name.	className _ renamed keyAtValue: newName ifAbsent: [newName].		"A problem here if two classes map to the same one!!"	(steady includes: newClass) ifTrue: [^ nil].	oldInstVars _ structures at: className ifAbsent: [			self error: 'class is not in structures list'].	"Missing in object file"	fakeClass _ Object subclass: ('Fake37',className) asSymbol		instanceVariableNames: oldInstVars allButFirst		classVariableNames: ''		poolDictionaries: ''		category: 'Obsolete'.	mapFakeClassesToReal at: fakeClass put: newClass.	^ fakeClass! !