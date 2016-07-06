'From Squeak2.7 of 5 January 2000 [latest update: #1762] on 12 January 2000 at 1:05:13 pm'!!ChangeSet methodsFor: 'fileIn/Out' stamp: 'di 1/9/2000 10:54'!fileOut	"File out the receiver, to a file whose name is a function of the change-set name and either of the date & time or chosen to have a unique numeric tag, depending on the preference 'sequentialChangeSetFileNames'"	| file slips nameToUse |	self checkForConversionMethods.	nameToUse _ Preferences changeSetVersionNumbers		ifTrue:			[FileDirectory default nextNameFor: self name extension: 'cs']		ifFalse:			[(self name, FileDirectory dot, Utilities dateTimeSuffix, 				FileDirectory dot, 'cs') asFileName].	Cursor write showWhile:		[file _ FileStream newFileNamed: nameToUse.		file header; timeStamp.		self fileOutPreambleOn: file.		self fileOutOn: file.		self fileOutPostscriptOn: file.		file trailer; close].	Preferences suppressCheckForSlips ifTrue: [^ self].	slips _ self checkForSlips.	(slips size > 0 and: [self confirm: 'Methods in this fileOut have haltsor references to the Transcriptor other ''slips'' in them.Would you like to browse them?'])		ifTrue: [Smalltalk browseMessageList: slips							name: 'Possible slips in ', name]! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 1/9/2000 11:49'!discardOddsAndEnds	"This method throws out lots of classes that are not frequently used."	"Smalltalk discardOddsAndEnds"	SystemOrganization removeSystemCategory: 'System-Serial Port'.	SystemOrganization removeSystemCategory: 'Graphics-Symbols'.	SystemOrganization removeSystemCategory: 'Interface-File Contents Browser'.	SystemOrganization removeSystemCategory: 'System-Compression'.	SystemOrganization removeSystemCategory: 'Interface-Explorer'.	Form removeSelector: #edit.	Smalltalk at: #FormView ifPresent:		[:c | c compile: 'defaultControllerClass  ^ NoController'			classified: 'controller access'].	Smalltalk removeClassNamed: #FormEditorView.	Smalltalk removeClassNamed: #FormEditor.	SystemOrganization removeSystemCategory: 'Graphics-Paths'.	"bit editor (remove Form editor first):"	Form removeSelector: #bitEdit.	Form removeSelector: #bitEditAt:scale:.	StrikeFont removeSelector: #edit:.	Smalltalk removeClassNamed: #FormButtonCache.	Smalltalk removeClassNamed: #FormMenuController.	Smalltalk removeClassNamed: #FormMenuView.	Smalltalk removeClassNamed: #BitEditor.	"inspector for Dictionaries of Forms"	Dictionary removeSelector: #inspectFormsWithLabel:.	SystemDictionary removeSelector: #viewImageImports.	ScreenController removeSelector: #viewImageImports.	Smalltalk removeClassNamed: #FormHolderView.	Smalltalk removeClassNamed: #FormInspectView.	"experimental hand-drawn character recoginizer:"	ParagraphEditor removeSelector: #recognizeCharacters.	ParagraphEditor removeSelector: #recognizer:.	ParagraphEditor removeSelector: #recognizeCharactersWhileMouseIn:.	Smalltalk removeClassNamed: #CharRecog.	"experimental updating object viewer:"	Object removeSelector: #evaluate:wheneverChangeIn:.	Smalltalk removeClassNamed: #ObjectViewer.	Smalltalk removeClassNamed: #ObjectTracer.	"miscellaneous classes:"	Smalltalk removeClassNamed: #Array2D.	Smalltalk removeClassNamed: #DriveACar.	Smalltalk removeClassNamed: #EventRecorder.	Smalltalk removeClassNamed: #FindTheLight.	Smalltalk removeClassNamed: #PluggableTest.	Smalltalk removeClassNamed: #SystemMonitor.	Smalltalk removeClassNamed: #DocLibrary.	Smalltalk removeClassNamed: #ProtocolBrowser.	Smalltalk removeClassNamed: #ObjectExplorerWrapper.	Smalltalk removeClassNamed: #HierarchyBrowser.	Smalltalk removeClassNamed: #LinkedMessageSet.	Smalltalk removeClassNamed: #ObjectExplorer.	Smalltalk removeClassNamed: #PackageBrowser.	Smalltalk removeClassNamed: #AbstractHierarchicalList.	Smalltalk removeClassNamed: #ChangeList.	Smalltalk removeClassNamed: #VersionsBrowser.	Smalltalk removeClassNamed: #ChangeRecord.	Smalltalk at: #SampledSound ifPresent: [:c |		(Smalltalk confirm: 'Remove all sounds from the SampledSound library?')		ifTrue: [c initialize]].	#(Helvetica Palatino Courier ComicBold ComicPlain) do:		[:k | TextConstants removeKey: k ifAbsent: []].Preferences setButtonFontTo:	(StrikeFont familyName: #NewYork size: 12).Preferences setFlapsFontTo:	(StrikeFont familyName: #NewYork size: 12).#(GZipConstants ZipConstants KlattResonatorIndices ) do:	[:k | Smalltalk removeKey: k ifAbsent: []].! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 1/12/2000 13:05'!lastRemoval  "Smalltalk lastRemoval" 	#(abandonSources browseAllSelect: printSpaceAnalysis browseObsoleteReferences  lastRemoval) do:		[:sel | SystemDictionary removeSelector: sel].	[self removeAllUnSentMessages > 0] whileTrue.	Set withAllSubclassesDo:		[:cls | cls allInstances do: [:s | s rehash]].	Smalltalk allClassesDo: [:c | c zapOrganization].	Smalltalk changes initialize.! !