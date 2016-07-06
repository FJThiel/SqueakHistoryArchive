'From Squeak 2.2beta of Sept 16, 1998 on 23 September 1998 at 4:30:38 pm'!Object subclass: #FFT	instanceVariableNames: 'nu n sinTable permTable realData imagData '	classVariableNames: ''	poolDictionaries: ''	category: 'System-Sound'!ImageMorph subclass: #MovieFrameSyncMorph	instanceVariableNames: 'moviePlayerMorph frameNumber '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Demo'!ReadWriteStream subclass: #RWBinaryOrTextStream	instanceVariableNames: 'isBinary '	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Streams'!Object subclass: #SharedQueue	instanceVariableNames: 'contentsArray readPosition writePosition accessProtect readSynch '	classVariableNames: ''	poolDictionaries: ''	category: 'System-Network'!!Dictionary methodsFor: 'user interface' stamp: 'di 9/23/1998 12:20'!inspectFormsWithLabel: aLabel	"Open a Form Dictionary inspector on the receiver, with the given label.  "	| viewClass |	viewClass _ PluggableTextView.	Smalltalk at: #FormInspectView		ifPresent: [:formInspectView | viewClass _ formInspectView].	^ DictionaryInspector openOn: self withEvalPane: true		withLabel: aLabel		valueViewClass: viewClass! !!InputSensor methodsFor: 'user interrupts' stamp: 'di 9/23/1998 16:12'!userInterruptWatcher	"Wait for user interrupts and open a notifier on the active process when one occurs."	[true] whileTrue: [		InterruptSemaphore wait.		Display deferUpdates: false.		Smalltalk shutDownSound.		Smalltalk handleUserInterrupt]! !!Inspector class methodsFor: 'instance creation' stamp: 'di 9/23/1998 12:38'!openOn: anObject withEvalPane: withEval withLabel: label valueViewClass: valueViewClass	| topView inspector listView valueView evalView |	World ifNotNil:		[^ self openAsMorphOn: anObject withEvalPane: withEval			withLabel: label valueViewClass: valueViewClass].	inspector _ self inspect: anObject.	topView _ StandardSystemView new model: inspector.	topView borderWidth: 1.	listView _ PluggableListView on: inspector		list: #fieldList		selected: #selectionIndex		changeSelected: #toggleIndex:		menu: #fieldListMenu:		keystroke: #inspectorKey:from:.	(inspector isMemberOf: DictionaryInspector)		ifTrue: [listView menu: #dictionaryMenu:].	listView window: (0 @ 0 extent: 40 @ 40).	topView addSubView: listView.	valueView _ valueViewClass new.		"PluggableTextView or PluggableFormView"	(valueView respondsTo: #getText) ifTrue: [		valueView on: inspector 			text: #contents accept: #accept:			readSelection: #contentsSelection menu: #codePaneMenu:shifted:].	(valueViewClass inheritsFrom: FormView) ifTrue: [		valueView model: inspector].	valueView window: (0 @ 0 extent: 75 @ 40).	topView addSubView: valueView toRightOf: listView.		withEval ifTrue:		[evalView _ PluggableTextView new on: inspector 			text: #trash accept: #trash:			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.		evalView window: (0 @ 0 extent: 115 @ 20).		evalView askBeforeDiscardingEdits: false.		topView addSubView: evalView below: listView].	topView label: label.	topView minimumSize: 180 @ 120.	topView controller open! !!Scamper methodsFor: 'changing page' stamp: 'di 9/23/1998 11:46'!displayDocument: mimeDocument	"switch to viewing the given MIMEDocument"	| newUrl  newSource handled  formatter fileName file image imageMorph attrib text |	newUrl _ mimeDocument url.	newSource _ mimeDocument.	handled _ false.	"add it to the history"	recentDocuments removeAllSuchThat: [ :d | d url = mimeDocument url ].	recentDocuments addLast: mimeDocument.	recentDocuments size > 20 ifTrue: [ recentDocuments removeFirst ].			newSource mainType = 'image' ifTrue: [		"an image--embed it in a text"		document _ nil.		formattedPage _ [			image _ ImageReadWriter formFromStream: (RWBinaryOrTextStream with: newSource content) binary reset.			imageMorph _ ImageMorph new image: image.			attrib _ TextAnchor new anchoredMorph: imageMorph.			text _ ' * ' asText.			text addAttribute: attrib from: 2 to: 2.			text		] ifError: [ :msg :ctx | msg ].		currentUrl _ newUrl.		pageSource _ newSource content.		handled _ true. 		self status: 'sittin'.		self changed: #currentUrl.			self changed: #title.		self changed: #hasLint.		self changed: #lint.		self changed: #formattedPage.		self changed: #formattedPageSelection.  		"remove it from the history--these thigns are too big!!"		"ideally, there would be a smarter history mechanism that can do things like remove items when memory consumption gets too high...."		recentDocuments removeLast.	].	newSource contentType = 'text/html' ifTrue: [		"HTML page--format it"		currentUrl _ newUrl.		pageSource _ newSource content.		self status: 'parsing...'.		document _ (HtmlParser parse: (ReadStream on: pageSource)).		self status: 'laying out...'.		formatter _ HtmlFormatter new.		formatter browser: self.		formatter baseUrl: currentUrl.		document addToFormatter: formatter.		formattedPage _ formatter text.		currentUrl fragment			ifNil: [ currentAnchorLocation _ nil ]			ifNotNil: [ currentAnchorLocation _				formatter anchorLocations 					at: currentUrl fragment asLowercase					ifAbsent: [ nil ] ].		self startDownloadingMorphState: (formatter incompleteMorphs).		handled _ true.		self status: 'sittin'.		self changed: #currentUrl.			self changed: #title.		self changed: #hasLint.		self changed: #lint.		self changed: #formattedPage.		self changed: #formattedPageSelection.  ].	(#('audio/midi' 'audio/x-midi') includes: newSource contentType) ifTrue: [		Smalltalk at: #MIDIFileReader ifPresent:			[:reader |			reader playStream: (RWBinaryOrTextStream with: newSource content) reset binary.			self status: 'sittin'.			handled _ true]].	(handled not and: [ newSource mainType = 'text']) ifTrue: [		"treat as plain text"		pageSource _ newSource content.		document _ nil.		formattedPage _ pageSource withSqueakLineEndings.		currentUrl _ newUrl.				self status: 'sittin'.		self changed: #currentUrl.		self changed: #title.		self changed: #hasLint.		self changed: #lint.		self changed: #formattedPage.  		handled _ true].	handled ifFalse: [		"offer to save it to a file"		self status: 'sittin'.		(self confirm: 'unkown content-type ', newSource contentType,'--Would you like to save to a file?') ifFalse: [			^self ].		fileName _ ''.		[			fileName _ FillInTheBlank request: 'file to save in' initialAnswer: fileName.			fileName isEmpty ifTrue: [ ^self ].			file _ FileStream fileNamed: fileName.			file == nil		] whileTrue.		file reset.		file binary.		file nextPutAll: newSource content.		file close.	].! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/23/1998 13:00'!discardMorphic	"Discard Morphic."	| subs |	subs _ OrderedCollection new.	Morph allSubclassesWithLevelDo: [:c :i | subs addFirst: c]		startingLevel: 0.	subs do: [:c | c removeFromSystem].	SystemOrganization removeCategoriesMatching: 'User Objects'.	SystemOrganization removeCategoriesMatching: 'Experimental-'.	SystemOrganization removeCategoriesMatching: 'Morphic-*'.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/16/1998 14:59'!discardNetworking	"Discard the support for TCP/IP networking."	Smalltalk discardPluggableWebServer.	SystemOrganization removeCategoriesMatching: 'HTML-*'.	SystemOrganization removeCategoriesMatching: 'NetTools-*'.	SystemOrganization removeCategoriesMatching: 'Network-*'.	SystemOrganization removeCategoriesMatching: 'System-Network'.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/23/1998 13:27'!discardOddsAndEnds	"This method throws out lots of classes that are not frequently used."	"Smalltalk discardOddsAndEnds"	SystemOrganization removeCategoriesMatching: 'System-Serial Port'.	"old Form editor:"	SystemOrganization removeSystemCategory: 'Graphics-Symbols'.	Form removeSelector: #edit.	Smalltalk at: #FormView ifPresent:		[:c | c compile: 'defaultControllerClass  ^ NoController'			classified: 'controller access'].	Smalltalk removeClassNamed: #FormEditorView.	Smalltalk removeClassNamed: #FormEditor.	SystemOrganization removeSystemCategory: 'Graphics-Paths'.	"bit editor (remove Form editor first):"	Form removeSelector: #bitEdit.	Form removeSelector: #bitEditAt:scale:.	StrikeFont removeSelector: #edit:.	Smalltalk removeClassNamed: #FormButtonCache.	Smalltalk removeClassNamed: #FormMenuController.	Smalltalk removeClassNamed: #FormMenuView.	Smalltalk removeClassNamed: #BitEditor.	"inspector for Dictionaries of Forms"	Dictionary removeSelector: #inspectFormsWithLabel:.	SystemDictionary removeSelector: #viewImageImports.	ScreenController removeSelector: #viewImageImports.	Smalltalk removeClassNamed: #FormHolderView.	Smalltalk removeClassNamed: #FormInspectView.	"curve fitting:"	Smalltalk at: #FormEditor ifPresent: [:c | c removeSelector: #curve].	Smalltalk removeClassNamed: #CurveFitter.	Smalltalk removeClassNamed: #LinearFit.	Smalltalk removeClassNamed: #Spline.	"experimental hand-drawn character recoginizer:"	ParagraphEditor removeSelector: #recognizeCharacters.	ParagraphEditor removeSelector: #recognizer:.	ParagraphEditor removeSelector: #recognizeCharactersWhileMouseIn:.	Smalltalk removeClassNamed: #CharRecog.	"experimental updating object viewer:"	Object removeSelector: #evaluate:wheneverChangeIn:.	Smalltalk removeClassNamed: #ObjectViewer.	Smalltalk removeClassNamed: #ObjectTracer.	"HTML formatted fileout support:"	StandardFileStream removeSelector: #asHtml.	Smalltalk removeClassNamed: #HtmlFileStream.	"miscellaneous classes:"	Smalltalk removeClassNamed: #Array2D.	Smalltalk removeClassNamed: #DriveACar.	Smalltalk removeClassNamed: #EventRecorder.	Smalltalk removeClassNamed: #FindTheLight.	Smalltalk removeClassNamed: #PluggableTest.	Smalltalk removeClassNamed: #SystemMonitor.	Smalltalk at: #SampledSound ifPresent: [:c |		(Smalltalk confirm: 'Remove all sounds from the SampledSound library?')		ifTrue: [c initialize]].	#(Helvetica Palatino ComicAll Courier) do:		[:k | TextConstants removeKey: k].	(TextConstants at: #ComicBold) newFontArray:		((TextConstants at: #ComicBold) fontArray copyFrom: 1 to: 5).! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/23/1998 11:55'!discardSoundSynthesis	"Discard the sound synthesis facilities, and the methods and classes that use it. This also discards MIDI."	Smalltalk discardMIDI.	Smalltalk removeClassNamed: #EnvelopeLineMorph.	Smalltalk removeClassNamed: #EnvelopeEditorMorph.	Smalltalk removeClassNamed: #PianoKeyboardMorph.	Smalltalk removeClassNamed: #WaveEditor.	Smalltalk removeClassNamed: #SoundSequencerMorph.	Smalltalk removeClassNamed: #SoundMorph.	Smalltalk removeClassNamed: #SoundLoopMorph.	Smalltalk removeClassNamed: #InterimSoundMorph.	Smalltalk removeClassNamed: #RecordingControlsMorph.	Smalltalk removeClassNamed: #PermanentRecordingControlsMorph.	Smalltalk removeClassNamed: #SoundDemoMorph.	Smalltalk at: #GraphMorph ifPresent: [:graphMorph |		#(loadCoffeeCupClink play playBach playOnce		  readDataFromFile registerWaveform stopPlaying)			do: [:sel | graphMorph removeSelector: sel]].	Smalltalk at: #TrashCanMorph ifPresent: [:trashMorph |		trashMorph class removeSelector: #samplesForDelete.		trashMorph class removeSelector: #samplesForMouseEnter.		trashMorph class removeSelector: #samplesForMouseLeave].	SystemOrganization removeCategoriesMatching: 'System-Sound'.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/16/1998 13:50'!discardVMConstruction	"Discard the virtual machine construction classes and the Smalltalk-to-C translator. These are only needed by those wishing to build or study the Squeak virtual machine, or by those wishing to construct new primitives via Smalltalk-to-C translation."	"remove the code for virtual machines"	Smalltalk removeKey: #InterpreterLog ifAbsent: [].	SystemOrganization removeCategoriesMatching: 'Squeak-Jitter'.	SystemOrganization removeCategoriesMatching: 'Squeak-Interpreter'.	"remove the Smalltalk-to-C translator"	Smalltalk at: #CCodeGenerator ifPresent: [:codeGen | codeGen removeCompilerMethods].	SystemOrganization removeCategoriesMatching: 'Squeak-Translation to C'.	Smalltalk removeClassNamed: #SystemTracer.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/23/1998 15:38'!majorShrink    "Smalltalk majorShrink; abandonSources; lastRemoval"	"This method throws out lots of the system that is not needed for, eg, operation in a hand-held PC.  The shrink process is being improved and, in conjunction with removeAllUnSentMessages, yields an image around ?? in size."Smalltalk discardVMConstruction.  "663k"Smalltalk discardSoundSynthesis.  "330k"Smalltalk discardOddsAndEnds.  "228k"Smalltalk discardNetworking.  "261k"Smalltalk discardMorphic.  "2,231k""Above altogether saves 3,541k"	"Remove references to a few classes to be deleted, so that they won't leave obsolete versions around."	FileList removeSelector: #fileIntoNewChangeSet.	ChangeSet class compile: 'defaultName		^ ''Changes'' ' classified: 'initialization'.	ScreenController removeSelector: #openChangeManager.	ScreenController removeSelector: #exitProject.	ScreenController removeSelector: #openProject.	ScreenController removeSelector: #viewImageImports.	"Now delete lots of classes.."	SystemOrganization removeSystemCategory: 'Graphics-Symbols'.	SystemOrganization removeSystemCategory: 'Graphics-Files'.	SystemOrganization removeSystemCategory: 'Interface-Projects'.	SystemOrganization removeSystemCategory: 'System-Object Storage'.	Smalltalk removeClassNamed: #FormSetFont.	Smalltalk removeClassNamed: #FontSet.	Smalltalk removeClassNamed: #InstructionPrinter.	Smalltalk removeClassNamed: #ChangeSorter.	Smalltalk removeClassNamed: #DualChangeSorter.	Smalltalk removeClassNamed: #EmphasizedMenu.	Smalltalk removeClassNamed: #MessageTally.	StringHolder class removeSelector: #originalWorkspaceContents.	CompiledMethod removeSelector: #symbolic.	RemoteString removeSelector: #makeNewTextAttVersion.	Utilities class removeSelector: #absorbUpdatesFromServer.	Smalltalk removeClassNamed: #PenPointRecorder.	Smalltalk removeClassNamed: #Path.	Smalltalk removeClassNamed: #Base64MimeConverter.	Smalltalk removeClassNamed: #EToySystem.	Smalltalk removeClassNamed: #RWBinaryOrTextStream.	Smalltalk removeClassNamed: #AttributedTextStream.	TextStyle allInstancesDo:		[:ts | ts newFontArray: (ts fontArray copyFrom: 1 to: 2)].	ListParagraph initialize.	PopUpMenu initialize.	StandardSystemView initialize.	Smalltalk noChanges.	ChangeSorter classPool at: #AllChangeSets 		put: (OrderedCollection with: Smalltalk changes).	[self removeAllUnSentMessages > 0] whileTrue.	Smalltalk allClassesDo: [:c | c zapOrganization].	Symbol rehash.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'di 9/23/1998 15:46'!removeAllUnSentMessages   "Smalltalk removeAllUnSentMessages" 	"Remove all implementations of unsent messages."	| sels n |	sels _ self allUnSentMessages.	"The following should be preserved for doIts, etc"	#(printSpaceAnalysis		dragon: hilberts: mandala: web test3 factorial benchmark benchFib		newDepth: restoreAfter: forgetDoIts		removeAllUnSentMessages abandonSources removeUnreferencedKeys		reclaimDependents zapOrganization condenseChanges browseObsoleteReferences		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:		startTimerInterruptWatcher) do:		[:sel | sels remove: sel ifAbsent: []].	"The following may be sent by perform: in dispatchOnChar..."	(ParagraphEditor classPool at: #CmdActions) asSet do:		[:sel | sels remove: sel ifAbsent: []].	(ParagraphEditor classPool at: #ShiftCmdActions) asSet do:		[:sel | sels remove: sel ifAbsent: []].	sels size = 0 ifTrue: [^ 0].	n _ 0. Smalltalk allBehaviorsDo: [:x | n _ n+1].	'Removing ', sels size printString , ' messages . . .'		displayProgressAt: Sensor cursorPoint		from: 0 to: n		during:		[:bar |		n _ 0.		self allBehaviorsDo:			[:class | bar value: (n _ n+1).			sels do:				[:sel | class removeSelectorSimply: sel]]].	MethodDictionary allInstancesDo: [:d | d rehash].	^ sels size! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 9/23/1998 15:23'!makeExternalRelease		"Smalltalk makeExternalRelease"	(self confirm: self version , 'Is this the correct version designation?If not, choose no, and fix it.') ifFalse: [^ self].	(Object classPool at: #DependentsFields) size > 1 ifTrue: [self halt].	Browser initialize.	Undeclared isEmpty ifFalse: [self halt].	Smalltalk garbageCollect.	Smalltalk obsoleteClasses isEmpty ifFalse: [self halt].	Display newDepth: 8.	Project allInstancesDo: [:p | p displayDepth: 8].	Utilities removeDisney.	ScriptingSystem prepareForExternalReleaseNamed: 'Squeak2.2beta'.	StandardScriptingSystem removeSelector: #serverUrls.	EToySystem class removeSelector: #serverUrls.	ServerDirectory removeServerNamed: 'UpdatesAtDOL'.	ServerDirectory removeServerNamed: 'UpdatesAtWebPage'.	#(Helvetica Palatino ComicAll Courier) do:		[:k | TextConstants removeKey: k].	(TextConstants at: #ComicBold) newFontArray:		((TextConstants at: #ComicBold) fontArray copyFrom: 1 to: 5).	SystemDictionary removeSelector: #makeExternalRelease.	Symbol rehash.	self halt: 'Ready to condense sources'.	Smalltalk condenseSources! !!SystemDictionary methodsFor: 'miscellaneous' stamp: 'di 9/23/1998 16:11'!handleUserInterrupt	Preferences cmdDotEnabled ifTrue:		[Smalltalk isMorphic			ifTrue: [[Project current interruptName: 'User Interrupt'] fork]			ifFalse: [[ScheduledControllers interruptName: 'User Interrupt'] fork]]! !SystemDictionary removeSelector: #minorShrink!SystemDictionary removeSelector: #discardVMConstructionClasses!SystemDictionary removeSelector: #discardSerialPort!Smalltalk removeClassNamed: #Courier!"Postscript:Get rid of Courier FontSet."Smalltalk removeClassNamed: #Courier.Sensor installInterruptWatcher.!