'From Squeak2.9alpha of 13 June 2000 [latest update: #2912] on 7 November 2000 at 1:04:17 pm'!"Change Set:		PreserveLinkedTimbresDate:			7 November 2000Author:			Dan IngallsRewrites AbstactSound>>updateScorePlayers and associated code so that score players that share a sound with an active envelope editor will retain that binding rather than have it overridden by a new copy from the sound library."!!AbstractSound class methodsFor: 'sound library' stamp: 'di 11/7/2000 12:12'!soundNamed: soundName put: aSound	Sounds at: soundName put: aSound.	AbstractSound updateScorePlayers.! !!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'di 11/7/2000 12:50'!fileInSoundLibraryNamed: fileName	"File in the sound library with the given file name, and add its contents to the current sound library."	| s newSounds |	s _ FileStream oldFileNamed: fileName.	newSounds _ s fileInObjectAndCode.	s close.	newSounds associationsDo:		[:assoc | self storeFiledInSound: assoc value named: assoc key].	AbstractSound updateScorePlayers.	Smalltalk garbageCollect.  "Large objects may have been released"! !!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'di 11/7/2000 13:00'!updateScorePlayers	| soundsBeingEdited |	"Force all ScorePlayers to update their instrument list from the sound library. This may done after loading, unloading, or replacing a sound to make all ScorePlayers feel the change."	ScorePlayer allSubInstancesDo:		[:p | p pause].	SoundPlayer shutDown.	soundsBeingEdited _ EnvelopeEditorMorph allSubInstances collect: [:ed | ed soundBeingEdited].	ScorePlayerMorph allSubInstancesDo:		[:p | p updateInstrumentsFromLibraryExcept: soundsBeingEdited].! !!EnvelopeEditorMorph methodsFor: 'initialization' stamp: 'di 11/7/2000 12:45'!soundBeingEdited	^ sound! !!MidiInputMorph methodsFor: 'as yet unclassified' stamp: 'di 11/7/2000 12:22'!invokeMenu	"Invoke a menu of additonal commands."	| aMenu |	aMenu _ CustomMenu new.	aMenu add: 'add channel' action: #addChannel.	aMenu add: 'reload instruments' target: AbstractSound selector: #updateScorePlayers.	midiSynth isOn ifFalse: [		aMenu add: 'set MIDI port' action: #setMIDIPort.		midiSynth midiPort			ifNotNil: [aMenu add: 'close MIDI port' action: #closeMIDIPort]].		aMenu invokeOn: self defaultSelection: nil.! !!MidiInputMorph methodsFor: 'as yet unclassified' stamp: 'di 11/7/2000 12:56'!updateInstrumentsFromLibraryExcept: soundsBeingEdited	"The instrument library has been modified. Update my instruments with the new versions from the library. Use a single instrument prototype for all parts with the same name; this allows the envelope editor to edit all the parts by changing a single sound prototype."	"soundsBeingEdited is a collection of sounds being edited (by an EnvelopeEditor).  If any of my instruments share one of these, then they will be left alone so as not to disturb that dynamic linkage."	| unloadPostfix myInstruments name displaysAsUnloaded isUnloaded |	unloadPostfix _ '(out)'.	myInstruments _ Dictionary new.	1 to: instrumentSelector size do: [:i |		name _ (instrumentSelector at: i) contents.		displaysAsUnloaded _ name endsWith: unloadPostfix.		displaysAsUnloaded ifTrue: [			name _ name copyFrom: 1 to: name size - unloadPostfix size].		(myInstruments includesKey: name) ifFalse: [			myInstruments at: name put:				(name = 'clink'					ifTrue: [						(SampledSound							samples: SampledSound coffeeCupClink							samplingRate: 11025) copy]					ifFalse: [						(AbstractSound							soundNamed: name							ifAbsent: [								(instrumentSelector at: i) contentsClipped: 'default'.								FMSound default]) copy])].		(soundsBeingEdited includes: (midiSynth instrumentForChannel: i)) ifFalse:			["Do not update any instrument that is currently being edited"			midiSynth instrumentForChannel: i put: (myInstruments at: name)].		"update loaded/unloaded status in instrumentSelector if necessary"		isUnloaded _ (myInstruments at: name) isKindOf: UnloadedSound.		(displaysAsUnloaded and: [isUnloaded not])			ifTrue: [(instrumentSelector at: i) contentsClipped: name].		(displaysAsUnloaded not and: [isUnloaded])			ifTrue: [(instrumentSelector at: i) contentsClipped: name, unloadPostfix]].! !!ScorePlayerMorph methodsFor: 'menu' stamp: 'di 11/7/2000 12:22'!invokeMenu	"Invoke a menu of additonal functions for this WaveEditor."	| aMenu |	aMenu _ CustomMenu new.	aMenu add: 'reload instruments' target: AbstractSound selector: #updateScorePlayers.	aMenu add: 'open a MIDI file' action: #openMIDIFile.	scorePlayer midiPort		ifNil: [			aMenu add: 'play via MIDI' action: #openMIDIPort]		ifNotNil: [			aMenu add: 'play via built in synth' action: #closeMIDIPort.			aMenu add: 'new MIDI controller' action: #makeMIDIController:].	aMenu invokeOn: self defaultSelection: nil.! !!ScorePlayerMorph methodsFor: 'menu' stamp: 'di 11/7/2000 12:42'!updateInstrumentsFromLibraryExcept: soundsBeingEdited	"The instrument library has been modified. Update my instruments with the new versions from the library. Use a single instrument prototype for all parts with the same name; this allows the envelope editor to edit all the parts by changing a single sound prototype."	"soundsBeingEdited is a collection of sounds being edited (by an EnvelopeEditor).  If any of my instruments share one of these, then they will be left alone so as not to disturb that dynamic linkage."	| unloadPostfix myInstruments name displaysAsUnloaded isUnloaded |	unloadPostfix _ '(out)'.	myInstruments _ Dictionary new.	1 to: instrumentSelector size do: [:i |		name _ (instrumentSelector at: i) contents.		displaysAsUnloaded _ name endsWith: unloadPostfix.		displaysAsUnloaded ifTrue: [			name _ name copyFrom: 1 to: name size - unloadPostfix size].		(myInstruments includesKey: name) ifFalse: [			myInstruments at: name put:				(name = 'clink'					ifTrue: [						(SampledSound							samples: SampledSound coffeeCupClink							samplingRate: 11025) copy]					ifFalse: [						(AbstractSound							soundNamed: name							ifAbsent: [								(instrumentSelector at: i) contentsClipped: 'default'.								FMSound default]) copy])].		(soundsBeingEdited includes: (scorePlayer instrumentForTrack: i)) ifFalse:			["Do not update any instrument that is currently being edited"			scorePlayer instrumentForTrack: i put: (myInstruments at: name)].		"update loaded/unloaded status in instrumentSelector if necessary"		isUnloaded _ (myInstruments at: name) isKindOf: UnloadedSound.		(displaysAsUnloaded and: [isUnloaded not])			ifTrue: [(instrumentSelector at: i) contentsClipped: name].		(displaysAsUnloaded not and: [isUnloaded])			ifTrue: [(instrumentSelector at: i) contentsClipped: name, unloadPostfix]].! !ScorePlayerMorph removeSelector: #updateInstrumentsFromLibrary!MidiInputMorph removeSelector: #updateInstrumentsFromLibrary!AbstractSound class removeSelector: #pvtSoundNamed:put:!