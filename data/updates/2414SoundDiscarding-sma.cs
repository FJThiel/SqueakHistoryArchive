'From Squeak2.9alpha of 13 June 2000 [latest update: #2411] on 18 June 2000 at 11:52:48 am'!"Change Set:		153SoundDiscarding-smaDate:			18 June 2000Author:			Stefan Matthias AustFixed, actually enabled discarding of sound and speech (which obviously requires sound).  This was only part of method #discardFor2Point7.This might still leave dangling references in Morphic."!!KlattSynthesizer class methodsFor: 'class initialization' stamp: 'sma 6/18/2000 11:22'!obsolete	Smalltalk removeKey: #KlattResonatorIndices ifAbsent: [].	super obsolete! !!Project methodsFor: 'file in/out' stamp: 'sma 6/16/2000 23:43'!beep	(PluckedSound pitch: 261.625*4 dur: 1 loudness: 0.1) play! !!Project methodsFor: 'file in/out' stamp: 'sma 6/16/2000 23:43'!storeSomeSegment	"Try all projects to see if any is ready to go out.  Send at most three of them.	Previous one has to wait for a garbage collection before it can go out."	| cnt pList start proj gain |	cnt _ 0.  gain _ 0.	pList _ Project allInstances.	start _ pList size atRandom.	"start in a random place"	start to: pList size + start do: [:ii | 		proj _ pList atWrap: ii.		proj storeSegment ifTrue: ["Yes, did send its morphs to the disk"			gain _ gain + (proj projectParameters at: #segmentSize 						ifAbsent: [0]).	"a guess"			self beep.			(cnt _ cnt + 1) >= 2 ifTrue: [^ gain]]].	self beep.	^ gain! !!Project methodsFor: 'file in/out' stamp: 'sma 6/16/2000 23:43'!storeToMakeRoom	"Write out enough projects to fulfill the space goals.	Include the size of the project about to come in."	| params memoryEnd goalFree cnt gain proj skip tried |	GoalFreePercent ifNil: [GoalFreePercent _ 33].	GoalNotMoreThan ifNil: [GoalNotMoreThan _ 20000000].	params _ Smalltalk getVMParameters.	memoryEnd	_ params at: 3."	youngSpaceEnd	_ params at: 2.	free _ memoryEnd - youngSpaceEnd."	goalFree _ GoalFreePercent asFloat / 100.0 * memoryEnd.	goalFree _ goalFree min: GoalNotMoreThan.	world isInMemory ifFalse: ["enough room to bring it in"		goalFree _ goalFree + (self projectParameters at: #segmentSize ifAbsent: [0])].	cnt _ 30.	gain _ Smalltalk garbageCollectMost.	"skip a random number of projects that are in memory"	proj _ self.  skip _ 6 atRandom.	[proj _ proj nextInstance ifNil: [Project someInstance].		proj world isInMemory ifTrue: [skip _ skip - 1].		skip > 0] whileTrue.	cnt _ 0.  tried _ 0.	[gain > goalFree] whileFalse: [		proj _ proj nextInstance ifNil: [Project someInstance].		proj storeSegment ifTrue: ["Yes, did send its morphs to the disk"			gain _ gain + (proj projectParameters at: #segmentSize 						ifAbsent: [20000]).	"a guess"			self beep.			(cnt _ cnt + 1) > 5 ifTrue: [^ self]].	"put out 5 at most"		(tried _ tried + 1) > 23 ifTrue: [^ self]].	"don't get stuck in a loop"! !!SystemDictionary methodsFor: 'shrinking' stamp: 'sma 6/18/2000 11:52'!discardSoundAndSpeech	"NOTE: This leaves 26 references to obsolete classes, one in SystemDictionary	class>>initialize, one in ImageSegment>>restoreEndianness, one in DataStream	class>>initialize and 23 in Morphic and Flash classes."	SystemOrganization removeCategoriesMatching: 'Sound-*'.	SystemOrganization removeCategoriesMatching: 'Speech-*'.	Smalltalk removeClassNamed: #KlattSynthesizerPlugin.	Smalltalk removeSelector: #(DigitalSignatureAlgorithm randomBitsFromSoundInput:).	Smalltalk removeSelector: #(Project beep).	Preferences setPreference: #soundsEnabled toValue: false! !!SystemDictionary methodsFor: 'shrinking' stamp: 'sma 6/18/2000 11:34'!removeSelector: descriptor	"Safely remove a selector from a class (or metaclass). If the class	or the method doesn't exist anymore, never mind and answer nil.	This method should be used instead of 'Class removeSelector: #method'	to omit global class references."	| class sel |	class _ Smalltalk at: descriptor first ifAbsent: [^ nil].	(descriptor size > 2 and: [descriptor second == #class])		ifTrue:			[class _ class class.			sel _ descriptor third]		ifFalse: [sel _ descriptor second].	^ class removeSelector: sel! !