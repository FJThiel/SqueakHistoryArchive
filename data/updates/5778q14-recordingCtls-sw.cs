'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:54 pm'!
"Change Set:		q14-recordingCtls-sw
Date:			10 June 2003
Author:			Scott Wallace

Adapted from Squeakland update 0179recordingCtls-sw.
Integrated with 3.7a 3/3/04 by Scott Wallace -- one babel collision only.

Fixes the wording of the prompt for a sound name in the RecordingControlsMorph 'make tile' command,

Also guards against falling into debuggers when Morph, Tile, Trim, Show, or Play buttons are pressed before any recording has taken place."!


!RecordingControlsMorph methodsFor: 'button commands' stamp: 'sw 6/10/2003 12:59'!
makeSoundMorph

	| m |
	recorder verifyExistenceOfRecordedSound ifFalse: [^ self].

	recorder pause.
	m _ SoundEventMorph new sound: recorder recordedSound.
	self world firstHand attachMorph: m.
! !

!RecordingControlsMorph methodsFor: 'button commands' stamp: 'sw 3/3/2004 19:49'!
makeTile
	"Make a tile representing my sound.  Get a sound-name from the user by which the sound is to be known."

	| newStyleTile sndName tile |
	recorder verifyExistenceOfRecordedSound ifFalse: [^ self].
	recorder pause.
	newStyleTile _ true.
	newStyleTile
		ifTrue:
			[sndName _ FillInTheBlank
				request: 'Please name your new sound' translated
				initialAnswer: 'sound' translated.
			sndName isEmpty ifTrue: [^ self].

			sndName _ SampledSound unusedSoundNameLike: sndName.
			SampledSound
				addLibrarySoundNamed: sndName
				samples: recorder condensedSamples
				samplingRate: recorder samplingRate.
			tile _ SoundTile new literal: sndName]
		ifFalse:
			[tile _ InterimSoundMorph new sound: 
				(SampledSound
					samples: recorder condensedSamples
					samplingRate: recorder samplingRate)].

	tile bounds: tile fullBounds.
	tile openInHand! !

!RecordingControlsMorph methodsFor: 'button commands' stamp: 'sw 6/10/2003 12:59'!
playback
	"The user hit the playback button"

	recorder verifyExistenceOfRecordedSound ifFalse: [^ self].
	recorder pause.
	recorder playback.
! !

!RecordingControlsMorph methodsFor: 'button commands' stamp: 'sw 6/10/2003 12:59'!
show
	"Show my samples in a WaveEditor."

	| ed w |
	recorder verifyExistenceOfRecordedSound ifFalse: [^ self].
	recorder pause.
	ed _ WaveEditor new.
	ed data: recorder condensedSamples.
	ed samplingRate: recorder samplingRate.
	w _ self world.
	w activeHand
		ifNil: [w addMorph: ed]
		ifNotNil: [w activeHand attachMorph: ed].

! !

!RecordingControlsMorph methodsFor: 'button commands' stamp: 'sw 6/10/2003 12:59'!
trim
	"Show my samples in a GraphMorph."
	
	recorder verifyExistenceOfRecordedSound ifFalse: [^ self].
	recorder pause.
	recorder trim: 1400 normalizedVolume: 80.0.
! !


!SoundRecorder methodsFor: 'recording controls' stamp: 'sw 6/10/2003 12:34'!
hasRecordedSound
	"Answer whether the receiver currently has any recorded sound"

	^ self recordedSound notNil! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'sw 3/3/2004 19:49'!
verifyExistenceOfRecordedSound
	"If the receiver has a recorded sound, answer true; if not, put up an informer and answer false"

	^ self recordedSound
		ifNotNil:
			[true]
		ifNil:
			[self inform: 'please record a sound first' translated.
			false]! !

