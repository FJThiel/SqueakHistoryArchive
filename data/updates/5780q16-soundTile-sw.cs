'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:54 pm'!
"Change Set:		q16-soundTile-sw
Date:			24 November 2003
Author:			Scott Wallace

Adapted for 3.7a from Squeakland 0186soundImprovements-sw on 4 March 2004; several integration issues encountered because of the recent addition of thte SoundService architecture.

When the user clicks on the name of a sound on either a SoundTile or a SoundReadoutTile, a pop-up of available sounds is now presented, from whence the user can choose one.  This can be faster and more direct (and quieter!!) than cycling through the available sounds one by one.  Note that this restores an earlier behavior that somehow got lost a couple of years ago.

Note that one method from the original Squeakland update has been moved to the preserveSounds update in this same batch."!


!SoundReadoutTile commentStamp: 'sw 11/24/2003 15:25' prior: 0!
A tile comprising a readout for a sound-valued instance variable in a Viewer.  It sports up/down  arrows, and a click on the sound name results in a pop-up menu, offering the user the opportunity to choose a new one.!


!AbstractSoundSystem methodsFor: 'misc' stamp: 'sw 3/4/2004 02:40'!
soundNameFromUser
	"Pop up a list of available sound names and answer the one the user chooses, or nil if no choice made"

	^ (SelectionMenu selections: self sampledSoundChoices asSortedArray) startUpWithCaption: 'Sounds' translated

"
SoundService default soundNameFromUser
"! !


!SoundReadoutTile methodsFor: 'arrows' stamp: 'sw 11/24/2003 14:54'!
handlerForMouseDown: anEvent
	"Return the (prospective) handler for a mouse down event. The handler is temporarily installed and can be used for morphs further down the hierarchy to negotiate whether the inner or the outer morph should finally handle the event"

	^ ((self findA: UpdatingStringMorph) bounds containsPoint: anEvent cursorPoint)
		ifTrue:
			[self]
		ifFalse:
			[super handlerForMouseDown: anEvent]! !

!SoundReadoutTile methodsFor: 'arrows' stamp: 'sw 3/4/2004 02:42'!
mouseDown: evt
	"Handle a mouse down event"

	| aPoint index isUp soundChoices adjustment |
	upArrow ifNotNil: [((isUp _ upArrow containsPoint: (aPoint _ evt cursorPoint)) or:  [downArrow containsPoint: aPoint])
		ifTrue:
			[soundChoices _ SoundService default sampledSoundChoices.
			index _ soundChoices indexOf: literal ifAbsent: [1].
			index > 0 ifTrue:
				[adjustment _ isUp ifTrue: [1] ifFalse: [-1].
				self literal: (soundChoices atWrap: (index + adjustment))].
			self playSoundNamed: literal.
			^ self]].
	SoundService default soundNameFromUser ifNotNilDo:
		[:aSoundName |
			self literal: aSoundName.
			self playSoundNamed: literal]! !

!SoundReadoutTile methodsFor: 'arrows' stamp: 'sw 11/24/2003 14:59'!
setLiteral: aLiteral
	super  setLiteral: aLiteral.
	(self findA: UpdatingStringMorph) lock! !

!SoundReadoutTile methodsFor: 'private' stamp: 'sw 11/24/2003 15:12'!
updateLiteralLabel
	"Update the wording emblazoned on the tile, if needed"

	super updateLiteralLabel.
	(self findA: UpdatingStringMorph)  lock! !


!SoundTile methodsFor: 'user interface' stamp: 'sw 3/4/2004 02:42'!
mouseDown: evt
	"Process a mouseDown event"
	
	| aPoint index isUp soundChoices adjustment |
	upArrow ifNotNil: [((isUp _ upArrow containsPoint: (aPoint _ evt cursorPoint)) or:  [downArrow containsPoint: aPoint])
		ifTrue:
			[soundChoices _ #('silence').  "default, if no SampledSound class"
			soundChoices _  SoundService default sampledSoundChoices.
			index _ soundChoices indexOf: literal ifAbsent: [1].
			index > 0 ifTrue:
				[adjustment _ isUp ifTrue: [1] ifFalse: [-1].
				self literal: (soundChoices atWrap: (index + adjustment))].
			self playSoundNamed: literal.
			^ self]].
	SoundService default soundNameFromUser ifNotNilDo:
		[:aSoundName |
			self literal: aSoundName.
			self playSoundNamed: literal]! !

!SoundTile methodsFor: 'mouse handling' stamp: 'sw 11/24/2003 14:44'!
handlerForMouseDown: anEvent
	"Return the (prospective) handler for a mouse down event. The handler is temporarily installed and can be used for morphs further down the hierarchy to negotiate whether the inner or the outer morph should finally handle the event"

	^ ((self findA: UpdatingStringMorph) bounds containsPoint: anEvent cursorPoint)
		ifTrue:
			[self]
		ifFalse:
			[super handlerForMouseDown: anEvent]! !

