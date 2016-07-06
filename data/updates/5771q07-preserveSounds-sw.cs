'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:53 pm'!
"Change Set:	q07-preserveSounds-sw
Date:			14 April 2003
Author:			Scott Wallace
			
Adapted for 3.7a on 4 March 2004 from Squeakland updates 0160preserveSounds-sw and 0164bareSoundTile-sw and one method also plucked out from 0186soundImprovements-sw.  Note that also essential is ProjectLoading.openName: stream: fromDirectory:withProjectView:, whose code, integrated with the sound-preservation code of this update, will arrive in the next update, securityEtc-mir.

Writes out relevant additions to the sound library along with a saved project, so that sounds used in a project in one image and referenced in its scripts or in any instance variable well be accessible in images into which the saved project is subsequently loaded.

Extends the sound-preservation logic so that sounds referenced by bare sound tiles visible in the project are also preserved -- formerly, they had to have been used somewhere in the etoy structures of the project."!


!PasteUpMorph methodsFor: 'misc' stamp: 'sw 11/23/2003 03:46'!
prepareToBeSaved
	"Prepare for export via the ReferenceStream mechanism"

	| exportDict soundKeyList players |
	super prepareToBeSaved.
	turtlePen _ nil.
	self isWorldMorph
		ifTrue: [soundKeyList _ Set new.
			(players _ self presenter allExtantPlayers)
				do: [:aPlayer | aPlayer slotInfo
						associationsDo: [:assoc | assoc value type == #Sound
								ifTrue: [soundKeyList
										add: (aPlayer instVarNamed: assoc key)]]].
			players
				do: [:p | p allScriptEditors
						do: [:e | (e allMorphs
								select: [:m | m isKindOf: SoundTile])
								do: [:aTile | soundKeyList add: aTile literal]]].
			(self allMorphs
				select: [:m | m isKindOf: SoundTile])
				do: [:aTile | soundKeyList add: aTile literal].
			soundKeyList removeAllFoundIn: SampledSound universalSoundKeys.
			soundKeyList
				removeAllSuchThat: [:aKey | (SampledSound soundLibrary includesKey: aKey) not].
			soundKeyList isEmpty
				ifFalse: [exportDict _ Dictionary new.
					soundKeyList
						do: [:aKey | exportDict
								add: (SampledSound soundLibrary associationAt: aKey)].
					self setProperty: #soundAdditions toValue: exportDict]]! !


!SampledSound class methodsFor: 'sound library' stamp: 'sw 4/14/2003 00:01'!
assimilateSoundsFrom: aDictionary
	"assimilate sounds with new keys from the given dictionary"

	aDictionary associationsDo:
		[:assoc | (SoundLibrary includesKey: assoc key) ifFalse:
			[SoundLibrary add: assoc]]! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 4/13/2003 20:58'!
universalSoundKeys
	"Answer a list of the sound-names that are expected to be found in the SoundLibrary of every image."

	^ #('splash' 'peaks' 'clink' 'croak' 'scratch' 'chirp' 'scritch' 'warble' 'scrape' 'camera' 'coyote' 'silence' 'motor')

! !

