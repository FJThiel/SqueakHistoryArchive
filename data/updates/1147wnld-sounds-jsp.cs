'From Squeak 2.3 of January 14, 1999 on 22 April 1999 at 11:31:56 am'!"Change Set:		wnld-sounds-jspDate:			22 April 1999Author:			Jeff PierceAdds the ability to play SampledSounds using actor playSounds:"!!WonderlandActor methodsFor: 'playing sound' stamp: 'jsp 4/22/1999 11:26'!playSound: soundFile	"Create an animation that plays the sound and lasts the duration of the sound"	| aSound extension |	extension _ (soundFile findTokens: '.') last.	(extension = 'wav')		ifTrue: [ [ aSound _ SampledSound fromWaveFileNamed: soundFile ]					ifError: [ :msg : rcvr | myWonderland reportErrorToUser:										(soundFile asString) , ' is not a valid sound.'.											^ nil ]				]		ifFalse: [ aSound _ SampledSound soundNamed: soundFile.				  aSound ifNil: [ ^ nil ] ].	^ myWonderland doTogether: {				self do: [ aSound play ].				self wait: (aSound duration)								}.! !