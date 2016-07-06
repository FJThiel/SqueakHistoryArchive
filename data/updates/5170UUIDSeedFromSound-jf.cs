'From Squeak3.5alpha of ''7 January 2003'' [latest update: #5169] on 8 February 2003 at 12:28:13 am'!"Change Set:		UUIDSeedFromSound-jfDate:			2 February 2003Author:			Julian Fitzell and Doug WayUUIDGenerator>>makeSeedFromSound uses #ifCurtailed: to catch any errors.On Windows, when you remove the sound stuff from the image, an error is generated but doesn't trigger the error block.  As a result you can't get a UUID.(Modified by Doug Way so that the call to #makeSeedFromSound is completely commented out of #makeSeed, since its reliability is unproven.  Julian's fix is still in the method, though, if needed.)Tim Rowledge said on the mailing list:    ifCurtailed: only runswhen the Process is terminated!!    Sigh. I provided a 3.4b change to do this for the makeUnixSeed but never    spotted the sound one."!!UUIDGenerator methodsFor: 'random seed' stamp: 'dew 2/8/2003 00:28'!makeSeed	"Try various methods of getting good seeds"	| seed |	seed := self makeUnixSeed.	seed ifNotNil: [^seed].	"not sure if this is reliably random... commented out for now. -dew"	"seed := self makeSeedFromSound.	seed ifNotNil: [^seed]."		"default"	[seed := (Time millisecondClockValue bitAnd: 16r3FFFFFFF) bitXor: self hash.	seed := seed bitXor: (Time totalSeconds bitAnd: 16r3FFFFFFF).	seed = 0] whileTrue: ["Try again if ever get a seed = 0"].	^seed! !!UUIDGenerator methodsFor: 'random seed' stamp: 'JF 2/2/2003 21:31'!makeSeedFromSound	| answer |	[answer := DigitalSignatureAlgorithm new randomBitsFromSoundInput: 32	] ifError: [answer := nil].	^answer! !