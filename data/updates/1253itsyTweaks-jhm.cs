'From Squeak 2.4c of May 10, 1999 on 7 June 1999 at 11:36:08 am'!"Change Set:		itsyTweaks-jhmDate:			7 June 1999Author:			John MaloneyA few tweaks made to support the Itsy demo."!!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/4/1999 09:26'!majorScaleOn: aSound from: aPitch octaves: octaveCount	"(AbstractSound majorScaleOn: FMSound oboe1 from: #c2 octaves: 5) play"	| startingPitch pitches chromatic |	startingPitch _ aPitch isNumber		ifTrue: [aPitch]		ifFalse: [self pitchForName: aPitch].	pitches _ OrderedCollection new.	0 to: octaveCount - 1 do: [:i |		chromatic _ self chromaticPitchesFrom: startingPitch * (2 raisedTo: i).		#(1 3 5 6 8 10 12) do: [:j | pitches addLast: (chromatic at: j)]].	pitches addLast: startingPitch * (2 raisedTo: octaveCount).	^ self noteSequenceOn: aSound		from: (pitches collect: [:pitch | Array with: pitch with: 0.5 with: 300])! !!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 5/31/1999 14:09'!beUnlooped	scaledLoopLength _ 0.! !!SampledInstrument class methodsFor: 'instance creation' stamp: 'jm 6/7/1999 11:26'!buildSmallOrchestra	"Example of how to build a skeleton orchestra that uses less memory (about 14 MBytes)."	"SampledInstrument buildSmallOrchestra"	| dir |	AbstractSound unloadSampledTimbres.	dir _ 'Tosh:Not Backed Up:Sample Library:Orchestra'.	#(clarinet oboe bassoon trombone tympani) do: [:instName |		SampledInstrument			readSimpleInstrument: instName			fromDirectory: dir.		(AbstractSound soundNamed: instName, '-f') pruneToNotesPerOctave: 1].	#(flute bass) do: [:instName |		SampledInstrument			readSimpleInstrument: instName			fromDirectory: dir.		(AbstractSound soundNamed: instName, '-f') pruneToNotesPerOctave: 2].	(AbstractSound soundNamed: 'bass-f') allNotes do: [:n |		n firstSample: (n findStartPointForThreshold: 2500)].	(AbstractSound soundNamed: 'bassoon-f') allNotes do: [:n |		n beUnlooped.		n firstSample: (n findStartPointForThreshold: 0)].	(AbstractSound soundNamed: 'trombone-f') allNotes do: [:n |		n firstSample: (n findStartPointForThreshold: 1800)].	AbstractSound soundNamed: 'trumpet-f' put: (AbstractSound soundNamed: 'trombone-f').	AbstractSound soundNamed: 'horn-f' put: (AbstractSound soundNamed: 'trombone-f').	AbstractSound soundNamed: 'violin-f' put: (AbstractSound soundNamed: 'bass-f').	AbstractSound soundNamed: 'viola-f' put: (AbstractSound soundNamed: 'bass-f').	AbstractSound soundNamed: 'cello-f' put: (AbstractSound soundNamed: 'bass-f').	(AbstractSound soundNamed: 'bassoon-f') allNotes do: [:n | n beUnlooped].! !!SoundPlayer class methodsFor: 'player process' stamp: 'jm 6/7/1999 10:40'!startReverb	"Start a delay-line style reverb with the given tap delays and gains. Tap delays are given in samples and should be prime integers; the following comment gives an expression that generates primes."	"Integer primesUpTo: 22050"	UseReverb _ true.	ReverbState _ ReverbSound new		tapDelays: #(1601 7919) gains: #(0.12 0.07).! !!SoundPlayer class methodsFor: 'private' stamp: 'jm 6/7/1999 10:35'!startPlayingImmediately: aSound	"Private!! Start playing the given sound as soon as possible by mixing it into the sound output buffers of the underlying sound driver."	| totalSamples buf n leftover src rest |	"first, fill a double-size buffer with samples"	"Note: The code below assumes that totalSamples contains two	 buffers worth of samples, and the insertSamples primitive is	 expected to consume at least one buffer's worth of these	 samples. The remaining samples are guaranteed to fit into	 a single buffer."	totalSamples _ Buffer stereoSampleCount * 2.  "two buffer's worth"	buf _ SoundBuffer newStereoSampleCount: totalSamples.	aSound playSampleCount: totalSamples into: buf startingAt: 1.	ReverbState == nil ifFalse: [		ReverbState applyReverbTo: buf startingAt: 1 count: totalSamples].	PlayerSemaphore critical: [		"insert as many samples as possible into the sound driver's buffers"		n _ self primSoundInsertSamples: totalSamples			from: buf			samplesOfLeadTime: 1024.		leftover _ totalSamples - n.		"copy the remainder of buf into Buffer"		"Note: the following loop iterates over 16-bit words, not two-word stereo slices"		"assert: 0 < leftover <= Buffer stereoSampleCount"		src _ 2 * n.		1 to: 2 * leftover do:			[:dst | Buffer at: dst put: (buf at: (src _ src + 1))].		"generate enough additional samples to finish filling Buffer"		rest _ Buffer stereoSampleCount - leftover.		aSound playSampleCount: rest into: Buffer startingAt: leftover + 1.		ReverbState == nil ifFalse: [			ReverbState applyReverbTo: Buffer startingAt: leftover + 1 count: rest].		"record the fact that this sound has already been played into Buffer so that we don't process it again this time around"		SoundJustStarted _ aSound.		ActiveSounds add: aSound].! !!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jm 6/7/1999 11:35'!itsyVoltage	"On the Itsy, answer the approximate Vcc voltage. The Itsy will shut itself down when this value reaches 2.0 volts. This method allows one to build a readout of the current battery condition."	| n |	n _ self getSystemAttribute: 1200.	n ifNil: [^ 'no voltage attribute'].	^ ((n asNumber / 150.0) roundTo: 0.01) asString, ' volts'! !