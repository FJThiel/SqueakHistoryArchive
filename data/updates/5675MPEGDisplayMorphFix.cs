'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 25 June 2003 at 6:35:01 pm'!"Change Set:		MPEGDisplayMorphFixDate:			25 June 2003Author:			Masashi UmezawaMPEGDisplayMorph failed to play MPEG files if pathnames of the files were changed between snapshots.Now, MPEGDisplayMorph asks new pathnames if it encounters such situation."!!MPEGDisplayMorph methodsFor: 'accessing' stamp: 'mu 6/25/2003 02:41'!moviePosition	"Answer a number between 0.0 and 1.0 indicating the current position within the movie."	mpegFile ifNil: [^ 0.0].	mpegFile fileHandle ifNil: [^ 0.0].	(FileStream isAFileNamed: mpegFile fileName) ifFalse: [^0.0].	mpegFile hasVideo		ifTrue: [^ ((mpegFile videoGetFrame: 0) asFloat / (mpegFile videoFrames: 0)) min: 1.0].	soundTrack ifNotNil: [^ soundTrack soundPosition].	^ 0.0! !!MPEGDisplayMorph methodsFor: 'commands' stamp: 'mu 6/25/2003 02:42'!startPlaying	"Start playing the movie at the current position."	| frameIndex |	self stopPlaying.	self mpegFileIsOpen ifFalse: [^ self].	(FileStream isAFileNamed: mpegFile fileName) ifFalse: [ | newFileResult newFileName |		self inform: 'Path changed. Enter new one for: ', (FileDirectory localNameFor: mpegFile fileName).		newFileResult _ StandardFileMenu oldFile.		newFileName _ newFileResult directory fullNameFor: newFileResult name.			mpegFile openFile: newFileName].		mpegFile hasAudio		ifTrue: [			mpegFile hasVideo ifTrue: [				"set movie frame position from soundTrack position"				soundTrack reset.  "ensure file is open before positioning"				soundTrack soundPosition: (mpegFile videoGetFrame: 0) asFloat / (mpegFile videoFrames: 0).				"now set frame index from the soundtrack position for best sync"				frameIndex _ ((soundTrack millisecondsSinceStart * desiredFrameRate) // 1000).				frameIndex _ (frameIndex max: 0) min: ((mpegFile videoFrames: 0) - 3).				mpegFile videoSetFrame: frameIndex stream: 0].			SoundPlayer stopReverb.			soundTrack volume: volume.			soundTrack repeat: repeat.			soundTrack resumePlaying.			startFrame _ startMSecs _ 0]		ifFalse: [			soundTrack _ nil.			startFrame _ mpegFile videoGetFrame: 0.			startMSecs _ Time millisecondClockValue].	running _ true.! !