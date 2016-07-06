'From Squeak2.9alpha of 13 June 2000 [latest update: #3395] on 2 February 2001 at 8:10:08 pm'!"Change Set:		SoundPlayerNamedPrimsDate:			2 February 2001Author:			tim@sumeru.stanford.eduThe sound stop prim method needs to not raise an error if the plugin is absent - doing so can break the user interrupt watcher, messup image saves and so on. This trivial fix could usefully be improved by checking to see if the SoundPlugin has even been loaded, since at the moment we try to load it simply to shut it down!!"!!SoundPlayer class methodsFor: 'private' stamp: 'tpr 2/2/2001 19:46'!primSoundStop	"Stop double-buffered sound output. Must not raise an error because it is used inside error handling and at system shutdown"	<primitive: 'primitiveSoundStop' module: 'SoundPlugin'>! !