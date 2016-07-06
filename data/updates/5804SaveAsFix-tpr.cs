'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 6 March 2004 at 8:29 pm'!"Change Set:		3-7-5764-saveasDate:			6 March 2004Author:			tim@sumeru.stanford.eduYoshiki noticed that with the revised saveAs code in 3.7-5764 there could be odd problems if the path include a directory with a name like foo.bar - the FileDirectory class>baseNameFor: code would then strip too much from the filename ifthe user simply entered 'blim' as an image name. Instead of getting   'ADFS::HardDisc4/$/Squeak/sq37.foo/blim.image' you would get 'ADFS::HardDisc4/$/Squeak/sq37.image' which isn't quite the desired effect.This change alters baseNameFor: to split the path and only trim any extension from the leaf. Note that a) there are users of baseNameFor: that might need alteration or that can be simplifiedb) this still leaves huge amounts of crap alive in the file naming code. Still, one down, six trillion to go."!!FileDirectory class methodsFor: 'name utilities' stamp: 'tpr 3/6/2004 20:18'!baseNameFor: fileName	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"	"The test is 		FileDirectory baseNameFor: ((FileDirectory default directoryNamed: 'foo.bar') fullNameFor:'blim.blam') 		should end 'foo.bar/blim' (or as appropriate for your platform AND		FileDirectory baseNameFor: ((FileDirectory default directoryNamed: 'foo.bar') fullNameFor:'blim')		should be the same and NOT  'foo'		Oh, and FileDirectory baseNameFor: 'foo.bar' should be 'foo' not '/foo' "	| delim i leaf |	self splitName: fileName to: [:path : fn|				delim _ DirectoryClass extensionDelimiter.		i _ fn findLast: [:c | c = delim].		leaf _ i = 0			ifTrue: [fn]			ifFalse: [fn copyFrom: 1 to: i - 1].		path isEmpty ifTrue:[^leaf].		^path, self slash, leaf]! !