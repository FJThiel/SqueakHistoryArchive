'From Squeak3.7-m17n of 30 June 2004 [latest update: #6] on 23 July 2004 at 1:02:47 pm'!"Change Set:		contentStreamFixDate:			23 July 2004Author:			Yoshiki OhshimaMake the default to MacRoman."!!ZipArchiveMember methodsFor: 'accessing' stamp: 'yo 7/23/2004 12:54'!contentStream	"Answer my contents as a string."	| s |	s _ MultiByteBinaryOrTextStream on: (String new: self uncompressedSize).	self extractTo: s.	s reset.	s converter: MacRomanTextConverter new.	^ s.! !