'From Squeak2.8alpha of 12 April 2000 [latest update: #2006] on 1 May 2000 at 7:32:01 am'!!StrikeFont methodsFor: 'file in/out' stamp: 'di 5/1/2000 07:31'!objectForDataStream: refStrm	"I am about to be written on an object file.  Write a reference to a known Font in the other system instead.  "	"A path to me"	(TextConstants at: #forceFontWriting ifAbsent: [false]) ifTrue: [^ self].		"special case for saving the default fonts on the disk.  See collectionFromFileNamed:"	^ DiskProxy global: #StrikeFont selector: #familyName:size:emphasized:			args: (Array with: self familyName   with: self height					with: self emphasis).! !