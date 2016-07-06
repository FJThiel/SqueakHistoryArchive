'From Squeak3.4beta of ''1 December 2002'' [latest update: #5138] on 5 December 2002 at 11:19:05 am'!"Change Set:		macColonAgain-nkDate:			5 December 2002Author:			Ned KonzThe change set 5131macRelativeFNFix-nk introduced an incorrect result for the isAbsolute test on Mac filenames starting with a double colon. This change set removes the double colon logic from the isAbsolute test.And my prior version (update 5140) was still wrong, as it consulted the file system.With this CS the Mac isAbsolute logic is:If a path begins with a colon, it is relative.Otherwise,  If it contains a colon anywhere, it is absolute and the first component  is the volume name.  Otherwise,    It is relative."!!MacFileDirectory class methodsFor: 'class initialization' stamp: 'nk 12/5/2002 11:17'!isAbsolute: fileName	"Return true if the given fileName is absolute. The rules are:If a path begins with a colon, it is relative.Otherwise,  If it contains a colon anywhere, it is absolute and the first component is the volume name.  Otherwise,    It is relative."	^fileName first ~= $:		and: [ fileName includes: $: ]! !