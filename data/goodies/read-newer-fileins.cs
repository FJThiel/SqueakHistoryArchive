"read-newer-fileins.cs
Allow systems older than 1.3 to read and ignore the styles text in fileOuts from 1.3 and later.

        AUTHOR          Ted Kaehler (TedK@WDI.Disney.com)
        VERSION         any before 1.3
        PREREQUISITES   none
        DISTRIBUTION    world
        VERSION DATE    January 15, 1998"!

'From Squeak 1.23 of Oct 18, 1997 on 29 December 1997 at 10:25:41 pm'!

ClassCategoryReader subclass: #ClassCommentReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!ClassCommentReader methodsFor: 'as yet unclassified' stamp: 'tk 12/29/97 22:25'!
scanFrom: aStream 
	"File in the class comment from aStream.  Not string-i-fied, just a text, exactly as it is in the browser.  Move to changes file."

	| |
	class theNonMetaClass comment: (aStream nextChunk).
		"Writes it on the disk and saves a RemoteString ref"
! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'tk 12/13/97 14:21'!
commentStamp: changeStamp prior: indexAndOffset
	"Prior source link ignored when filing in."

	^ ClassCommentReader new setClass: self
				category: #Comment
				changeStamp: changeStamp
! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'tk 12/29/97 22:04'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| terminator out ch pos |
	terminator _ $!!.
	out _ WriteStream on: (String new: 1000).
	self skipSeparators.
	[(ch _ self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				pos _ self position.
				self skipSeparators.
				(self next: 7) = ']style['
					ifTrue: [self nextChunk]		"absorb the style chunk"
					ifFalse: [self position: pos].
				^ out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ out contents! !


