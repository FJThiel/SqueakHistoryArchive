'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 9 March 2004 at 4:14:35 pm'!!TTCFont class methodsFor: 'class initialization' stamp: 'yo 3/9/2004 15:56'!initialize"	self initialize"	| tt |	Smalltalk addToShutDownList: TTCFont.	tt _ TTFontDescription default.	tt ifNotNil: [self newTextStyleFromTT: tt].	(FileList respondsTo: #registerFileReader:) ifTrue: [		FileList registerFileReader: self	].! !!TTCFont class methodsFor: 'class initialization' stamp: 'yo 3/9/2004 15:56'!shutDown	self recreateCache.! !TTCFont initialize!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."TTCFont initialize.!