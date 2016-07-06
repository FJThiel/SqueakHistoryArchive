"StringAdds.st
Adds many useful string crunching methods--stripTrailingSpaces, padTo:, breakIntoLinesOf:, stripCRs. cr.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    December 7, 1994"!

!Character methodsFor: 'converting'!
asString
	"Answer the receiver converted into a String."

	^String with: self! !

!Character methodsFor: 'testing'!
isWhite
	"Answer whether the receive is a space or a tab."

	^(self = $ "space") | (self = $	"tab")!

isCRlike
	"Answer whether the some kind of line end--cr, lf, ff, ^d, ^z, etc."

	| code |
	code := self asInteger.
	^(code = 13 "cr") | (code = 10 "lf") | (code = 12 "ff") 
		| (code = 0 "null") | (code = 26 "^z") | (code = 4 "^d")! !

!String methodsFor: 'accessing'!
includesString: subString
	"Answer whether the receiver includes the argument as a substring."

	^((self indexOfSubCollection: subString startingAt: 1) ~= 0)! !


!String methodsFor: 'converting'!
stripTrailingSpaces
	"Strip off all trailing spaces in lines in the receiver."
	"'123      
	123     ' stripTrailingSpaces"

	| copy line start stop space |
	copy := self.
	start := 1.
	stop := self size.
		[line := copy indexOf: Character cr startingAt: start ifAbsent: [nil].
		line isNil ifTrue: [line := stop].
		space := line - 1.
		space = 0
			ifFalse: [[(copy at: space) isWhite]
						whileTrue: [space := space - 1].
				copy := copy copyReplaceFrom: (space + 1) to: (line -1) with: ''.
				stop  :=  copy size].
		start := line + 1.
		line = stop] whileFalse.
	^copy!

padTo: num
	"Answer a string padded to num characters with spaces."
	"'1234' padTo: 8 ==>  '1234    '"

	num < self size ifTrue: [^self clipTo: num].
	^self, (self class new: (num - self size) withAll: $ )!

breakIntoLinesOf: width
	"Break the receiver into lines of width 'width' or less."
" 'Print this entire comment to test it--Break the receiver into lines of width 40 or less. Break the receiver into lines of width 40 or less.'breakIntoLinesOf: 40"

	| reader breaks leftPos lastPos cr cArray isBreak |
	cArray := self copy.
	reader := ReadStream on: cArray.
	breaks := OrderedCollection new: cArray size // width.
	leftPos := lastPos := reader position.
	[reader atEnd] whileFalse:		"Collect line breaks"
		[isBreak := false.
		[(reader atEnd) or: [isBreak]]
			whileFalse: [isBreak := reader next isSeparator].
		reader atEnd
			ifFalse: [reader peek isCRlike
				ifTrue: [leftPos := lastPos := reader position + 1]
				ifFalse: [reader position + 1 - leftPos > width
						ifTrue: ["add a break"
							breaks add: lastPos.
							leftPos := lastPos + 1].
					"update lastPos" 
					lastPos := reader position]]].
	cr := Character cr.					"Now add breaks"
	breaks do:
		[ :break | 
		cArray at: break put: cr].
	^cArray!

stripCRs
	"Unwrap the lines of the receiver; answer a new copy."
	"'mumble
baz 
bar 
blech' stripCRs"

	| space reader writer ch prevCh |
	space := $ .
	prevCh := space.
	reader := ReadStream on: self.
	writer := WriteStream on: (self class new: self size).
	[reader atEnd]
		whileFalse: [ch := reader next.
			ch isCRlike
				ifTrue: [prevCh isSeparator
					ifFalse: [writer nextPut: space]
					ifTrue: [prevCh isCRlike
						ifTrue: [writer nextPut: ch]]]
				ifFalse: [writer nextPut: ch].
			prevCh := ch].
	^writer contents! !

!String class methodsFor: 'instance creation'!
cr
	"Answer a string with the carriage-return character."

	^self with: Character cr!

crtab
	"Answer a string with the carriage-return character."

	^self with: Character cr with: Character tab! !
