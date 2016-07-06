'From Squeak 2.2 of Sept 23, 1998 on 19 November 1998 at 1:41:15 pm'!"Change Set:		Newmatch-diDate:			19 November 1998Author:			Dan IngallsThis changeSet defines a new version of <string> match: that takes advantage of the new string search primitives.  It retains support for #, and also the ability to backtrack from early false matches."!!String methodsFor: 'comparing' stamp: 'di 11/19/1998 13:37'!match: text	"Answer whether text matches the pattern in this string.	Matching ignores upper/lower case differences.	Where this string contains #, text may contain any character.	Where this string contains *, text may contain any sequence of characters."	^ self startingAt: 1 match: text startingAt: 1"	'*'			match: 'zort' true	'*baz'		match: 'mobaz' true	'*baz'		match: 'mobazo' false	'*baz*'		match: 'mobazo' true	'*baz*'		match: 'mozo' false	'foo*'		match: 'foozo' true	'foo*'		match: 'bozo' false	'foo*baz'	match: 'foo23baz' true	'foo*baz'	match: 'foobaz' true	'foo*baz'	match: 'foo23bazo' false	'foo'		match: 'Foo' true	'foo*baz*zort' match: 'foobazort' false	'foo*baz*zort' match: 'foobazzort' false	'*foo#zort'	match: 'afoo3zortthenfoo3zort' true	'*foo*zort'	match: 'afoodezortorfoo3zort' true"! !!String methodsFor: 'comparing' stamp: 'di 11/19/1998 13:28'!startingAt: keyStart match: text startingAt: textStart	"Answer whether text matches the pattern in this string.	Matching ignores upper/lower case differences.	Where this string contains #, text may contain any character.	Where this string contains *, text may contain any sequence of characters."	| anyMatch matchStart matchEnd i matchStr j ii jj |	i _ keyStart.	j _ textStart.	"Check for any #'s"	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].	(self at: i) = $#] whileTrue:		["# consumes one char of key and one char of text"		j > text size ifTrue: [^ false "no more text"].		i _ i+1.  j _ j+1].	"Then check for *"	(self at: i) = $*		ifTrue: [i = self size ifTrue:					[^ true "Terminal * matches all"].				"* means next match string can occur anywhere"				anyMatch _ true.				matchStart _ i + 1]		ifFalse: ["Otherwise match string must occur immediately"				anyMatch _ false.				matchStart _ i].	"Now determine the match string"	matchEnd _ self size.	(ii _ self indexOf: $* startingAt: matchStart) > 0 ifTrue:		[ii = 1 ifTrue: [self error: '** not valid -- use * instead'].		matchEnd _ ii-1].	(ii _ self indexOf: $# startingAt: matchStart) > 0 ifTrue:		[ii = 1 ifTrue: [self error: '*# not valid -- use #* instead'].		matchEnd _ matchEnd min: ii-1].	matchStr _ self copyFrom: matchStart to: matchEnd.	"Now look for the match string"	[jj _ text findString: matchStr startingAt: j caseSensitive: false.	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]		whileTrue:		["Found matchStr at jj.  See if the rest matches..."		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:			[^ true "the rest matches -- success"].		"The rest did not match."		anyMatch ifFalse: [^ false].		"Preceded by * -- try for a later match"		j _ j+1].	^ false "Failed to find the match string"! !String removeSelector: #xmatch:!