'From Squeak2.7 of 5 January 2000 [latest update: #1789] on 6 February 2000 at 12:37:42 pm'!"Change Set:		1022MethodFinderEnh-hhDate:			20 January 2000Author:			Helge HorchAdds Cmd+n and Cmd+m shortcuts to method finder."!!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'sma 2/6/2000 11:42'!messageListKey: aChar from: view	"Respond to a command key. Handle (m) and (n) here,	else defer to the StringHolder behaviour."	aChar == $m ifTrue: [^ self implementors].	aChar == $n ifTrue: [^ self senders].	super messageListKey: aChar from: view! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'hh 1/20/2000 00:15'!selectorMenu: aMenu	^ aMenu labels:'senders (n)implementors (m)copy selector to clipboard'	lines: #()	selections: #(senders implementors copyName)! !