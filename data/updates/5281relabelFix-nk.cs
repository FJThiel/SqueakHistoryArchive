'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5184] on 26 April 2003 at 11:32:30 am'!"Change Set:		relabelFix-nkDate:			26 April 2003Author:			Ned KonzThis fixes a problem experienced by some SystemWindows whose model is nil."!!SystemWindow methodsFor: 'label' stamp: 'nk 4/26/2003 11:31'!update: aSymbol	aSymbol = #relabel		ifTrue: [^ model ifNotNil: [ self setLabel: model labelString ] ].! !