﻿'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6497] on 6 December 2004 at 4:33:17 pm'!"Change Set:		MethodFinderCommentFix-mdDate:			6 December 2004Author:			Marcus DenkerFix for mantis #598:oshiki: -------- MethodFinder test2:   Somebody should tell me that the commented area, which doesn't seem to work, should be there or not. Marcus As the method was last touched in 1999 i'd say: remove it."!!MethodFinder methodsFor: 'initialize' stamp: 'md 12/6/2004 16:32'!test2: anArray	"look for bad association"	anArray do: [:sub |		sub class == Association ifTrue: [			(#('true' '$a' '2' 'false') includes: sub value printString) ifFalse: [				self error: 'bad assn'].			(#('3' '5.6' 'x' '''abcd''') includes: sub key printString) ifFalse: [				self error: 'bad assn'].		].		sub class == Array ifTrue: [			sub do: [:element | 				element class == String ifTrue: [element first asciiValue < 32 ifTrue: [						self error: 'store into string in data']].				element class == Association ifTrue: [					element value class == Association ifTrue: [						self error: 'bad assn']]]].		sub class == Date ifTrue: [sub year isInteger ifFalse: [				self error: 'stored into input date!!!!']].		sub class == Dictionary ifTrue: [				sub size > 0 ifTrue: [					self error: 'store into dictionary']].		sub class == OrderedCollection ifTrue: [				sub size > 4 ifTrue: [					self error: 'store into OC']].		].! !