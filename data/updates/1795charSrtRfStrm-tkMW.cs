'From Squeak2.7 of 5 January 2000 [latest update: #1788] on 17 January 2000 at 11:29:38 am'!"Change Set:		charSrtRfStrm-tkMWDate:			17 January 2000Author:			Ted KaehlerDataStreams need to return existing Characters in target system, not new instances of class Character."!!Character methodsFor: 'object fileIn' stamp: 'tk 1/17/2000 11:27'!comeFullyUpOnReload: smartRefStream	"Use existing an Character.  Don't use the new copy."	^ self class value: value! !