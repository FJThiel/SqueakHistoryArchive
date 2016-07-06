'From Squeak3.0 of 4 February 2001 [latest update: #3512] on 16 February 2001 at 3:32:21 pm'!"Change Set:		NoCharsInSeg-tkDate:			16 February 2001Author:			Ted KaehlerForbids Characters from getting into the list of objects for an exported ImageSegment.  The characters end up in outPointers where they are fixed up when they are read in.  The problem was that new Character instances were being created."!!Character methodsFor: 'object fileIn' stamp: 'tk 2/16/2001 14:52'!objectForDataStream: refStrm	"I am being collected for inclusion in a segment.  Do not include Characters!!  Let them be in outPointers."	refStrm insideASegment		ifFalse: ["Normal use" ^ self]		ifTrue: ["recording objects to go into an ImageSegment"						"remove it from references.  Do not trace."			refStrm references removeKey: self ifAbsent: [].			^ nil]! !