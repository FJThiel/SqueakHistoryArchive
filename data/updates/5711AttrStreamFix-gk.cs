'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 13 February 2004 at 12:00:24 am'!"Change Set:		AttrStreamFix-gkDate:			12 February 2004Author:			G�ran KrampeAfter having looked at Boris Gaertner's fix for Scamper I agree with the override AttributeTextStream class>>new. No need to make any other changes to that class at this time. Just added a comment etc."!!AttributedTextStream class methodsFor: 'instance creation' stamp: 'gk 2/9/2004 18:50'!new	"For this class we override Stream class>>new since this	class actually is created using #new, even though it is a Stream."		^self basicNew initialize! !