'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 12 February 2004 at 12:56:50 pm'!"Change Set:		TTCfontNameWithPointSizeFix-nkDate:			12 February 2004Author:			Ned KonzFix for missing method."!!TTCFont methodsFor: '*connectors-fixes' stamp: 'nk 12/16/2003 09:21'!fontNameWithPointSize	^self name withoutTrailingDigits, ' ', self pointSize printString! !