'From Squeak3.9alpha of 4 July 2005 [latest update: #6708] on 14 January 2006 at 1:46:04 pm'!"Change Set:		6709VMMakerAndTraitsFixesDate:			14 January 2006Author:			stephane ducassetraits fixesTIM VMMaker related fixes:		6669VMM38-64bit-imageUpdates.1.cs		6669VMM38-gc-instrument-image.1.cs		LocalePluginAddins.1.cs		RemoveLeftoverVMMbits-38b4.1.cs		WideStringPrimKill.1.cs"!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://source.squeakfoundation.org/39a'                user: ''                password: ''.(repository loadVersionFromFileNamed: 'ScriptLoader-stephaneducasse.70.mcz') load.ScriptLoader new updateFrom6708!