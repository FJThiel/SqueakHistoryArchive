'From Squeak3.9alpha of 4 July 2005 [latest update: #7006] on 9 March 2006 at 4:53:12 pm'!"Change Set:		7007updateFromMCDate:			09 March 2006Author:			Marcus Denker	- pretty print use := by default	0003249: Mac VM pops up debugger on every launch (InternetConfiguration broken?)	0003254: faster #binding	0003258: [ENH] Entry in Fonts menu for easily getting all big fonts	0003150: In 7002 Asking for priorVersions causes browsing some primitive methods 			to get a DNU	0003268: NumberTest>>testPrintShowingDecimalPlaces: broken		"!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://source.squeakfoundation.org/39a'                user: ''                password: ''.(repository loadVersionFromFileNamed: 'ScriptLoader-md.149.mcz') load.ScriptLoader new updateFrom7006. !