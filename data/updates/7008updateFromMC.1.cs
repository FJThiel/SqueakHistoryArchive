'From Squeak3.9alpha of 4 July 2005 [latest update: #7007] on 14 March 2006 at 7:09:11 pm'!"Change Set:		7008updateFromMCDate:			14 March 2006Author:			Marcus Denker	Mantis-3061-SnapPop, fix by Jerome Peace (wiz)	Mantis-3075-MixedCurves, fix by Jerome Peace (wiz)	Mantis-2589-ButtonTargets, fix by Jerome Peace (wiz)	Mantis-2709-CurveBoundsTRFix, fix by Jerome Peace (wiz)	Mantis-2496-Halos-Menus, fix by Jerome Peace (wiz)	0003294: KeyedSet>>add: cand send #errorKeyAlreadyExists: which is not implemented	0003301: FileList has two install buttons	0003303: Browser --> open does allways open Browser (not SystemBrowser default)	"!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://source.squeakfoundation.org/39a'                user: ''                password: ''.(repository loadVersionFromFileNamed: 'ScriptLoader-md.150.mcz') load.ScriptLoader new updateFrom7007. !