"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://source.squeakfoundation.org/39a'                user: ''                password: ''.(repository loadVersionFromFileNamed: 'ScriptLoader-sd.196.mcz') load.ScriptLoader new updateFrom7033!