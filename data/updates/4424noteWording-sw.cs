'From Squeak3.2alpha of 2 October 2001 [latest update: #4423] on 6 October 2001 at 2:13:21 pm'!"Change Set:		noteWording-swDate:			6 October 2001Author:			Scott WallaceFixes up a flaw in the logic for reformulating etoy vocabularies when any #additionsToViewerCategories method is recompiled.  The flaw was that any artificially-imposed wording override provided in an edit to #wordingForOperator: may well *not* yet be filed in at the time the reformulation takes place.  The workaround is to reformulate the etoy vocabularies upon recompilation of #wordingForOperator."!!StandardScriptingSystem class methodsFor: 'class initialization' stamp: 'sw 10/6/2001 14:10'!noteCompilationOf: aSelector meta: isMeta	aSelector == #wordingForOperator: ifTrue:		[Vocabulary changeMadeToViewerAdditions]! !"Postscript:"Vocabulary changeMadeToViewerAdditions.!