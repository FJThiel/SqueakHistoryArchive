'From Squeak3.2alpha of 4 October 2001 [latest update: #4418] on 6 October 2001 at 12:38:05 pm'!"Change Set:		FontSetFixDate:			6 October 2001Author:			Jim BensonModifies FontSet>>compileFont: to allow for TextStyles greater than 5000 characters. Also appends the point size to the name of the font for class method creation so that multiple font sizes from TextStyles may be represented"!!FontSet class methodsFor: 'compiling' stamp: 'jlb 10/6/2001 12:16'!compileFont: strikeFont 	| tempName literalString header |	tempName _ 'FontTemp.sf2'.	strikeFont writeAsStrike2named: tempName.	literalString _ (FileStream readOnlyFileNamed: tempName) contentsOfEntireFile fullPrintString.	header _ 'sizeNNN	^ self fontNamed: ''NNN'' fromLiteral:' copyReplaceAll: 'NNN' with: ( strikeFont name , (strikeFont pointSize asString )).	self class		compile: header , literalString		classified: 'fonts'		notifying: nil.	FileDirectory default deleteFileNamed: tempName! !