'From Squeak2.9alpha of 17 July 2000 [latest update: #3190] on 19 December 2000 at 1:09:23 pm'!"Change Set:		fixQueryMorphDate:			19 December 2000Author:			Bob Arning- another of those subtle alignment morph differences"!!SuperSwikiServer methodsFor: 'for real' stamp: 'RAA 12/19/2000 12:47'!showQueryAsPVM: resultStream	| answer gif currentRow whatToShow projectName fileName firstURL |"SuperSwikiServer testOnlySuperSwiki queryProjectsAndShow"	resultStream reset; nextLine.	answer _ AlignmentMorphBob1 newColumn		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		useRoundedCorners;		borderWidth: 8;		borderColor: Color blue;		color: Color paleBlue.	currentRow _ nil.	[resultStream atEnd] whileFalse: [		projectName _ resultStream nextLine.		fileName _ resultStream nextLine.		gif _ self oldFileOrNoneNamed: projectName,'.gif'.		gif ifNotNil: [gif _ GIFReadWriter formFromStream: gif].		(currentRow isNil or: [currentRow fullBounds width > 600]) ifTrue: [			currentRow _ answer addARow: {}		].		gif ifNil: [			gif _ (StringMorph contents: 'No GIF for ',projectName) imageForm		].		firstURL _ self url.		firstURL last == $/ ifFalse: [firstURL _ firstURL, '/'].		whatToShow _ ProjectViewMorph new			image: (gif asFormOfDepth: Display depth);			lastProjectThumbnail: gif;			setProperty: #SafeProjectName toValue: projectName;			project: (DiskProxy 				global: #Project 				selector: #namedUrl: 				args: {firstURL,fileName}			).		currentRow addMorphBack: (			answer inAColumn: {(answer inARow: {whatToShow})					borderWidth: 2; borderColor: Color gray; hResizing: #shrinkWrap}		).	].	currentRow ifNil: [		answer addARow: {			(StringMorph contents: 'No projects found for your criteria') color: Color red		}	].	answer openCenteredInWorld.! !