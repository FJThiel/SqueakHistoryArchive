'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 5 June 2000 at 8:06:03 pm'!"Change Set:		126PackBrowserFix-akDate:			4 June 2000Author:			Andreas KuckartzClasses with categories which do not contain a hyphen were not displayed in the class list.TODO: The switches do not work as expected for such classes."!!PackageBrowser methodsFor: 'class list' stamp: 'ak 6/4/2000 09:57'!classList	"Answer an array of the class names of the selected category. Answer an 	empty array if no selection exists."	^systemCategoryListIndex = 0		ifTrue: [			self systemCategoryList isEmpty				ifTrue: [systemOrganizer listAtCategoryNumber: (systemOrganizer categories indexOf: self package asSymbol)]				ifFalse: [Array new]]		ifFalse: [systemOrganizer listAtCategoryNumber:			(systemOrganizer categories indexOf: self selectedSystemCategoryName asSymbol)]! !!PackageBrowser methodsFor: 'class list' stamp: 'ak 6/4/2000 09:07'!selectedClass	"Answer the class that is currently selected. Answer nil if no selection 	exists."	| name envt |	(name _ self selectedClassName) ifNil: [^ nil].	"(envt _ self selectedEnvironment) ifNil: [^ nil]."	envt_(Smalltalk environmentForCategory: self selectedSystemCategoryName).	^ envt at: name! !