'From Squeak3.1alpha of 7 March 2001 [latest update: #4173] on 3 August 2001 at 7:17:20 pm'!"Change Set:		emptyCategoryFix-nkDate:			3 August 2001Author:			Ned KonzThis fixes the problem where if you click on the method category or method panes of a new browser without selecting a class you get walkbacks."!!Browser methodsFor: 'accessing' stamp: 'nk 8/3/2001 19:15'!contents	"Depending on the current selection, different information is retrieved.	Answer a string description of that information. This information is the	method of the currently selected class and message."	| comment theClass latestCompiledMethod |	latestCompiledMethod _ currentCompiledMethod.	currentCompiledMethod _ nil.	editSelection == #none ifTrue: [^ ''].	editSelection == #editSystemCategories 		ifTrue: [^ systemOrganizer printString].	editSelection == #newClass 		ifTrue: [^ (theClass _ self selectedClass)			ifNil:				[Class template: self selectedSystemCategoryName]			ifNotNil:				[Class templateForSubclassOf: theClass category: self selectedSystemCategoryName]].	editSelection == #editClass 		ifTrue: [^ self selectedClassOrMetaClass ifNotNilDo: [ :c | c definitionST80: Preferences printAlternateSyntax not]].	editSelection == #editComment 		ifTrue: [(theClass _ self selectedClass) ifNil: [^ ''].				comment _ theClass comment.				comment size = 0				ifTrue: [^ 'This class has not yet been commented.']				ifFalse: [^ comment]].	editSelection == #hierarchy 		ifTrue: [^ self selectedClassOrMetaClass printHierarchy].	editSelection == #editMessageCategories 		ifTrue: [^ self classOrMetaClassOrganizer printString].	editSelection == #newMessage		ifTrue: [^ self selectedClassOrMetaClass ifNotNilDo: [ :c | c sourceCodeTemplate] ].	editSelection == #editMessage		ifTrue:			[self showingByteCodes ifTrue:				[^ (self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName) symbolic asText].			currentCompiledMethod _ latestCompiledMethod.			^ self selectedMessage].	self error: 'Browser internal error: unknown edit selection.'! !