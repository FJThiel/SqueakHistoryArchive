'From Squeak 2.3 of January 14, 1999 on 23 March 1999 at 9:36:47 pm'!"Change Set:		categoryFixes-swDate:			23 March 1999Author:			Scott WallaceFixes two bugs reported on 3/23/99 by Chris Norton:(1)  When you used the 'change category' feature in a browser to change the category of a method from 'as yet unclassified' to something else, it could drop you into a debugger if the change left no items remaining unclassified.(2)  Attempts to move a classified method back into the 'as yet classified' category were not being honored."!!Browser methodsFor: 'initialize-release' stamp: 'sw 3/23/1999 16:07'!methodCategoryChanged	self changed: #messageCategoryList.	self changed: #messageList.	self messageListIndex: 0! !!Browser methodsFor: 'message list' stamp: 'sw 3/23/1999 21:36'!messageList	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!), it is reset to zero."	^ messageCategoryListIndex = 0		ifTrue:			[Array new]		ifFalse:			[(self classOrMetaClassOrganizer  listAtCategoryNumber: messageCategoryListIndex)				ifNil: [messageCategoryListIndex _ 0.  Array new]]! !!ClassOrganizer methodsFor: 'accessing' stamp: 'sw 3/23/1999 15:58'!listAtCategoryNumber: anInteger 	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."	| firstIndex lastIndex |	firstIndex _ 		(anInteger > 1			ifTrue: [categoryStops at: anInteger - 1]			ifFalse: [0])		+ 1.	(categoryStops size < anInteger) ifTrue:		[^ nil].  "It can happen, if Default category got aggressively removed by some automatic operation"	lastIndex _ categoryStops at: anInteger.	^elementArray copyFrom: firstIndex to: lastIndex! !!ClassOrganizer methodsFor: 'compiler access' stamp: 'sw 3/23/1999 17:02'!classify: element under: heading suppressIfDefault: aBoolean	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"	| catName catIndex elemIndex realHeading |	((heading = NullCategory) or: [heading == nil])		ifTrue: [realHeading _ Default]		ifFalse: [realHeading _ heading asSymbol].	(catName _ self categoryOfElement: element) = realHeading		ifTrue: [^ self].  "done if already under that category"	catName ~~ nil ifTrue: 		[(aBoolean and: [realHeading = Default])				ifTrue: [^ self].	  "return if non-Default category already assigned in memory"		self removeElement: element].	"remove if in another category"	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].	catIndex _ categoryArray indexOf: realHeading.	elemIndex _ 		catIndex > 1			ifTrue: [categoryStops at: catIndex - 1]			ifFalse: [0].	[(elemIndex _ elemIndex + 1) <= (categoryStops at: catIndex) 		and: [element >= (elementArray at: elemIndex)]] whileTrue.	"elemIndex is now the index for inserting the element. Do the insertion before it."	elementArray _ elementArray copyReplaceFrom: elemIndex to: elemIndex-1						with: (Array with: element).	"add one to stops for this and later categories"	catIndex to: categoryArray size do: 		[:i | categoryStops at: i put: (categoryStops at: i) + 1].	(self listAtCategoryNamed: Default) size = 0 ifTrue: [self removeCategory: Default]! !!ClassOrganizer methodsFor: 'compiler access' stamp: 'sw 3/23/1999 17:04'!classify: element under: heading 	self classify: element under: heading suppressIfDefault: true! !!ClassOrganizer methodsFor: 'method dictionary' stamp: 'sw 3/23/1999 17:04'!letUserReclassify: anElement	"Put up a list of categories and solicit one from the user.  Answer true if user indeed made a change, else false"	"ClassOrganizer organization letUserReclassify: #letUserReclassify:"	| currentCat newCat |	currentCat _ self categoryOfElement: anElement.	newCat _ self categoryFromUserWithPrompt: 'Choose Category (currently "', currentCat, '")'.	(newCat ~~ nil and: [newCat ~= currentCat])		ifTrue:			[self classify: anElement under: newCat suppressIfDefault: false.			^ true]		ifFalse:			[^ false]! !