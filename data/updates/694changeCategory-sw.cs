'From Squeak 2.3 of January 14, 1999 on 25 February 1999 at 4:44:46 pm'!"Change Set:		changeCategory-swDate:			24 February 1999Author:			Scott WallaceAdds a UI for directly changing the category of any method.  -- look in the shifted side of any message-list menu for 'change category...'.Underlying support is now available for a similar feature for class category, but the list of categories would be long."!!ClassOrganizer reorganize!('accessing' categories categories: categoryOfElement: changeFromString: classComment classComment: commentRemoteStr commentStamp commentStamp: hasNoComment listAtCategoryNamed: listAtCategoryNumber: numberOfCategoryOfElement: removeElement: removeEmptyCategories)('compiler access' classify:under: classifyAll:under:)('method dictionary' addCategory: addCategory:before: letUserReclassify: removeCategory: renameCategory:toBe:)('printing' printOn:)('fileIn/Out' fileOutCommentOn:moveSource:toFile: moveChangedCommentToFile:numbered: objectForDataStream: putCommentOnFile:numbered:moveSource:forClass: scanFrom:)('private' categoryFromUserWithPrompt: setDefaultList:)('object fileIn' convertgcce0:gccec0:)!!ClassOrganizer methodsFor: 'method dictionary' stamp: 'sw 2/24/1999 14:49'!letUserReclassify: anElement	"Put up a list of categories and solicit one from the user.  Answer true if user indeed made a change, else false"	"ClassOrganizer organization letUserReclassify: #letUserReclassify:"	| currentCat newCat |	currentCat _ self categoryOfElement: anElement.	newCat _ self categoryFromUserWithPrompt: 'Choose Category (currently "', currentCat, '")'.	(newCat ~~ nil and: [newCat ~= currentCat])		ifTrue:			[self classify: anElement under: newCat.			^ true]		ifFalse:			[^ false]! !!ClassOrganizer methodsFor: 'private' stamp: 'sw 2/24/1999 15:26'!categoryFromUserWithPrompt: aPrompt	"SystemDictionary organization categoryFromUserWithPrompt: 'testing'"	| aMenu  |	aMenu _ CustomMenu new.	self categories do:		[:cat | aMenu add: cat asString action: cat].	^ aMenu startUpWithCaption: aPrompt! !!StringHolder methodsFor: 'method category' stamp: 'sw 2/24/1999 18:31'!changeCategory	"Present a menu of the categories of messages for the current class, and let the user choose a new category for the current message"	| aClass aSelector |	(aClass _ self selectedClassOrMetaClass) ifNotNil:		[(aSelector _ self selectedMessageName) ifNotNil:			[(aClass organization letUserReclassify: aSelector) ifTrue:				[Smalltalk changes reorganizeClass: aClass.				self methodCategoryChanged]]]! !!StringHolder methodsFor: 'method category' stamp: 'sw 2/24/1999 18:34'!methodCategoryChanged	self changed: #annotation! !!Browser methodsFor: 'initialize-release' stamp: 'sw 2/24/1999 18:32'!methodCategoryChanged	self changed: #messageList.	self messageListIndex: 0! !!Browser methodsFor: 'message functions' stamp: 'sw 2/22/1999 09:54'!messageListMenu: aMenu shifted: shifted	^ shifted ifFalse: [aMenu labels:'browse full (b)fileOutprintOutsenders of... (n)implementors of... (m)method inheritanceversions (v)inst var refs...inst var defs...class var refs...class variablesclass refs (N)removemore...'	lines: #(3 7 12)	selections:		#(browseMethodFull fileOutMessage printOutMessage		browseSendersOfMessages browseMessages methodHierarchy browseVersions		browseInstVarRefs browseInstVarDefs browseClassVarRefs 			browseClassVariables browseClassRefs		removeMessage shiftedYellowButtonActivity )]	ifTrue: [aMenu labels: 'browse class hierarchybrowse classbrowse methodimplementors of sent messageschange sets with this methodinspect instancesinspect subinstancesremove from this browserchange category...revert to previous versionremove from current change setrevert and forgetfetch documentationmore...' 	lines: #(5 7 9 12)	selections: #(classHierarchy browseClass 		buildMessageBrowser browseAllMessages findMethodInChangeSets 		inspectInstances inspectSubInstances		removeMessageFromBrowser 		changeCategory		revertToPreviousVersion 		removeFromCurrentChanges revertAndForget 		fetchDocPane		unshiftedYellowButtonActivity)]! !!ChangeSorter methodsFor: 'message list' stamp: 'sw 2/22/1999 12:58'!shiftedMessageMenu: aMenu	^ aMenu labels: 'browse class hierarchybrowse classbrowse methodimplementors of sent messageschange sets with this methodinspect instancesinspect subinstanceschange category...revert to previous versionrevert and forgetmore...' 	lines: #(5 7 8 10)	selections: #(classHierarchy browseClass 		buildMessageBrowser browseAllMessages findMethodInChangeSets 		inspectInstances inspectSubInstances		changeCategory		revertToPreviousVersion revertAndForget		unshiftedYellowButtonActivity)! !!MessageSet methodsFor: 'message functions' stamp: 'sw 2/24/1999 18:31'!methodCategoryChanged	self changed: #annotation! !