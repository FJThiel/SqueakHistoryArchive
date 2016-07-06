'From Squeak3.7alpha of ''15 October 2003'' [latest update: #5607] on 13 December 2003 at 2:28:31 pm'!"Change Set:		showCategoryFixDate:			13 December 2003Author:			Boris GaertnerThe feature 'show message chategory' is broken. Required changes messages are not sent and as aresult the method category list is not updated at all and the method name list is not updated as it should. In Squeak 3.4 this feature worked. A change that was later introduced seemingly tried to avoid an unnecessary update of the method name list (which was updated twice), but did to much."!!Browser methodsFor: 'message category list' stamp: 'BG 12/13/2003 14:23'!selectOriginalCategoryForCurrentMethod	"private - Select the message category for the current method. 	 	 Note:  This should only be called when somebody tries to save  	 a method that they are modifying while ALL is selected. 	 	 Returns: true on success, false on failure."	| aSymbol selectorName |	aSymbol _ self categoryOfCurrentMethod.	selectorName _ self selectedMessageName.	(aSymbol notNil and: [aSymbol ~= ClassOrganizer allCategory])		ifTrue: 			[messageCategoryListIndex _ (self messageCategoryList indexOf: aSymbol).			messageListIndex _ (self messageList indexOf: selectorName).			self changed: #messageCategorySelectionChanged.			self changed: #messageCategoryListIndex.	"update my selection"			self changed: #messageList.			self changed: #messageListIndex.			self contentsChanged.			^ true].	^ false! !