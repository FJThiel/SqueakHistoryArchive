'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 1 June 2000 at 1:45:33 pm'!"Change Set:		101CelesteStatusCleanup-sgeDate:			29 May 2000Author:			Steve ElkinsThese changes directly connect the change notification mechanism to updates of Celeste's 'status' instance variable."!!Celeste methodsFor: 'categories pane' stamp: 'sge 5/29/2000 20:40'!messages: actuallyShown from: possible 	self status: 'Showing ' , actuallyShown printString , ' of ' , 				possible printString , ' messages in ', self category! !!Celeste methodsFor: 'categories pane' stamp: 'sge 5/29/2000 20:27'!setCategory: newCategory 	"Change the currently selected category. We must also compute the table  	of contents and message list for the new category."	| messageCount |	currentCategory _ newCategory.	newCategory isNil		ifTrue: 			[self status: nil.			currentMessages _ currentTOC _ currentMsgID _ nil]		ifFalse: 			[currentMessages _ self filteredMessagesIn: newCategory.			messageCount _ currentMessages size.			messageCount > self maxMessageCount				ifTrue: 					[self messages: self maxMessageCount from: messageCount.					currentMessages _ currentMessages copyLast: self maxMessageCount]				ifFalse: [self messages: messageCount from: messageCount].			self updateToc].	self changed: #category.	self changed: #tocEntryList.	self changed: #tocEntry.	self changed: #messageText! !!Celeste methodsFor: 'other' stamp: 'sge 5/29/2000 20:29'!status: aStringOrNil	status _ aStringOrNil.	self changed: #status! !