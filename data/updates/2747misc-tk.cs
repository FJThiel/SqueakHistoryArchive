'From Squeak2.9alpha of 16 June 2000 [latest update: #2776] on 29 September 2000 at 12:47:23 pm'!"Change Set:		misc-tkDate:			26 September 2000Author:			Ted Kaehler1) SequenceableCollection.hasEqualElements: caused an error if the argument was not a Collection.  Called by RunArray.=  2) DamageRecorder.recordInvalidRect: corrects a spelling error in a comment.3) ServerDirectory.checkServers fixed an error when it is detected that the updates.list file does not match on the two servers.  (It happened for the first time recently!!)4) Upon fileOut, there is a dialog box that asks if you want to see the methods with slips in them.  Reversed the order of the choises, and changed the wording.5) Provided the reverse conversion method so that Projects written with no 'exitFlag' variable can be read into older BJ images."!!ChangeSet methodsFor: 'fileIn/Out' stamp: 'tk 9/26/2000 11:53'!lookForSlips	| slips nameLine msg |	nameLine _ '"', self name, '"'.	(slips _ self checkForSlips) size == 0 ifTrue:		[^ self inform: 'No slips detected in change set', nameLine].	msg _ slips size == 1		ifTrue:			[ 'One method in change set', nameLine, 'has a halt, reference to the Transcript,and/or some other ''slip'' in it.Would you like to browse it? ?']		ifFalse:			[ slips size printString,' methods in change set', nameLine, 'have halts or references to theTranscript or other ''slips'' in them.Would you like to browse them?'].	(PopUpMenu withCaption: msg chooseFrom: 'Ignore\Browser them') = 2		ifTrue: [Smalltalk browseMessageList: slips							name: 'Possible slips in ', name]! !!DamageRecorder methodsFor: 'recording' stamp: 'tk 9/25/2000 23:06'!recordInvalidRect: aRectangle	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle."	| mergeRect |	totalRepaint ifTrue: [^ self].  "planning full repaint; don't bother collecting damage"	invalidRects do: [:rect |		(rect intersects: aRectangle) ifTrue: [			"merge rectangle in place (see note below) if there is any overlap"			rect setOrigin: (rect origin min: aRectangle origin) truncated				corner: (rect corner max: aRectangle corner) truncated.			^ self]].	invalidRects size >= 15 ifTrue: [		"if there are too many separate areas, just repaint all"		"totalRepaint _ true.""Note:  The totalRepaint policy has poor behavior when many local rectangles (such as parts of a text selection) force repaint of the entire screen.  As an alternative, this code performs a simple merge of all rects whenever there are more than 10."		mergeRect _ Rectangle merging: invalidRects.		self reset.		invalidRects addLast: mergeRect].	"add the given rectangle to the damage list"	"Note: We make a deep copy of all rectangles added to the damage list,	 since rectangles in this list may be extended in place."	invalidRects addLast: (aRectangle topLeft truncated corner: aRectangle bottomRight truncated).! !!Project methodsFor: 'object fileIn' stamp: 'tk 9/29/2000 12:43'!convertdwctppdvtngpiivuel0: varDict dwctppdaevtngpiivuel0: smartRefStrm	"These variables are automatically stored into the new instance #('dependents' 'world' 'changeSet' 'transcript' 'parentProject' 'previousProject' 'displayDepth' 'viewSize' 'thumbnail' 'nextProject' 'guards' 'projectParameters' 'isolatedHead' 'inForce' 'version' 'urlList' 'environment' 'lastDirectory').	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"These are new #('activeProcess' 'exitFlag')."self instVarNamed: 'exitFlag' put: false.! !!SequenceableCollection methodsFor: 'comparing' stamp: 'tk 9/26/2000 12:45'!hasEqualElements: otherCollection	"Answer whether the receiver's size is the same as otherCollection's	size, and each of the receiver's elements equal the corresponding 	element of otherCollection.	This should probably replace the current definition of #= ."	| size |	otherCollection isCollection ifFalse: [^ false].	(size _ self size) = otherCollection size ifFalse: [^ false].	1 to: size do:		[:index |		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].	^ true! !!ServerDirectory methodsFor: 'updates' stamp: 'tk 9/25/2000 11:58'!checkServers	"Check that all servers are up and have the latest Updates.list.Warn user when can't write to a server that can still be read."	| final fileSize this serverList theUpdates decided myUpdates abort |	serverList _ group			ifNil: [Array with: self]			ifNotNil: [group value].	final _ OrderedCollection new.	fileSize _ 0.  theUpdates _ ''.	"list of updates"	abort _ false.	serverList do: [:aServer |		decided _ false.		this _ aServer getFileNamed: 'updates.list'.		(this = #error:) ifTrue: [^'' "Not found"].		this class == String ifTrue: ["no ftp"			(PopUpMenu labels: 'Cancel entire update' withCRs)				startUpWithCaption: 'Server ', aServer moniker,				' is unavailable.\Please consider phoning the administator.\' withCRs, this.			abort _ true.			decided _ true].		decided not & (this size > fileSize) ifTrue: ["new has a longer update.list"			fileSize _ this size.			final do: [:each | abort _ self outOfDate: each].			(final _ OrderedCollection new) add: aServer.			theUpdates _ this contentsOfEntireFile.			decided _ true].		decided not & (this size < fileSize) ifTrue: [			abort _ self outOfDate: aServer.  decided _ true].		decided not ifTrue: [myUpdates _ this contentsOfEntireFile.			myUpdates = theUpdates				ifTrue: [final add: aServer]				ifFalse: [abort _ self outOfDate: aServer]].		abort ifTrue: [^ Array new].		].	^ final! !