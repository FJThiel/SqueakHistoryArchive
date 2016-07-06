'From Squeak3.1alpha of 28 February 2001 [latest update: #4134] on 6 June 2001 at 6:36:42 pm'!CodeHolder subclass: #ChangeSorter	instanceVariableNames: 'parent myChangeSet currentClassName currentSelector priorChangeSetList changeSetCategory '	classVariableNames: 'AllChangeSets ChangeSetCategories PreviousSet RecentUpdateMarker '	poolDictionaries: ''	category: 'Tools-Changes'!!ChangeSorter class methodsFor: 'adding' stamp: 'tk 6/6/2001 18:36'!newChangesFromStream: aStream named: aName	"File in the code from the stream into a new change set whose	name is derived from aName. Leave the 'current change set'	unchanged. Return the new change set or nil on failure."	| oldChanges newName newSet |	oldChanges _ Smalltalk changes.	PreviousSet _ oldChanges name. 		"so a Bumper update can find it"	newName _ aName sansPeriodSuffix.	newSet _ self basicNewChangeSet: newName.	newSet ifNotNil:		[Smalltalk newChanges: newSet.		aStream fileInAnnouncing: 'Loading ', newName, '...'.		Transcript cr; show: 'File ', aName, ' successfully filed in to change set ', newName].	aStream close.	Smalltalk newChanges: oldChanges.	^ newSet! !