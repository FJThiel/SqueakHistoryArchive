'From Squeak3.6beta of ''4 July 2003'' [latest update: #5352] on 21 July 2003 at 10:07:53 am'!"Change Set:		SystemDictionary-bkvDate:			21 July 2003Author:			Brent Vukmer <brentvukmer@yahoo.com>This is a one-line fix that corrects the message text for the warning thrown when SystemDictionary >> newChanges: is called."!!SystemDictionary methodsFor: 'sources, change log' stamp: 'bkv 7/21/2003 10:01'!newChanges: aChangeSet 	"Set the system ChangeSet to be the argument, aChangeSet.  Tell the current project that aChangeSet is now its change set.  When called from Project enter:, the setChangeSet: call is redundant but harmless; when called from code that changes the current-change-set from within a project, it's vital"	self deprecatedExplanation: 'Use ChangeSet newChanges:'. 	ChangeSet newChanges: aChangeSet! !