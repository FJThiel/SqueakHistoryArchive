'From Squeak3.3alpha of 12 January 2002 [latest update: #4778] on 26 February 2002 at 11:51:37 pm'!"Change Set:		cleanups-swDate:			27 February 2002Author:			Scott WallaceVarious small cleanups.� Fixes bug that could launch global flaps if none already exist upon a squeak-window resize.� Corrects balloon help for Squeak flap.� Makes the default visible set of flaps be only the widely-used four, as in earlier Squeak releases.  The reason why the seldom-used Stack Tools flap was for a time included in the default visible had long since vanished.� A minor cleanup of the #demandsBoolean implementors.� Adds a convenience method making it easy to test-drive the retrofitting of selected updates to an earlier image, e.g. from 3.3a to 3.2gamma."!!Flaps class methodsFor: 'predefined flaps' stamp: 'sw 2/7/2002 16:26'!addStandardFlaps	"Initialize the standard default out-of-box set of global flaps.  This method creates them and places them in my class variable #SharedFlapTabs, but does not itself get them displayed."	SharedFlapTabs ifNil: [SharedFlapTabs _ OrderedCollection new].	SharedFlapTabs add: self newSqueakFlap.	SharedFlapTabs add: self newSuppliesFlap.	SharedFlapTabs add: self newToolsFlap.	SharedFlapTabs add: self newWidgetsFlap.	SharedFlapTabs add: self newStackToolsFlap.	SharedFlapTabs add: self newNavigatorFlap.	SharedFlapTabs add: self newPaintingFlap.	self disableGlobalFlapWithID: 'Stack Tools'.	self disableGlobalFlapWithID: 'Painting'.	self disableGlobalFlapWithID: 'Navigator'.	^ SharedFlapTabs! !!Morph methodsFor: 'classification' stamp: 'sw 2/26/2002 23:29'!demandsBoolean	"Answer whether the receiver will only accept a drop if it is boolean-valued.  Particular to tile-scripting."	^ self hasProperty: #demandsBoolean! !!FlapTab methodsFor: 'miscellaneous' stamp: 'sw 2/7/2002 17:24'!balloonTextForFlapsMenu	"Answer the balloon text to show on a menu item in the flaps menu that governs the visibility of the receiver in the current project"	| id |	id _ self flapID.	#(	('Squeak'		'Has a few generally-useful controls; it is also a place where you can "park" objects')	('Tools'			'A quick way to get browsers, change sorters, file lists, etc.')	('Widgets'		'A variety of controls and media tools')	('Supplies' 		'A source for many basic types of objects')	('Stack Tools' 	'Tools for building stacks.  Caution!!  Powerful but young and underdocumented')	('Scripting'		'Tools useful when doing tile scripting')	('Navigator'		'Project navigator:  includes controls for navigating through linked projects.  Also supports finding, loading and publishing projects in a shared environment')	('Painting'		'A flap housing the paint palette.  Click on the closed tab to make make a new painting')) do:		[:pair | (FlapTab givenID: id matches: pair first) ifTrue: [^ pair second]].	^ self balloonText! !!PasteUpMorph methodsFor: 'world state' stamp: 'sw 2/7/2002 16:22'!repositionFlapsAfterScreenSizeChange	"Reposition flaps after screen size change"	(Flaps globalFlapTabsIfAny, ActiveWorld localFlapTabs) do:		[:aFlapTab |			aFlapTab applyEdgeFractionWithin: self bounds].	Flaps doAutomaticLayoutOfFlapsIfAppropriate! !!Utilities class methodsFor: 'fetching updates' stamp: 'sw 2/26/2002 23:19'!fileInFromUpdatesFolder: numberList	"File in a series of updates with the given updates numbers, from the updates folder in the default directory.  The file-ins are done in numeric order, even if numberList was not sorted upon entry.	This is useful for test-driving the retrofitting of a possibly discontinguous list of updates from an alpha version back to a stable release.	Utilities fileInFromUpdatesFolder: #(4745 4746 4747 4748 4749 4750 4751 4752 4754 4755 4761 4762 4767 4769)."	| fileNames fileNamesInOrder file updateDirectory |	updateDirectory _ FileDirectory default directoryNamed: 'updates'.	fileNames _ updateDirectory fileNames select:		[:n | n first isDigit			and: [numberList includes: n initialIntegerOrNil]].	(file _ fileNames select: [:n | (n occurrencesOf: $.) > 1]) size > 0		ifTrue: [self error: file first , ' has multiple periods'].	fileNamesInOrder _ fileNames asSortedCollection:		[:a :b | a initialIntegerOrNil < b initialIntegerOrNil].	fileNamesInOrder do:		[:aFileName | (updateDirectory readOnlyFileNamed: aFileName) fileIntoNewChangeSet]! !PasteUpMorph removeSelector: #demandsBoolean!AlignmentMorph removeSelector: #demandsBoolean!