'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5526] on 10 November 2003 at 1:16:11 pm'!"Change Set:		graphicalDict-swDate:			10 November 2003Author:			Scott WallaceThe 'GraphicalDictionaryMenu' is a tool that allows you to browse and edit a repository of graphical images that are saved as a Dictionary.  This change-set offers a number of cleanups and enhancements to this facility:�  'Find again' command added.�  'Browse symbol references' command added, to browse all references to the key symbol of the currently-selected entry.�  'Browse string references' command added, to track down all methods with string literals that may refer to the currently-selected entry.�  The tool's main menu now has a 'stay-up item'.�  The last keyword searched for is used as the default for 'find...'�  When there is no new match in a search, but the current item does match that search, the tool flashes rather than claiming no match.�  Querying the StandardScriptingSystem for a form at a given key no longer needlessly interns the key if it's not already interned.This version converted for 3.7a compatibility"!GraphicalMenu subclass: #GraphicalDictionaryMenu	instanceVariableNames: 'baseDictionary entryNames lastSearchString '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Menus'!!GraphicalDictionaryMenu methodsFor: 'menu commands' stamp: 'sw 11/10/2003 13:14'!browseIconReferences	"Browse all calls on the symbol by which the currently-seen graphic is keyed"	self systemNavigation browseAllCallsOn: self nameOfGraphic! !!GraphicalDictionaryMenu methodsFor: 'menu commands' stamp: 'sw 11/10/2003 13:14'!browseStringIconReferences	"Browse string references to the selected entry's key"	self systemNavigation browseMethodsWithString: self nameOfGraphic asString matchCase: true! !!GraphicalDictionaryMenu methodsFor: 'menu commands' stamp: 'sw 2/24/2003 16:04'!findAgain	"Look for the next occurrence of the search string"	| toFind searchIndex |	lastSearchString ifNil: [lastSearchString _ 'controls'].	searchIndex _ currentIndex + 1.	toFind _ '*', lastSearchString, '*'.	[toFind match: (entryNames at: searchIndex) asString]		whileFalse:			[searchIndex _ (searchIndex \\ entryNames size) + 1.			searchIndex == currentIndex ifTrue:				[^ (toFind match: (entryNames at: searchIndex) asString)					ifFalse:						[self inform: 'not found']					ifTrue:						[self flash]]].	currentIndex _ searchIndex.	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'menu commands' stamp: 'sw 2/24/2003 15:57'!findEntry	"Prompt the user for a search string and find the next match for it"	| toFind searchIndex |	lastSearchString ifNil: [lastSearchString _ 'controls'].	toFind _ FillInTheBlank request: 'Type name or fragment: ' initialAnswer: lastSearchString.	toFind isEmptyOrNil ifTrue: [^ self].	lastSearchString _ toFind asLowercase.	searchIndex _ currentIndex + 1.	toFind _ '*', lastSearchString, '*'.	[toFind match: (entryNames at: searchIndex) asString]		whileFalse:			[searchIndex _ (searchIndex \\ entryNames size) + 1.			searchIndex == currentIndex ifTrue: [^ self inform: 'not found']].	currentIndex _ searchIndex.	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'menu commands' stamp: 'sw 2/24/2003 15:59'!showMenu	"Show the receiver's menu"	| aMenu |	aMenu _ MenuMorph new defaultTarget: self.	aMenu title: 'Graphics Library'.	aMenu addStayUpItem.	aMenu addList: #(		('remove'			removeEntry			'Remove this entry from the dictionary')		('rename'			renameEntry			'Rename this entry')		('repaint'			repaintEntry			'Edit the actual graphic for this entry' )		-		('hand me one'		handMeOne				'Hand me a morph with this picture as its form')		('browse symbol references'							browseIconReferences	'Browse methods that refer to this icon''s name')		('browse string references'							browseStringIconReferences'													'Browse methods that refer to string constants that contian this icon''s name)		('copy name'		copyName				'Copy the name of this graphic to the clipboard')		-		('find...'			findEntry				'Find an entry by name')		('find again'		findAgain				'Find the next match for the keyword previously searched for')).	aMenu popUpInWorld! !!StandardScriptingSystem methodsFor: 'form dictionary' stamp: 'sw 2/24/2003 16:28'!formAtKey: aString	"Answer the form saved under the given key"	Symbol hasInterned: aString ifTrue:		[:aKey | ^ FormDictionary at: aKey ifAbsent: [nil]].	^ nil! !