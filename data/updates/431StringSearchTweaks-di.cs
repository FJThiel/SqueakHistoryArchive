'From Squeak 2.2 of Sept 23, 1998 on 23 November 1998 at 4:06:44 pm'!"Change Set:		StringSearchTweaks-diDate:			23 November 1998Author:			Dan IngallsThe 'method strings with it' command and the 'method source with it' command now search without regard to upper/lower case differences.  Use the shift key to force a case-sensitive search.The 'compare to clipboard' command in the editor's special menu now spawns a window containing the diff text with insertions and deletions properly highlighted.Similarly, the ChangeList and Versions browsers now offer a command, 'compare to current version', that will spawn a window with the diff text, if the selected method exists in the system."!!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 11/23/1998 15:21'!compareToClipboard	"Check to see if whether the receiver's text is the same as the text currently on the clipboard, and inform the user."	| s1 s2 |	s1 _ self clipboardText string.	s2 _ paragraph text string.	s1 = s2 ifTrue: [^ self inform: 'Exact match'].	(StringHolder new textContents:		(TextDiffBuilder buildDisplayPatchFrom: s1 to: s2))		openLabel: 'Comparison to Clipboard Text'! !!StringHolder methodsFor: 'accessing' stamp: 'di 11/23/1998 15:21'!textContents: aStringOrText 	"Set aStringOrText to be the contents of the receiver."	contents _ aStringOrText! !!ChangeList methodsFor: 'menu actions' stamp: 'di 11/23/1998 15:44'!changeListMenu: aMenu^ aMenu labels:'fileIn selectionsfileOut selections...compare to currentselect conflictsselect conflicts withselect unchanged methodsselect alldeselect allremove doItsremove older versionsremove selections'	lines: #(2 8)	selections: #(fileInSelections fileOutSelectionscompareToCurrentVersion selectConflicts selectConflictsWith selectUnchangedMethods selectAll deselectAllremoveDoIts removeOlderMethodVersions removeSelections)! !!ChangeList methodsFor: 'menu actions' stamp: 'di 11/23/1998 15:51'!compareToCurrentVersion	"If the current selection corresponds to a method in the system,	then spawn a window showing the diffs as text"	| change class s1 s2 |	listIndex = 0 ifTrue: [^ self].	change _ changeList at: listIndex.	((class _ change methodClass) notNil			and: [class includesSelector: change methodSelector])	ifTrue:		[s1 _ (class sourceCodeAt: change methodSelector) asString.		s2 _ change string.		s1 = s2 ifTrue: [^ self inform: 'Exact Match'].		(StringHolder new textContents:			(TextDiffBuilder buildDisplayPatchFrom: s1 to: s2))			openLabel: 'Comparison to Current Version']	ifFalse:		[self flash].! !!SystemDictionary methodsFor: 'browsing' stamp: 'di 11/23/1998 12:05'!browseMethodsWithSourceString: aString	"Smalltalk browseMethodsWithSourceString: 'SourceString' "	"Launch a browser on all methods whose source code contains aString as a substring."	| caseSensitive suffix |	(caseSensitive _ Sensor shiftPressed)		ifTrue: [suffix _ ' (case-sensitive)']		ifFalse: [suffix _ ' (use shift for case-sensitive)'].	^ self browseMessageList: (self allMethodsWithSourceString: aString									matchCase: caseSensitive)		name: 'Methods containing ' , aString printString , suffix autoSelect: aString! !!SystemDictionary methodsFor: 'browsing' stamp: 'di 11/23/1998 12:05'!browseMethodsWithString: aString	"Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive."	| caseSensitive suffix |	(caseSensitive _ Sensor shiftPressed)		ifTrue: [suffix _ ' (case-sensitive)']		ifFalse: [suffix _ ' (use shift for case-sensitive)'].	self browseAllSelect:			[:method |				method  hasLiteralSuchThat: [:lit |					lit class == String and:					[lit includesSubstring: aString caseSensitive: caseSensitive]]]		name:  'Methods with string ', aString printString, suffix		autoSelect: aString.! !!SystemDictionary methodsFor: 'retrieving' stamp: 'di 11/23/1998 12:02'!allMethodsWithSourceString: aString matchCase: caseSensitive	"Answer a SortedCollection of all the methods that contain, in source code, aString as a substring.  The search is case-insensitive."	| list classCount |	list _ Set new.'Searching all source code...'displayProgressAt: Sensor cursorPointfrom: 0 to: Smalltalk classNames sizeduring:	[:bar | classCount _ 0.	Smalltalk allClassesDo:		[:class | bar value: (classCount _ classCount + 1).		(Array with: class with: class class) do:			[:cl | cl selectorsDo:				[:sel | 				((cl sourceCodeAt: sel) findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0					ifTrue:					[sel == #DoIt ifFalse: [list add: cl name , ' ' , sel]]]]]].	^ list asSortedCollection! !!Text methodsFor: 'accessing' stamp: 'di 11/23/1998 11:53'!findString: aString startingAt: start caseSensitive: caseSensitive	"Answer the index of subString within the receiver, starting at index 	start. If the receiver does not contain subString, answer 0."	^string findString: aString asString startingAt: start caseSensitive: caseSensitive! !SystemDictionary removeSelector: #allMethodsWithSourceString:!