'From Squeak 2.3 of January 14, 1999 on 16 March 1999 at 10:41:00 am'!"Change Set:		UpdateTweaksDate:			28 January 1999Author:			Dan IngallsFixes an update-1000 bug in Utilities>>broadcastUpdatesFrom:to:except:Enhances ChangeSet summaryString to deal with offset numbering."!!ChangeSet methodsFor: 'fileIn/Out' stamp: 'di 3/15/1999 20:23'!summaryString	"Answer the string summarizing this changeSet"	^ self summaryStringDelta: 0! !!ChangeSet methodsFor: 'fileIn/Out' stamp: 'di 3/15/1999 20:27'!summaryStringDelta: delta	"Answer the string summarizing this changeSet"	| ps s2 date author line intName |	^ String streamContents:		[:s |		intName _ self name splitInteger.		s nextPutAll: (intName first + delta) printString , intName last.		(ps _ self preambleString)			ifNil: [s cr]			ifNotNil:			[s2 _ ReadStream on: ps.			s2 match: 'Date:'; skipSeparators.  date _ s2 upTo: Character cr.			s2 match: 'Author:'; skipSeparators.  author _ s2 upTo: Character cr.			s nextPutAll: ' -- '; nextPutAll: author; nextPutAll: ' -- '; nextPutAll: date; cr.			[s2 atEnd] whileFalse:				[line _ s2 upTo: Character cr.				(line isEmpty or: [line = '"']) ifFalse: [s nextPutAll: line; cr]]]]."To summarize all recent changeSets...(FileStream newFileNamed: 'ChangeSummaries.txt') nextPutAll:	(String streamContents:		[:s | (ChangeSorter changeSetsNamedSuchThat:			[:name | name first isDigit and: [name initialInteger >= 948]])			 do: [:cs | s nextPutAll: (cs summaryStringDelta: -289); cr]]);	close"! !!Utilities class methodsFor: 'fetching updates' stamp: 'di 3/16/1999 07:34'!broadcastUpdatesFrom: n1 to: n2 except: skipList"	ChangeSorter removeChangeSetsNamedSuchThat:		[:name | name first isDigit and: [name initialInteger > 412]].	Utilities readServerUpdatesSaveLocally: true updateImage: true.	Utilities broadcastUpdatesFrom: 413 to: 999 except: #().	Utilities readServerUpdatesSaveLocally: true updateImage: false       The expression above ftps all updates not in the current image over to the local       hard disk, but does NOT absorb them into the current image"	| fileNames fileNamesInOrder fileNamesUnnumbered names choice |	names _ ServerDirectory groupNames asSortedArray.	choice _ (SelectionMenu labelList: names selections: names) startUp.	choice == nil ifTrue: [^ self].	fileNames _ FileDirectory default fileNames select:		[:n | n first isDigit			and: [(n initialInteger between: n1 and: n2)			and: [(skipList includes: n initialInteger) not]]].	fileNamesInOrder _ fileNames asSortedCollection: [:a :b | a initialInteger < b initialInteger].	fileNamesUnnumbered _ fileNamesInOrder collect:		[:n | n copyFrom: (n findFirst: [:c | c isDigit not]) to: n size].	fileNamesInOrder with: fileNamesUnnumbered do:		[:n :nu | FileDirectory default rename: n toBe: nu].	(ServerDirectory groupNamed: choice) putUpdateMulti: fileNamesUnnumbered! !