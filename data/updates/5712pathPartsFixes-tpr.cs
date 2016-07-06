'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 9 February 2004 at 3:23:46 pm'!"Change Set:		pathPartsFixesDate:			9 February 2004Author:			tim@sumeru.stanford.eduSome mild improvements to users of FileDirectory>pathParts.Revised per Frank Shearer's discovery of stupidity in respect to ServerDirectory>pathParts"!!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'tpr 11/28/2003 14:02'!hasContents	"Return whether this directory has subfolders. The value is cached to 	avoid a performance penalty.	Also for performance reasons, the code 	below will just assume that the directory does indeed have contents in a 	few of cases:  	1. If the item is not a FileDirectory (thus avoiding the cost 	of refreshing directories that are not local) 	2. If it's the root directory of a given volume 	3. If there is an error computing the FileDirectory's contents	"	hasContents		ifNil: [hasContents := true. "default"			["Best test I could think of for determining if this is a local directory "			((item isKindOf: FileDirectory)					and: ["test to see that it's not the root directory"						"there has to be a better way of doing this test -tpr"						item pathParts size > 1])				ifTrue: [hasContents := self contents notEmpty]]				on: Error				do: [hasContents := true]].	^ hasContents! !!FileList methodsFor: 'volume list and pattern' stamp: 'tpr 11/28/2003 11:44'!deleteDirectory	"Remove the currently selected directory"	| localDirName |	directory entries size = 0 ifFalse:[^self inform:'Directory must be empty'].	localDirName _ directory localName.	(self confirm: 'Really delete ' , localDirName , '?') ifFalse: [^ self].	self volumeListIndex: self volumeListIndex-1.	directory deleteDirectory: localDirName.	self updateFileList.! !!FileList2 methodsFor: 'initialization' stamp: 'tpr 12/1/2003 17:14'!directory: dir	"Set the path of the volume to be displayed."	self okToChange ifFalse: [^ self].	self modelSleep.	directory _ dir.	self modelWakeUp.	sortMode == nil ifTrue: [sortMode _ #date].	volList _ Array with: '[]'.	directory ifNotNil: [		volList _ volList, directory pathParts.  "Nesting suggestion from RvL"	].	volList _ volList withIndexCollect: [:each :i | ( String new: i-1 withAll: $ ), each].	self changed: #relabel.	self changed: #volumeList.	self pattern: pattern.	directoryChangeBlock ifNotNil: [directoryChangeBlock value: directory].! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'tpr 11/27/2003 14:15'!openPath: anArray	anArray isEmpty ifTrue: [^container setSelectedMorph: nil].	self withSiblingsDo: [:each | 		(anArray first isNil or: [each complexContents asString = anArray first]) ifTrue: [			each isExpanded ifFalse: [				each toggleExpandedState.				container adjustSubmorphPositions.			].			each changed.			anArray size = 1 ifTrue: [				^container setSelectedMorph: each			].			each firstChild ifNil: [^container setSelectedMorph: nil].			^each firstChild openPath: anArray allButFirst.		].	].	^container setSelectedMorph: nil! !!StandardFileMenu methodsFor: 'menu building' stamp: 'tpr 11/28/2003 15:12'!menuLinesArray: aDirectory"Answer a menu lines object corresponding to aDirectory"	| typeCount nameCnt dirDepth|	typeCount _ canTypeFileName 		ifTrue: [1] 		ifFalse: [0].	nameCnt _ aDirectory directoryNames size.	dirDepth _ aDirectory pathParts size.	^Array streamContents: [:s |		canTypeFileName ifTrue: [s nextPut: 1].		s nextPut: dirDepth + typeCount + 1.		s nextPut: dirDepth + nameCnt + typeCount + 1]! !