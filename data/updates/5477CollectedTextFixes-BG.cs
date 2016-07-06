'From Squeak3.6beta of ''4 July 2003'' [latest update: #5387] on 1 October 2003 at 3:02:29 am'!"Change Set:		CollectedTextFixesDate:			12 June 2003Author:			Boris GaertnerThis change set contains fixes for all failures in  the tests for Text and TextStream. A fix from Tim Olson is included in modified form. This is still not the much desired clean up, but it is a modest improvement. "!!RunArray methodsFor: 'adding' stamp: 'BG 6/12/2003 11:07'!rangeOf: attr startingAt: startPos	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.Note that an attribute may span several adjancent runs. "	self at: startPos 		setRunOffsetAndValue:             [:run :offset :value |                ^(value includes: attr)                  ifFalse: [startPos to: startPos - 1]                  ifTrue:                    [ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |                     lastRelevantPosition := startPos - offset + (runs at: run) - 1.                     firstRelevantPosition := startPos - offset.                     idxOfCandidateRun := run + 1.                     [idxOfCandidateRun <= runs size                              and: [(values at: idxOfCandidateRun) includes: attr]]                        whileTrue:                          [lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).                           idxOfCandidateRun := idxOfCandidateRun + 1].                      idxOfCandidateRun := run - 1.                     [idxOfCandidateRun >= 1                              and: [(values at: idxOfCandidateRun) includes: attr]]                        whileTrue:                          [firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).                           idxOfCandidateRun := idxOfCandidateRun - 1].                      firstRelevantPosition to: lastRelevantPosition]		  ]! !!RunArray methodsFor: 'converting' stamp: 'BG 6/8/2003 15:17'!reversed

  ^self class runs: runs reversed values: values reversed! !!Text methodsFor: 'accessing' stamp: 'BG 6/8/2003 16:18'!rangeOf: attribute startingAt: index"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index.  "   ^string size = 0      ifTrue: [index to: index - 1]	 ifFalse: [runs rangeOf: attribute startingAt: index]! !!Text methodsFor: 'accessing' stamp: 'dvf 10/1/2003 02:50'!rangeOf: attribute startingAt: index forStyle: aTextStyle"aTextStyle is not really needed, it is kept for compatibility with an earlier method version "	self deprecatedExplanation: 'Use Text>>rangeOf:startingAt: instead.'.	^self rangeOf: attribute startingAt: index! !!Text methodsFor: 'converting' stamp: 'dvf 10/1/2003 02:58'!replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. Do it to both the string and the runs."

	| rep newRepRuns |
	rep _ replacement asText.	"might be a string"
	string replaceFrom: start to: stop with: rep string startingAt: repStart.
	newRepRuns _ rep runs copyFrom: repStart to: repStart + stop - start.	runs _ runs copyReplaceFrom: start to: stop with: newRepRuns! !!Text methodsFor: 'converting' stamp: 'BG 6/8/2003 16:38'!reversed

	"Answer a copy of the receiver with element order reversed."

	^ self class string: string reversed runs: runs reversed.

  "  It is assumed that  self size = runs size  holds. "! !!WriteStream methodsFor: 'accessing' stamp: 'BG 5/26/2003 08:01'!next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."


	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex].

	newEnd _ position + anInteger.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: startIndex.
	position _ newEnd.

	^aCollection! !!WriteStream methodsFor: 'accessing' stamp: 'BG 5/24/2003 20:41'!nextPutAll: aCollection

	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.! !!WriteStream methodsFor: 'private' stamp: 'BG 5/24/2003 22:49'!growTo: anInteger

   " anInteger is the required minimal new size of the collection "
	| oldSize grownCollection newSize |
	oldSize _ collection size.
     newSize := anInteger + (oldSize // 4 max: 20).
	grownCollection _ collection class new: newSize.
	collection _ grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit _ collection size.
! !!TextStream methodsFor: 'as yet unclassified' stamp: 'dvf 10/1/2003 02:51'!nextPutAll: aCollection 
	"Optimized access to get around Text at:Put: overhead"
	| n |
	n _ aCollection size.
     position + n > writeLimit
       ifTrue:
        [self growTo: position + n + 10].
	collection 
		replaceFrom: position+1
		to: position + n
		with: aCollection
		startingAt: 1.
	position _ position + n! !