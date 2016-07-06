'From Squeak3.10.1-basic of 13 May 2008 [latest update: #7164] on 20 May 2008 at 7:25:19 am'!"Change Set:		SortedCollection-reversed-Test-M6956-niceDate:			26 February 2008Author:			niceThis tests that a SortedCollection reversed- is a SortedCollection- is reversedThis also classifies SortedCollectionTest in CollectionTests-Sequenceable rather than Unordered."!TestCase subclass: #SortedCollectionTest	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CollectionsTests-Sequenceable'!!SortedCollectionTest methodsFor: 'basic' stamp: 'nice 2/26/2008 22:52'!testReversed		| sc1 sc2 sc3 |	sc1 := #(1 2 3 4) asSortedCollection.	self assert: sc1 reversed asArray = sc1 asArray reversed.		self		assert: sc1 reversed class = SortedCollection		description: 'reversing a SortedCollection should answer a SortedCollection'.		sc1 removeFirst; removeLast.	sc2 := sc1 reversed.	self assert: sc2 reversed asArray = sc1 asArray.		sc2 add: 3/2; add: 1/2; add: 7/2.	self assert: sc2 asArray = {7/2. 3. 2. 3/2. 1/2}.			sc3 := #(1 2 3 3.0 4) asSortedCollection.	self assert: sc3 reversed asArray = #(4 3.0 3 2 1).	self assert: (sc3 reversed at: 2) class = Float.	! !