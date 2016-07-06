'From Squeak3.2alpha of 3 October 2001 [latest update: #4599] on 5 January 2002 at 1:55:24 pm'!"Change Set:		SUnitCSAllTestSelectorsFixDate:			5 January 2002Author:			Robert HirschfeldNarrows down the collection of SUnit test selectors to those that match 'test*' AND that take zero arguments. (Test included...)"!TestCase subclass: #AllTestSelectorsFixTest	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'SUnit-Tests'!!AllTestSelectorsFixTest methodsFor: 'running' stamp: 'rhi 1/5/2002 13:52'!testAllTestSelectors	| testSelectors |	testSelectors _ self class allTestSelectors.	self assert: testSelectors size = 2.	self assert: (testSelectors includes: #testTestSelectors).	self assert: (testSelectors includes: #testAllTestSelectors).	self deny: (testSelectors includes: #testXyz:).! !!AllTestSelectorsFixTest methodsFor: 'running' stamp: 'rhi 1/5/2002 13:52'!testTestSelectors	| testSelectors |	testSelectors _ self class testSelectors.	self assert: testSelectors size = 2.	self assert: (testSelectors includes: #testTestSelectors).	self assert: (testSelectors includes: #testAllTestSelectors).	self deny: (testSelectors includes: #testXyz:).! !!AllTestSelectorsFixTest methodsFor: 'private' stamp: 'rhi 1/5/2002 13:41'!testXyz: anObject	self assert: false. "Just in case..."! !!TestCase class methodsFor: 'Accessing' stamp: 'rhi 1/5/2002 13:22'!allTestSelectors	^ self sunitAllSelectors select: [:each | ('test*' sunitMatch: each) and: [each numArgs = 0]]! !!TestCase class methodsFor: 'Accessing' stamp: 'rhi 1/5/2002 13:50'!testSelectors	^ self sunitSelectors select: [:each | ('test*' sunitMatch: each) and: [each numArgs = 0]]! !