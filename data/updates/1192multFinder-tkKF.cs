'From Squeak 2.4b of April 23, 1999 on 4 May 1999 at 8:28:19 pm'!"Change Set:		multFinder-tkKFDate:			4 May 1999Author:			Ted KaehlerFind multiple selectors that work with the data.  Return a (data1 beginsWith: data2) description for each one.  Put the selectors into the upper part of the SelectorFinder window and clear the other panes."!Object subclass: #MethodFinder	instanceVariableNames: 'data answers selector argMap thisData mapStage mapList expressions cachedClass cachedArgNum cachedSelectorLists '	classVariableNames: 'AddAndRemove Approved Blocks Dangerous '	poolDictionaries: ''	category: 'Kernel-Methods'!!MethodFinder methodsFor: 'initialize' stamp: 'tk 5/4/1999 17:30'!load: dataWithAnswers	"Find a function that takes the data and gives the answers.  Odd list entries are data for it, even ones are the answers. ""  (MethodFinder new) load: #( (4 3) 7  (-10 5) -5  (-3 11) 8);		findMessage  "| fixed |data _ Array new: dataWithAnswers size // 2.1 to: data size do: [:ii | data at: ii put: (dataWithAnswers at: ii*2-1)].answers _ Array new: data size.1 to: answers size do: [:ii | answers at: ii put: (dataWithAnswers at: ii*2)].fixed _ false.data do: [:list | 	(list isKindOf: SequenceableCollection) ifFalse: [		^ self inform: 'first and third items are not Arrays'].	list withIndexDo: [:arg :ind | 			arg == #true ifTrue: [list at: ind put: true.  fixed _ true].			arg == #false ifTrue: [list at: ind put: false.  fixed _ true].			]].answers withIndexDo: [:arg :ind | 			arg == #true ifTrue: [answers at: ind put: true.  fixed _ true].			arg == #false ifTrue: [answers at: ind put: false.  fixed _ true].			].fixed ifTrue: [self inform: '#(true false) are Symbols, not Booleans.  Next time use { true. false }'].argMap _ (1 to: data first size) asArray.data do: [:list | list size = argMap size ifFalse: [		self inform: 'data arrays must all be the same size']].argMap size > 5 ifTrue: [self inform: 'No more than a receiver and four arguments allowed'].	"Really only test receiver and three args." thisData _ data copy.mapStage _ mapList _ nil.! !!MethodFinder methodsFor: 'initialize' stamp: 'tk 5/4/1999 20:18'!testFromTuple: nth	"verify that the methods allowed don't crash the system.  Try N of each of the fundamental types.  up to 4 of each kind." | objects nonRepeating even other aa cnt |objects _ #((1 4 17 42) ($a $b $c $d) ('one' 'two' 'three' 'four')	(x + rect: new) ((a b 1 4) (c 1 5) ($a 3 d) ()) (4.5 0.0 3.2 100.3)	).objects _ objects, {{true. false. true. false}. {Point. SmallInteger. Association. Array}.	{Point class. SmallInteger class. Association class. Array class}.	"{ 4 blocks }."	{Date today. '1 Jan 1950' asDate. '25 Aug 1987' asDate. '1 Jan 2000' asDate}.	{'15:16' asTime. '1:56' asTime. '4:01' asTime. '6:23' asTime}.	{Dictionary new. Dictionary new. Dictionary new. Dictionary new}.	{#(a b 1 4) asOrderedCollection. #(c 1 5) asOrderedCollection. 		#($a 3 d) asOrderedCollection. #() asOrderedCollection}.	{3->true. 5.6->$a. #x->2. 'abcd'->false}.	{9@3 extent: 5@4. 0@0 extent: 45@9. -3@-7 extent: 2@2. 4@4 extent: 16@16}.	{Color red.  Color blue. Color black. Color gray}}.self test2: objects."rec+0, rec+1, rec+2, rec+3 need to be tested.  " cnt _ 0.nth to: 4 do: [:take |	nonRepeating _ OrderedCollection new.	objects do: [:each |		nonRepeating addAll: (each copyFrom: 1 to: take)].	"all combinations of take, from nonRepeating"	even _ true.	nonRepeating combinations: take atATimeDo: [:tuple |		even ifTrue: [other _ tuple clone]			ifFalse: [self load: (aa _ Array with: tuple with: 1 with: other with: 7).				(cnt _ cnt+1) \\ 50 = 0 ifTrue: [					Transcript cr; show: aa first printString].				self search: true.				self test2: aa.				self test2: nonRepeating.				"self test2: objects"].		even _ even not].	].! !!MethodFinder methodsFor: 'initialize' stamp: 'tk 5/4/1999 20:19'!testRandom	"verify that the methods allowed don't crash the system.  Pick 3 or 4 from a mixed list of the fundamental types." | objects other aa cnt take tuple fName sss |objects _ #((1 4 17 42) ($a $b $c $d) ('one' 'two' 'three' 'four')	(x + rect: new) ((a b 1 4) (c 1 5) ($a 3 d) ()) (4.5 0.0 3.2 100.3)	).objects _ objects, {{true. false. true. false}. {Point. SmallInteger. Association. Array}.	{Point class. SmallInteger class. Association class. Array class}.	"{ 4 blocks }."	{Date today. '1 Jan 1950' asDate. '25 Aug 1987' asDate. '1 Jan 2000' asDate}.	{'15:16' asTime. '1:56' asTime. '4:01' asTime. '6:23' asTime}.	{Dictionary new. Dictionary new. Dictionary new. Dictionary new}.	{#(a b 1 4) asOrderedCollection. #(c 1 5) asOrderedCollection. 		#($a 3 d) asOrderedCollection. #() asOrderedCollection}.	{3->true. 5.6->$a. #x->2. 'abcd'->false}.	{9@3 extent: 5@4. 0@0 extent: 45@9. -3@-7 extent: 2@2. 4@4 extent: 16@16}.	{Color red.  Color blue. Color black. Color gray}}.self test2: objects."rec+0, rec+1, rec+2, rec+3 need to be tested.  " fName _ (FileDirectory default fileNamesMatching: '*.ran') first.sss _ fName splitInteger first.(Collection classPool at: #RandomForPicking) seed: sss.cnt _ 0.[take _ #(3 4) atRandom.	tuple _ (1 to: take) collect: [:ind | (objects atRandom) atRandom].	other _ (1 to: take) collect: [:ind | (objects atRandom) atRandom].	self load: (aa _ Array with: tuple with: 1 with: other with: 7).	((cnt _ cnt+1) \\ 10 = 0) " | (cnt > Skip)" ifTrue: [		Transcript cr; show: cnt printString; tab; tab; show: aa first printString].	cnt > (Smalltalk at: #StopHere) ifTrue: [self halt].		"stop just before crash"	cnt > (Smalltalk at: #Skip) ifTrue: ["skip this many at start"		self search: true.		self test2: aa first.  self test2: (aa at: 3).		"self test2: objects"		].	true] whileTrue.	! !!MethodFinder methodsFor: 'search' stamp: 'tk 5/4/1999 18:50'!findMessage	"Control the search."	| selFinder |	data do: [:alist | 		(alist isKindOf: SequenceableCollection) ifFalse: [			^ 'first and third items are not Arrays']].	Approved ifNil: [self initialize].	"Sets of allowed selectors"	expressions _ WriteStream on: (String new: 400).	self search: true.	"multi"	selector isEmpty ifTrue: [^ 'no single method does that function'].	Smalltalk isMorphic ifTrue: [		((selFinder _ World submorphs first model) isKindOf: SelectorBrowser) ifTrue: [			selFinder selectorList: selector asSortedArray]].	^ expressions contents			! !!MethodFinder methodsFor: 'search' stamp: 'tk 5/4/1999 17:51'!search: multi	"if Multi is true, collect all selectors that work."	| old |	selector _ OrderedCollection new.	"list of them"	old _ Preferences autoAccessors.	Preferences disable: #autoAccessors.	self simpleSearch.	multi not & (selector isEmpty not) ifTrue: [		old ifTrue: [Preferences enable: #autoAccessors].		^ selector].	[self permuteArgs] whileTrue: [		self simpleSearch.		multi not & (selector isEmpty not) ifTrue: [			old ifTrue: [Preferences enable: #autoAccessors].			^ selector]].	old ifTrue: [Preferences enable: #autoAccessors].	^ #()! !!MethodFinder methodsFor: 'search' stamp: 'tk 5/4/1999 20:06'!simpleSearch	"Run through first arg's class' selectors, looking for one that works."| class supers listOfLists |class _ thisData first first class."Cache the selectors for the receiver class"(class == cachedClass and: [cachedArgNum = ((argMap size) - 1)]) 	ifTrue: [listOfLists _ cachedSelectorLists]	ifFalse: [supers _ class withAllSuperclasses.		listOfLists _ OrderedCollection new.		supers do: [:cls |			listOfLists add: (cls selectorsWithArgs: (argMap size) - 1)].		cachedClass _ class.		cachedArgNum _ (argMap size) - 1.		cachedSelectorLists _ listOfLists].listOfLists do: [:selectorList |	selectorList do: [:aSel |		(selector includes: aSel) ifFalse: [			((Approved includes: aSel) or: [AddAndRemove includes: aSel]) ifTrue: [				(self testPerfect: aSel) ifTrue: [					selector add: aSel.					expressions nextPut: $(.					expressions nextPutAll: 'data', argMap first printString.					aSel keywords doWithIndex: [:key :ind |						expressions nextPutAll: ' ',key.						(key last == $:) | (key first isLetter not)							ifTrue: [expressions nextPutAll: ' data', 								(argMap at: ind+1) printString]].					expressions nextPut: $); space.					]]]]].! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 5/4/1999 19:47'!selectorList: anExternalList	self contents: ''.	classList _ #().  classListIndex _ 0.	selectorIndex _ 0.   	selectorList _ anExternalList.	self changed: #contents.	self changed: #messageList.	self changed: #classList.! !MethodFinder removeSelector: #search!