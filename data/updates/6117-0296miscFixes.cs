'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #278] on 31 August 2004 at 5:36:02 pm'!"Change Set:		FlexibleVocabulariesPreload-nkDate:			21 August 2004Author:			Ned Konz / (disconnected by Marcus Denker + Bert Freudenberg)Method stubs required to be loaded before FlexibleVocabularies-nk.*.mcz"!!Morph class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/20/2004 16:36'!allAdditionsToViewerCategories	"Answer a Dictionary of (<categoryName> <list of category specs>) that characterizes the	phrases this kind of morph wishes to add to various Viewer categories.	This version factors each category definition into a separate method.	Subclasses that have additions can either:		- override #additionsToViewerCategories, or		- (preferably) define one or more additionToViewerCategory* methods.	The advantage of the latter technique is that class extensions may be added	by external packages without having to re-define additionsToViewerCategories.	"	"Morph allAdditionsToViewerCategories size"	| dict |	dict _ IdentityDictionary new.	(self class includesSelector: #additionsToViewerCategories)		ifTrue: [self additionsToViewerCategories				do: [:group | group pairsDo: [:key :list |					(dict at: key ifAbsentPut: [OrderedCollection new])						addAll: list]]].	self additionToViewerCategorySelectors do: [ :aSelector |		(self perform: aSelector) pairsDo: [ :key :list |			(dict at: key ifAbsentPut: [ OrderedCollection new ]) addAll: list ]].	^dict! !!Morph class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/20/2004 16:36'!noteCompilationOf: aSelector meta: isMeta 	"Any change to an additionsToViewer... method can invalidate existing etoy vocabularies.	The #respondsTo: test is to allow loading the FlexibleVocabularies change set without having to worry about method ordering."	"do nothing because this is just the preload."	super noteCompilationOf: aSelector meta: isMeta! !!StandardScriptingSystem class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/20/2004 16:36'!noteCompilationOf: aSelector meta: isMeta 	"Any change to an additionsToViewer... method can invalidate existing etoy vocabularies.	The #respondsTo: test is to allow loading the FlexibleVocabularies change set without having to worry about method ordering."	"do nothing because this is just the preload."	super noteCompilationOf: aSelector meta: isMeta! !'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #293] on 2 September 2004 at 9:54:10 am'!"Change Set:		TextMorphSelectionAndFocusFixes-sw-nkDate:			31 August 2004Author:			Ned KonzScott's 'textSelectionFix-sw' with a fix, and an addition to avoid invalidation of excessively large areas.Scott's comment:Assures that the multiple-visible-selection regime of update 5849TextmorphSelectionFix-rr is confined to TextMorphs seen in windows; for loose TextMophs, the former behavior is restored."!!TextMorph methodsFor: 'event handling' stamp: 'nk 8/29/2004 21:29'!keyboardFocusChange: aBoolean 	| w |	paragraph isNil ifFalse:[paragraph focused: aBoolean].	aBoolean 		ifTrue: 			["A hand is wanting to send us characters..."			self hasFocus ifFalse: [self editor	"Forces install"]]		ifFalse: 			["A hand has clicked elsewhere..."			(w := self world) isNil 				ifFalse: 					[w handsDo: [:h | h keyboardFocus == self ifTrue: [^self]].					"Release control unless some hand is still holding on"					self releaseEditor]]! !!TextMorph methodsFor: 'private' stamp: 'nk 8/29/2004 21:40'!selectionChanged	"Invalidate all the selection rectangles. 	Make sure that any drop shadow is accounted for too."	self paragraph selectionRects		do: [:r | self				invalidRect: (self expandFullBoundsForDropShadow: (r intersect: self fullBounds))]! !!TextMorphForEditView methodsFor: 'event handling' stamp: 'sw 8/29/2004 23:09'!keyboardFocusChange: aBoolean 	"rr 3/21/2004 22:55 : removed the #ifFalse: branch, 	which was responsible of the deselection of text when the 	paragraph lost focus. This way selection works in a more standard 	way, and this permits the menu keyboard control to be really effective"	paragraph isNil ifFalse:[paragraph focused: aBoolean].	aBoolean 		ifTrue: 			["A hand is wanting to send us characters..."			self hasFocus ifFalse: [self editor	"Forces install"]].	self changed.! !'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #293] on 2 September 2004 at 9:55:45 am'!"Change Set:		Misc293Fixes-nkDate:			2 September 2004Author:			Ned KonzFixes for 0293 Squeakland images."!!Object methodsFor: 'macpal' stamp: 'nk 9/1/2004 10:41'!currentHand	"Return a usable HandMorph -- the one associated with the object's current environment.  This method will always return a hand, even if it has to conjure one up as a last resort.  If a particular hand is actually handling events at the moment (such as a remote hand or a ghost hand), it will be returned."	^ActiveHand ifNil: [ self currentWorld primaryHand ]! !!ChangeList class methodsFor: 'fileIn/Out' stamp: 'nk 8/31/2004 08:59'!browseCompressedChangesFile: fullName 	"Browse the selected file in fileIn format."	| zipped unzipped stream |	fullName ifNil: [^Beeper beep].	stream := FileStream readOnlyFileNamed: fullName.	stream converter: Latin1TextConverter new.	zipped := GZipReadStream on: stream.	unzipped := zipped contents asString.	stream := (MultiByteBinaryOrTextStream with: unzipped) reset.	ChangeList browseStream: stream! !!HaloMorph methodsFor: 'private' stamp: 'nk 9/2/2004 09:18'!doGrow: evt with: growHandle	"Called while the mouse is down in the grow handle"	| newExtent extentToUse |	evt hand obtainHalo: self.	newExtent _ (target pointFromWorld: (target griddedPoint: evt cursorPoint - positionOffset))								- target topLeft.	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].	(newExtent x < 1 or: [newExtent y < 1 ]) ifTrue: [^ self].	target renderedMorph setExtentFromHalo: (extentToUse _ newExtent).	growHandle position: evt cursorPoint - (growHandle extent // 2).	self layoutChanged.	(self valueOfProperty: #commandInProgress) ifNotNilDo:  		[:cmd | "Update the final extent"		cmd redoTarget: target selector: #setExtentFromHalo: argument: extentToUse]! !!JoystickMorph methodsFor: 'menu' stamp: 'nk 8/31/2004 08:59'!chooseJoystickNumber	"Allow the user to select a joystick number"	| result aNumber str |	str := self lastRealJoystickIndex asString.	result := FillInTheBlank 				request: 'Joystick device number (currently ' translated , str , ')'				initialAnswer: str.	[aNumber := result asNumber] on: Error do: [:err | ^self beep].	(aNumber > 0 and: [aNumber <= 32]) 		ifFalse: 			["???"			^Beeper beep].	realJoystickIndex := aNumber.	self setProperty: #lastRealJoystickIndex toValue: aNumber.	self startStepping! !!PartsBin methodsFor: 'initialization' stamp: 'nk 9/1/2004 20:09'!listDirection: aListDirection quadList: quadList buttonClass: buttonClass	"Initialize the receiver to run horizontally or vertically, obtaining its elements from the list of tuples of the form:		(<receiver> <selector> <label> <balloonHelp>)"	| aButton aClass |	self layoutPolicy: TableLayout new.	self listDirection: aListDirection.	self wrapCentering: #topLeft.	self layoutInset: 2.	self cellPositioning: #bottomCenter.	aListDirection == #leftToRight		ifTrue:			[self vResizing: #rigid.			self hResizing: #spaceFill.			self wrapDirection: #topToBottom]		ifFalse:			[self hResizing: #rigid.			self vResizing: #spaceFill.			self wrapDirection: #leftToRight].	quadList do:		[:tuple |			aClass _ Smalltalk at: tuple first.			aButton _ buttonClass new initializeWithThumbnail: (self class thumbnailForQuad: tuple color: self color) withLabel: tuple third andColor: self color andSend: tuple second to: aClass.			(tuple size > 3 and: [tuple fourth isEmptyOrNil not]) ifTrue:				[aButton setBalloonText: tuple fourth]. 			self addMorphBack: aButton]! !!SMSqueakMap methodsFor: 'public-installation' stamp: 'nk 8/31/2004 09:02'!noteInstalledPackageWithId: aPackageId autoVersion: aVersion name: aName 	"The package release was just successfully installed.	Can be used to inform SM of an installation not been	done using SM, even when the map isn't loaded.	We record the fact in our Dictionary of installed packages	and log a 'do it' to mark this in the changelog.	The doit helps keeping track of the packages when	recovering changes etc - not a perfect solution but should help.	The map used is the default map.	The id of the package is the key and the value is an OrderedCollection	of Arrays with the release auto version, the point in time and the current installCounter."	| time name id v |	v := aVersion isString ifTrue: [aVersion asVersion] ifFalse: [aVersion].	aName ifNil: [name := '<unknown package name>'] ifNotNil: [name := aName].	id := UUID fromString: aPackageId.	time := Time totalSeconds.	self countInstall.	self 		markInstalled: id		version: v		time: time		counter: installCounter.	SmalltalkImage current		logChange: '"Installed ' , name , ' auto version ' , v versionString 				, '".(Smalltalk at: #SMSqueakMap ifAbsent: []) ifNotNil:[	SMSqueakMap noteInstalledPackageWithId: ' 					, id asString storeString , ' autoVersion: ' 				, v storeString , ' atSeconds: ' 				, time asString , ' number: ' 				, installCounter asString , ']'! !!SimpleServiceEntry methodsFor: 'performing service' stamp: 'nk 8/31/2004 19:30'!performServiceFor: anObject	"carry out the service I provide"	^selector numArgs = 0		ifTrue: [provider perform: selector]		ifFalse: [			selector numArgs = 1				ifTrue: [ provider perform: selector with: (self getArgumentsFrom: anObject) ]				ifFalse: [ provider perform: selector withArguments: (self getArgumentsFrom: anObject) ]]! !!UpdatingTextMorph methodsFor: 'target access' stamp: 'nk 8/30/2004 16:18'!contentsFromTarget	"private - answer the contents from the receiver's target"	(target isNil			or: [getSelector isNil])		ifTrue: [^ self contents].	""	^ (target perform: getSelector) asString! !!Vocabulary methodsFor: '*flexibleVocabularies-testing' stamp: 'nk 9/1/2004 08:42'!isEToyVocabulary	^false! !!EToyVocabulary methodsFor: '*flexibleVocabularies-testing' stamp: 'nk 8/29/2004 17:20'!isEToyVocabulary	^true! !!EToyVocabulary reorganize!('initialization' addCustomCategoriesTo: encompassesAPriori: includesSelector:forInstance:ofClass:limitClass: methodInterfaceFrom: objectForDataStream: setCategoryDocumentationStrings)('category list' categoryListForInstance:ofClass:limitClass:)('method list' allMethodsInCategory:forInstance:ofClass: masterOrderingOfPhraseSymbols phraseSymbolsToSuppress)('*flexiblevocabularies-initialization' initialize)('*flexibleVocabularies-testing' isEToyVocabulary)!'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #293] on 2 September 2004 at 9:53:59 am'!!EToyVocabulary methodsFor: '*flexiblevocabularies-initialization' stamp: 'nk 8/31/2004 16:10'!initialize	"Initialize the receiver (automatically called when instances are created via 'new')"	|   classes aMethodCategory selector selectors categorySymbols aMethodInterface |	super initialize.	self vocabularyName: #eToy.	self documentation: '"EToy" is a vocabulary that provides the equivalent of the 1997-2000 etoy prototype'.	categorySymbols _ Set new.	classes _ self class morphClassesDeclaringViewerAdditions.	classes do:		[:aMorphClass | categorySymbols addAll: aMorphClass basicNew categoriesForViewer].	self addCustomCategoriesTo: categorySymbols.  "For benefit, e.g., of EToyVectorVocabulary"	categorySymbols asOrderedCollection do:		[:aCategorySymbol |			aMethodCategory _ ElementCategory new categoryName: aCategorySymbol.			selectors _ Set new.			classes do:				[:aMorphClass |					 (aMorphClass additionsToViewerCategory: aCategorySymbol) do:						[:anElement |						aMethodInterface _ self methodInterfaceFrom: anElement.						selectors add: (selector _ aMethodInterface selector).						(methodInterfaces includesKey: selector) ifFalse:							[methodInterfaces at: selector put: aMethodInterface].						self flag: #deffered.						"NB at present, the *setter* does not get its own method interface.  Need to revisit"].			(selectors copyWithout: #unused) asSortedArray do:				[:aSelector |					aMethodCategory elementAt: aSelector put: (methodInterfaces at: aSelector)]].				 			self addCategory: aMethodCategory].	self addCategoryNamed: ScriptingSystem nameForInstanceVariablesCategory.	self addCategoryNamed: ScriptingSystem nameForScriptsCategory.	self setCategoryDocumentationStrings.	(self respondsTo: #applyMasterOrdering)		ifTrue: [ self applyMasterOrdering ].! !!EToyVocabulary class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 16:54'!vocabularySummary	"Answer a string describing all the vocabulary defined anywhere in the 	system."	"	(StringHolder new contents: EToyVocabulary vocabularySummary)  	openLabel: 'EToy Vocabulary' translated 	"	| etoyVocab rt interfaces |	etoyVocab := Vocabulary eToyVocabulary.	^ String		streamContents: [:s | self morphClassesDeclaringViewerAdditions				do: [:cl | 					s nextPutAll: cl name;						 cr.					cl allAdditionsToViewerCategories						keysAndValuesDo: [:cat :additions | 							interfaces := (etoyVocab categoryAt: cat) elementsInOrder.							interfaces := interfaces										select: [:ea | additions												anySatisfy: [:tuple | (tuple first = #slot														ifTrue: [tuple at: 7]														ifFalse: [tuple at: 2])														= ea selector]].							s tab; nextPutAll: cat translated; cr.							interfaces								do: [:if | 									s tab: 2.									rt := if resultType.									rt = #unknown										ifTrue: [s nextPutAll: 'command' translated]										ifFalse: [s nextPutAll: 'property' translated;												 nextPut: $(;												 nextPutAll: (if companionSetterSelector													ifNil: ['RO']													ifNotNil: ['RW']) translated;												 space;												 nextPutAll: rt translated;												 nextPutAll: ') '].									s tab; print: if wording; space.									if argumentVariables										do: [:av | s nextPutAll: av variableName;												 nextPut: $(;												 nextPutAll: av variableType asString;												 nextPut: $)]										separatedBy: [s space].									s tab; nextPutAll: if helpMessage; cr]]]]! !!Morph methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 16:33'!categoriesForViewer	"Answer a list of symbols representing the categories to offer in the viewer, in order"	| aClass aList dict n additions |	aClass _ self renderedMorph class.	dict _ IdentityDictionary new.	n _ 3.	[aClass == Morph superclass ] whileFalse:		[(aClass hasAdditionsToViewerCategories) ifTrue: [			additions _ aClass allAdditionsToViewerCategories keys.			additions asOrderedCollection do: [:categorySpec |				dict at: categorySpec put: n. n _ n + 0.001 ]		].		aClass _ aClass superclass.		n _ aClass == Morph ifTrue: [ 2 ] ifFalse: [ n + 1 ].	]. 	n _ 1.	#(basic #'color & border' geometry motion #'pen use' tests layout #'drag & drop' scripting observation button search miscellaneous) do: [ :ea |		(dict includesKey: ea) ifTrue: [ dict at: ea put: n. n _ n + 0.001 ]	].	self filterViewerCategoryDictionary: dict.	aList _ SortedCollection sortBlock: [ :a :b | a value < b value ].	dict associationsDo: [ :assoc | aList add: assoc ].	^aList collect: [ :ea | ea key ]! !!Morph methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 17:09'!selectorsForViewer	"Answer a list of symbols representing all the selectors available in all my viewer categories"	| aClass aList itsAdditions added addBlock |	aClass := self renderedMorph class.	aList := OrderedCollection new.	added := Set new.	addBlock := [ :sym | (added includes: sym) ifFalse: [ added add: sym. aList add: sym ]].	[aClass == Morph superclass] whileFalse: 			[(aClass hasAdditionsToViewerCategories) 				ifTrue: 					[itsAdditions := aClass allAdditionsToViewerCategories.					itsAdditions do: [ :add | add do: [:aSpec |									"the spec list"									aSpec first == #command ifTrue: [ addBlock value: aSpec second].									aSpec first == #slot 										ifTrue: 											[ addBlock value: (aSpec seventh).											 addBlock value: aSpec ninth]]]].			aClass := aClass superclass].	^aList copyWithoutAll: #(#unused #dummy)	"SimpleSliderMorph basicNew selectorsForViewer"! !!Morph methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 17:14'!selectorsForViewerIn: aCollection	"Answer a list of symbols representing all the selectors available in all my viewer categories, selecting only the ones in aCollection"	| aClass aList itsAdditions added addBlock |	aClass := self renderedMorph class.	aList := OrderedCollection new.	added := Set new.	addBlock := [ :sym |		(added includes: sym) ifFalse: [ (aCollection includes: sym)			ifTrue: [ added add: sym. aList add: sym ]]].	[aClass == Morph superclass] whileFalse: 			[(aClass hasAdditionsToViewerCategories) 				ifTrue: 					[itsAdditions := aClass allAdditionsToViewerCategories.					itsAdditions do: [ :add | add do: [:aSpec |									"the spec list"									aSpec first == #command ifTrue: [ addBlock value: aSpec second].									aSpec first == #slot 										ifTrue: 											[ addBlock value: (aSpec seventh).											 addBlock value: aSpec ninth]]]].			aClass := aClass superclass].	^aList copyWithoutAll: #(#unused #dummy)	"SimpleSliderMorph basicNew selectorsForViewerIn: 	#(setTruncate: getColor setColor: getKnobColor setKnobColor: getWidth setWidth: getHeight setHeight: getDropEnabled setDropEnabled:)	"! !!Morph class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 16:35'!additionsToViewerCategory: aCategoryName	"Answer a list of viewer specs for items to be added to the given category on behalf of the receiver.  Each class in a morph's superclass chain is given the opportunity to add more things"	aCategoryName == #vector ifTrue:		[^ self vectorAdditions].	^self allAdditionsToViewerCategories at: aCategoryName ifAbsent: [ #() ].! !!Morph class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 8/29/2004 16:36'!allAdditionsToViewerCategories	"Answer a Dictionary of (<categoryName> <list of category specs>) that characterizes the	phrases this kind of morph wishes to add to various Viewer categories.	This version factors each category definition into a separate method.	Subclasses that have additions can either:		- override #additionsToViewerCategories, or		- (preferably) define one or more additionToViewerCategory* methods.	The advantage of the latter technique is that class extensions may be added	by external packages without having to re-define additionsToViewerCategories.	"	"Morph allAdditionsToViewerCategories size"	| dict |	dict _ IdentityDictionary new.	(self class includesSelector: #additionsToViewerCategories)		ifTrue: [self additionsToViewerCategories				do: [:group | group pairsDo: [:key :list |					(dict at: key ifAbsentPut: [OrderedCollection new])						addAll: list]]].	self additionToViewerCategorySelectors do: [ :aSelector |		(self perform: aSelector) pairsDo: [ :key :list |			(dict at: key ifAbsentPut: [ OrderedCollection new ]) addAll: list ]].	^dict! !!Morph class methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 10/11/2003 17:48'!noteCompilationOf: aSelector meta: isMeta 	"Any change to an additionsToViewer... method can invalidate existing etoy vocabularies.	The #respondsTo: test is to allow loading the FlexibleVocabularies change set without having to worry about method ordering."	(isMeta			and: [(aSelector beginsWith: 'additionsToViewer')					and: [self respondsTo: #hasAdditionsToViewerCategories]])		ifTrue: [Vocabulary changeMadeToViewerAdditions].	super noteCompilationOf: aSelector meta: isMeta! !!StandardScriptingSystem class methodsFor: '*flexibleVocabularies-class initialization' stamp: 'nk 9/29/2003 12:07'!noteCompilationOf: aSelector meta: isMeta	aSelector == #wordingForOperator: ifTrue:		[Vocabulary changeMadeToViewerAdditions].	super noteCompilationOf: aSelector meta: isMeta! !!SyntaxMorph class methodsFor: '*flexiblevocabularies-accessing' stamp: 'nk 8/29/2004 16:52'!allSpecs	"Return all specs that the Viewer knows about. Cache them."	"SyntaxMorph allSpecs"	^AllSpecs ifNil: [		AllSpecs _ Dictionary new.		(EToyVocabulary morphClassesDeclaringViewerAdditions)			do: [:cls | cls allAdditionsToViewerCategories keysAndValuesDo: [ :k :v | 				(AllSpecs at: k ifAbsentPut: [ OrderedCollection new ]) addAll: v ] ].		AllSpecs	]! !!Preferences class methodsFor: '*customevents-preferences' stamp: 'nk 8/18/2004 18:01'!allowEtoyUserCustomEvents	^ (self valueOfFlag: #allowEtoyUserCustomEvents		ifAbsent: [false]) and: [ self eToyFriendly not ]! !!Object methodsFor: '*standardyellowbuttonmenus-graph model' stamp: 'dgd 8/26/2004 14:58'!addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph 	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"	Preferences cmdGesturesEnabled ifTrue: [ "build mode"		aCustomMenu add: 'inspect model' translated target: self action: #inspect.	].	^aCustomMenu! !IRMethod class removeSelector: #new!IRBuilderMock class removeSelector: #new!IRBuilder class removeSelector: #new!