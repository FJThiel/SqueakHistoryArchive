'From Squeak3.6beta of ''4 July 2003'' [latest update: #5352] on 20 July 2003 at 11:06:16 pm'!"Change Set:		PackageInfo-BaseDate:			20 July 2003Author:			Avi BryantVersion 1.30 of the PackageInfo package, for Squeak 3.6."!Object subclass: #PackageExporter	instanceVariableNames: 'package stream '	classVariableNames: ''	poolDictionaries: ''	category: 'PackageInfo-Base'!PackageExporter subclass: #ChangeSetPackageExporter	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'PackageInfo-Base'!Object subclass: #PackageInfo	instanceVariableNames: 'packageName methodCategoryPrefix '	classVariableNames: 'Registry '	poolDictionaries: ''	category: 'PackageInfo-Base'!!PackageInfo commentStamp: '<historical>' prior: 0!Subclass this class to create new Packages.!!Character methodsFor: '*packageinfo-base' stamp: 'ab 5/31/2003 17:15'!escapeEntities
	#($< '&lt;' $> '&gt;' $& '&amp;') pairsDo:
		[:k :v |
		self = k ifTrue: [^ v]].
	^ String with: self! !!Class methodsFor: '*packageinfo-base' stamp: 'ab 11/17/2002 00:14'!noteCompilationOf: aSelector meta: isMeta
	"the cleanest place we can hook into this"
	InMidstOfFileinNotification signal
		ifFalse: [Utilities changed: #recentMethodSubmissions].
! !!Collection methodsFor: '*packageinfo-base' stamp: 'ab 9/30/2002 19:26'!gather: aBlock
	^ Array streamContents:
		[:stream |
		self do: [:ea | stream nextPutAll: (aBlock value: ea)]]! !!MethodReference methodsFor: '*packageinfo-base' stamp: 'ab 5/23/2003 22:58'!category
	^ self actualClass organization categoryOfElement: methodSymbol! !!MethodReference methodsFor: '*packageinfo-base' stamp: 'ab 5/23/2003 22:58'!sourceCode
	^ self actualClass sourceCodeAt: methodSymbol! !!PackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:47'!fileOut
	self subclassResponsibility! !!PackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:46'!package: aPackageInfo
	package _ aPackageInfo! !!PackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/31/2003 17:23'!prettyPrint: aString
	"for now just replace _ with :="
	^ String streamContents:
		[:s |
		aString do:
			[:c |
			c = $_
				ifFalse: [s nextPut: c]
				ifTrue: [s nextPutAll: ':=']]]! !!PackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:47'!stream: aStream
	stream _ aStream! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:56'!fileOut
	self fileOutSystemCategories.
	self fileOutClassDefinitions.
	self fileOutClassComments.
	self fileOutMethods.
	self fileOutInitializers.
! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:52'!fileOutClassComments
	package classes do:
		[:class |
		class organization classComment isEmpty
			ifFalse: [self fileOutCommentForClass: class ]].
		! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 23:10'!fileOutClassDefinitions
	(ChangeSet superclassOrder: package classesAndMetaClasses)
		do: [:class | self fileOutDefinitionForClass: class]
		displayingProgress: 'Filing out classes...'.! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:52'!fileOutCommentForClass: aClass
	aClass organization
		putCommentOnFile: stream
		numbered: 0
		moveSource: false
		forClass: aClass! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:51'!fileOutDefinitionForClass: aClass
	stream nextChunkPut: aClass definition; cr; cr! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:55'!fileOutInitializerForClass: aClass
	stream nextChunkPut: aClass name, ' initialize'; cr! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:55'!fileOutInitializers		
	(package classes select: [:c | c class includesSelector: #initialize])
		do: [:class | self fileOutInitializerForClass: class].! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:54'!fileOutMethod: aMethodReference
	aMethodReference actualClass
		printMethodChunk: aMethodReference methodSymbol
		withPreamble: true
		on: stream
		moveSource: false
		toFile: 0! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:54'!fileOutMethods
	self sortedMethods
		do: [:ref | self fileOutMethod: ref]
		displayingProgress: 'Filing out methods...'.! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:56'!fileOutSystemCategories
	package systemCategories do: [:cat | self fileOutSystemCategory: cat].
	stream cr; cr.

	! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:50'!fileOutSystemCategory: categoryName
	stream
		nextChunkPut: 'SystemOrganization addCategory: ', categoryName printString;
		cr! !!ChangeSetPackageExporter methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 22:54'!sortedMethods
 ^ package methods sortBy:
		[:a :b |
		a methodSymbol < b methodSymbol
			or: [a methodSymbol = b methodSymbol
					and: [a classSymbol <= b classSymbol]]]! !!PackageExporter class methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 23:09'!fileOutPackage: aPackageInfo on: aStream
	self new
		package: aPackageInfo;
		stream: aStream;
		fileOut! !!PackageExporter class methodsFor: 'as yet unclassified' stamp: 'ab 5/23/2003 23:09'!fileOutPackageNamed: packageName on: aStream
	self fileOutPackage: (PackageInfo named: packageName) on: aStream! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:18'!category: categoryName matches: prefix
	^ categoryName = prefix or: [categoryName beginsWith: prefix, '-']! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:18'!coreCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | (self isForeignClassExtension: cat) not]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:22'!coreMethodsForClass: aClass
	^ (aClass selectors difference:
		((self foreignExtensionMethodsForClass: aClass) collect: [:r | r methodSymbol]))
			asArray collect: [:sel | self referenceForMethod: sel ofClass: aClass]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:20'!extensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isYourClassExtension: cat]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:25'!extensionMethodsForClass: aClass
	^ (self extensionCategoriesForClass: aClass)
		gather: [:cat | (aClass organization listAtCategoryNamed: cat)
							collect: [:sel | self referenceForMethod: sel ofClass: aClass]]! !!PackageInfo methodsFor: 'testing' stamp: 'dvf 10/18/2002 23:22'!extensionMethodsFromClasses: classes
	^classes
		gather: [:class | self extensionMethodsForClass: class]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:22'!foreignExtensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isForeignClassExtension: cat]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:23'!foreignExtensionMethodsForClass: aClass
	^ (self foreignExtensionCategoriesForClass: aClass)
		gather: [:cat | (aClass organization listAtCategoryNamed: cat)
						  collect: [:sel | self referenceForMethod: sel ofClass: aClass]]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:23'!includesClass: aClass
	^ self includesSystemCategory: aClass theNonMetaClass category! !!PackageInfo methodsFor: 'testing' stamp: 'ab 12/5/2002 00:16'!includesMethod: aSymbol ofClass: aClass
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass organization categoryOfElement: aSymbol)
										ifNil: [' '])
		ofClass: aClass! !!PackageInfo methodsFor: 'testing' stamp: 'dvf 9/17/2002 00:18'!includesMethodCategory: categoryName ofClass: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/14/2002 18:06'!includesMethodReference: aMethodRef
	^ self includesMethod: aMethodRef methodSymbol ofClass: aMethodRef actualClass! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:23'!includesSystemCategory: categoryName
	^ self category: categoryName matches: self systemCategoryPrefix! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:23'!isForeignClassExtension: categoryName
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:23'!isYourClassExtension: categoryName
	^ self category: categoryName asLowercase matches: self methodCategoryPrefix! !!PackageInfo methodsFor: 'testing' stamp: 'dvf 10/18/2002 23:22'!outsideClasses
	^ProtoObject withAllSubclasses difference: self classesAndMetaClasses! !!PackageInfo methodsFor: 'testing' stamp: 'ab 11/13/2002 01:25'!referenceForMethod: aSymbol ofClass: aClass
	^ MethodReference new setStandardClass: aClass methodSymbol: aSymbol! !!PackageInfo methodsFor: 'naming' stamp: 'ab 10/17/2002 00:05'!categoryName
	|category|
	category _ self class category.
	^ (category endsWith: '-Info')
		ifTrue: [category copyUpToLast: $-]
		ifFalse: [category]! !!PackageInfo methodsFor: 'naming' stamp: 'ab 10/16/2002 21:22'!externalName
	^ self packageName! !!PackageInfo methodsFor: 'naming' stamp: 'ab 6/10/2003 17:21'!methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [methodCategoryPrefix _ '*', self packageName asLowercase]! !!PackageInfo methodsFor: 'naming' stamp: 'ab 10/16/2002 16:57'!packageName
	^ packageName ifNil: [packageName _ self categoryName]! !!PackageInfo methodsFor: 'naming' stamp: 'ab 10/16/2002 16:56'!packageName: aString
	packageName _ aString! !!PackageInfo methodsFor: 'naming' stamp: 'ab 10/28/2002 10:38'!systemCategoryPrefix
	^ self packageName! !!PackageInfo methodsFor: 'listing' stamp: 'ac 5/14/2003 16:23'!classes
	^(self systemCategories gather:
		[:cat |
		(SystemOrganization listAtCategoryNamed: cat)
			collect: [:className | Smalltalk at: className]])
				sortBy: [:a :b | a className <= b className]! !!PackageInfo methodsFor: 'listing' stamp: 'dvf 9/17/2002 00:56'!classesAndMetaClasses
	| baseClasses |
	baseClasses := self classes.
	^baseClasses , (baseClasses collect: [:c | c class])! !!PackageInfo methodsFor: 'listing' stamp: 'ab 11/13/2002 01:23'!coreMethods
	^ self classesAndMetaClasses gather: [:class | self coreMethodsForClass: class]! !!PackageInfo methodsFor: 'listing' stamp: 'ab 6/10/2003 17:12'!extensionMethods
	^ self externalClasses gather: [:class | self extensionMethodsForClass: class]! !!PackageInfo methodsFor: 'listing' stamp: 'ab 12/3/2002 14:38'!foreignClasses
	| s |
	s _ IdentitySet new.
	self foreignSystemCategories
		do: [:c | (SystemOrganization listAtCategoryNamed: c)
				do: [:cl | 
					| cls | 
					cls _ Smalltalk at: cl. 
					s add: cls;
					  add: cls class]].
	^ s! !!PackageInfo methodsFor: 'listing' stamp: 'ab 12/3/2002 14:34'!foreignSystemCategories
	^ SystemOrganization categories
		reject: [:cat | self includesSystemCategory: cat] ! !!PackageInfo methodsFor: 'listing' stamp: 'ab 7/6/2003 21:49'!methods
	^ (self extensionMethods, self coreMethods)
		select: [:method | method isValid and: [(#(DoIt DoItIn:) includes: method methodSymbol) not]]! !!PackageInfo methodsFor: 'listing' stamp: 'ab 11/14/2002 18:39'!selectors
	^ self methods collect: [:ea | ea methodSymbol]! !!PackageInfo methodsFor: 'listing' stamp: 'ab 11/11/2002 21:51'!systemCategories
	^ SystemOrganization categories select: [:cat | self includesSystemCategory: cat]! !!PackageInfo methodsFor: 'dependencies' stamp: 'ab 11/18/2002 01:16'!externalCallers
	^ self 
		externalRefsSelect: [:literal | literal isKindOf: Symbol] 
		thenCollect: [:l | l].! !!PackageInfo methodsFor: 'dependencies' stamp: 'ab 6/10/2003 17:18'!externalClasses
	| myClasses |
	myClasses _ self classesAndMetaClasses.
	^ Array streamContents:
		[:s |
		ProtoObject withAllSubclassesDo:
			[:class |
			(myClasses includes: class) ifFalse: [s nextPut: class]]]! !!PackageInfo methodsFor: 'dependencies' stamp: 'ab 2/12/2003 19:28'!externalRefsSelect: selBlock thenCollect: colBlock
	| pkgMethods dependents refs extMethods otherClasses otherMethods classNames |

	classNames _ self classes collect: [:c | c name].
	extMethods _ self extensionMethods collect: [:mr | mr methodSymbol].
	otherClasses _ self externalClasses difference: self externalSubclasses.
	otherMethods _  otherClasses gather: [:c | c selectors].
	pkgMethods _ self methods asSet collect: [:mr | mr methodSymbol].
	pkgMethods removeAllFoundIn: otherMethods.

	dependents _ Set new.
	otherClasses do: [:c |
		c selectorsAndMethodsDo:
			[:sel :compiled |
			refs _ compiled literals select: selBlock thenCollect: colBlock.
			refs do: [:ea |
				((classNames includes: ea)
					or: [(pkgMethods includes: ea)  and: [(extMethods includes: ea) not]])
						ifTrue: [dependents add: (self referenceForMethod: sel ofClass: c) -> ea]]]].
	^ dependents! !!PackageInfo methodsFor: 'dependencies' stamp: 'cwp 11/13/2002 00:24'!externalSubclasses
	| pkgClasses subClasses |
	pkgClasses _ self classes.
	subClasses _ Set new.
	pkgClasses do: [:c | subClasses addAll: (c allSubclasses)].
	^ subClasses difference: pkgClasses
! !!PackageInfo methodsFor: 'dependencies' stamp: 'ab 11/18/2002 01:15'!externalUsers
	^ self 
		externalRefsSelect: [:literal | literal isVariableBinding] 
		thenCollect: [:l | l key]! !!PackageInfo methodsFor: 'fileOut' stamp: 'ab 7/3/2003 12:56'!fileOut
	| stream |
	stream _ FileStream forceNewFileNamed: self externalName, '.st'.
	self fileOutOnStream: stream.
	stream close.! !!PackageInfo methodsFor: 'fileOut' stamp: 'ab 5/23/2003 23:08'!fileOutOnStream: aStream
	ChangeSetPackageExporter fileOutPackage: self on: aStream! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:28'!allPackageNames
	^ Registry asSortedCollection! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:28'!allPackages
	^ self allPackageNames collect: [:ea | self named: ea]! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 10/16/2002 16:41'!default
	^ default ifNil: [default _ self new]! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:28'!initialize
	Registry _ Set new! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:39'!named: aString
	self registerPackageName: aString.
	self allSubclassesDo: [:c | c default packageName = aString ifTrue: [^ c default]].
	^ self new packageName: aString! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:51'!registerPackageName: aString
	Registry add: aString.
	self changed: #allPackageNames! !!PackageInfo class methodsFor: 'as yet unclassified' stamp: 'ab 7/7/2003 20:52'!unregisterPackageName: aString
	Registry remove: aString.
	self changed: #allPackageNames! !!PositionableStream methodsFor: '*packageinfo-base' stamp: 'nk 6/17/2003 07:45'!untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
			[:bar |
			[self atEnd] whileFalse:
				[bar value: self position.
				aBlock value]].! !!ReadStream methodsFor: '*packageinfo-base' stamp: 'ab 5/24/2003 14:28'!untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
			[:bar |
			[self atEnd] whileFalse:
				[bar value: self position.
				aBlock value]].! !!SequenceableCollection methodsFor: '*packageinfo-base' stamp: 'ab 9/17/2002 01:02'!do: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
			[:bar |
			self withIndexDo:
				[:each :i |
				bar value: i.
				aBlock value: each]]! !!String methodsFor: '*packageinfo-base' stamp: 'ab 5/31/2003 17:13'!escapeEntities
	^ String streamContents: [:s | self do: [:c | s nextPutAll: c escapeEntities]]
! !PackageInfo initialize!"Postscript:""Register package 'PackageInfo' 1.30 as installed"SMSqueakMap default noteInstalledPackage:  '73fb0862-5f49-4b90-b899-c0101335b7c5' version: '1.30'.!