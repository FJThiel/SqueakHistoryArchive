'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6229] on 23 September 2004 at 10:10:26 pm'!!SystemDictionary methodsFor: 'deprecated' stamp: 'sd 9/23/2004 21:47'!browseObsoleteMethodReferences	"Open a browser on all referenced behaviors that are obsolete"	"Smalltalk browseObsoleteMethodReferences"	| list |	self deprecated: 'Use SmalltalkImage current browseObsoleteMethodReferences'.	list _ self obsoleteMethodReferences.	self systemNavigation  browseMessageList: list name:'Method referencing obsoletes' autoSelect: nil! !!SystemDictionary methodsFor: 'deprecated' stamp: 'sd 9/23/2004 22:05'!obsoleteBehaviors	"Smalltalk obsoleteBehaviors inspect"	"Find all obsolete behaviors including meta classes"	| obs |	self deprecated: 'Use SmalltalkNavigation default obsoleteBehaviors'.	obs _ OrderedCollection new.	Smalltalk garbageCollect.	self systemNavigation		allObjectsDo: [:cl | (cl isBehavior					and: [cl isObsolete])				ifTrue: [obs add: cl]].	^ obs asArray! !!SystemDictionary methodsFor: 'deprecated' stamp: 'sd 9/23/2004 22:09'!obsoleteClasses   "Smalltalk obsoleteClasses inspect"	"NOTE:  Also try inspecting comments below"	| obs c |	self deprecated: 'Use SystemNavigation default obsoleteClasses'.	obs _ OrderedCollection new.  Smalltalk garbageCollect.	Metaclass allInstancesDo:		[:m | c _ m soleInstance.		(c ~~ nil and: ['AnOb*' match: c name asString])			ifTrue: [obs add: c]].	^ obs asArray"Likely in a ClassDict or Pool...(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray""Obsolete class refs or super pointer in last lit of a method...| n l found |Smalltalk browseAllSelect:	[:m | found _ false.	1 to: m numLiterals do:		[:i | (((l _ m literalAt: i) isMemberOf: Association)				and: [(l value isKindOf: Behavior)				and: ['AnOb*' match: l value name]])			ifTrue: [found _ true]].	found]"! !!SystemDictionary methodsFor: 'deprecated' stamp: 'sd 9/23/2004 22:09'!obsoleteMethodReferences	"Smalltalk obsoleteMethodReferences"	"Smalltalk browseObsoleteMethodReferences"	"Open a browser on all referenced behaviors that are obsolete"	| obsClasses obsRefs references |	self deprecated: 'Use SystemNavigation default obsoleteMethodReferences'.	references _ WriteStream on: Array new.	obsClasses _ self obsoleteBehaviors.	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor cursorPoint		from: 1 to: obsClasses size during:[:bar|	obsClasses keysAndValuesDo:[:index :each|		bar value: index.		obsRefs _ self pointersTo: each except: obsClasses.		obsRefs do:[:ref|			"Figure out if it may be a global"			((ref isVariableBinding) and:[ref key isString "or Symbol"]) ifTrue:[				(self pointersTo: ref) do:[:meth|					(meth isKindOf: CompiledMethod) ifTrue:[						meth methodReference ifNotNilDo:[:mref|							references nextPut: mref]]]]]].	].	^references contents! !!SystemNavigation methodsFor: 'query' stamp: 'sd 9/23/2004 22:03'!obsoleteBehaviors	"SystemNavigation default obsoleteBehaviors inspect"	"Find all obsolete behaviors including meta classes"	| obs |	obs _ OrderedCollection new.	Smalltalk garbageCollect.	self 		allObjectsDo: [:cl | (cl isBehavior					and: [cl isObsolete])				ifTrue: [obs add: cl]].	^ obs asArray! !!SystemNavigation methodsFor: 'query' stamp: 'sd 9/23/2004 22:06'!obsoleteClasses   	"SystemNavigation default obsoleteClasses inspect"	"NOTE:  Also try inspecting comments below"	| obs c |	obs _ OrderedCollection new.  Smalltalk garbageCollect.	Metaclass allInstancesDo:		[:m | c _ m soleInstance.		(c ~~ nil and: ['AnOb*' match: c name asString])			ifTrue: [obs add: c]].	^ obs asArray"Likely in a ClassDict or Pool...(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray""Obsolete class refs or super pointer in last lit of a method...| n l found |Smalltalk browseAllSelect:	[:m | found _ false.	1 to: m numLiterals do:		[:i | (((l _ m literalAt: i) isMemberOf: Association)				and: [(l value isKindOf: Behavior)				and: ['AnOb*' match: l value name]])			ifTrue: [found _ true]].	found]"! !!SystemNavigation methodsFor: 'query' stamp: 'sd 9/23/2004 22:04'!obsoleteMethodReferences	"SystemNavigation default obsoleteMethodReferences"	"Open a browser on all referenced behaviors that are obsolete"	| obsClasses obsRefs references |	references _ WriteStream on: Array new.	obsClasses _ self obsoleteBehaviors.	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor cursorPoint		from: 1 to: obsClasses size during:[:bar|	obsClasses keysAndValuesDo:[:index :each|		bar value: index.		obsRefs _ Smalltalk pointersTo: each except: obsClasses.		obsRefs do:[:ref|			"Figure out if it may be a global"			((ref isVariableBinding) and:[ref key isString "or Symbol"]) ifTrue:[				(Smalltalk pointersTo: ref) do:[:meth|					(meth isKindOf: CompiledMethod) ifTrue:[						meth methodReference ifNotNilDo:[:mref|							references nextPut: mref]]]]]].	].	^references contents! !!SystemNavigation methodsFor: '*tools-browser' stamp: 'sd 9/23/2004 21:48'!browseObsoleteMethodReferences	"Open a browser on all referenced behaviors that are obsolete"	"SystemNavigation default browseObsoleteMethodReferences"	| list |	list _ Smalltalk obsoleteMethodReferences.	self browseMessageList: list name:'Method referencing obsoletes' autoSelect: nil! !!SystemNavigation reorganize!('browse' allMethodsInCategory: browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:from: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllObjectReferencesTo:except:ifNone: browseAllReferencesToPool:from: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassVarRefs: browseClassVariables: browseClassesWithNamesContaining:caseSensitive: browseInstVarDefs: browseInstVarRefs: browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWhoseNamesContain: browseMethodsWithLiteral: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseObsoleteReferences browseUncommentedMethodsWithInitials: methodHierarchyBrowserForClass:selector: spawnHierarchyForClass:selector:)('query' allBehaviorsDo: allCallsOn: allCallsOn:and: allCallsOn:from: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsNoDoitsSelect: allMethodsSelect: allMethodsWithSourceString:matchCase: allObjectsDo: allObjectsSelect: allPrimitiveMethods allPrimitiveMethodsInCategories: allReferencesToPool:from: allSelectorsWithAnyImplementorsIn: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnimplementedNonPrimitiveCalls allUnreferencedClassVariablesOf: allUnusedClassesWithout: hierarchyOfClassesSurrounding: hierarchyOfImplementorsOf:forClass: isThereAnImplementorOf: numberOfImplementorsOf: obsoleteBehaviors obsoleteClasses obsoleteMethodReferences selectAllMethods: selectAllMethodsNoDoits: unimplemented)('ui' confirmRemovalOf:on: showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)('*tools-browser' browseClass: browseHierarchy: browseObsoleteMethodReferences browserClass browserClass: defaultBrowserClass defaultHierarchyBrowserClass hierarchyBrowserClass hierarchyBrowserClass:)!!SystemDictionary reorganize!('accessing' organization)('class names' classNamed: classNames flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: renameClass:as: renameClassNamed:as:)('dictionary access' associationAtOrAbove:ifAbsent: associationOrUndeclaredAt: at:put: atOrAbove:ifAbsent: atOrBelow:ifAbsent: environmentForCategory: includesKeyOrAbove: kernelCategories scopeFor:from:envtAndPathIfFound:)('housekeeping' cleanOutUndeclared compressSources condenseChanges condenseSources forgetDoIts makeExternalRelease makeInternalRelease makeSqueaklandRelease reclaimDependents recompileAllFrom: reconstructChanges reformatChangesToUTF8 removeAllLineFeeds removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess memoryHogs okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)('miscellaneous' exitToDebugger handleUserInterrupt hasMorphic logError:inContext:to: m17nVersion nihongoVersion setMacFileInfoOn: verifyMorphicAvailability)('objects from disk' objectForDataStream: storeDataOn:)('printing' printElementsOn:)('retrieving' allClasses allClassesDo: pointersTo: pointersTo:except: pointersToItem:of: poolUsers)('shrinking' abandonSources abandonTempNames cleanUpUndoCommands computeImageSegmentation discard3D discardDiscards discardFFI discardFlash discardMIDI discardMVC discardMorphic discardNetworking discardOddsAndEnds discardSUnit discardSoundAndSpeech discardSoundSynthesis discardSpeech discardTrueType discardWonderland lastRemoval majorShrink presumedSentMessages removeAllUnSentMessages removeNormalCruft removeSelector: reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout: writeImageSegmentsFrom:withKernel: zapAllOtherProjects zapMVCprojects)('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: isMorphic processShutDownList: processStartUpList: quitPrimitive removeFromShutDownList: removeFromStartUpList: send:toClassesNamedIn:with: setGCParameters shutDown shutDownSound snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)('sources, change log' aboutThisSystem assureStartupStampLogged copyright currentChangeSetString currentProjectDo: datedVersion endianness externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources lastUpdateString recover: systemInformationString timeStamp: writeRecentCharacters:toFileNamed: writeRecentToFile)('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)('copying' assureUniClass veryDeepCopyWith: vocabularyDemanded)('deprecated' browseObsoleteMethodReferences changeImageNameTo: changesName clearProfile closeSourceFiles dumpProfile extraVMMemory extraVMMemory: fullNameForChangesNamed: fullNameForImageNamed: getFileNameFromUser getSystemAttribute: getVMParameters imageName imageName: imagePath isBigEndian isLittleEndian lastQuitLogPosition listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logChange: obsoleteBehaviors obsoleteClasses obsoleteMethodReferences openSourceFiles osVersion platformName platformSubtype profile: readDocumentFile removeClassFromSystem:logged: saveAs saveAsEmbeddedImage saveAsNewVersion saveChangesInFileNamed: saveImageInFileNamed: saveImageSegments saveSession setPlatformPreferences snapshot:andQuit: snapshot:andQuit:embedded: sourcesName startProfiling stopProfiling swapBytesIn:from:to: unloadModule: version vmParameterAt: vmParameterAt:put: vmPath vmVersion)('ui' inspectGlobals)('*Refactory-RBAddonsReasonable')('image, changes name' primImageName primImageName: primVmPath)('squeakland' fixObsoleteReferences makeSqueaklandReleasePhaseCleanup makeSqueaklandReleasePhaseFinalSettings makeSqueaklandReleasePhasePrepare makeSqueaklandReleasePhaseStripping)!