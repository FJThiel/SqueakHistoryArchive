'From Squeak2.9alpha of 13 June 2000 [latest update: #2988] on 26 November 2000 at 9:52:16 am'!"Change Set:		253BetterAlbaphetizer-sqrDate:			16 November 2000Author:			Andres ValloudA better category alphabetizer, with a convenient method so you can do	Smalltalk sortAllCategories"!!Browser methodsFor: 'message category functions' stamp: 'SqR 11/16/2000 13:53'!alphabetizeMessageCategories	classListIndex = 0 ifTrue: [^ false].	self okToChange ifFalse: [^ false].	Smalltalk changes reorganizeClass: self selectedClassOrMetaClass.	self classOrMetaClassOrganizer sortCategories.	self clearUserEditFlag.	self editClass.	self classListIndex: classListIndex.	^ true! !!ClassOrganizer methodsFor: 'accessing' stamp: 'SqR 11/16/2000 13:52'!sortCategories	| privateCategories publicCategories newCategories |	privateCategories _ self categories select:		[:one | (one findString: 'private' startingAt: 1 caseSensitive: false) = 1].	publicCategories _ self categories copyWithoutAll: privateCategories.	newCategories _ publicCategories asSortedCollection asOrderedCollection		addAll: privateCategories asSortedCollection;		asArray.	self categories: newCategories! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'SqR 11/16/2000 13:46'!sortAllCategories	ClassOrganizer allInstances do: [:x | x sortCategories]! !!SystemDictionary reorganize!('accessing' organization)('browsing' browseAllCallsOn: browseAllCallsOn:and: browseAllImplementorsOf: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllObjectReferencesTo:except:ifNone: browseAllSelect: browseAllSelect:name:autoSelect: browseAllUnSentMessages browseAllUnimplementedCalls browseChangedMessages browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWhoseNamesContain: browseMethodsWithSourceString: browseMethodsWithString: browseObsoleteReferences showMenuOf:withFirstItem:ifChosenDo:)('class names' classNamed: classNames flushClassNameCache hasClassNamed: removeClassFromSystem: removeClassFromSystemUnlogged: removeClassNamed: renameClass:as: renameClassNamed:as:)('dictionary access' associationAtOrAbove:ifAbsent: associationOrUndeclaredAt: at:put: atOrAbove:ifAbsent: atOrBelow:ifAbsent: environmentForCategory: includesKeyOrAbove: kernelCategories scopeFor:from:envtAndPathIfFound:)('housekeeping' cleanOutUndeclared condenseChanges condenseSources forgetDoIts makeInternalRelease obsoleteBehaviors obsoleteClasses reclaimDependents recompileAllFrom: removeAllLineFeeds removeEmptyMessageCategories sortAllCategories testDecompiler testFormatter testFormatter2 verifyChanges)('image, changes name' changeImageNameTo: changesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath sourcesName vmPath)('memory space' bytesLeft createStackOverflow garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess memoryHogs okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: signalLowSpace useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)('miscellaneous' beep clipboardText clipboardText: compareTallyIn:to: compilerDisable compilerEnable exitToDebugger extraVMMemory extraVMMemory: forceDisplayUpdate fullScreenMode: getSystemAttribute: getVMParameters gifReaderClass handleUserInterrupt hasMorphic imageImports imageReaderClass itsyVoltage jpegReaderClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: platformName setMacFileInfoOn: spaceForInstancesOf: spaceTally spaceTallyTo: unloadModule: verifyMorphicAvailability viewImageImports vmParameterAt: vmParameterAt:put: vmVersion)('objects from disk' objectForDataStream: storeDataOn:)('printing' printElementsOn:)('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allImplementedMessages allImplementorsOf: allMethodsInCategory: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjectsDo: allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSelectNoDoits: allSentMessages allUnSentMessages allUnSentMessagesIn: allUnimplementedCalls pointersTo: pointersTo:except: pointersToItem:of:)('shrinking' abandonSources abandonTempNames discard3D discardDiscards discardFFI discardFlash discardMVC discardMorphic discardNetworking discardOddsAndEnds discardPluggableWebServer discardSUnit discardSoundAndSpeech discardSoundSynthesis discardSpeech discardTrueType discardVMConstruction lastRemoval majorShrink printSpaceAnalysis printSpaceAnalysis:on: printSpaceDifferenceFrom:to: removeAllUnSentMessages removeSelector: unusedClasses zapMVCprojects)('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: getFileNameFromUser isMorphic lastQuitLogPosition processShutDownList: processStartUpList: processUpdates quitPrimitive readDocumentFile removeFromShutDownList: removeFromStartUpList: saveAs saveChangesInFileNamed: saveImageInFileNamed: saveImageSegments saveSession send:toClassesNamedIn:with: setGCParameters shutDown shutDownSound snapshot:andQuit: snapshotPrimitive unbindExternalPrimitives)('sources, change log' aboutThisSystem assureStartupStampLogged changes closeSourceFiles copyright currentChangeSetString currentProjectDo: endianness externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources lastUpdateString logChange: newChanges: noChanges openSourceFiles recover: setVersion: systemInformationString timeStamp: version writeRecentCharacters:toFileNamed: writeRecentToFile)('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)('copying' veryDeepCopyWith:)!