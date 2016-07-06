'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6262] on 25 September 2004 at 10:11:45 pm'!"Change Set:		KCPCleanSystDic004Date:			25 September 2004Author:			Stephane ducasseMove recompileAllFrom: to CompilerCompiler recompileAllFrom: 'AAA'"!!Compiler class methodsFor: 'utilities' stamp: 'sd 9/25/2004 15:07'!recompileAllFrom: firstName 	"Recompile all classes, starting with given name."	Smalltalk forgetDoIts.	Smalltalk allClassesDo: 		[:class | class name >= firstName			ifTrue: 				[Transcript show: class name; cr.				class compileAll]]	"Compiler recompileAllFrom: 'AAABodyShop'."! !!SystemDictionary methodsFor: 'deprecated' stamp: 'sd 9/25/2004 15:08'!recompileAllFrom: firstName 	"Recompile all classes, starting with given name."	self deprecated: 'Use Compiler recompileAllFrom: firstName'.	Smalltalk forgetDoIts.	self allClassesDo: 		[:class | class name >= firstName			ifTrue: 				[Transcript show: class name; cr.				class compileAll]]	"Smalltalk recompileAllFrom: 'AAABodyShop'."! !!SystemDictionary reorganize!('accessing' organization)('class names' classNamed: classNames flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: renameClass:as: renameClassNamed:as:)('dictionary access' associationAtOrAbove:ifAbsent: associationOrUndeclaredAt: at:put: atOrAbove:ifAbsent: atOrBelow:ifAbsent: environmentForCategory: includesKeyOrAbove: kernelCategories scopeFor:from:envtAndPathIfFound:)('housekeeping' cleanOutUndeclared compressSources condenseChanges condenseSources forgetDoIts makeExternalRelease makeInternalRelease makeSqueaklandRelease reclaimDependents reconstructChanges reformatChangesToUTF8 removeAllLineFeeds removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow createStackOverflow: garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess memoryHogs okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)('miscellaneous' exitToDebugger handleUserInterrupt hasMorphic logError:inContext:to: m17nVersion nihongoVersion setMacFileInfoOn: verifyMorphicAvailability)('objects from disk' objectForDataStream: storeDataOn:)('printing' printElementsOn:)('retrieving' allClasses allClassesDo: pointersTo: pointersTo:except: pointersToItem:of: poolUsers)('shrinking' abandonSources abandonTempNames cleanUpUndoCommands computeImageSegmentation discard3D discardDiscards discardFFI discardFlash discardMIDI discardMVC discardMorphic discardNetworking discardOddsAndEnds discardSUnit discardSoundAndSpeech discardSoundSynthesis discardSpeech discardTrueType discardWonderland lastRemoval majorShrink presumedSentMessages removeAllUnSentMessages removeNormalCruft removeSelector: reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout: writeImageSegmentsFrom:withKernel: zapAllOtherProjects zapMVCprojects)('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: isMorphic processShutDownList: processStartUpList: quitPrimitive removeFromShutDownList: removeFromStartUpList: send:toClassesNamedIn:with: setGCParameters shutDown shutDownSound snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)('sources, change log' aboutThisSystem assureStartupStampLogged copyright currentChangeSetString currentProjectDo: datedVersion endianness externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources lastUpdateString recover: systemInformationString timeStamp: writeRecentCharacters:toFileNamed: writeRecentToFile)('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)('copying' assureUniClass veryDeepCopyWith: vocabularyDemanded)('deprecated' browseObsoleteMethodReferences changeImageNameTo: changesName clearProfile closeSourceFiles dumpProfile extraVMMemory extraVMMemory: fullNameForChangesNamed: fullNameForImageNamed: getFileNameFromUser getSystemAttribute: getVMParameters imageName imageName: imagePath isBigEndian isLittleEndian lastQuitLogPosition listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logChange: obsoleteBehaviors obsoleteClasses obsoleteMethodReferences openSourceFiles osVersion platformName platformSubtype profile: readDocumentFile recompileAllFrom: removeClassFromSystem:logged: saveAs saveAsEmbeddedImage saveAsNewVersion saveChangesInFileNamed: saveImageInFileNamed: saveImageSegments saveSession setPlatformPreferences snapshot:andQuit: snapshot:andQuit:embedded: sourcesName startProfiling stopProfiling swapBytesIn:from:to: unloadModule: version vmParameterAt: vmParameterAt:put: vmPath vmVersion)('ui' inspectGlobals)('*Refactory-RBAddonsReasonable')('image, changes name' primImageName primImageName: primVmPath)('squeakland' fixObsoleteReferences makeSqueaklandReleasePhaseCleanup makeSqueaklandReleasePhaseFinalSettings makeSqueaklandReleasePhasePrepare makeSqueaklandReleasePhaseStripping)!!Compiler class reorganize!('accessing' couldEvaluate: new old parserClass)('evaluating' evaluate: evaluate:for:logged: evaluate:for:notifying:logged: evaluate:logged: evaluate:notifying:logged:)('utilities' recompileAllFrom:)!