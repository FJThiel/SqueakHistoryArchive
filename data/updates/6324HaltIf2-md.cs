'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6322] on 17 October 2004 at 6:46:01 pm'!"Change Set:		HaltIf2-mdDate:			17 October 2004Author:			Marcus Denkermove Object>>haltIfNil into the debugging category, enhanceshaltIf: to take optionally a 1-arg block:  1 haltIf: [:o | o = 1]fixes a wrongly categorized method in collection."!!Object methodsFor: 'debugging' stamp: 'md 10/17/2004 18:40'!haltIf: condition	"This is the typical message to use for inserting breakpoints during 	debugging.  Param can be a block or expression, halt if true.	If the Block has one arg, the receiver is bound to that. 	If the condition is a selector, we look up in the callchain. Halt if      any method's selector equals selector."	| cntxt |	condition isSymbol ifTrue:[		"only halt if a method with selector symbol is in callchain"		cntxt := thisContext sender.		[cntxt sender isNil] whileFalse: [			(cntxt selector = condition) ifTrue: [Halt signal].			cntxt := cntxt sender.			].		^self.	].	(condition isBlock 			ifTrue: [condition valueWithPossibleArgument: self] 			ifFalse: [condition] 	) ifTrue: [		Halt signal	].! !!ObjectTest methodsFor: 'testing - debugging' stamp: 'md 10/17/2004 18:39'!testHaltIf	self should: [self haltIf: true] raise: Halt.	self shouldnt: [self haltIf: false] raise: Halt.	self should: [self haltIf: [true]] raise: Halt.	self shouldnt: [self haltIf: [false]] raise: Halt.	self should: [self haltIf: #testHaltIf.] raise: Halt.	self shouldnt: [self haltIf: #teadfasdfltIf.] raise: Halt.	self should: [self a] raise: Halt.	self shouldnt: [self a1] raise: Halt.	self should: [self haltIf: [:o | o class = self class]] raise: Halt.	self shouldnt: [self haltIf: [:o | o class ~= self class]] raise: Halt.! !!Collection reorganize!('accessing' anyOne atRandom capacity size)('adapting' adaptToCollection:andSend: adaptToComplex:andSend: adaptToNumber:andSend: adaptToPoint:andSend: adaptToString:andSend:)('adding' add: add:withOccurrences: addAll: addIfNotPresent:)('arithmetic' * + - / // \\ raisedTo:)('comparing' hash)('converting' asArray asBag asByteArray asCharacterSet asIdentitySet asIdentitySkipList asOrderedCollection asSet asSkipList asSkipList: asSortedArray asSortedCollection asSortedCollection: topologicallySortedUsing:)('copying' , copyWith: copyWithDependent: copyWithout: copyWithoutAll:)('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:thenDo: collect:thenSelect: count: detect: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: groupBy:having: inject:into: intersection: noneSatisfy: reject: reject:thenDo: select: select:thenCollect: select:thenDo: union:)('filter streaming' contents flattenOnStream: write:)('math functions' abs arcCos arcSin arcTan average ceiling cos degreeCos degreeSin exp floor ln log max median min negated range reciprocal roundTo: rounded sign sin sqrt squared sum tan truncated)('printing' printElementsOn: printNameOn: printOn: storeOn:)('private' emptyCheck errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed toBraceStack:)('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifEmpty:ifNotEmptyDo: ifNotEmpty: ifNotEmpty:ifEmpty: ifNotEmptyDo: ifNotEmptyDo:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable isZero notEmpty occurrencesOf:)('*packageinfo-base' gather:)!!Object reorganize!('*sunit-preload' sunitAddDependent: sunitChanged: sunitRemoveDependent:)('*system-support' systemNavigation)('*tools-browser' browse browseHierarchy)('Breakpoint' break)('accessing' addInstanceVarNamed:withValue: at: at:modify: at:put: basicAt: basicAt:put: basicSize bindWithTemp: doIfNotNil: ifNil:ifNotNilDo: ifNotNilDo: ifNotNilDo:ifNil: in: presenter readFromString: size yourself)('associating' ->)('binding' bindingOf:)('casing' caseOf: caseOf:otherwise:)('class membership' class inheritsFromAnyIn: isKindOf: isKindOf:orOf: isMemberOf: respondsTo: xxxClass)('comparing' = closeTo: hash hashMappedBy: identityHashMappedBy: identityHashPrintString literalEqual: ~=)('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asActionSequence asActionSequenceTrappingErrors asDraggableMorph asOrderedCollection asString asStringOrText complexContents mustBeBoolean mustBeBooleanIn: printDirectlyToDisplay withoutListWrapper)('copying' clone copy copyAddedStateFrom: copyFrom: copySameFrom: copyTwoLevel deepCopy initialDeepCopierSize postCopy shallowCopy veryDeepCopy veryDeepCopySibling veryDeepCopyUsing: veryDeepCopyWith: veryDeepFixupWith: veryDeepInner:)('creation' asMorph asStringMorph asTextMorph openAsMorph)('dependents access' addDependent: breakDependents canDiscardEdits dependents evaluate:wheneverChangeIn: hasUnacceptedEdits myDependents myDependents: release removeDependent:)('deprecated' beep: beepPrimitive)('drag and drop' acceptDroppingMorph:event:inMorph: dragAnimationFor:transferMorph: dragPassengerFor:inMorph: dragTransferType dragTransferTypeForMorph: wantsDroppedMorph:event:inMorph:)('debugging' assert: halt halt: haltIf: haltIfNil)('error handling' backwardCompatibilityOnly: caseError confirm: confirm:orCancel: deprecated: deprecated:block: deprecated:explanation: deprecatedExplanation: doesNotUnderstand: dpsTrace: dpsTrace:levels: dpsTrace:levels:withContext: error: externalCallFailed handles: notify: notify:at: notifyWithLabel: primitiveFailed shouldBeImplemented shouldNotImplement subclassResponsibility tryToDefineVariableAccess:)('evaluating' value valueWithArguments:)('events-accessing' actionForEvent: actionForEvent:ifAbsent: actionMap actionSequenceForEvent: actionsDo: createActionMap hasActionForEvent: setActionSequence:forEvent: updateableActionMap)('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)('events-triggering' triggerEvent: triggerEvent:ifNotHandled: triggerEvent:with: triggerEvent:with:ifNotHandled: triggerEvent:withArguments: triggerEvent:withArguments:ifNotHandled:)('filter streaming' byteEncode: drawOnCanvas: elementSeparator encodePostscriptOn: flattenOnStream: fullDrawPostscriptOn: printOnStream: putOn: storeOnStream: writeOnFilterStream:)('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)('flagging' isThisEverCalled isThisEverCalled: logEntry logExecution logExit)('graph model' addModelYellowButtonMenuItemsTo:forMorph:hand: hasModelYellowButtonMenuItems)('inspecting' basicInspect inspect inspectorClass)('locales' localeChanged)('macpal' codeStrippedOut: contentsChanged currentEvent currentHand currentVocabulary currentWorld flash ifKindOf:thenDo: instanceVariableValues isUniversalTiles objectRepresented playSoundNamed: refusesToAcceptCode scriptPerformer slotInfo)('message handling' perform: perform:orSendTo: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: withArgs:executeMethod:)('objects from disk' comeFullyUpOnReload: convertToCurrentVersion:refStream: indexIfCompact objectForDataStream: readDataFrom:size: saveOnFile storeDataOn:)('parts bin' descriptionForPartsBin)('printing' fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString nominallyUnsent: printOn: printString printStringLimitedTo: propertyList reportableSize storeOn: storeString stringForReadout stringRepresentation)('scripting' adaptedToWorld: contentsGetz: defaultFloatPrecisionFor: evaluateUnloggedForSelf: methodInterfacesForCategory:inVocabulary:limitClass: methodInterfacesForInstanceVariablesCategoryIn: methodInterfacesForScriptsCategoryIn: selfWrittenAsIll selfWrittenAsIm selfWrittenAsMe selfWrittenAsMy selfWrittenAsThis)('scripts-kernel' universalTilesForGetterOf: universalTilesForInterface:)('system primitives' asOop becomeForward: becomeForward:copyHash: className creationStamp instVarAt: instVarAt:put: instVarNamed: instVarNamed:put: oopString primitiveChangeClassTo: rootStubInImageSegment: someObject)('testing' basicType beViewed costumes haveFullProtocolBrowsed haveFullProtocolBrowsedShowingSelector: isBehavior isBlock isBlockClosure isCharacter isCollection isColor isColorForm isCompiledMethod isComplex isFloat isForm isFraction isHeap isInteger isInterval isMessageSend isMorph isMorphicEvent isMorphicModel isNumber isPoint isPseudoContext isStream isString isSymbol isSystemWindow isText isTransparent isVariableBinding isWebBrowser knownName name nameForViewer notNil openInstanceBrowserWithTiles renameTo: showDiffs stepAt:in: stepIn: stepTime stepTimeIn: vocabularyDemanded wantsDiffFeedback wantsSteps wantsStepsIn:)('translation support' inline: var:declareC:)('undo' capturedState commandHistory purgeAllCommands redoFromCapturedState: refineRedoTarget:selector:arguments:in: refineUndoTarget:selector:arguments:in: rememberCommand: rememberUndoableAction:named: undoFromCapturedState:)('updating' changed changed: changed:with: handledListVerification noteSelectionIndex:for: okToChange update: update:with: updateListsAndCodeIn: windowIsClosing)('user interface' addModelItemsToWindowMenu: addModelMenuItemsTo:forMorph:hand: asExplorerString beep defaultBackgroundColor defaultLabelForInspector eToyStreamedRepresentationNotifying: explore fullScreenSize hasContentsInExplorer inform: initialExtent inspectWithLabel: launchPartVia: launchPartVia:label: launchTileToRefer modelSleep modelWakeUp modelWakeUpIn: mouseUpBalk: newTileMorphRepresentative notYetImplemented windowActiveOnFirstClick windowReqNewLabel:)('viewer' assureUniClass belongsToUniClass browseOwnClassSubProtocol categoriesForViewer: categoriesForVocabulary:limitClass: chooseNewNameForReference defaultLimitClassForVocabulary: defaultNameStemForInstances elementTypeFor:vocabulary: externalName graphicForViewerTab hasUserDefinedSlots infoFor:inViewer: initialTypeForSlotNamed: isPlayerLike methodInterfacesInPresentationOrderFrom:forCategory: newScriptorAround: offerViewerMenuFor:event: offerViewerMenuForEvt:morph: renameScript: tilePhrasesForCategory:inViewer: tilePhrasesForMethodInterfaces:inViewer: tilePhrasesForSelectorList:inViewer: tileToRefer uniqueInstanceVariableNameLike:excluding: uniqueNameForReference uniqueNameForReferenceFrom: uniqueNameForReferenceOrNil updateThresholdForGraphicInViewerTab usableMethodInterfacesIn:)('world hacking' couldOpenInMorphic)('private' errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: species storeAt:inTempFrame:)('thumbnail' iconOrThumbnailOfSize:)!