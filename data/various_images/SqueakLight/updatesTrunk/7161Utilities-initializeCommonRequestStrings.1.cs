'From Squeak3.10beta of 22 July 2007 [latest update: #7159] on 17 July 2009 at 7:33:48 pm'!"Change Set:		7160Utilities-initializeCommonRequestStringsDate:			17 July 2009Author:			Edgar J.De CleeneSome convenince Utilities for test updates in developer computer and from remote ftp"!!Utilities class methodsFor: 'common requests' stamp: 'edc 7/17/2009 19:27'!initializeCommonRequestStrings	"Initialize the common request strings, a directly-editable list of expressions that can be evaluated from the 'do...' menu."	CommonRequestStrings _ StringHolder new contents: 'Utilities emergencyCollapse.Utilities closeAllDebuggers.Utilities applyUpdatesFromDisk.Utilities ftpUpdates.Utilities browseRecentSubmissions.SmalltalkImage current aboutThisSystem.-Sensor keyboard.ParagraphEditor abandonChangeText.Cursor normal show.-CommandHistory resetAllHistory.Project allInstancesDo: [:p | p displayDepth: 16].ScriptingSystem inspectFormDictionary.Form fromUser bitEdit.Display border: (0@0 extent: 640@480) width: 2.-Undeclared inspect.Undeclared removeUnreferencedKeys; inspect.Transcript clear.Utilities grabScreenAndSaveOnDisk.FrameRateMorph new openInHand.-Utilities reconstructTextWindowsFromFileNamed: ''TW''.Utilities storeTextWindowContentsToFileNamed: ''TW''.ChangeSorter removeEmptyUnnamedChangeSets.ChangeSorter reorderChangeSets.-ActiveWorld installVectorVocabulary.ActiveWorld abandonVocabularyPreference.Smalltalk saveAsNewVersion'"Utilities initializeCommonRequestStrings"! !Utilities initializeCommonRequestStrings!