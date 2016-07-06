'From Squeak3.7gamma of ''17 July 2004'' [latest update: #5978] on 2 August 2004 at 1:34:28 am'!"Change Set:		PrepareReleaseImage-dewDate:			1 August 2004Author:			Doug WayAdds SmalltalkImage>>prepareReleaseImage which can be executed to perform various cleanups in preparation for a gamma candidate release image."!!SmalltalkImage methodsFor: 'image cleanup' stamp: 'dew 8/1/2004 17:37'!fixObsoleteReferences	"SmalltalkImage current fixObsoleteReferences"	| informee obsoleteBindings obsName realName realClass |	Preference allInstances do: [:each | 		informee _ each instVarNamed: #changeInformee.		((informee isKindOf: Behavior)			and: [informee isObsolete])			ifTrue: [				Transcript show: each name; cr.				each instVarNamed: #changeInformee put: (Smalltalk at: (informee name copyReplaceAll: 'AnObsolete' with: '') asSymbol)]]. 	CompiledMethod allInstances do: [:method |		obsoleteBindings _ method literals select: [:literal |			literal isVariableBinding				and: [literal value isBehavior]				and: [literal value isObsolete]].		obsoleteBindings do: [:binding |			obsName _ binding value name.			Transcript show: obsName; cr.			realName _ obsName copyReplaceAll: 'AnObsolete' with: ''.			realClass _ Smalltalk at: realName asSymbol ifAbsent: [UndefinedObject].			binding isSpecialWriteBinding				ifTrue: [binding privateSetKey: binding key value: realClass]				ifFalse: [binding key: binding key value: realClass]]].	Behavior flushObsoleteSubclasses.	Smalltalk garbageCollect; garbageCollect.	Smalltalk obsoleteBehaviors size > 0		ifTrue: [Smalltalk obsoleteBehaviors inspect]! !!SmalltalkImage methodsFor: 'image cleanup' stamp: 'dew 8/2/2004 01:33'!prepareReleaseImage	"Perform various image cleanups in preparation for making a Squeak gamma release candidate image."	"SmalltalkImage current prepareReleaseImage"		| projectChangeSetNames |	(self confirm: 'Are you sure you want to prepare a release image?This will perform several irreversible cleanups on this image.')		ifFalse: [^ self].	Undeclared removeUnreferencedKeys.	StandardScriptingSystem initialize.	Preferences initialize.	Preferences chooseInitialSettings.	(Object classPool at: #DependentsFields) size > 1 ifTrue: [self error:'Still have dependents'].	Undeclared isEmpty ifFalse: [self error:'Please clean out Undeclared'].	Browser initialize.	ScriptingSystem deletePrivateGraphics.  "?"		"Delete all changesets except those currently used by existing projects."	projectChangeSetNames _ Project allSubInstances collect: [:proj | proj changeSet name].	ChangeSorter removeChangeSetsNamedSuchThat:		[:cs | (projectChangeSetNames includes: cs) not].	ChangeSet current clear.	ChangeSet current name: 'Unnamed1'.	Smalltalk garbageCollect.	"Reinitialize DataStream; it may hold on to some zapped entitities"	DataStream initialize.	"Remove existing player references"	References keys do:[:k| References removeKey: k].  "?"	Smalltalk garbageCollect.	ScheduledControllers _ nil.	Smalltalk garbageCollect.		Language recreateFlaps.		Behavior flushObsoleteSubclasses.	Smalltalk garbageCollect; garbageCollect.	Smalltalk obsoleteBehaviors isEmpty ifFalse:[self error:'Still have obsolete behaviors'].	DataStream initialize.	self fixObsoleteReferences.	Smalltalk forgetDoIts.  "?"	Smalltalk flushClassNameCache.  "?"	3 timesRepeat: [		Smalltalk garbageCollect.		Symbol compactSymbolTable.	].! !!SmalltalkImage reorganize!('endian' endianness isBigEndian isLittleEndian)('external' exitToDebugger unbindExternalPrimitives)('image cleanup' fixObsoleteReferences prepareReleaseImage)('image, changes names' changeImageNameTo: changesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath sourceFileVersionString sourcesName vmPath)('modules' listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules unloadModule:)('preferences' setPlatformPreferences)('quit' quitPrimitive)('snapshot and quit' getFileNameFromUser readDocumentFile saveSession shutDown snapshot:andQuit: snapshot:andQuit:embedded: snapshotEmbeddedPrimitive snapshotPrimitive)('sources, changes log' assureStartupStampLogged closeSourceFiles event: forceChangesToDisk lastQuitLogPosition lastQuitLogPosition: logChange: openSourceFiles saveAs saveAsEmbeddedImage saveAsNewVersion saveChangesInFileNamed: saveImageInFileNamed: saveImageSegments)('system attribute' extractParameters getSystemAttribute: osVersion platformName platformSubtype vmVersion)('utilities' stripMethods:messageCode:)('vm parameters' extraVMMemory extraVMMemory: getVMParameters vmParameterAt: vmParameterAt:put:)('vm profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)('vm statistics' textMarkerForShortReport vmStatisticsReportString vmStatisticsShortString)('private source file' sourceFileVersionString:)!