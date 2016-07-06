'From Squeak3.1alpha of 7 February 2001 [latest update: #3683] on 21 February 2001 at 6:41:07 am'!"Change Set:		flapsFixFix-swDate:			21 February 2001Author:			Scott WallaceFixes bug that did not preserve global flaps for future use when asked to."!!PasteUpMorph methodsFor: 'flaps' stamp: 'sw 2/21/2001 06:36'!addGlobalFlaps 	"Must make global flaps adapt to world.  Do this even if not shown, so the old world will not be pointed at by the flaps."	| use thisWorld |	use _ Preferences useGlobalFlaps.	CurrentProjectRefactoring currentFlapsSuppressed ifTrue: [use _ false].	"Smalltalk isMorphic ifFalse: [use _ false]."	thisWorld _ use 		ifTrue: [self]		ifFalse: [PasteUpMorph new initForProject:  "fake to be flap owner"						WorldState new;					bounds: (0@0 extent: 4000@4000);					viewBox: (0@0 extent: 4000@4000)].		Utilities globalFlapTabsIfAny do: [:aFlapTab |		(CurrentProjectRefactoring isFlapEnabled: aFlapTab) ifTrue:			[(aFlapTab world == thisWorld) ifFalse:				[thisWorld addMorphFront: aFlapTab.				aFlapTab adaptToWorld: thisWorld].	"always do"			use ifTrue:				[aFlapTab spanWorld.				aFlapTab adjustPositionAfterHidingFlap.				aFlapTab flapShowing ifTrue: [aFlapTab showFlap]]]]."	Utilities clobberFlapTabList.	Utilities initializeStandardFlaps.	self currentWorld deleteAllFlapArtifacts.	self currentWorld addGlobalFlaps."! !!PasteUpMorph methodsFor: 'flaps' stamp: 'sw 2/21/2001 06:29'!enableGlobalFlaps 	"Restore saved global flaps, or obtain brand-new system defaults if necessary"	Utilities globalFlapTabs. 		 "If nil, creates new ones"	self addGlobalFlaps 			 "put them on screen"! !!Preferences class methodsFor: 'preferences dictionary' stamp: 'sw 2/21/2001 06:29'!noteThatFlag: prefSymbol justChangedTo: aBoolean	"Provides a hook so that a user's toggling of a preference might precipitate some immediate action"	| keep |	prefSymbol == #useGlobalFlaps ifTrue:		[aBoolean			ifFalse:		"Turning off use of flaps"				[keep _ self confirm:'Do you want to preserve the existingglobal flaps for future use?'.				Utilities globalFlapTabsIfAny do:					[:aFlapTab | Utilities removeFlapTab: aFlapTab keepInList: keep.					aFlapTab isInWorld ifTrue: [self error: 'Flap problem']].				keep ifFalse: [Utilities clobberFlapTabList]]			ifTrue:		"Turning on use of flaps"				[Smalltalk isMorphic ifTrue:					[self currentWorld enableGlobalFlaps]]].	prefSymbol == #roundedWindowCorners ifTrue: [Display repaintMorphicDisplay].	prefSymbol == #optionalButtons ifTrue:		[Utilities replacePartSatisfying: [:el | (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isKindOf: FileList]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  FileList openAsMorph applyModelExtent].	(prefSymbol == #optionalButtons  or: [prefSymbol == #annotationPanes]) ifTrue:		[Utilities replaceBrowserInToolsFlap].	(prefSymbol == #smartUpdating) ifTrue:		[SystemWindow allSubInstancesDo:			[:aWindow | aWindow amendSteppingStatus]].	(prefSymbol == #eToyFriendly) ifTrue:		[ScriptingSystem customizeForEToyUsers: aBoolean].	((prefSymbol == #infiniteUndo) and: [aBoolean not]) ifTrue:		[CommandHistory resetAllHistory].	(prefSymbol == #showProjectNavigator) ifTrue:		[Project current assureNavigatorPresenceMatchesPreference].	prefSymbol == #universalTiles ifTrue:		[(self isProjectPreference: prefSymbol) ifFalse:			[^ self inform: 'This is bad -- you should not have done that, becausethe change will take effect for *all projects*, includingpre-existing ones.  Unfortunately this check is done afterthe damage is done, so you are hosed.  Fortunately, however,you can simply reverse your choice right now andno deep damage will probably have been done.'].		aBoolean			ifFalse:				[self inform: 'CAUTION -- this branch is not supported;once you go to universal tiles in a project,basically there is no going back, so kindlyjust toggle this preference back to true... sorry(If you really want to use "classic tile" open upa new project that does NOT have the universalTiles already set.)']			ifTrue:				[Preferences capitalizedReferences ifFalse:					[Preferences enable: #capitalizedReferences.					self inform: 'Note that the "capitalizedReferences" flaghas now been automatically set to true foryou, since this is required for the use ofuniversal tiles.'].				World isMorph ifTrue:					[World flapTabs do: [:ff | (ff isKindOf: ViewerFlapTab) ifTrue:						[ff referent delete.  ff delete]]]]]! !