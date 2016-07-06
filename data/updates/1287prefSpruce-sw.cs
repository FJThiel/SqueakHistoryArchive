'From Squeak 2.4c of May 10, 1999 on 29 June 1999 at 3:01:36 pm'!"Change Set:		prefSpruce-swDate:			29 June 1999Author:			Scott Wallace(1)  Removes reliance on the doesNotUnderstand mechanism for Preferences; instead, each defined preference is represented by an explicit retrieval method.  The old mechanism remains in place, but could now be removed, or replaced by something that has the same effect but logs the call to the Transcript as a warning that an undefined preference is being invoked.(2)  Adds a way that default values for new preferences, and help messages for preferences, can be added or changed without needing to modify huge monolithic methods.(3)  Adds a method you can call to get hard-coded preferences compiled simply."!!Preferences class methodsFor: 'help' stamp: 'sw 6/29/1999 14:36'!initHelpMsgsA	"Automatically called whenever you call   	Preferences initializeHelpMessages		or	Preferences callHelpMessageInitializersby virtue of its being in the 'help' category of Preferences class"	#((menuColorFromWorld			'Governs whether the colors used in morphic menus should be derived from the color of the world background.')	(roundedWindowCorners			'Governs whether morphic system windows should have rounded corners')) do:		[:pair | HelpDictionary at: pair first put: 			(pair first, ':', pair last)]! !!Preferences class methodsFor: 'initialization' stamp: 'sw 6/29/1999 08:53'!callHelpMessageInitializers	"Preferences callHelpMessageInitializers"	(self class organization listAtCategoryNamed: #help) do:		[:aSelector | aSelector numArgs = 0 ifTrue:			[self perform: aSelector]]! !!Preferences class methodsFor: 'initialization' stamp: 'sw 6/29/1999 14:33'!initializeHelpMessages	"Preferences initializeHelpMessages"  	HelpDictionary _ Dictionary new.	#((allowLabelDragging'If true, allow dragging of system windows when clicking on the label')(allowSoundQuickStart'If true, attempt to start playing sounds using optional "quick start"')(allowSysWindowEmbedding'Determines whether, in Morphic, SystemWindows should automatically be droppable into willing receptors')(automaticViewerPlacement'If true, new viewers are automatically positioned near the objects they view; if false, new viewers are attached to the hand, from whence you much choose a destination for them')(balloonHelpEnabled'Whether balloon help should be offered when the cursor lingers over certain objects.')(browseWithPrettyPrint'If true, browsers will automatically format their contents')(cautionBeforeClosing 'If true, Morphic windows seen in an mvc project will put up a warning before allowing themselves to be dismissed')(cmdDotEnabled'If true, cmd-dot brings up a debugger;if false, the cmd-dot interrupt is disabled')(confirmFirstUseOfStyle'If true, the first attempt to submit a method with non-standard style will bring up a confirmation dialog')(disableSounds'If true, all sound playing is disabled')	(editPlayerScriptsInPlace 'If true, textual player scripts are edited in place in Scriptors (still imperfectly implemented)')(eToyScheme'If true, new scripting spaces place the Playfield to the left and the the palette to the right of the window; if false, the opposite is true.')(fastDragWindowForMorphic'If true, morphic window drag will be done by dragging an outline of the window.')(fenceEnabled'Whether an object obeying motion scripts should stop moving when it reaches the edge of its container.')(ignoreStyleIfOnlyBold'If true, then any method submission in which the only style change is for bolding will be treated as a method with no style specifications')(inboardScrollbars'If true, then ScrollPane will place scrollbars inside on the right and will not hide them on exit')(logDebuggerStackToFile'If true, whenever you fall into a debugger a summary of its stack will be written to a file named''SqueakDebug.log''')(mouseOverHalosEnabled'If false, halos will not be put up on mouseovers even if they otherwise might be.')(noviceMode 'If true, certain novice-mode accommodations are made.')(printAlternateSyntax'If true, thenprettyPrint using experimental syntax.Otherwise use normal ST-80 syntax.')(reverseWindowStagger'If true, a reverse-stagger strategy  is used for determining where newly launched windows will be placed; if false, a direct- stagger strategy is used.')(showDebugHaloHandle 'If true, a special debugging halo handle is displayed at the right of the halo; if false, no such handle is shown.')(showDiffsInChangeList'If true, changeList browsers and Versions browsers reveal the differences between successive versions or between the in-memory code and the code on disk')(showPlayerSource'If true, then all Player methods with fewer than 2 arguments are included in Viewers, whether or not they are intended for end-user use.  This can be dangerous')(showProjectZoom'If true, then show a zoom effect when entering or leaving projects.  This can be costly of memory (at least an extra screen buffer) so dont use it in low space situations.  But it is cool.')(showScriptSource'If true, then the actual Smalltalk source code for methods is shown in the detail panes for scripts in a viewer; if false, then a help message for scripts is shown instead.')(showTimeStampsInMenuTitles'If true, then the author''s timestamp is displayed as the menu title of any message list; if false, no author''s timestamps are shown')(suppressCheckForSlips 'If false, then whenever you file out a change set, it is checked for ''slips'' and if any are found, you are so informed and given a chance to open a browser on them')(suppressUpdateServerPrompt'If true, the prompt for server choice when updating code from the server is suppressed.  Set this to true to leave the server choice unchanged from update to update.')(thoroughSenders'If true, then ''senders'' browsers will dive inside structured literals in their search')(uniformWindowColors'If true, then all standard windows are given the same color rather than their customized window-type-specific colors')(unlimitedPaintArea'If true, the painting area for a new drawing will not be limited in size; if false, a reasonablelimit will be applied, in an attempt to hold down memory and time price.')(updateRemoveSequenceNum'If true, then remove the leading sequence number from the filename before automatically saving a local copy of any update loaded.')(updateSavesFile'If true, then when an update is loaded from the server, a copy of it will automatically be saved on a local file as well.')(useAnnotationPanes'If true, a thin horizontal annotation pane is used in message-list browsers.')(useDetailPanesInViewers'If true, then Viewers will have an extra "�" control at the left of each row, the hitting of which toggles the appearance of a textual detail pane.')(useGlobalFlaps'If true, then flaps are shown along the edges of Morphic projects.')(useNewViewers'If true, then the new kinds of viewers introduced in Squeak 2.3 are used; if false, then the old style, from earlier releases, are still used.  Old viewers will hopefully soon be removed from the system.')(warnIfNoChangesFile'If true, then you will be warned, whenever you start up, if no changes filecan be found')(warnIfNoSourcesFile 'If true, then you will be warned, whenever you start up, if no sources file can be found')) do:		[:pair | HelpDictionary at: pair first put: 			(pair first, ':', pair last)].	self callHelpMessageInitializers "pick up further elements from various methods that may have accumulated in the 'help' method category"! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:25'!allowLabelDragging	^ self valueOfFlag: #allowLabelDragging! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!allowMVCprojects	^ self valueOfFlag: #allowMVCprojects! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 20:48'!allowSoundQuickStart	^ self valueOfFlag: #allowSoundQuickStart! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!allowSysWindowEmbedding	^ self valueOfFlag: #allowSysWindowEmbedding! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!autoAccessors	^ self valueOfFlag: #autoAccessors! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 20:48'!automaticViewerPlacement	^ self valueOfFlag: #automaticViewerPlacement! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!balloonHelpEnabled	^ self valueOfFlag: #balloonHelpEnabled! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!browseWithPrettyPrint	^ self valueOfFlag: #browseWithPrettyPrint! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!cmdDotEnabled	^ self valueOfFlag: #cmdDotEnabled! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:26'!compressFlashImages	^ self valueOfFlag: #compressFlashImages! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!confirmFirstUseOfStyle	^ self valueOfFlag: #confirmFirstUseOfStyle! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!disableSounds	^ self valueOfFlag: #disableSounds! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!extractFlashInHighQuality	^ self valueOfFlag: #extractFlashInHighQuality! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!extractFlashInHighestQuality	^ self valueOfFlag: #extractFlashInHighestQuality! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!fastDragWindowForMorphic	^ self valueOfFlag: #fastDragWindowForMorphic! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:27'!fenceEnabled	^ self valueOfFlag: #fenceEnabled! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!ignoreStyleIfOnlyBold	^ self valueOfFlag: #ignoreStyleIfOnlyBold! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!inboardScrollbars	^ self valueOfFlag: #inboardScrollbars! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!logDebuggerStackToFile	^ self valueOfFlag: #logDebuggerStackToFile! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!mouseOverHalosEnabled	^ self valueOfFlag: #mouseOverHalosEnabled! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!noviceMode	^ self valueOfFlag: #noviceMode! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 20:50'!printAlternateSyntax	^ self valueOfFlag: #printAlternateSyntax! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!reverseWindowStagger	^ self valueOfFlag: #reverseWindowStagger! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:28'!showDebugHaloHandle	^ self valueOfFlag: #showDebugHaloHandle! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:29'!showDiffsInChangeList	^ self valueOfFlag: #showDiffsInChangeList! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:29'!showProjectZoom	^ self valueOfFlag: #showProjectZoom! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:29'!showTimeStampsInMenuTitles	^ self valueOfFlag: #showTimeStampsInMenuTitles! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 20:51'!suppressCheckForSlips	^ self valueOfFlag: #suppressCheckForSlips! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 20:51'!suppressUpdateServerPrompt	^ self valueOfFlag: #suppressUpdateServerPrompt! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:29'!thoroughSenders	^ self valueOfFlag: #thoroughSenders! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!unlimitedPaintArea	^ self valueOfFlag: #unlimitedPaintArea! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!updateSavesFile	^ self valueOfFlag: #updateSavesFile! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!useAnnotationPanes	^ self valueOfFlag: #useAnnotationPanes! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!useGlobalFlaps	^ self valueOfFlag: #useGlobalFlaps! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!warnIfNoChangesFile	^ self valueOfFlag: #warnIfNoChangesFile! !!Preferences class methodsFor: 'standard preferences' stamp: 'sw 6/28/1999 16:30'!warnIfNoSourcesFile	^ self valueOfFlag: #warnIfNoSourcesFile! !!Preferences class methodsFor: 'personalization' stamp: 'sw 6/29/1999 13:44'!compileHardCodedPref: prefName enable: aBoolean	"Compile a method that returns a simple true or false (depending on the value of aBoolean) when Preferences is sent prefName as a message"	| oldInitials |	oldInitials _ Utilities authorInitialsPerSe.	Utilities setAuthorInitials: 'programmatic'.	self class compile: (prefName asString, '	"compiled programatically -- return hard-coded preference value"	^ ', aBoolean storeString) classified: 'hard-coded prefs'.		Utilities setAuthorInitials: oldInitials."Preferences compileHardCodedPref: #testing enable: false"! !Preferences class removeSelector: #initBulletProofingHelp!"Postscript:"Preferences initializeHelpMessages.!