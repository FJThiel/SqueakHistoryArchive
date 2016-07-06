'From Squeak2.9alpha of 5 August 2000 [latest update: #3339] on 31 January 2001 at 2:09:46 am'!"Change Set:		allScriptsTool-swDate:			31 January 2001Author:			Scott WallaceProvides a tool letting you view and change the status of all scripts of all objects in a project.  Consult the 'authoring tools' menu."!AlignmentMorph subclass: #AllScriptsTool	instanceVariableNames: 'presenter showingOnlyActiveScripts showingAllInstances '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Scripting'!!AllScriptsTool methodsFor: 'initialization' stamp: 'sw 1/30/2001 23:21'!initializeFor: aPresenter	"Initialize the receiver as a tool which shows, and allows the user to change the status of, all the instantiations of all the user-written scripts in the world"	| aButton aRow outerButton |	presenter _ aPresenter.	showingOnlyActiveScripts _ true.	showingAllInstances _ true.	self color: Color brown muchLighter muchLighter; wrapCentering: #center; cellPositioning: #topCenter; vResizing: #shrinkWrap; hResizing: #shrinkWrap.	self useRoundedCorners.	self borderWidth: 4; borderColor: Color brown darker.	self addMorph: ScriptingSystem scriptControlButtons.	aButton _ SimpleButtonMorph new target: aPresenter; actionSelector: #updateContentsFor:; arguments: (Array with: self); label: 'Update'; color: Color lightYellow; actWhen: #buttonDown.	aButton setBalloonText: 'Press here to get the lists of scripts updated'.	aRow _ AlignmentMorph newRow listCentering: #center; color: Color transparent.	aRow addMorphBack: aButton.	outerButton _ AlignmentMorph newRow.	outerButton wrapCentering: #center; cellPositioning: #leftCenter.	outerButton color:  Color transparent.	outerButton hResizing: #shrinkWrap; vResizing: #shrinkWrap.	outerButton addMorph: (aButton _ UpdatingThreePhaseButtonMorph checkBox).	aButton		target: self;		actionSelector: #toggleWhetherShowingOnlyActiveScripts;		getSelector: #showingOnlyActiveScripts.	outerButton addMorphBack: (StringMorph contents: 'tickers only') lock.	outerButton setBalloonText: 'If checked, then only scripts that are paused or ticking will be shown'.	aRow addMorphBack: outerButton.	outerButton _ AlignmentMorph newRow.	outerButton wrapCentering: #center; cellPositioning: #leftCenter.	outerButton color:  Color transparent.	outerButton hResizing: #shrinkWrap; vResizing: #shrinkWrap.	outerButton addMorph: (aButton _ UpdatingThreePhaseButtonMorph checkBox).	aButton		target: self;		actionSelector: #toggleWhetherShowingAllInstances;		getSelector: #showingAllInstances.	outerButton addMorphBack: (StringMorph contents: 'all instances') lock.	outerButton setBalloonText: 'If checked, then status of all instances will be shown, but if not checked, scripts for only one exemplar of each uniclass will be shown'.	aRow addMorphBack: outerButton.	self addMorphBack: aRow.	aPresenter updateContentsFor: self.	self layoutChanged.! !!AllScriptsTool methodsFor: 'toggles' stamp: 'sw 1/30/2001 23:18'!showingAllInstances	"Answer whether the receiver is currently showing controls for all instances of each uniclass." 	^ showingAllInstances ! !!AllScriptsTool methodsFor: 'toggles' stamp: 'sw 1/30/2001 23:18'!showingOnlyActiveScripts	"Answer whether the receiver is currently showing only active scripts" 	^ showingOnlyActiveScripts ! !!AllScriptsTool methodsFor: 'toggles' stamp: 'sw 1/31/2001 00:58'!toggleWhetherShowingAllInstances	"Toggle whether the receiver is showing all instances or only one exemplar per uniclass"	showingAllInstances _ showingAllInstances not.	presenter updateContentsFor: self! !!AllScriptsTool methodsFor: 'toggles' stamp: 'sw 1/31/2001 00:58'!toggleWhetherShowingOnlyActiveScripts	"Toggle whether the receiver is showing only active scripts"	showingOnlyActiveScripts _ showingOnlyActiveScripts not.	presenter updateContentsFor: self! !!AllScriptsTool methodsFor: 'stepping' stamp: 'sw 1/31/2001 00:31'!step	"Update the contents of the tool -- but this is currently not reached because of some drastic performance bug at present"	presenter updateContentsFor: self.! !!AllScriptsTool methodsFor: 'stepping' stamp: 'sw 1/31/2001 00:48'!stepTime	"Answer the interval between steps -- but see the comment in #wantsSteps -- auto-updating is out for the time being."	^ 10000! !!AllScriptsTool methodsFor: 'stepping' stamp: 'sw 1/31/2001 00:52'!wantsSteps	"Answer whether the receiver wishes to receive the #step message"	self flag: #noteToAndreas. 	 ^ false.	"when this is returns true, the updating of the tool is unbelievably slow, because though we reimplement stepTime to try to get a 10-second pause between steps, our #stepTime is never called and so the thing runs flat out and grinds the system to a halt."! !!AllScriptsTool class methodsFor: 'as yet unclassified' stamp: 'sw 1/30/2001 23:06'!launchAllScriptsToolFor: aPresenter	"Launch an AllScriptsTool to view scripts of the given presenter"	| aTool |	aTool _ self newColumn.	aTool initializeFor: aPresenter.	self currentHand attachMorph: aTool.	aPresenter associatedMorph world startSteppingSubmorphsOf: aTool! !!Presenter methodsFor: 'playerList' stamp: 'sw 1/30/2001 23:07'!toolToViewScriptInstantiations	"Open a tool which shows, and allows the user to change the status of, all the instantiations of all the user-written scripts in the world"	AllScriptsTool launchAllScriptsToolFor: self	"self currentWorld presenter toolToViewScriptInstantiations"! !!Presenter methodsFor: 'playerList' stamp: 'sw 1/31/2001 02:07'!updateContentsFor: aStatusViewer 	"Rebuild the contents of the script-statusViewing tool"	| showingOnlyActiveScripts candidateList firstTwo |	showingOnlyActiveScripts _ aStatusViewer showingOnlyActiveScripts.	self flushPlayerListCache.	firstTwo  _ {aStatusViewer submorphs first.  aStatusViewer submorphs second}.	aStatusViewer removeAllMorphs; addAllMorphs: firstTwo.		candidateList _ self allExtantPlayers.	aStatusViewer showingAllInstances "take all instances of all classes"		ifTrue:			[candidateList _ self allExtantPlayers]  		ifFalse:  "include only one exemplar per uniclass.  Try to get one that has some qualifying scripts"			[candidateList _ Set new.			self allExtantPlayers do:				[:aPlayer |					(candidateList detect: [:plyr | plyr isMemberOf:  aPlayer class] ifNone: [nil]) ifNil:						[aPlayer instantiatedUserScriptsDo: [:aScriptInstantiation |							(showingOnlyActiveScripts not or: [#(paused ticking) includes: aScriptInstantiation status]) 								ifTrue:									[candidateList add: aPlayer]]]]].	candidateList do:		[:aPlayer | aPlayer instantiatedUserScriptsDo:			[:aScriptInstantiation |				(showingOnlyActiveScripts not or: [#(paused ticking) includes: aScriptInstantiation status]) ifTrue:					[aStatusViewer addMorphBack: (aScriptInstantiation statusControlRowIn: aStatusViewer)]]].	associatedMorph startSteppingSubmorphsOf: aStatusViewer ! !!TheWorldMenu methodsFor: 'construction' stamp: 'sw 1/30/2001 23:54'!scriptingMenu        "Build the scripting menu for the world."        ^ self fillIn: (self menu: 'authoring tools...') from: {                 { 'standard parts bin' . { self . #createStandardPartsBin}. 'A bin of standard parts, from which you can drag out useful morphs.'}.                { 'custom parts bin' . { self . #launchCustomPartsBin}. 'A customized bin of parts.  To define what the custom parts bin is, edit any existing parts bin and tell it to be saved as the custom parts bin.'}.                { 'view trash contents' . { #myWorld . #openScrapsBook:}. 'The place where all your trashed morphs go.'}.                { 'empty trash can' . { Utilities . #emptyScrapsBook}. 'Empty out all the morphs that have accumulated in the trash can.'}.			nil.                { 'new scripting area' . { #myWorld . #detachableScriptingSpace}. 'A window set up for simple scripting.'}.			 { 'summary of scripts' . {#myWorld . #printScriptSummary}. 'Produces a summary of scripted objects in the project, and all of their scripts.'}.			 { 'status of scripts' . {#myWorld . #showStatusOfAllScripts}. 'Lets you view the status of all the scripts belonging to all the scripted objects of the project.'}.                nil.                { 'unlock locked objects' . { #myWorld . #unlockContents}. 'If any items on the world desktop are currently locked, unlock them.'}.                { 'unhide hidden objects' . { #myWorld . #showHiders}. 'If any items on the world desktop are currently hidden, make them visible.'}.        }! !!AllScriptsTool reorganize!('initialization' initializeFor:)('toggles' showingAllInstances showingOnlyActiveScripts toggleWhetherShowingAllInstances toggleWhetherShowingOnlyActiveScripts)('stepping' step stepTime wantsSteps)!