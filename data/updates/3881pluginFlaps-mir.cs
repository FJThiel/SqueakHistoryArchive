'From Squeak3.1alpha of 28 February 2001 [latest update: #3891] on 3 April 2001 at 5:08:34 pm'!"Change Set:		pluginFlapsDate:			3 April 2001Author:			Michael RuegerAdds a mechanism to the PluginLauncher to enable global flaps (default is the supplies flap only). A parameter in the embed tag can be used to disable flaps or use a different set (none, etoy, all).Another embed parameter showSplash can be used to turn off the loading splash screen (showSplash=false)."!AbstractLauncher subclass: #PluginLauncher	instanceVariableNames: 'splashURL whichFlaps showSplash '	classVariableNames: 'SplashMorph '	poolDictionaries: ''	category: 'Framework-Download'!!PluginLauncher methodsFor: 'running' stamp: 'mir 4/3/2001 15:15'!hideSplashMorph	SplashMorph ifNil:[^self].	self showSplash		ifFalse: [^self].	SplashMorph delete.	World submorphs do:[:m| m visible: true]. "show all"! !!PluginLauncher methodsFor: 'running' stamp: 'mir 4/3/2001 15:15'!showSplashMorph	SplashMorph ifNil:[^self].	self showSplash		ifFalse: [^self].	World submorphs do:[:m| m visible: false]. "hide all"	World addMorphCentered: SplashMorph.	World displayWorldSafely.! !!PluginLauncher methodsFor: 'running' stamp: 'mir 4/3/2001 15:13'!startUp	| scriptName loader |	World ifNotNil: [World install].	HTTPClient determineIfRunningInBrowser.	HTTPClient isRunningInBrowser		ifFalse: [^self].	self setupFromParameters.	self setupFlaps.	scriptName _ self parameterAt: 'src'.	scriptName isEmpty ifTrue:[^self].	CodeLoader defaultBaseURL: (self parameterAt: 'Base').	loader _ CodeLoader new.	loader loadSourceFiles: (Array with: scriptName).	(scriptName asLowercase endsWith: '.pr') 		ifTrue:[self installProjectFrom: loader]		ifFalse:[loader installSourceFiles].! !!PluginLauncher methodsFor: 'initialization' stamp: 'mir 4/3/2001 16:47'!initialize	super initialize.	showSplash _ true.	whichFlaps _ 'etoy'! !!PluginLauncher methodsFor: 'initialization' stamp: 'mir 4/3/2001 16:56'!setupFlaps	whichFlaps = 'etoy'		ifTrue: [			Utilities addGlobalFlap: Utilities standardBottomFlap.			Utilities toggleWhetherToUseGlobalFlaps].	whichFlaps = 'all'		ifTrue: [			Utilities toggleWhetherToUseGlobalFlaps].	! !!PluginLauncher methodsFor: 'initialization' stamp: 'mir 4/3/2001 17:08'!setupFromParameters	(self includesParameter: 'showSplash')		ifTrue: [showSplash _ (self parameterAt: 'showSplash') asUppercase = 'TRUE'].	(self includesParameter: 'flaps')		ifTrue: [whichFlaps _ (self parameterAt: 'flaps')].! !!PluginLauncher methodsFor: 'private' stamp: 'mir 4/3/2001 15:15'!showSplash	^showSplash! !AbstractLauncher subclass: #PluginLauncher	instanceVariableNames: 'showSplash splashURL whichFlaps '	classVariableNames: 'SplashMorph '	poolDictionaries: ''	category: 'Framework-Download'!