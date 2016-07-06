'From Squeak3.1alpha of 28 February 2001 [latest update: #4181] on 14 July 2001 at 1:09:30 pm'!"Change Set:		handPosFix.csDate:			14 July 2001Author:			Michael RuegerResets the ActiveHand position in case the image was launched from the plugin.The inform: dialogs tended to be off screen on smaller displays."!!AutoStart class methodsFor: 'updating' stamp: 'mir 7/14/2001 13:09'!checkForPluginUpdate	| pluginVersion updateURL |	World 		ifNotNil: [			World install.			ActiveHand position: 100@100].	HTTPClient determineIfRunningInBrowser.	HTTPClient isRunningInBrowser		ifFalse: [^false].	pluginVersion _ AbstractLauncher extractParameters		at: (Smalltalk platformName copyWithout: Character space) asUppercase		ifAbsent: [^false].	updateURL _ AbstractLauncher extractParameters		at: 'UPDATE_URL'		ifAbsent: [^false].	^SystemVersion check: pluginVersion andRequestPluginUpdate: updateURL! !!AutoStart class methodsFor: 'updating' stamp: 'mir 7/14/2001 13:09'!checkForUpdates	| availableUpdate |	World 		ifNotNil: [			World install.			ActiveHand position: 100@100].	HTTPClient determineIfRunningInBrowser.	HTTPClient isRunningInBrowser		ifFalse: [^self processUpdates].	availableUpdate _ (AbstractLauncher extractParameters		at: 'UPDATE'		ifAbsent: [''] ) asInteger.	availableUpdate		ifNil: [^false].	^SystemVersion checkAndApplyUpdates: availableUpdate! !