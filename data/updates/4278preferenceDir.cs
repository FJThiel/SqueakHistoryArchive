'From Squeak3.1alpha of 28 February 2001 [latest update: #4277] on 22 August 2001 at 7:59:15 pm'!"Change Set:		preferenceDirDate:			22 August 2001Author:			Michael RuegerReverts a change to the lookup of external settings.Lookup order is now again image-path then VM-path.Should avoid problems esp. when running the standalone version on Macs with multiple VMs."!!ExternalSettings class methodsFor: 'accessing' stamp: 'mir 6/26/2001 00:17'!preferenceDirectory	| prefDirName prefDir |	prefDirName _ 'prefs'.	^(FileDirectory default directoryExists: prefDirName)		ifTrue: [FileDirectory default directoryNamed: prefDirName]		ifFalse: [			((FileDirectory on: Smalltalk vmPath) directoryExists: prefDirName)				ifTrue: [(FileDirectory on: Smalltalk vmPath) directoryNamed: prefDirName]				ifFalse: [					prefDir _ FileDirectory default directoryNamed: prefDirName.					prefDir assureExistance.					prefDir]]! !