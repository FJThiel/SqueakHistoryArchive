'From MinimalMorphic of 8 December 2006 [latest update: #7253] on 31 December 2009 at 6:28:48 pm'!!FileList2 class methodsFor: 'accessing' stamp: 'edc 10/3/2009 08:14'!lastSelDir^ lastSelDir! !!FileList2 class methodsFor: 'accessing' stamp: 'edc 10/3/2009 08:14'!lastSelDir: aDir^ lastSelDir := aDir! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'edc 10/3/2009 08:18'!modalFolderSelectorself lastSelDir  ifNil: [^self modalFolderSelector: FileDirectory default]ifNotNil:[^self modalFolderSelector: self lastSelDir ]	! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'edc 10/3/2009 08:19'!modalFolderSelector: aDir	| window fileModel |	window _ self morphicViewFolderSelector: aDir.	fileModel _ window model.	window openInWorld: self currentWorld extent: 300@400.	self modalLoopOn: window.	^self lastSelDir: fileModel getSelectedDirectory withoutListWrapper! !!FileList2 class methodsFor: 'morphic ui' stamp: 'edc 10/8/2009 09:46'!morphicViewFileSelectorForSuffixes: aList 	"Answer a morphic file-selector tool for the given suffix list."	^ self morphicViewFileSelectorForSuffixes: aList directory: self modalFolderSelector! !