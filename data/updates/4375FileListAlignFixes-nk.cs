'From Squeak3.1alpha of 16 September 2001 [latest update: #4332] on 20 September 2001 at 1:27:43 am'!"Change Set:		FileListAlignmentFixes-nkDate:			9 February 2001Author:			Ned KonzThis change set makes the FileList2 buttons and patternpart of the same pane, so that resizing the top pane won'tresult in badly sized panes."!!FileList2 class methodsFor: 'instance creation' stamp: 'nk 7/12/2000 11:03'!openMorphicViewInWorld	"FileList2 openMorphicViewInWorld"	^self morphicView openInWorld! !!FileList2 class methodsFor: 'morphic ui' stamp: 'nk 2/9/2001 15:50'!morphicView	| dir aFileList window fileListBottom midLine fileListTopOffset buttonPane |	dir _ FileDirectory default.	aFileList _ self new directory: dir.	window _ (SystemWindow labelled: dir pathName) model: aFileList.	fileListTopOffset _ (TextStyle defaultFont pointSize * 2) + 14.	fileListBottom _ 0.4.	midLine _ 0.4.	buttonPane _ aFileList optionalButtonRow addMorph:		(aFileList morphicPatternPane vResizing: #spaceFill; yourself).	self addFullPanesTo: window from: {		{buttonPane. 0@0 corner: 1@0. 0@0 corner: 0@fileListTopOffset}.		{aFileList morphicDirectoryTreePane. 0@0 corner: midLine@fileListBottom. 					0@fileListTopOffset corner: 0@0}.		{aFileList morphicFileListPane. midLine @ 0 corner: 1@fileListBottom. 					0@fileListTopOffset corner: 0@0}.		{aFileList morphicFileContentsPane. 0@fileListBottom corner: 1@1. nil}.	}.	aFileList postOpen.	^ window ! !