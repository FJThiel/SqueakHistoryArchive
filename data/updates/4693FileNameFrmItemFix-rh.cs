'From Squeak3.2alpha of 3 October 2001 [latest update: #4441] on 14 November 2001 at 4:40:42 pm'!"Change Set:		FileNameFrmItemFixDate:			14 November 2001Author:			Robert HirschfeldFixes a problem with FileList>>fileNameFromFormattedItem: that does not work when files in a file list are sorted by name and the file to be opened contains at least one $( character in its name. To reproduce the problem, insert a $( character in a file name and then try to open it with a file browser/list..."!!FileList methodsFor: 'private' stamp: 'rhi 9/8/2001 02:17'!fileNameFromFormattedItem: item	"Extract fileName and folderString from a formatted fileList item string"	| from to |	self sortingByName		ifTrue: [			from _ item lastIndexOf: $( ifAbsent: [0].			to _ item lastIndexOf: $) ifAbsent: [0]]		ifFalse: [			from _ item indexOf: $( ifAbsent: [0].			to _ item indexOf: $) ifAbsent: [0]].	^ (from * to = 0		ifTrue: [item]		ifFalse: [item copyReplaceFrom: from to: to with: '']) withBlanksTrimmed! !