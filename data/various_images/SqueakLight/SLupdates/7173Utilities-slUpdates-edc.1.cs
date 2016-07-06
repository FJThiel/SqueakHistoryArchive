'From SqueakLight|II of 31 May 2008 [latest update: #7181] on 26 June 2008 at 10:52:53 am'!

!Utilities class methodsFor: 'fetching updates' stamp: 'edc 6/26/2008 10:52'!
slUpdates
	"Utilities slUpdates"
	| numero server dir  count sourceFile fileName pos |
	numero :=  (ChangeSorter highestNumberedChangeSet + 1) asString.
	
server := (ServerDirectory serverNamed: 'SL').
dir := server directoryNamed: 'SLupdates'.
count := 0.
dir entries select: [:c| (c name startsWithDigit) & (c name > numero) ] thenCollect:[ :any|  
	(any name endsWith: '.sqz')  ifTrue:[fileName := any name .
			pos := fileName findString: '.sqz'.
			fileName := fileName copyFrom: 1 to: pos - 1.
			
				sourceFile :=   HTTPLoader default retrieveContentsFor: 'ftp.squeak.org/various_images/SqueakLight//SLupdates/', any name.
				sourceFile := RWBinaryOrTextStream with: (sourceFile content unzipped).
				]
		ifFalse:[sourceFile :=   RWBinaryOrTextStream with: (dir getFileNamed: any name ) contents.
		sourceFile reset].
			ChangeSorter newChangesFromStream:  sourceFile named:  any name .
	count := count + 1].
PopUpMenu inform:  count asString,' new update file(s) processed.'.
SystemVersion current registerUpdate: ChangeSorter highestNumberedChangeSet! !