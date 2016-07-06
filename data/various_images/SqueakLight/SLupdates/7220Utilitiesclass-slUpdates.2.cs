'From SqueakLight|II of 31 May 2008 [latest update: #7228] on 10 June 2009 at 7:51:43 am'!!Utilities class methodsFor: 'fetching updates' stamp: 'edc 6/10/2009 07:51'!slUpdates	"Utilities slUpdates"	| previousHighest server dir count sourceFile fileName pos x |	previousHighest := SystemVersion current highestUpdate.	server := ServerDirectory serverNamed: 'SL'.	dir := server directoryNamed: 'SLupdates'.	count := 0.	dir entries		select: [:c | 			x := c name initialIntegerOrNil.			x				ifNil: [x := 0].			x > previousHighest]		thenCollect: [:any | 			(any name endsWith: '.sqz')				ifTrue: [fileName := any name.					pos := fileName findString: '.sqz'.					fileName := fileName copyFrom: 1 to: pos - 1.					sourceFile := HTTPLoader default retrieveContentsFor: 'ftp.squeak.org/various_images/SqueakLight//SLupdates/' , any name.					sourceFile := RWBinaryOrTextStream with: sourceFile content unzipped]				ifFalse: [sourceFile := RWBinaryOrTextStream with: (dir getFileNamed: any name) contents.					sourceFile reset.					self saveUpdate: sourceFile onFile: any name].			ChangeSorter newChangesFromStream: sourceFile named: any name.			count := count + 1].	PopUpMenu inform: count asString , ' new update file(s) processed.'.	SystemVersion current registerUpdate: ChangeSorter highestNumberedChangeSet.	SmalltalkImage current aboutThisSystem.! !