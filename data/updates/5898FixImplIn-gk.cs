'From Squeak3.7beta of ''1 April 2004'' [latest update: #5878] on 26 April 2004 at 2:17:55 pm'!"Change Set:		FixImplIn-gkDate:			26 April 2004Author:			G�ran KrampeSomeone somewhere (?) fumbled the ball when letting 5577 into the stream.The method Debugger>>contextStackMenu:shifted:first got modified by Ned (autocreation of stubs) in 5511 and then was run over in 5577 (breakpoints). I am not sure how this slipped by - I have been trying to see in BFAV if this was due to an approved item not yet pushed out in the stream, but I am not sure. Might be interesting to figure out how this happened.This cs puts that 'implement in' choice back in the menu. Simple as that."!!Debugger methodsFor: 'context stack menu' stamp: 'gk 4/26/2004 14:09'!contextStackMenu: aMenu shifted: shifted	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"	^ shifted ifFalse: 		[self selectedContext selector = #doesNotUnderstand: ifTrue:			[aMenu 				add: 'implement in...' 				subMenu: (self populateImplementInMenu: (Smalltalk isMorphic ifTrue: [MenuMorph new defaultTarget: self] ifFalse: [CustomMenu new]))				target: nil 				selector: nil 				argumentList: #(nil)].		aMenu labels: 'fullStack (f)restart (r)proceed (p)step (t)step through (T)send (e)where (w)peel to first like thistoggle break on entrysenders of... (n)implementors of... (m)inheritance (i)versions (v)inst var refs...inst var defs...class var refs...class variablesclass refs (N)browse full (b)file out mail out bug reportmore...'	lines: #(8 9 13 15 18 21)	selections: #(fullStack restart proceed doStep stepIntoBlock send where peelToFirst toggleBreakOnEntrybrowseSendersOfMessages browseMessages methodHierarchy browseVersionsbrowseInstVarRefs browseInstVarDefsbrowseClassVarRefs browseClassVariables browseClassRefsbrowseMethodFull fileOutMessage mailOutBugReportshiftedYellowButtonActivity)]	ifTrue: [aMenu labels: 'browse class hierarchybrowse classbrowse method (O)implementors of sent messageschange sets with this methodinspect instancesinspect subinstancesrevert to previous versionremove from current change setrevert & remove from changesmore...' 	lines: #(5 7 10)	selections: #(classHierarchy browseClass 		openSingleMessageBrowser browseAllMessages findMethodInChangeSets 		inspectInstances inspectSubInstances		revertToPreviousVersion 		removeFromCurrentChanges revertAndForget		unshiftedYellowButtonActivity)]! !