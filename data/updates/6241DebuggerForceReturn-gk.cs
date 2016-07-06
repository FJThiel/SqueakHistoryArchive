'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6232] on 24 September 2004 at 9:45:25 pm'!"Change Set:		Debugger-enhDate:			18 December 2003Author:			G�ran KrampeI was talking to Jonas here over lunch and he told me he wanted a way of force returning a context in the debugger and at the same time entering an expression for the return value.It was a very good excuse to finally see how the debugger works. And it was easy!!This ENH adds a little menu choice in the debugger called 'return entered value'.A FillInTheBlank pops up and you enter an expression that will be evaluated in the context/receiver. Then it will be returned and the stack pops - but you are still in the debugger.NOTE: I haven't tested this much, but it seems to work.Minor revision by tpr to update to post 5707 (change in debugger context view menu). Trivial test by3 halt +4 (printit, open debugger and use the new menu option 'return new value' to return some other answer than 7"!!Debugger methodsFor: 'context stack menu' stamp: 'kfr 9/24/2004 21:42'!contextStackMenu: aMenu shifted: shifted	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"	^ shifted ifFalse: 		[self selectedContext selector = #doesNotUnderstand: ifTrue:			[aMenu 				add: 'implement in...' 				subMenu: (self populateImplementInMenu: (Smalltalk isMorphic ifTrue: [MenuMorph new defaultTarget: self] ifFalse: [CustomMenu new]))				target: nil 				selector: nil 				argumentList: #(nil)].		aMenu labels: 'fullStack (f)restart (r)proceed (p)step (t)step through (T)send (e)where (w)peel to first like thisreturn entered valuetoggle break on entrysenders of... (n)implementors of... (m)inheritance (i)versions (v)inst var refs...inst var defs...class var refs...class variablesclass refs (N)browse full (b)file out mail out bug reportmore...'	lines: #(8 9 13 15 18 21)	selections: #(fullStack restart proceed doStep stepIntoBlock send where peelToFirst returnValue toggleBreakOnEntrybrowseSendersOfMessages browseMessages methodHierarchy browseVersionsbrowseInstVarRefs browseInstVarDefsbrowseClassVarRefs browseClassVariables browseClassRefsbrowseMethodFull fileOutMessage mailOutBugReportshiftedYellowButtonActivity)]	ifTrue: [aMenu labels: 'browse class hierarchybrowse classbrowse method (O)implementors of sent messageschange sets with this methodinspect instancesinspect subinstancesrevert to previous versionremove from current change setrevert & remove from changesmore...' 	lines: #(5 7 10)	selections: #(classHierarchy browseClass 		openSingleMessageBrowser browseAllMessages findMethodInChangeSets 		inspectInstances inspectSubInstances		revertToPreviousVersion 		removeFromCurrentChanges revertAndForget		unshiftedYellowButtonActivity)]! !!Debugger methodsFor: 'context stack menu' stamp: 'gk 12/18/2003 13:44'!returnValue	"Force a return of a given value to the previous context!!"	| previous selectedContext expression value |	contextStackIndex = 0 ifTrue: [^ self beep].	selectedContext _ self selectedContext.	expression _ FillInTheBlank request: 'Enter expression for return value:'.	value _ Utilities evaluate: expression in: selectedContext to: selectedContext receiver.	previous _ selectedContext sender.	self resetContext: previous.	interruptedProcess popTo: previous value: value! !!Process methodsFor: 'changing suspended state' stamp: 'gk 12/18/2003 13:09'!popTo: aContext value: aValue	"Replace the suspendedContext with aContext, releasing all contexts 	between the currently suspendedContext and it."	| callee |	self == Processor activeProcess		ifTrue: [^ self error: 'The active process cannot pop contexts'].	callee _ (self calleeOf: aContext) ifNil: [^ self].  "aContext is on top"	self return: callee value: aValue! !