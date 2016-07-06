'From Squeak3.2alpha of 7 October 2001 [latest update: #4567] on 7 January 2002 at 6:29:34 pm'!"Change Set:		ProcessBrowser-menuFixDate:			7 January 2002Author:			Leandro CanigliaFixes a minor problem encountered in MVC while trying to pop the ProcessBrowser menu up."!!ProcessBrowser methodsFor: 'process list' stamp: 'LC 1/7/2002 16:35'!processListMenu: menu 	| pw |	selectedProcess		ifNotNil: [| nameAndRules | 			nameAndRules _ self nameAndRulesForSelectedProcess.			menu addList: {{'inspect (i)'. #inspectProcess}. {'explore (I)'. #exploreProcess}. {'inspect Pointers (P)'. #inspectPointers}}.	(Smalltalk includesKey: #PointerFinder)		ifTrue: [ menu add: 'chase pointers (c)' action: #chasePointers.  ].			nameAndRules second				ifTrue: [menu add: 'terminate (t)' action: #terminateProcess.					selectedProcess isSuspended						ifTrue: [menu add: 'resume (r)' action: #resumeProcess]						ifFalse: [menu add: 'suspend (s)' action: #suspendProcess]].			nameAndRules third				ifTrue: [menu addList: {{'change priority (p)'. #changePriority}. {'debug (d)'. #debugProcess}}].			menu addList: {{'profile messages (m)'. #messageTally}}.			(selectedProcess suspendingList isKindOf: Semaphore)				ifTrue: [menu add: 'signal Semaphore (S)' action: #signalSemaphore].			menu add: 'full stack (k)' action: #moreStack.			menu addLine].	menu addList: {{'find context... (f)'. #findContext}. {'find again (g)'. #nextContext}}.	menu addLine.	menu		add: (self isAutoUpdating				ifTrue: ['turn off auto-update (a)']				ifFalse: ['turn on auto-update (a)'])		action: #toggleAutoUpdate.	menu add: 'update list (u)' action: #updateProcessList.	pw _ Smalltalk at: #CPUWatcher ifAbsent: [].	pw ifNotNil: [		menu addLine.		pw isMonitoring				ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher ]				ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher  ]	].	^ menu! !