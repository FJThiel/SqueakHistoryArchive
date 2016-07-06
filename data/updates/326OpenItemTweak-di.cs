'From Squeak 2.2 of Sept 23, 1998 on 24 September 1998 at 8:22:19 am'!"Change Set:		New ChangesDate:			24 September 1998Author:			Dan IngallsChanged the screenMenu item 'open window...' to 'open...' to differentiate better from the following 'windows...' item."!!HandMorph methodsFor: 'world menu' stamp: 'di 9/24/1998 08:19'!buildWorldMenu	"Build the meta menu for the world."	| menu |	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'previous project' target: owner action: #goBack.	menu add: 'jump to project...' action: #jumpToProject.	menu add: 'restore display' target: World action: #fullRepaintNeeded.	menu addLine.	menu add: 'new morph...' action: #newMorph.	menu add: 'scripting...' action: #scriptingDo.	menu add: 'remote...' action: #remoteDo.	menu add: 'debug...' action: #debugDo.	menu addLine.	menu add: 'open...' action: #openWindow.	menu add: 'windows...' action: #windowsDo.	menu add: 'changes...' action: #changesDo.	menu add: 'help...' action: #helpDo.	menu add: 'do...' target: Utilities action: #offerCommonRequests.	menu addLine.	menu add: 'save' action: #saveSession.	menu add: 'save as...' action: #saveAs.	menu add: 'save and quit' action: #saveAndQuit.	menu add: 'quit' action: #quitSession.	^ menu! !!ScreenController methodsFor: 'nested menus' stamp: 'di 9/24/1998 08:18'!projectScreenMenu	"Answer the project screen menu."	^ SelectionMenu labelList:		#(	'keep this menu up'			'previous project'			'jump to project...'			'restore display'			'open...'			'windows...'			'changes...'			'help...'			'do...'			'save'			'save as...'			'save and quit'			'quit')		lines: #(1 4 9)		selections: #(durableScreenMenureturnToPreviousProject jumpToProject restoreDisplaypresentOpenMenu presentWindowMenu presentChangesMenu presentHelpMenu commonRequestssnapshot saveAs snapshotAndQuit quit )"ScreenController new projectScreenMenu startUp"! !