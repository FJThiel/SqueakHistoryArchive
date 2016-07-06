'From Squeak2.9alpha of 13 June 2000 [latest update: #2915] on 12 November 2000 at 11:16:36 am'!"Change Set:		225DebuggerEnh-kfrDate:			11 November 2000Author:			Karl RambergChanges the pre debug window to use a PluggableListMorph so one canclick on the stack line that one is interested in to bring the debugger upon just that method."!!Debugger methodsFor: 'initialize' stamp: 'kfr 10/4/2000 22:13'!debugAt: anInteger	self toggleContextStackIndex: anInteger. 	 ^ self debug.! !!Debugger methodsFor: 'context stack menu' stamp: 'sma 11/12/2000 11:15'!buildMorphicNotifierLabelled: label message: messageString	| notifyPane window contentTop extentToUse |	self expandStack.	window _ (PreDebugWindow labelled: label) model: self.	contentTop _ 0.2.	extentToUse _ 450 @ 156. "nice and wide to show plenty of the error msg"	window addMorph: (self buttonRowForPreDebugWindow: window)				frame: (0@0 corner: 1 @ contentTop).	Preferences eToyFriendly		ifFalse:			[notifyPane _(PluggableListMorph on: self list: #contextStackList				selected: #contextStackIndex changeSelected: #debugAt:				menu: #contextStackMenu:shifted: keystroke: #contextStackKey:from:)]		ifTrue:			[notifyPane _ PluggableTextMorph on: self text: nil accept: nil				readSelection: nil menu: #debugProceedMenu:.			notifyPane editString: (self preDebugNotifierContentsFrom: messageString);				askBeforeDiscardingEdits: false].	window addMorph: notifyPane frame: (0@contentTop corner: 1@1).	"window deleteCloseBox.		chickened out by commenting the above line out, sw 8/14/2000 12:54"	window setBalloonTextForCloseBox.	^ window openInWorldExtent: extentToUse! !