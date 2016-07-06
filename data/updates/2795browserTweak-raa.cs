'From Squeak2.9alpha of 17 July 2000 [latest update: #2833] on 9 October 2000 at 10:54:31 am'!"Change Set:		browserTweakDate:			9 October 2000Author:			Bob Arning- fix overlapping panes in morphic message category browser"!!Browser methodsFor: 'initialize-release' stamp: 'RAA 10/9/2000 10:52'!openAsMorphMsgCatEditing: editString	"Create a pluggable version a Browser on just a messageCategory."	| window codePane baseline aTextMorph |	window _ (SystemWindow labelled: 'later') model: self.	window addMorph: ((PluggableListMorph on: self list: #messageCatListSingleton			selected: #indexIsOne changeSelected: #indexIsOne:			menu: #messageCategoryMenu:) enableDragNDrop: Preferences browseWithDragNDrop)		frame: (0@0 corner: 1.0@0.06).	window addMorph: ((PluggableListMorph on: self list: #messageList			selected: #messageListIndex changeSelected: #messageListIndex:			menu: #messageListMenu:shifted:			keystroke: #messageListKey:from:) enableDragNDrop: Preferences browseWithDragAndDrop)		frame: (0@0.06 corner: 1.0@(baseline _ 0.30)).	Preferences useAnnotationPanes		ifTrue:			[aTextMorph _ PluggableTextMorph on: self					text: #annotation accept: nil					readSelection: nil menu: nil.			aTextMorph askBeforeDiscardingEdits: false.			window addMorph: aTextMorph				frame: (0@baseline corner: 1@(baseline + 0.05)).			baseline _ baseline + 0.05].	Preferences optionalButtons		ifTrue:			[window addMorph: self optionalButtonRow frame: ((0@baseline corner: 1 @ (baseline + 0.08))).			baseline _ baseline + 0.08].	codePane _ PluggableTextMorph on: self text: #contents accept: #contents:notifying:			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.	editString ifNotNil: [codePane editString: editString.					codePane hasUnacceptedEdits: true].	window addMorph: codePane		frame: (0@baseline corner: 1@1).	window setUpdatablePanesFrom: #(messageCatListSingleton messageList).	^ window! !