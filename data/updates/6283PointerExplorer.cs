'From Squeak3.8alpha of ''17 July 2004'' [latest update: #6272] on 30 September 2004 at 5:07:53 pm'!"Change Set:		PointerExplorerDate:			29 September 2004Author:			avimd: repackaged as a changeset.A variant on the ObjectExlorer that works 'backwards': like the ObjectExplorer, it shows a tree of objects, but expanding a node won't show the objects which that node references, but rather the objects that reference that node.  Its main use is to track down memory leaks: if you want to know why a particular object is still alive, open a PointerExplorer on it and drill down until you find the root object that's referencing it.  For example, find all the references to the symbol #zot with:PointerExplorer new openExplorerFor: #zotFor the 'name' of the object, the PointerExplorer shows each object's identityHash, to allow the user to identify when two similar objects are identical and notice cycles."!ObjectExplorer subclass: #PointerExplorer	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-PointerExplorer'!!PointerExplorer commentStamp: 'avi 8/21/2004 20:01' prior: 0!A variant on the ObjectExlorer that works "backwards": like the ObjectExplorer, it shows a tree of objects, but expanding a node won't show the objects which that node references, but rather the objects that reference that node.  Its main use is to track down memory leaks: if you want to know why a particular object is still alive, open a PointerExplorer on it and drill down until you find the root object that's referencing it.  For example, find all the references to the symbol #zot with:PointerExplorer new openExplorerFor: #zotFor the "name" of the object, the PointerExplorer shows each object's identityHash, to allow the user to identify when two similar objects are identical and notice cycles.!ObjectExplorerWrapper subclass: #PointerExplorerWrapper	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-PointerExplorer'!!PointerExplorerWrapper commentStamp: 'avi 8/21/2004 19:58' prior: 0!A subclass of ObjectExplorerWrapper for use with PointerExplorer.  #contents is overridden to work backwards: it returns wrappers for the objects pointing to item rather than for the objects that item points to.!!Inspector methodsFor: 'menu commands' stamp: 'avi 2/18/2004 01:31'!explorePointers	PointerExplorer new openExplorerFor: self selection! !!Inspector methodsFor: 'menu commands' stamp: 'md 9/30/2004 16:42'!fieldListMenu: aMenu	"Arm the supplied menu with items for the field-list of the receiver"	Smalltalk isMorphic ifTrue:		[aMenu addStayUpItemSpecial].	aMenu addList: #(		('inspect (i)'						inspectSelection)		('explore (I)'						exploreSelection)).	self addCollectionItemsTo: aMenu.	aMenu addList: #(		-		('method refs to this inst var'		referencesToSelection)		('methods storing into this inst var'	defsOfSelection)		('objects pointing to this value'		objectReferencesToSelection)		('chase pointers'					chasePointers)		('explore pointers'				explorePointers)		-		('browse full (b)'					browseMethodFull)		('browse class'						browseClass)		('browse hierarchy (h)'					classHierarchy)		('browse protocol (p)'				browseFullProtocol)		-		('inst var refs...'					browseInstVarRefs)		('inst var defs...'					browseInstVarDefs)		('class var refs...'					classVarRefs)		('class variables'					browseClassVariables)		('class refs (N)'						browseClassRefs)		-		('copy name (c)'					copyName)				('basic inspect'						inspectBasic)).	Smalltalk isMorphic ifTrue:		[aMenu addList: #(			-			('tile for this value	(t)'			tearOffTile)			('viewer for this value (v)'		viewerForValue))].	^ aMenu"			-			('alias for this value'			aliasForValue)			('watcher for this slot'			watcherForSlot)"! !!PointerExplorer methodsFor: 'accessing' stamp: 'ab 8/22/2003 18:51'!getList	^Array with: (PointerExplorerWrapper with: rootObject name: rootObject identityHash asString model: self)! !!PointerExplorerWrapper methodsFor: 'testing' stamp: 'ab 8/22/2003 18:39'!hasContents	^true! !!PointerExplorerWrapper methodsFor: 'accessing' stamp: 'md 9/29/2004 22:42'!contents	| objects |	objects _ PointerFinder pointersTo: item except: (Array with: self with: model).		^(objects reject: [:ea | ea class = self class])		collect: [:ea| self class with: ea name: ea identityHash asString model: item]! !