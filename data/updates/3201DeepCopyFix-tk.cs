'From Squeak2.9alpha of 16 June 2000 [latest update: #3235] on 13 January 2001 at 2:11:16 pm'!"Change Set:		DeepCopyFix-tkDate:			13 January 2001Author:			Ted KaehlerFixes a bug when dragging out a new script.  In mapUniClasses in DeepCopier that tried to fix up the References dictionary."!!DeepCopier methodsFor: 'like fullCopy' stamp: 'tk 1/13/2001 13:53'!mapUniClasses	"For new Uniclasses, map their class vars to the new objects.  And their additional class instance vars.  (scripts slotInfo) and cross references like (player321)."	"Players also refer to each other using associations in the References dictionary.  Search the methods of our Players for those.  Make new entries in References and point to them."| pp oldPlayer newKey newAssoc oldSelList newSelList |"Uniclasses use class vars to hold onto siblings who are referred to in code"pp _ Object class instSize + 1.uniClasses do: [:playersClass | "values = new ones"	playersClass classPool associationsDo: [:assoc |		assoc value: (assoc value veryDeepCopyWith: self)].	playersClass scripts: (playersClass privateScripts veryDeepCopyWith: self).	"pp+0"	"(pp+1) slotInfo was deepCopied in copyUniClass and that's all it needs"	pp+2 to: playersClass class instSize do: [:ii | 		playersClass instVarAt: ii put: 			((playersClass instVarAt: ii) veryDeepCopyWith: self)].	]."Make new entries in References and point to them."References keys "copy" do: [:playerName |	oldPlayer _ References at: playerName.	(references includesKey: oldPlayer) ifTrue: [		newKey _ (references at: oldPlayer) "new player" uniqueNameForReference.		"now installed in References"		(references at: oldPlayer) renameTo: newKey]].uniClasses "values" do: [:newClass |	oldSelList _ OrderedCollection new.   newSelList _ OrderedCollection new.	newClass selectorsDo: [:sel | 		(newClass compiledMethodAt: sel)	 literals do: [:assoc |			assoc class == Association ifTrue: [				(References associationAt: assoc key ifAbsent: [nil]) == assoc ifTrue: [					newKey _ (references at: assoc value ifAbsent: [assoc value]) 									externalName asSymbol.					(assoc key ~= newKey) & (References includesKey: newKey) ifTrue: [						newAssoc _ References associationAt: newKey.						newClass methodDictionary at: sel put: 							(newClass compiledMethodAt: sel) clone.	"were sharing it"						ii _ (newClass compiledMethodAt: sel) literals indexOf: assoc.						(newClass compiledMethodAt: sel) literalAt: ii put: newAssoc.						(oldSelList includes: assoc key) ifFalse: [							oldSelList add: assoc key.  newSelList add: newKey]]]]]].	oldSelList with: newSelList do: [:old :new |			newClass replaceSilently: old to: new]].	"This is text replacement and can be wrong"! !