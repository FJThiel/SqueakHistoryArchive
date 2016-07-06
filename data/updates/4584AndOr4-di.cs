'From Squeak3.2alpha of 2 October 2001 [latest update: #4570] on 5 December 2001 at 11:24:34 am'!"Change Set:		AndOr4Date:			5 December 2001Author:			Dan IngallsExtends non-evaluating ands and ors to up to 4 blocks without extra nesting.For example now you can write...	^ self class = aRenameVariableChange class		and: [className = aRenameVariableChange changeClassName]		and: [isMeta = aRenameVariableChange isMeta]		and: [oldName = aRenameVariableChange oldName]		and: [newName = aRenameVariableChange newName]Inspired by a (different) suggestion from Andres Valloud on 10/2/2001."!!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:50'!and: block1 and: block2	"Nonevaluating conjunction without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as false, then return false immediately,		without evaluating any further blocks.	If all return true, then return true."	self ifFalse: [^ false].	block1 value ifFalse: [^ false].	block2 value ifFalse: [^ false].	^ true! !!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:49'!and: block1 and: block2 and: block3	"Nonevaluating conjunction without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as false, then return false immediately,		without evaluating any further blocks.	If all return true, then return true."	self ifFalse: [^ false].	block1 value ifFalse: [^ false].	block2 value ifFalse: [^ false].	block3 value ifFalse: [^ false].	^ true! !!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:49'!and: block1 and: block2 and: block3 and: block4	"Nonevaluating conjunction without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as false, then return false immediately,		without evaluating any further blocks.	If all return true, then return true."	self ifFalse: [^ false].	block1 value ifFalse: [^ false].	block2 value ifFalse: [^ false].	block3 value ifFalse: [^ false].	block4 value ifFalse: [^ false].	^ true! !!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:52'!or: block1 or: block2	"Nonevaluating alternation without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as true, then return true immediately,		without evaluating any further blocks.	If all return false, then return false."	self ifTrue: [^ true].	block1 value ifTrue: [^ true].	block2 value ifTrue: [^ true].	^ false! !!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:52'!or: block1 or: block2 or: block3	"Nonevaluating alternation without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as true, then return true immediately,		without evaluating any further blocks.	If all return false, then return false."	self ifTrue: [^ true].	block1 value ifTrue: [^ true].	block2 value ifTrue: [^ true].	block3 value ifTrue: [^ true].	^ false! !!Boolean methodsFor: 'controlling' stamp: 'di 12/5/2001 10:52'!or: block1 or: block2 or: block3 or: block4	"Nonevaluating alternation without deep nesting.	The reciever is evaluated, followed by the blocks in order.	If any of these evaluates as true, then return true immediately,		without evaluating any further blocks.	If all return false, then return false."	self ifTrue: [^ true].	block1 value ifTrue: [^ true].	block2 value ifTrue: [^ true].	block3 value ifTrue: [^ true].	block4 value ifTrue: [^ true].	^ false! !