"ClassDescription-subclassOf.st
This fixes a bug that made it impossible to re-define Object..

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    July 8, 1997"!

!ClassDescription methodsFor: 'initialize-release'!
subclassOf: newSuper oldClass: oldClass instanceVariableNames: newInstVarString variable: v words: w pointers: p ifBad: badBlock 
	"Basic initialization message for creating classes using the information 
	provided as arguments. Answer whether old instances will be 
	invalidated."
	| oldNames newNames usedNames invalid oldSuperMeta newInstVarArray oldSpec |
	oldNames _ self allInstVarNames.
	usedNames _ #(self super thisContext true false nil ) asSet.
	newInstVarArray _ Scanner new scanFieldNames: newInstVarString.
	newNames _ newSuper allInstVarNames , newInstVarArray.
	newNames size > 254 ifTrue:
		[self error: 'A class cannot have more than 254 instance variables'.
		^ badBlock value].
	newNames do: 
		[:fieldName | 
		(usedNames includes: fieldName)
			ifTrue: 
				[self error: fieldName , ' is reserved (maybe in a superclass)'.
				^ badBlock value].
		usedNames add: fieldName].
	(invalid _ superclass ~~ newSuper)
		ifTrue: 
			["superclass changed"
			oldSuperMeta _ superclass class.
			superclass isNil
				ifFalse: [superclass removeSubclass: self.
					superclass _ newSuper.
					superclass addSubclass: self].
			self class superclass == oldSuperMeta 
				ifTrue: ["Only false when self is a metaclass"
						self class superclass: newSuper class]].
	instanceVariables _ newInstVarArray size = 0 ifFalse: [newInstVarArray].
	invalid _ invalid | (newNames  ~= oldNames).   "field names changed"
	oldSpec _ self instSpec.
	self format: newNames size
		variable: v
		words: w
		pointers: p.
	invalid _ invalid | (self instSpec ~= oldSpec).  "format changed"
	^invalid! !
