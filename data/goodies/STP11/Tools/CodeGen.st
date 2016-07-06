"CodeGen.st
Adds the 'publish' method for automatically generating instance variable accessors. You simply need to edit and accept the class definition of a class with instance variables, and then do 'ClassName publish.'

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    May 19, 1985"!


!Class methodsFor: 'message generation'!
for: aCollection and: another create: aPattern classified: aProtocol 
	"Substitute each element into the pattern string at every occurrence of    
	'%1', and compile the resulting methods in the given protocol."

	aCollection do: [:strA | another do: [:strB | self compile: ((aPattern copyReplaceAll: '%1' with: strA asString)
					copyReplaceAll: '%2' with: strB asString)
				classified: aProtocol asSymbol]]!

for: aCollection create: aPattern classified: aProtocol 
	"Substitute each element into the pattern string at every occurrence of 
	'%1', and compile the resulting methods in the given protocol."

	aCollection do: [:str | self compile: (aPattern copyReplaceAll: '%1' with: str asString)
			classified: aProtocol asSymbol]!

for: aCollection with: another create: aPattern classified: aProtocol 
	"Substitute each element into the pattern string at every occurrence of   
	'%1', and compile the resulting methods in the given protocol."

	aCollection with: another do: [:strA :strB | self compile: ((aPattern copyReplaceAll: '%1' with: strA asString)
				copyReplaceAll: '%2' with: strB asString)
			classified: aProtocol asSymbol]!

for: collA with: collB with: collC create: aPattern classified: aProtocol 
	"Substitute each element into the pattern string at every occurrence of     
	'%1', and compile the resulting methods in the given protocol."

	1 to: collA size do: [:index | self compile: (((aPattern copyReplaceAll: '%1' with: (collA at: index) asString)
				copyReplaceAll: '%2' with: (collB at: index) asString)
				copyReplaceAll: '%3' with: (collC at: index) asString)
			classified: aProtocol asSymbol]!

makePublic: aCollection 
	"Make the given instanceVariables public."

	self
		for: aCollection
		create: '%1: anObject\	"Set the receiver''s instance variable ''%1'' to be anObject."\\	%1 := anObject' withCRs
		classified: #accessing.
	self
		for: aCollection
		create: '%1\	"Answer the receiver''s ''%1''."\\	^%1' withCRs
		classified: #accessing!

makePublic: aCollection typed: types
	"Make the given instanceVariables public with typed arguments."

	self
		for: aCollection
		with: types
		create: '%1: %2\	"Set the receiver''s instance variable ''%1'' to be %2."\\	%1 := %2' withCRs
		classified: #accessing.
	self
		for: aCollection
		create: '%1\	"Answer the receiver''s ''%1''."\\	^%1' withCRs
		classified: #accessing!

publish
	"Make all my instanceVariables public."

	self makePublic: self instVarNames! !

