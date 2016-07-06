"	This Squeak format file contains the ANSI SIF Program model global, class, and method additions required to read in a SIF stream.

 ******
 * $Name:  $
 * $Id: SIFMod.st 1.3 1999-10-14 14:05:08-05 Harmon Alpha $
 * $Log: SIFMod.st $
 * Revision 1.3  1999-10-14 14:05:08-05  Harmon
 * Fixed postinstall script, again.
 *
 * Revision 1.2  1999-10-14 13:14:54-05  Harmon
 * Fixed post install script.
 *
 * Revision 1.1  1999-10-12 06:24:50-05  Harmon
 * Initial revision
 *
 ******"!

" Preinstall script.:"
	"Define global Annotation as Association class:"
	Smalltalk
		at: #'Annotation'
		put: Association.

	"Define global PGM (will be initialized to the abstract program component class):"
	Smalltalk
		at: #'PGM'
		put: nil.

	"Define global SIF (will be initialized to the abstract program reader/eriter class):"
	Smalltalk
		at: #'SIF'
		put: nil.!

'From Squeak 2.5 of August 6, 1999 on 28 September 1999 at 10:16:47 am'!
Error subclass: #ProgramError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
Error subclass: #ProgramFatalError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
ProgramFatalError subclass: #DuplicateProgramError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
ProgramFatalError subclass: #FailedToLoadProgramError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
Object subclass: #ProgramManager
	instanceVariableNames: 'programs processEvents '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
Notification subclass: #ProgramNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
Object subclass: #ProgramReaderWriter
	instanceVariableNames: 'stream '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-SIF Filing'!
ProgramReaderWriter subclass: #ProgramReaderV1r0
	instanceVariableNames: 'chunkString definedClassNames fileLocator programComponentSetStack '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-SIF Filing'!
ProgramReaderWriter class
	instanceVariableNames: 'currentReader '!
Warning subclass: #ProgramWarning
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
ProgramReaderWriter subclass: #ProgramWriterV1r0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-SIF Filing'!
ProgramFatalError subclass: #SIFVersionError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Exception'!
Model subclass: #TreeModelAbstract
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
TreeModelAbstract subclass: #ProgramAbstractComponent
	instanceVariableNames: 'annotations changed messages messageLevel parent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ProgramAbstractComponent subclass: #ElementDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ElementDefinition subclass: #CodeDefinitionionElement
	instanceVariableNames: 'source '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
CodeDefinitionionElement subclass: #InitializerElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
InitializerElement subclass: #ClassInitializerElement
	instanceVariableNames: 'className '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
InitializerElement subclass: #GlobalInitializerElement
	instanceVariableNames: 'globalName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
CodeDefinitionionElement subclass: #MethodDefinitionElement
	instanceVariableNames: 'className isClassMethod methodName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ElementDefinition subclass: #ObjectDefinitionElement
	instanceVariableNames: 'objectName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ObjectDefinitionElement subclass: #ClassDefinitionElement
	instanceVariableNames: 'superclassName indexedInstanceVariablesType instanceVariableNames classVariableNames sharedPoolNames classInstanceVariableNames '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ClassDefinitionElement class
	instanceVariableNames: 'subclassCreatorMethodSelectorList '!
ObjectDefinitionElement subclass: #GlobalDefinitionElement
	instanceVariableNames: 'isConstant '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ObjectDefinitionElement subclass: #PoolDefinitionElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ElementDefinition subclass: #PoolValueDefinitionElement
	instanceVariableNames: 'isConstant poolName poolValueName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
InitializerElement subclass: #PoolValueInitializerElement
	instanceVariableNames: 'poolName poolValueName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ElementDefinition subclass: #ProgramCommentElement
	instanceVariableNames: 'comment '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ProgramAbstractComponent subclass: #ProgramComponentSet
	instanceVariableNames: 'components '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ProgramComponentSet subclass: #GroupingSet
	instanceVariableNames: 'groupingName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
GroupingSet subclass: #ClassAllSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
GroupingSet subclass: #GlobalAllSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
GroupingSet subclass: #PoolAllSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
ProgramComponentSet subclass: #ProgramDefinitionSet
	instanceVariableNames: 'fileLocator hasDescription noteUnhandledAnnotations prerequisiteProgramNames programName wasEvaluated '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!
InitializerElement subclass: #ProgramInitializerElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-ANSI Program-Model'!

!Character methodsFor: 'testing' stamp: 'RAH 9/19/1999 16:55'!
isBinarySelectorChar 
	"Answer a <boolean>, true if the receiver can be in an ANSI binary method selector, else false.

Return Values
	<boolean>		unspecified
"		#SIFMod.
	^'!!%&''*+,`/<=>?@\~|-' includes: self! !

!Character methodsFor: 'testing' stamp: 'RAH 9/19/1999 16:55'!
isUnaryOrKeywordSelectorChar 
	"Answer a <boolean>, true if the receiver can be in an ANSI unary or keyword method selector, else false.

Return Values
	<boolean>		unspecified
"		#SIFMod.
	^self isLetter or: [self isDigit]! !


!Class methodsFor: 'source filing-class definition' stamp: 'RAH 9/19/1999 16:44'!
indexedInstanceVariablesType
	"Answer a <symbol>, the ANSI SIF indexable variables type of the receiver, or an invalid type.

Return Values
	<symbol>		new
"		#SIFMod.
	self isVariable ifFalse: [
		^#'none'
	].
	self isBits ifFalse: [
		^#'object'
	].	
	self isBytes ifTrue: [
		^#'byte'
	].
	^#'invalid'! !


!Collection methodsFor: 'enumerating' stamp: 'RAH 7/17/1999 13:34'!
do: operation separatedBy: separator
	"Evaluate operation with each element of the receiver interspersed by evaluation of separator.  Note: Separator is not evaluated if there are less than two elements nor after the last element.
	Implementation Note: temp. version for SIFMod."
	| index |
		#SIFMod. "Protocol: <collection>."
	self size < 2 ifTrue: [
		^self do: operation
	].
	index := 1.
	self do: [ :element |
		operation value: element.
		index = self size ifFalse: [
			separator value.
		].
		index := index + 1
	].
! !


!Dictionary methodsFor: 'enumerating' stamp: 'RAH 7/15/1999 15:47'!
keysAndValuesDo: aBlock
	"Answer the receiver.  For each key in the receiver, evaluate aBlock with the key and the value as the arguments.
	Implementation Note: temp. version for SIFMod."
		#SIFMod. "Protocol: <Dictionary>."		"Chg SQ"
	self associationsDo: [ :anAssociation |
		aBlock
			value: anAssociation key
			value: anAssociation value
	]
! !


!FailedToLoadProgramError class methodsFor: 'instance creation' stamp: 'RAH 9/19/1999 10:04'!
signal: signalerText
	"Raise a new instance of the receiver, with the <readableString> message text, signalerText and an exception identifying tag.

Definition: <exceptionSignaler>
Refinement: <FailedToLoadProgramError factory>
Parameters
	signalerText	<readableString>	captured
Return Value
	<FailedToLoadProgramError>		unspecified
"		#SIFMod. "Protocol: <exceptionSignaler>."
	^super new
		tag: PGM failedToLoadProgramTag;
		signal: signalerText
! !


!FileStream class methodsFor: 'instance creation' stamp: 'RAH 9/14/1999 08:20'!
read: fileId
	"Returns a read file stream that reads text from the file with the given name.
	Implementation Note: temp. version for SIFMod."
		#SIFMod. "Protocol: <FileStream factory>."
	^self concreteStream
		readOnlyFileNamed: (self fullName: fileId)	"Chg SQ."
! !

!FileStream class methodsFor: 'instance creation' stamp: 'RAH 9/24/1999 08:57'!
write: fileId
        "Returns a write file stream that writes text to the file with the given name.
	Implementation Note: temp. version for SIFMod."
		#SIFMod. "Protocol: <FileStream factory>."
	^self newFileNamed: fileId! !


!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:11'!
compilerNotification
	"Answer an <Notification class>, a compiler notification class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Notification class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
"	^Smalltalk at: #'CompilerNotification'	Dolphin"
	^Error, Notification		"Chg SQ"! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:13'!
duplicateProgramError
	"Answer an <Error class>, a duplicate program error class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Error class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'DuplicateProgramError'! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:15'!
failedToLoadProgramError
	"Answer an <Error class>, a failed-to-load-program error class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Error class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'FailedToLoadProgramError'! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:17'!
programError
	"Answer an <Error class>, a recoverable program error class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Error class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramError'! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:18'!
programFatalError
	"Answer an <Error class>, a fatal and unrecoverable program error class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Error class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramFatalError'! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:20'!
programNotification
	"Answer an <Notification class>, a recoverable program notification class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Notification class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramNotification'! !

!ProgramAbstractComponent class methodsFor: 'constants-exceptions' stamp: 'RAH 8/24/1999 10:22'!
programWarning
	"Answer an <Warning class>, a recoverable program warning class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Warning class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramWarning'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
classAllSet
	"Answer an <Object class>, the ClassAllSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ClassAllSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
classDefinitionElement
	"Answer an <Object class>, the ClassDefinitionElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ClassDefinitionElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
classInitializerElement
	"Answer an <Object class>, the ClassInitializerElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ClassInitializerElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
globalAllSet
	"Answer an <Object class>, the GlobalAllSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'GlobalAllSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
globalDefinitionElement
	"Answer an <Object class>, the GlobalDefinitionElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'GlobalDefinitionElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
globalInitializerElement
	"Answer an <Object class>, the GlobalInitializerElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'GlobalInitializerElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:58'!
groupingSet
	"Answer an <Object class>, the GroupingSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'GroupingSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
methodDefinitionElement
	"Answer an <Object class>, the MethodDefinitionElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'MethodDefinitionElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
poolAllSet
	"Answer an <Object class>, the PoolAllSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'PoolAllSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
poolDefinitionElement
	"Answer an <Object class>, the PoolDefinitionElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'PoolDefinitionElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
poolValueDefinitionElement
	"Answer an <Object class>, the PoolValueDefinitionElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'PoolValueDefinitionElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
poolValueInitializerElement
	"Answer an <Object class>, the PoolValueInitializerElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'PoolValueInitializerElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
programCommentElement
	"Answer an <Object class>, the ProgramCommentElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramCommentElement'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
programComponentSet
	"Answer an <Object class>, the ProgramComponentSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramComponentSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 09:59'!
programDefinitionSet
	"Answer an <Object class>, the ProgramDefinitionSet class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramDefinitionSet'! !

!ProgramAbstractComponent class methodsFor: 'constants-classes' stamp: 'RAH 8/24/1999 10:00'!
programInitializerElement
	"Answer an <Object class>, the ProgramInitializerElement class object.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Object class>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Smalltalk at: #'ProgramInitializerElement'! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 08:59'!
newClass: classNameString initializer: initializerString
	"Answer a <ClassInitializerElement>, a new class initialization program element for the class named, classNameString, with initialization code, initializerString.

Definition: <ProgramAbstractComponent factory>
Parameters
	classNameString	<readableString>	captured
	initializerString	<readableString>	captured
Return Values
	<ClassInitializerElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM classInitializerElement new
		className: classNameString;
		source: initializerString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:05'!
newClassAllSet: classNameString
	"Answer a <ClassAllSet>, a new program component class all set for the class named, classNameString.
	Note: This has no corresponding definition in the ANSI Smalltalk Standard SIF specification, and is only a convenient way of specifying a class definition element and method definition elements for all existing class and instance methods to be included in a program definition.

Definition: <ProgramAbstractComponent factory>
Parameters
	classNameString	<readableString>	captured
Return Values
	<ClassAllSet>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM classAllSet new
		groupingName: classNameString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 09:30'!
newClassDefinitionOf: classNameString
superclass: superclassNameString
indexedInstanceVariables: indexableInstVarTypeSymbol
instanceVariableNames: instanceVariableNamesString
classVariableNames: classVariableNamesString
sharedPools: poolNamesString
classInstanceVariableNames: classInstanceVariableNamesString
	"Answer a <ClassDefinitionElement>, a new class definition program element for the class named, classNameString, with superclass named, superclassNameString, indexedInstanceVariables type, indexableInstVarTypeSymbol, instanceVariableNames named, instanceVariableNamesString, classVariableNames named, classVariableNamesString, sharedPools named, poolNamesString, and classInstanceVariableNames named, classInstanceVariableNamesString.

Definition: <ProgramAbstractComponent factory>
	Implementation Note: The class must be an already existing class because current tools assume an imperative instead of a declarative declaration program model.  Therefore a new class definition can specify all the parameters, or substitute nil for all but the class name.  In either case the existing class superclass name, etc. will be set to existing class's values when the element is filed out.
Parameters
	classNameString				<readableString>	captured
	superclassNameString			<readableString>	captured
	indexableInstVarTypeSymbol		<symbol>		captured
	instanceVariableNamesString		<readableString>	captured
	classVariableNamesString		<readableString>	captured
	poolNamesString				<readableString>	captured
	classInstanceVariableNamesString	<readableString>	captured
Return Values
	<ClassDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM classDefinitionElement new
		objectName: classNameString;
		superclassName: superclassNameString;
		indexedInstanceVariablesType: indexableInstVarTypeSymbol;
		instanceVariableNames: instanceVariableNamesString;
		classVariableNames: classVariableNamesString;
		sharedPoolNames: poolNamesString;
		classInstanceVariableNames: classInstanceVariableNamesString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 09:37'!
newClassMethodDefinitionOf: methodNameString in: classNameString
	"Answer a <MethodDefinitionElement>, a new class method definition program element for the class named, classNameString, with method named, methodNameString, and class method indicator set to true.

Definition: <ProgramAbstractComponent factory>
Parameters
	methodNameString	<readableString>	captured
	classNameString	<readableString>	captured
Return Values
	<MethodDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM methodDefinitionElement new
		className: classNameString;
		methodName: methodNameString;
		isClassMethod: true;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 09:40'!
newGlobal: globalNameString initializer: initializerString
	"Answer a <GlobalInitializerElement>, a new global initialization program element for the global named, globalNameString, with initialization code, initializerString.

Definition: <ProgramAbstractComponent factory>
Parameters
	globalNameString	<readableString>	captured
	initializerString	<readableString>	captured
Return Values
	<GlobalInitializerElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM globalInitializerElement new
		globalName: globalNameString;
		source: initializerString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:16'!
newGlobalAllSet: globalNameString
	"Answer a <GlobalAllSet>, a new program component global all set for the global named, globalNameString.
	Note: This has no corresponding definition in the ANSI Smalltalk Standard SIF specification, and is only a convenient way of specifying a global definition element and its initialization element to be included in a program definition.

Definition: <ProgramAbstractComponent factory>
Parameters
	globalNameString	<readableString>	captured
Return Values
	<GlobalAllSet>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM globalAllSet new
		groupingName: globalNameString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 09:52'!
newGlobalDefinitionOf: globalNameString constant: isConstantBoolean
	"Answer a <GlobalDefinitionElement>, a new global definition program element for the global named, globalNameString, and constant indicator set to isConstantBoolean.

Definition: <ProgramAbstractComponent factory>
Parameters
	globalNameString		<readableString>	captured
	isConstantBoolean		<boolean>			captured
Return Values
	<GlobalDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM globalDefinitionElement new
		objectName: globalNameString;
		isConstant: isConstantBoolean;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:14'!
newGroupingSet: groupingNameString
	"Answer a <GroupingSet>, a new program component grouping set for the grouping named, groupingNameString.
	Note: This has no corresponding definition in the ANSI Smalltalk Standard SIF specification, and is only a convenient way of grouping components in a program definition.

Definition: <ProgramAbstractComponent factory>
Parameters
	groupingNameString	<readableString>	captured
Return Values
	<GroupingSet>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM groupingSet new
		groupingName: groupingNameString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:11'!
newInstanceMethodDefinitionOf: methodNameString in: classNameString
	"Answer a <MethodDefinitionElement>, a new instance method definition program element for the class named, classNameString, with method named, methodNameString, and class method indicator set to false.

Definition: <ProgramAbstractComponent factory>
Parameters
	methodNameString	<readableString>	captured
	classNameString	<readableString>	captured
Return Values
	<MethodDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM methodDefinitionElement new
		className: classNameString;
		methodName: methodNameString;
		isClassMethod: false;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:20'!
newPoolAllSet: poolNameString
	"Answer a <PoolAllSet>, a new program component pool all set for the pool named, poolNameString.
	Note: This has no corresponding definition in the ANSI Smalltalk Standard SIF specification, and is only a convenient way of specifying a pool definition element, and pool value definition and initialization elements for an existing pool to be included in a program definition.

Definition: <ProgramAbstractComponent factory>
Parameters
	poolNameString		<readableString>	captured
Return Values
	<PoolAllSet>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM poolAllSet new
		groupingName: poolNameString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:23'!
newPoolDefinitionOf: poolNameString
	"Answer a <PoolDefinitionElement>, a new pool definition program element for the pool named, poolNameString.

Definition: <ProgramAbstractComponent factory>
Parameters
	poolNameString		<readableString>	captured
Return Values
	<PoolDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM poolDefinitionElement new
		objectName: poolNameString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:28'!
newPoolValue: poolValueNameString in: poolNameString initializer: initializerString
	"Answer a <PoolValueInitializerElement>, a new pool value initialization program element for the pool value named, poolValueNameString, in the pool named, poolNameString, with initialization code, initializerString.

Definition: <ProgramAbstractComponent factory>
Parameters
	poolValueNameString	<readableString>	captured
	poolNameString			<readableString>	captured
	initializerString		<readableString>	captured
Return Values
	<PoolValueInitializerElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM poolValueInitializerElement new
		poolName: poolNameString;
		poolValueName: poolValueNameString;
		source: initializerString;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:33'!
newPoolValueDefinitionOf: poolValueNameString
in: poolNameString
constant: isConstantBoolean
	"Answer a <PoolValueDefinitionElement>, a new pool value definition program element for the pool value named, poolValueNameString, in the pool named, poolNameString, and constant indicator set to isConstantBoolean.

Definition: <ProgramAbstractComponent factory>
Parameters
	poolValueNameString	<readableString>	captured
	poolNameString			<readableString>	captured
	isConstantBoolean		<boolean>			captured
Return Values
	<PoolValueDefinitionElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM poolValueDefinitionElement new
		poolName: poolNameString;
		poolValueName: poolValueNameString;
		isConstant: isConstantBoolean;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:41'!
newProgramComment: commentString
	"Answer a <ProgramCommentElement>, a new program comment element with content, commentString.

Definition: <ProgramAbstractComponent factory>
Parameters
	commentString		<readableString>	captured
Return Values
	<ProgramCommentElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM programCommentElement new
		comment: commentString! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 8/25/1999 10:43'!
newProgramInitializer: initializerString
	"Answer a <ProgramInitializerElement>, a new program initialization element with initialization code, initializerString.

Definition: <ProgramAbstractComponent factory>
Parameters
	initializerString	<readableString>	captured
Return Values
	<ProgramInitializerElement>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM programInitializerElement new
		source: initializerString! !

!ProgramAbstractComponent class methodsFor: 'instance creation' stamp: 'RAH 9/23/1999 10:17'!
newProgramNamed: programNameString
	"Answer a <ProgramDefinitionSet>, a new program component set named, programNameString.
	Note: This is the root component of an ANSI program definition (ie a program).

Definition: <ProgramAbstractComponent factory>
	Implementation Note: I intended that nested programs (a program grouping set or program can contain a program as a component) be allowed, but this may break some methods.
Parameters
	programNameString	<readableString>	captured
Return Values
	<ProgramDefinitionSet>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^PGM programDefinitionSet new
		initialize;
		programName: programNameString;
		noteUnhandledAnnotations: true;
		yourself! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/24/1999 10:25'!
createdByAppKey
	"Answer a <readableString>, the created-by-application annotation key of this Smalltalk dialect.
	Note: Used to annotate with the name and version of this Smalltalk dialect.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'createdByApp' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/24/1999 10:27'!
createdDateTimeKey
	"Answer a <readableString>, the created-date-and-time annotation key of this Smalltalk dialect.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'createdDateTime' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/25/1999 08:03'!
postinstallKey
	"Answer a <readableString>, the post-install script annotation key.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'postinstall' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/25/1999 08:04'!
postuninstallKey
	"Answer a <readableString>, the post-uninstall script annotation key.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'postuninstall' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/25/1999 08:05'!
preinstallKey
	"Answer a <readableString>, the pre-install script annotation key.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'preinstall' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/25/1999 08:06'!
preuninstallKey
	"Answer a <readableString>, the pre-uninstall script annotation key.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'preuninstall' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-annotation keys' stamp: 'RAH 8/25/1999 08:07'!
protocolKey
	"Answer a <readableString>, the protocol annotation key of this Smalltalk dialect.
	Note: Used to annotate with a protocol.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'protocol' copy! !

!ProgramAbstractComponent class methodsFor: 'constants-message levels' stamp: 'RAH 8/25/1999 08:41'!
messageErrorLevel
	"Answer an <integer>, an integer indicating a recoverable error level message.

Definition: <ProgramAbstractComponent factory>
Return Values
	<integer>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^75! !

!ProgramAbstractComponent class methodsFor: 'constants-message levels' stamp: 'RAH 8/25/1999 08:42'!
messageFatalLevel
	"Answer an <integer>, an integer indicating an fatal error level message.

Definition: <ProgramAbstractComponent factory>
Return Values
	<integer>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^100! !

!ProgramAbstractComponent class methodsFor: 'constants-message levels' stamp: 'RAH 8/25/1999 08:43'!
messageInformLevel
	"Answer an <integer>, an integer indicating an inform level message.

Definition: <ProgramAbstractComponent factory>
Return Values
	<integer>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^25! !

!ProgramAbstractComponent class methodsFor: 'constants-message levels' stamp: 'RAH 8/25/1999 08:44'!
messageWarningLevel
	"Answer an <integer>, an integer indicating a warning level message.

Definition: <ProgramAbstractComponent factory>
Return Values
	<integer>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^50! !

!ProgramAbstractComponent class methodsFor: 'constants-misc' stamp: 'RAH 8/25/1999 08:30'!
failedToLoadProgramTag
	"Answer a <readableString>, a signaled duplicate program error tag.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'FailedToLoadProgram' copy! !

!ProgramAbstractComponent class methodsFor: 'initializing' stamp: 'RAH 8/25/1999 08:36'!
initialize
	"Class initialization.  Example:

		ProgramAbstractComponent initialize

Definition: <ProgramAbstractComponent factory>
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	"Initialize PGM to be the receiver:"
	PGM := self! !

!ProgramAbstractComponent class methodsFor: 'default' stamp: 'RAH 8/25/1999 08:14'!
defaultAnnotationCollection
	"Answer a Dictionary, the default new annotation collection.  It will contain a key (author, category, etc.) and corresponding value object for each annotation.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Dictionary>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Dictionary new! !

!ProgramAbstractComponent class methodsFor: 'default' stamp: 'RAH 8/25/1999 08:27'!
defaultMessageCollection
	"Answer an <OrderedCollection>, the default new message collection.

Definition: <ProgramAbstractComponent factory>
Return Values
	<OrderedCollection>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^OrderedCollection new! !

!ProgramAbstractComponent class methodsFor: 'default' stamp: 'RAH 8/25/1999 08:27'!
defaultPrerequisiteProgramNameCollection
	"Answer a <Set>, the default new prerequisite program name collection.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Set>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^Set new! !

!ProgramAbstractComponent class methodsFor: 'private' stamp: 'RAH 07/08/1999 01:24:29'!
new
	"Private - Answer a new instance of the receiver.
	Note: May only be used by instance creation methods in this class."
		#SIFMod.
	^super new
		initialize;
		yourself
! !

!ProgramAbstractComponent class methodsFor: 'events' stamp: 'RAH 8/25/1999 11:07'!
publishedEventsOfInstances
	"Answer a <Set>, the set of <symbols> that describe the published events triggered by instances of the receiver.

Definition: <ProgramAbstractComponent factory>
Return Values
	<Set>	state
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^super publishedEventsOfInstances
		add: #componentAdded:;
		add: #componentUpdated:;
		add: #componentRemoved:;
		add: #componentRenamed:;
		yourself! !


!ClassDefinitionElement class methodsFor: 'private' stamp: 'RAH 8/28/1999 06:20'!
subclassCreatorMethodSelectorList
	"Private - Answer a Dictionary, the subclass creator method selectors for the indexed instance variables types."
		#SIFMod.
	^subclassCreatorMethodSelectorList! !

!ClassDefinitionElement class methodsFor: 'initializing' stamp: 'RAH 8/28/1999 06:20'!
initialize

"Definition: <ProgramAbstractComponent factory>
	Note: (Dialect Specific).  Example:

		ClassDefinitionElement initialize.

"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	"Associate each indexed instance variables type of subclass with an appropriate method selector to compile it."
	subclassCreatorMethodSelectorList := Dictionary new.
	subclassCreatorMethodSelectorList
 		at: #'byte'				"Chged SQ"
		put: #'variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:'.
	subclassCreatorMethodSelectorList
 		at: #'none'
		put: #'subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:'.
	subclassCreatorMethodSelectorList
 		at: #'object'
		put: #'variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:'.
! !


!ProgramComponentSet class methodsFor: 'default' stamp: 'RAH 9/23/1999 09:47'!
defaultComponentCollection
	"Answer an <OrderedCollection>, the default component collection object.

Definition: <ProgramComponentSet factory>
Return Values
	<OrderedCollection>		new
"		#SIFMod. "Protocol: <ProgramComponentSet factory>."
	^OrderedCollection new! !


!ProgramDefinitionSet class methodsFor: 'default' stamp: 'RAH 9/23/1999 09:50'!
defaultPrerequisiteProgramNameCollection
	"Answer a <Set>, the default prerequisite program name collection object.

Definition: <ProgramDefinitionSet factory>
Return Values
	<Set>		new
"		#SIFMod. "Protocol: <ProgramDefinitionSet factory>."
	^Set new! !


!DuplicateProgramError commentStamp: 'RAH 9/28/1999 09:53' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.


1.4.3.1 Protocol: <DuplicateProgramError>

Conforms To
	<ProgramFatalError>

Description
	Class DuplicateProgramError implements the behavior of a fatal error where an attempt was made to install or load a program with the same name (disregarding case) as one already in the program manager collection of programs.  No further processing is possible.
!

!ProgramManager commentStamp: 'RAH 9/28/1999 09:07' prior: 0!
This is the starting point for the SIF Model Protocols documentation.

1. SIF Model Protocols

	The SIF Model protocols contains four major sections consisting of the ProgramManager, ProgramAbstractComponent, ProgramReaderWriter, and ProgramException root classes.

	The graphs below shows the conformance relationships between the major sections root protocols defined in this section.

			     <Object>
				|
	-------------------------------------------------
	|	 |		|	|		|
<ProgramManager> | 	    	| <programReaderWriter>	|
   <ProgramManager factory>	|	<programReaderWriter factory>
				|
			-----------------
			|		|
		     <model>	<ProgramAbstractComponent factory>
			|
		<abstractTreeModel>
			|
		<ProgramAbstractComponent>



		. . .
		  |
	     <Exception>
		  |
	<programException>


Globals:

	There are two globals that are defined for convenience primarily in concice component creation, accessing defaults and constants (instead of pools).  They are:

	PGM	ProgramAbstractComponent class 
	SIF	ProgramReaderWriter class



1.1 ProgramManager Protocols

1.1.1 Protocol: <ProgramManager>

Conforms To
	<Object>

Description
	Class ProgramManager implements the behavior of a singleton instance to manage (created, removed, edited, etc.) Smalltalk Interchange Format (SIF) programs.  All programs must have unique name without regard to case.  Using ProgramReaderWriter a Program can be written or loaded or installed from a SIF file.  Loading only reads the program's components so the program may be viewed with a browser.  Installation installs a program and all prerequisite programs.  It first loads a program, then evaluates (creates/initializes) the Smalltalk objects specified by the program components.
	The program browser graphical user interface (GUI) is normally used to manage programs.

Instance Variables:

	programs	<OrderedCollection>	The collection of programs held by the manager.

	processEvents	<integer>		The count of the program manager's process-events locks.  Zero indicates that no one has a lock on the triggers and that events should be processed.  A value greater than zero indicates that processing is temporarily suspend until it is reduced back to zero.  A value less than zero indicates an invalid state.

Class Variables:

	Current	<ProgramManager>	The singleton instance of the program manager.




1.1.2 Protocol: <ProgramManager factory>

Conforms To
	<Object>

Description
	This protocol implements the behavior of creating, accessing, and removing the singleton instance of the program manager; and its published events.

Instance Variables:

	Current	<ProgramManager>	The singleton instance of the  program manager.

 
1.2 ProgramAbstractComponent Protocols

	See the ProgramAbstractComponent class comment.

1.3 ProgramReaderWriter Protocols

	See the ProgramReaderWriter class comment.

1.4 ProgramException Protocols

	This section includes protocols that define the behavior of the ProgramReaderWriter classes.

	The graphs below shows the conformance relationships between the protocols defined in this section.

		      . . .
			|
		   <Exception>
			|
		<programException>
			|
	-------------------------
	|			|
     	|		<Notification>
	|			|
	|		-------------------------
	|		|			|
	|	<ProgramNotification>	    <Warning>
	|					|
     <Error>				<ProgramWarning>
	|
	---------------------------------
	|				|
<ProgramError>			<ProgramFatalError>
	|- <DuplicateProgramError>
	|- <FailedToLoadProgramError>
	|- <SIFVersionError>


		      . . .
			|
		<exceptionSignaler>
			|
	-----------------------------------------
	|					|
<FailedToLoadProgramError factory>	<SIFVersionError factory>


1.4.1 Protocol: <programException>

Conforms To
	<Exception>

Description
	This protocol implements the behavior of program exceptions: errors, fatal errors, notifications, and warnings.


1.4.2 Protocol: <ProgramError>

	See the ProgramError class comment.


1.4.3 Protocol: <ProgramFatalError>

	See the ProgramFatalError class comment.

1.4.4 Protocol: <ProgramNotification>

	See the ProgramNotification class comment.

1.4.5 Protocol: <ProgramWarning>

	See the ProgramWarning class comment.

1.4.6 Protocol: <FailedToLoadProgramError factory>

	See the FailedToLoadProgramError class comment.


1.4.7 Protocol: <SIFVersionError factory>

	See the SIFVersionError class comment.

Notes:

1. Use of Exceptions:

	I think I misused exceptions in my error handling due to my limited experience with exceptions.  Allen Wirfs-Brock posted an excellent comment to the Squeak mailing list on 8/16/99 with the subject "Re: Exception Hierarchies" giving a few helpful points on exceptions.  I think I managed to do exactly what he recommended not be done in every one of his points.
	So the error handling is not reliable and needs to be reworked by someone more experienced in exceptions.

2. System Changed Elements:

	Notification of classes, methods, pools, and globals that are changed by the environment (SDE) is currently disabled in the initialization methods as the responses to these notices are not fully thought through, incomplete, and in error (a good possibility).
	This implementation assumes a declarative programming model but is implemented in an imperative programming model.  This means some aspect of a declarative model program can be changed by the imperative model SDE (change of class definition, added or removed pool value, method category, etc), and should be updated to the new aspect.  My attempt to do the somewhat complex updating is seriously flawed and incorrect.  Therefore I have commented out the code in the program manager initialization that would trigger notification of changes in the SDE.


Enhancements:

1. Error Handling:

	The message (error, warning, etc) needs to be sorted out, probably relying on exceptions.
	It would be useful to inform the user of the file position when an error occurs in loading a SIF file.
	It would be useful to ask if the user wishes to discontinue after a user set number of error have occurred.
	It would be useful to log errors to a file.

2. Documentation:

	I'd like to "automagically" generate documentation of protocols similar to the proposed ANSI standard, possibly HTML.  I think it would help to annotate elements with stuff like class, pool, global, and method comment containing the synopsis, definition the protocol the message is defined in, errors, parameters, implementation, etc.

3. User Interface (UI):

	 Drag and drop of components is needed.
	An Annotation presenter rather than an inspector for editing a component's annotation is needed.
	Commands find and find next on a string or something would be useful.

4. Declarative Vs Imperative Model:

	I thought it might be useful to add the SIF filing and declarative program stuff to the Refactoring Browser (RB) as part of a adding a Declarative Model to the Smalltalk Development Environment (SDE).  The RB has been ported to a lot of dialects.
	Without a Declarative Model, it would be useful if when some aspect of a program was changed by the SDE, the corresponding program component was marked changed for later updating, or modified to the new aspect.  This would include a change of class definition, added or removed pool value, method category, etc.

	I would like to add system programs that own all of the system classes, methods, pools, and globals which define the base Smalltalk system.  It would not possible to change, save, or install them.

5. Program Changes:

	I'd like to mark program components that have been changed but not saved.
	I'd like to implement an undo facility.

6. Optimizations:

	I think it would be helpful to have each program maintain lists of globals, classes, and pools that are referenced by its components.!

!ProgramManager methodsFor: 'searching' stamp: 'RAH 9/8/1999 08:37'!
includesProgramNamed: programNameString
	"Answer a <boolean>, true if the collection of programs held by the receiver program could be uninstalled from the collection of programs held by the receiver holds a program whose name matches nameString, false if not.

Definition: <ProgramManager>
Parameters
	programNameString	<readableString>		uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramManager>."
	self
		programAtName: programNameString
		ifAbsent: [
			^false
		].
	^true! !

!ProgramManager methodsFor: 'program filing' stamp: 'RAH 9/27/1999 08:06'!
buildLoadListOfProgramFileLocator: programFileLocator
loadList: programsToLoadCollection 
isPrerequisite: isPrerequisiteBoolean
	"Private - Build the collection, programsToLoadCollection, of ANSI SIF programs to be loaded starting with the program in the file specified by programFileLocator and all prerequisite programs.  If this file is not found, prompt the user for another file locator.

	Note: This calls itself recursively to get prerequisite programs to be loaded.   The programs in the collection contain only the program description set but no program components. 
Parameters
	programFileLocator		<readableString>		uncaptured
	programsToLoadCollection	<collection>			uncaptured
	isPrerequisiteBoolean	<boolean>			uncaptured
Errors
	Failed to find the file of program or prerequisite programs.
	Program with the same name already exists in the collection as the program in the specified file.
"	| actualLocator expectedLocator flExt flPath program |
		#SIFMod.
	(self portFunc fileExists: programFileLocator) ifTrue: [
		actualLocator := programFileLocator
	] ifFalse: [
		actualLocator := self requestProgramFileName: programFileLocator
	].
	actualLocator isEmpty ifTrue: [
		self errorFailedToFindProgramFileLocator: programFileLocator

	].

	program:= self loadProgramDescription: actualLocator.
	(self includesProgramNamed: program programName) ifTrue: [
		"Already loaded."
#todoSIFMod."Loaded but maybe not installed?"
		isPrerequisiteBoolean ifTrue: [
			^self	"It and all its prerequisites loaded."
		] ifFalse: [
			^self errorAlreadyInstalledProgram: program.
	]	].
	(programsToLoadCollection includes: program) ifFalse: [
		"Add it and all its prerequisites to load list."
		programsToLoadCollection add: program.
		program prerequisiteProgramNames do: [ :prereqProgramName |
			"Assume prereq. path is same as program path, filename
			same as prereq. name, and prereq. ext. same as program ext."
			self portFunc
				fileLocatorSplit: actualLocator
				to: [ :flPathString :flNameString :flExtString |
					flPath := flPathString.
					flExt := flExtString.
			].
			expectedLocator := self portFunc
				fileLocatorFromPath: flPath
				fileName: prereqProgramName
				extension: flExt.
			self buildLoadListOfProgramFileLocator: expectedLocator
				loadList: programsToLoadCollection 
				isPrerequisite: true
	]	].
! !

!ProgramManager methodsFor: 'program filing' stamp: 'RAH 9/27/1999 08:17'!
installProgramFileLocator: programFileLocator
	"Answer a <ProgramDefinitionSet>, the program after it has been loaded from the ANSI SIF file named programFileLocator then evaluated, or nil if it fails to install.

Definition: <ProgramManager>
	Note: The program and all prerequisite programs from the same path location as programFileLocator path are installed.  If a suitable program is not found in programFileLocator path, the user is prompted for one.
Parameters
	programFileLocator	<readableString>		captured
Return Values
	<ProgramDefinitionSet>	captured
	<nil>					unspecified
"	| programsToLoad |
		#SIFMod. "Protocol: <ProgramManager>."
	programsToLoad := OrderedCollection new.
	[self buildLoadListOfProgramFileLocator: programFileLocator
		loadList: programsToLoad
		isPrerequisite: false.
	] on: PGM failedToLoadProgramError do: [ :except |
		except tag = PGM failedToLoadProgramTag ifTrue: [
			^nil
		] ifFalse: [
			except pass
	]	].

	self beNotProcessingEvents.
	[	programsToLoad reverseDo: [ :program |
			program installProgramComponents.
			self addProgram: program.
		].
		programsToLoad isEmpty ifTrue: [
			^nil
		].
		^programsToLoad first
	] ensure: [
		self beProcessingEvents.
		self triggerProgramsLoadedChanged
	].
! !

!ProgramManager methodsFor: 'program filing' stamp: 'RAH 9/24/1999 07:01'!
loadProgramDescription: programFileLocator
	"Private - Answer a <ProgramDefinitionSet>, the program with description set but no program components from the ANSI SIF file named programFileLocator.

Parameters
	programFileLocator	<readableString>		captured
Return Values
	<ProgramDefinitionSet>	captured
"	| aStream msgTxt program reader |
		#SIFMod.
	aStream := FileStream read: programFileLocator.
	[	[reader := SIF newReaderOn: aStream.
		Cursor read show.		"Chged SQ"
		] on: SIF sifVersionError do: [ :except |
			except tag = SIF sifVersionTag ifTrue: [
				msgTxt := except description, '.  Aborting file "',
						programFileLocator, '" load.'.
				self portFunc promptInform: msgTxt.
				^PGM failedToLoadProgramError signal: msgTxt.
			] ifFalse: [
				except pass
		]	].

		program := reader newProgramFromDescription.
		program fileLocator: programFileLocator.
		^program


	] ensure: [
		aStream close.
		Cursor normal show.
	]
! !

!ProgramManager methodsFor: 'program filing' stamp: 'RAH 9/27/1999 08:26'!
loadProgramFileLocator: programFileLocator
	"Answer a <ProgramDefinitionSet>, the program after it has been loaded from the ANSI SIF file named programFileLocator but NOT evaluated, or nil if it fails to load.

Definition: <ProgramManager>
	Note: Prerequisite programs are NOT loaded.
Parameters
	programFileLocator	<readableString>		captured
Return Values
	<ProgramDefinitionSet>	captured
	<nil>					unspecified
"	| newProgram |
		#SIFMod. "Protocol: <ProgramManager>."
	[newProgram := self loadProgramDescription: programFileLocator.
	] on: PGM failedToLoadProgramError do: [ :except |
		except tag = PGM failedToLoadProgramTag ifTrue: [
			^nil
		] ifFalse: [
			except pass
	]	].
	self beNotProcessingEvents.
	[	newProgram loadProgramComponents.
		self addProgram: newProgram.
		^newProgram
	] ensure: [
		self beProcessingEvents.
		self triggerProgramsLoadedChanged
	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:10'!
onSystemClassAdded: aClass
	"Private - React to aClass being added to the Smalltalk system.

Parameters
	aClass	<classDescription>	uncaptured
"	| className |
		#SIFMod.
#todoSIFMod."This should add the class name to the receiver's list of unplaced classes. 
 	self isProcessingEvents ifTrue: [
		className := aClass name asString.
	].
"! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:11'!
onSystemClassRemoved: aClass
	"Private - React to aClass being removed from the Smalltalk system.  Ensure that none of the programs held by the receiver refer to aClass or its methods now it has departed.

Parameters
	aClass	<classDescription>	uncaptured
"	| className |
		#SIFMod.
#todoSIFMod."I'm not sure from what programs this class should be removed."
	self isProcessingEvents ifTrue: [
		className := aClass name asString.
		self programs do: [:program |
			(program includesClassNamed: className) ifTrue: [
				program sysClassRemovedNamed: className.
	]	]	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:11'!
onSystemClassUpdated: aClass
	"Private - React to aClass being updated.  This could be in response to its pool dictionaries changing, super class changing, etc.

Parameters
	aClass	<classDescription>	uncaptured
"	| className |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		className := aClass name asString.
		self programs do: [:program |
			(program includesClassNamed: className) ifTrue: [
				program sysClassUpdatedNamed: className.
	]	]	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:11'!
onSystemGlobalRemoved: globalSymbol
	"Private - React to globalName being removed from the system.  Ensure that none of the programs held by the receiver refer to globalObject now it has departed.

Parameters
	globalSymbol		<symbol>		uncaptured
"	| globalName |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		globalName := globalSymbol asString.
		self programs do: [:program |
			(program includesGlobalNamed: globalName) ifTrue: [
				program sysGlobalRemovedNamed: globalName.
	]	]	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:13'!
onSystemGlobalRenamed: nameSymbolArray
	"Private - A global has been renamed.  Ensure that all the receiver's programs are aware of this change.

Parameters
	nameSymbolArray		<Array>		unspecified
"	| oldGlobalName |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		oldGlobalName := nameSymbolArray first asString.
		self programs do: [:program |
			(program includesGlobalNamed: oldGlobalName) ifTrue: [
				program sysRenamedGlobalNamed: oldGlobalName
				to: nameSymbolArray last asString.
	]	]	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:16'!
onSystemMethodAdded: aCompiledMethod
	"Private - React to aCompiledMethod being a new method added to the system, or an existing method being recompiled.

Parameters
	aCompiledMethod	CompiledMethod	uncaptured
"	| className methodName found |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		"Is this an existing method being recompiled?"
		found := false.
		className := aCompiledMethod methodClass name asString.
		methodName := aCompiledMethod selector asString.
		self programs do: [:program |
			(program includesMethodNamed: methodName
				in: className
				isClassMethod: aCompiledMethod isClassMethod
			) ifTrue: [		"Existing method."
				program sysMethodUpdatedNamed: methodName
					in: className
					isClassMethod: aCompiledMethod isClassMethod.
				found := true.
			] ifFalse: [		"New method."
				(program includesClassAllNamed: className) ifTrue: [
					program sysMethodAddedNamed: methodName
						inClassAll: className
						isClassMethod: aCompiledMethod isClassMethod.
					found := true.
		]	]	].
#todoSIFMod."This should add the method name, etc. to the receiver's list of unplaced methods. 
		found ifFalse: [
		??No, a new method added to the system.??
		].
"
	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:16'!
onSystemMethodRemoved: aCompiledMethod
	"Private - React to aCompiledMethod being removed from the system.

Parameters
	aCompiledMethod	CompiledMethod	uncaptured
"	| className methodName |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		className := aCompiledMethod methodClass name asString.
		methodName := aCompiledMethod selector asString.
		self programs do: [:program |
			(program includesMethodNamed: methodName
				in: className
				isClassMethod: aCompiledMethod isClassMethod
			) ifTrue: [
				program sysMethodRemovedNamed: methodName
					in: className
					isClassMethod: aCompiledMethod isClassMethod
	]	]	].
! !

!ProgramManager methodsFor: 'event handling' stamp: 'RAH 9/8/1999 15:16'!
onSystemMethodUpdated: aCompiledMethod
	"Private - React to aCompiledMethod being updated in the system.

Parameters
	aCompiledMethod	CompiledMethod	uncaptured
"	| className methodName |
		#SIFMod.
	self isProcessingEvents ifTrue: [
		className := aCompiledMethod methodClass name asString.
		methodName := aCompiledMethod selector asString.
		self programs do: [:program |
			(program includesMethodNamed: methodName
				in: className
				isClassMethod: aCompiledMethod isClassMethod
			) ifTrue: [
				program sysMethodRemovedUpdated: methodName
					in: className
					isClassMethod: aCompiledMethod isClassMethod
	]	]	].
! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/20/1999 05:33'!
beNotProcessingEvents
	"Private - Temporarily suspend the processing of system events or triggering of our own.

	Note:   This is not a binary mode but a count, so the same number of beProcessingEvents must be sent to the receiver as beNotProcessingEvents before events are again processed.  Ignored events are NOT cached by the receiver for processing later.
"		#SIFMod.
	self changeProcessEventsBy: 1
! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 07:12'!
beProcessingEvents
	"Private - Recommence processing system events and sending our own only if an equal number of beProcessingEvents as beNotProcessingEvents have been sent to the receiver.
"		#SIFMod.
	self changeProcessEventsBy: -1
! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/20/1999 05:44'!
processEvents
	"Private - Answer the count of the receivers processEvents.  A value of zero indicates that no one has a lock on the triggers and that they should be processed as they are received from the rest of the system or generated by the receiver.  A value greater than 0 indicates that processing is temporarily suspend until it is reduced back to 0. Any events received while suspended are NOT cached for later processing."
		#SIFMod.
	^processEvents
! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 13:54'!
programAtName: programNameString
	"Answer a <ProgramDefinitionSet> the program named programNameString (ignoring case) in the collection of programs held by the receiver.

Definition: <ProgramManager>
Parameters
	programNameString	<readableString>		uncaptured
Return Values
	<ProgramDefinitionSet>		state
Errors
	Failed to find a program named programNameString.
"		#SIFMod. "Protocol: <ProgramManager>."
	^self
		programAtName: programNameString
		ifAbsent: [self errorNotFound: programNameString]! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 13:54'!
programAtName: programNameString ifAbsent: absentBlock
	"Answer a <ProgramDefinitionSet>, the program named programNameString (ignoring case) in the collection of programs held by the receiver, or an <Object>, the result of evaluating the <niladicValuable> argument, absentBlock, if none is found.

Definition: <ProgramManager>
Parameters
	programNameString	<readableString>		uncaptured
	absentBlock			<niladicValuable>	uncaptured
Return Values
	<ProgramDefinitionSet>	state
	<Object>					unspecified
"		#SIFMod. "Protocol: <ProgramManager>."
	^self programs
		detect: [ :program |
			(program programName asLowercase) = (programNameString asLowercase)
		]
		ifNone: [absentBlock value]! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 14:00'!
programNames
	"Answer a <collect>, a list of the name strings of the programs in the collection of programs held by the receiver.

Definition: <ProgramManager>
Return Values
	<collect>		new
"		#SIFMod. "Protocol: <ProgramManager>."
	^programs collect: [ :tmpProgram |
		tmpProgram programName
	]! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 14:05'!
programs
	"Answer a <collect>, the collection of programs held by the receiver.

Definition: <ProgramManager>
Return Values
	<collect>		state
"		#SIFMod. "Protocol: <ProgramManager>."
	^programs
! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 14:34'!
programsRequiring: aProgramDefinitionSet
	"Answer a <Set>, the programs held by the receiver that have the program, aProgramDefinitionSet, as a prerequisite program.

Definition: <ProgramManager>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
Return Values
	<collect>		state
"	| programsRequiring |
		#SIFMod. "Protocol: <ProgramManager>."
	programsRequiring := Set new.
	self programs do: [ :tmpProgram |
		(tmpProgram isPrerequisiteProgram: aProgramDefinitionSet) ifTrue: [
			programsRequiring add: tmpProgram	
	]	].
	^programsRequiring! !

!ProgramManager methodsFor: 'accessing' stamp: 'RAH 9/8/1999 14:48'!
renameProgram: aProgramDefinitionSet toName: newNameString
	"Rename the program, aProgramDefinitionSet, to new name, newNameString.

Definition: <ProgramManager>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	captured
	newNameString			<readableString>		captured
"		#SIFMod. "Protocol: <ProgramManager>."
	self programs remove: aProgramDefinitionSet.
	aProgramDefinitionSet programName: newNameString.
	self addProgram: aProgramDefinitionSet.
	self triggerProgramsLoadedChanged.! !

!ProgramManager methodsFor: 'initializing' stamp: 'RAH 9/20/1999 05:31'!
initialize
	"Private - Initialize the receivers instance variables and connect to a number of system triggers to ensure that the receivers programs remove references to any system objects which are removed."
	| system |
		#SIFMod.
	programs := self class defaultProgramCollection.
	processEvents := 0.

#todoSIFMod."I'm not sure this is a good idea.  For instance, when a user deletes a class in the class browser and I remove it from all program,  I fear the user may not have intended that result.  I'm not sure prompting before delete would help.
	self class environment
		when: #classAdded:
		send: #onSystemClassAdded:
		to: self.
	self class environment
		when: #classCategoryChanged:
		send: #onSystemClassUpdated:
		to: self.
	self class environment class
		when: #classRemoved:
		send: #onSystemClassRemoved:
		to: self.
	self class environment
		when: #classUpdated:
		send: #onSystemClassUpdated:
		to: self.

	system := SmalltalkSystem current.
	system
		when: #globalRemoved:
		send: #onSystemGlobalRemoved:
		to: self.
	system
		when: #globalRenamed:
		send: #onSystemGlobalRenamed:
		to: self.

	system
		when: #methodAdded:
		send: #onSystemMethodAdded:
		to: self.
	system
		when: #methodCategorized:
		send: #onSystemMethodUpdated:
		to: self.
	system
		when: #methodRemoved:
		send: #onSystemMethodRemoved:
		to: self.
"
! !

!ProgramManager methodsFor: 'exceptions' stamp: 'RAH 9/8/1999 14:28'!
errorAlreadyInstalledProgram: aProgramDefinitionSet
	"Private - Raise an error to inform that the program, aProgramDefinitionSet, is already installed in the receiver.

Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
"		#SIFMod.
	PGM duplicateProgramError signal: ('Program named ', aProgramDefinitionSet programName, ' is already installed.')! !

!ProgramManager methodsFor: 'exceptions' stamp: 'RAH 9/8/1999 08:27'!
errorFailedToFindProgramFileLocator: fileLocatorString
	"Private - Raise an error to inform that program file named, fileLocatorString, was not located.

Parameters
	fileLocatorString	<readableString>		uncaptured
"		#SIFMod.
	PGM failedToLoadProgramError signal: ('Failed to load program file: "', fileLocatorString, '".')! !

!ProgramManager methodsFor: 'exceptions' stamp: 'RAH 9/8/1999 14:31'!
errorUninstallProgram: aProgramDefinitionSet
	"Private - Raise an error to inform that the program, aProgramDefinitionSet, can not be uninstalled from the system as other programs held by the receiver require programs owned objects to continue working.

Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
"		#SIFMod.
	PGM programError signal: 'Program "',
		(aProgramDefinitionSet programName,
		'" can not be removed as (',
		aProgramDefinitionSet prerequisiteProgramNames,
		') programs require it.')! !

!ProgramManager methodsFor: 'exceptions' stamp: 'RAH 9/8/1999 08:33'!
errorUniqueProgramName: conflictNameString
	"Private - Raise an error to inform that programs in the collection of programs held by the receiver must have unique names as program named, conflictNameString, has the same name as an existing one.

Parameters
	conflictNameString	<readableString>		uncaptured
"		#SIFMod.
	PGM programError signal: 'A program named ', conflictNameString,
			' already exists. Program names must be unique.'! !

!ProgramManager methodsFor: 'removing' stamp: 'RAH 9/8/1999 14:23'!
basicUninstallUnloadProgram: aProgramDefinitionSet
	"Private - Answer a <ProgramDefinitionSet>, the program, aProgramDefinitionSet, after removing it from the collection of programs held bythe receiver.

	Note: The program will uninstall/unload itself from the system.
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
Return Values
	<ProgramDefinitionSet>		unspecified
"		#SIFMod.
	self beNotProcessingEvents.
	[	aProgramDefinitionSet uninstallUnload: [
			self programs remove: aProgramDefinitionSet.
			self triggerProgramsLoadedChanged.
		].
	] ensure: [
		self beProcessingEvents
	]! !

!ProgramManager methodsFor: 'removing' stamp: 'RAH 9/24/1999 07:00'!
clearProgramList
	"Clear the singleton receiver's list of programs.

Definition: <ProgramManager>
"	| nameList |
		#SIFMod. "Protocol: <ProgramManager>."
	self programs isEmpty ifFalse: [
 		nameList := String new.
		self programs
			do: [ :program | nameList := nameList, program programName]
			separatedBy: [nameList := nameList, ', '].
		(self portFunc promptYesNo: ('You are about to lose (', nameList, ').  Do it?')) ifTrue: [
			self initialize.
	]	]
! !

!ProgramManager methodsFor: 'removing' stamp: 'RAH 9/8/1999 14:54'!
uninstallUnloadProgram: program
	"Uninstall the program, aProgramDefinitionSet, by removing it from the receiver.

Definition: <ProgramManager>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	captured
"		#SIFMod. "Protocol: <ProgramManager>."
	(self canUninstall: program) ifFalse: [
		^self errorUninstallProgram: program
	].

	self beNotProcessingEvents.
	[self basicUninstallUnloadProgram: program
	] ensure: [
		self
			beProcessingEvents;
			triggerProgramsLoadedChanged
	].
! !

!ProgramManager methodsFor: 'events' stamp: 'RAH 9/8/1999 14:51'!
triggerProgramChanged: aProgramDefinitionSet
	"Inform any interested parties that the program, aProgramDefinitionSet, has changed.

Definition: <ProgramManager>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	captured
"		#SIFMod. "Protocol: <ProgramManager>."
	self isProcessingEvents ifTrue: [
#todoSIFMod."
		self trigger: #programChanged:
			with: aProgramDefinitionSet
"
	]! !

!ProgramManager methodsFor: 'events' stamp: 'RAH 9/8/1999 14:52'!
triggerProgramsLoadedChanged
	"Private - Inform any interested parties that the collection of programs held by the receiver has changed."
		#SIFMod.	"Chg SQ"
	self isProcessingEvents ifTrue: [
#todoSIFMod."
		self trigger: #programsLoadedChanged
"
	]! !

!ProgramManager methodsFor: 'testing' stamp: 'RAH 9/8/1999 14:25'!
canUninstall: aProgramDefinitionSet
	"Answer a <boolean>, true if the program, aProgramDefinitionSet, could be uninstalled from the collection of programs held by the receiver, false if not.

Definition: <ProgramManager>
	Note: A program can be uninstalled if none of the other programs held by the receiver have program as a prerequisite program.
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramManager>."
	self programs do: [ :tmpProgram |
		(tmpProgram isPrerequisiteProgram: aProgramDefinitionSet) ifTrue: [
			^false	
	]	].
	^true! !

!ProgramManager methodsFor: 'testing' stamp: 'RAH 9/8/1999 08:56'!
isProcessingEvents
	"Private - Answer a <boolean>, true if the receiver will act on triggers from the rest of the system that it is interested in, false if not.

Return Values
	<boolean>		unspecified
"		#SIFMod.
	^self processEvents = 0
! !

!ProgramManager methodsFor: 'operations' stamp: 'RAH 9/8/1999 07:23'!
changeProcessEventsBy: countChangeInteger
	"Private - Change the receivers count of the processEvents by countChangeInteger.
Parameters
	countChangeInteger	<integer>	uncaptured
Errors
	The process-events count becames corrupt.
"		#SIFMod.
	processEvents := self processEvents + countChangeInteger.
	processEvents < 0 ifTrue: [
		Notification signal: 'ProgramManager processEvents became corrupt, so reset it.'.
		processEvents := 0
	].! !

!ProgramManager methodsFor: 'operations' stamp: 'RAH 9/24/1999 07:03'!
requestProgramFileName: programFileLocator
	"Private - Answer a <readableString>, a prerequisite program file locator or an empty string if canceled.

Parameters
	programFileLocator	<readableString>		uncaptured
Return Values
	<readableString>		unspecified
"	| flExt flName |
		#SIFMod.
	self portFunc
		fileLocatorSplit: programFileLocator
		to: [ :flPathString :flNameString :flExtString |
			flName := flNameString.
			flExt := flExtString.
		].
	^self portFunc
		promptPath: ''
		fileName: flName
		extension: flExt
		fileType: SIF programFileDialogTypes
		title: 'Open prerequisite program: ', flName! !

!ProgramManager methodsFor: 'adding' stamp: 'RAH 9/8/1999 14:19'!
addProgram: aProgramDefinitionSet
	"Answer a <ProgramDefinitionSet>, the program, aProgramDefinitionSet, after adding it to the collection of programs held by the receiver.

Definition: <ProgramManager>
	Note: Each program must have a unique name.
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	captured
Return Values
	<ProgramDefinitionSet>		state
Errors
	A program with the same name already exists in the collection.
"		#SIFMod. "Protocol: <ProgramManager>."
	(self includesProgramNamed: aProgramDefinitionSet programName) ifTrue: [
		^self errorUniqueProgramName: aProgramDefinitionSet programName
	].
	self programs add: aProgramDefinitionSet.
	self triggerProgramsLoadedChanged.
	^aProgramDefinitionSet! !



!ProgramManager class methodsFor: 'instance creation' stamp: 'RAH 9/8/1999 15:19'!
current
	"Answer a <ProgramManager>, the singleton instance of the receiver.

Definition: <ProgramManager factory>
Return Values
	<ProgramManager>		state
"		#SIFMod. "Protocol: <ProgramManager factory>."
	^Current
! !

!ProgramManager class methodsFor: 'instance creation' stamp: 'RAH 9/20/1999 05:56'!
new
	"Invalid--the receiver is a singleton class.

Definition: <instantiator>
Refinement: <ProgramManager factory>
	Raise an exception as this is an inappropriate message since the receiver is a singleton class.
"		#SIFMod. "Protocol: <ProgramManager factory>."
	^self shouldNotImplement
! !

!ProgramManager class methodsFor: 'class hierarchy-removing' stamp: 'RAH 9/8/1999 15:32'!
removeFromSystem
	"Remove the receiver completely from the system.

Definition: <ProgramManager factory>
"		#SIFMod. "Protocol: <ProgramManager factory>."
	"Since this is a singleton class, first remove known instances."
	self current clearProgramList.
	Current := nil.

	super removeFromSystem.
! !

!ProgramManager class methodsFor: 'accessing' stamp: 'RAH 9/8/1999 15:31'!
publishedEventsOfInstances
	"Answer a <Set>, the Symbols that describe the published events triggered by instances of the receiver.

Definition: <ProgramManager factory>
Return Values
	<Set>	new
"		#SIFMod. "Protocol: <ProgramManager factory>."
	^super publishedEventsOfInstances
		add: #programsLoadedChanged;
		add: #programChanged:;
		yourself! !

!ProgramManager class methodsFor: 'default' stamp: 'RAH 9/8/1999 15:22'!
defaultProgramCollection
	"Private - Answer an <OrderedCollection>, the default new program collection."
		#SIFMod.
	^OrderedCollection new! !

!ProgramManager class methodsFor: 'initialize-release' stamp: 'RAH 9/14/1999 06:11'!
initialize
	"Class initialization.  Example:

		ProgramManager initialize

Definition: <ProgramManager factory>
"		#SIFMod. "Protocol: <ProgramManager factory>."
	"Create the singleton instance of the program manager:"
	Current := super new
		initialize.
! !


!ProgramReaderWriter commentStamp: 'RAH 9/28/1999 09:08' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.3 ProgramReaderWriter Protocols

	This section includes protocols that define the behavior of the ProgramReaderWriter classes.

	The graphs below shows the conformance relationships between the protocols defined in this section.

		     <Object>
			|
		-------------------------
		|			|
 	<ProgramReaderWriter>	<ProgramReaderWriter factory>
		|
	-------------------------
	|			|
<programReader>		<programWriter>


1.3.1 Protocol: <ProgramReaderWriter>

Conforms To
	<Object>

Description
	Provides the protocol for an abstract superclass that defines common behavior for readers and writers that handle the filing in and  out of a program in the ANSI Smalltalk Interchange Format (SIF) files.  A program is a collection of elements mostly corresponding to the program elements defined in the ANSI Smalltalk Standard v1 r9 (ANSI-1.9).  See the ProgramAbstractComponent Protocols for further information.

Instance Variables

	stream	<ReadStream>|<WriteStream>	A stream on SIF program (normally a file),
		| <nil> if none.

1.3.2 Protocol: <ProgramReaderWriter factory>

Conforms To
	<Object>

Description
	This protocol implements the behavior of creating a program reader or writer, and accessing defaults and constants.

Implementation:

	This implementation is intended to be a rudimentary facility to read and write portable code.  It is I hope a bridge to a declarative model Smalltalk development environment that I have no experience with.  It did allow me to file in/out code implementing the rest of the standard protocols.
	It assumes that a minor new SIF version/release will be implemented as a new reader and writer subclasses of previous ones with new behavior over-ridding the superseded.  Major new SIF version/release will be implemented as new reader and writer subclasses of this abstract superclass.

Line Termination:
	The way Squeak handles line termination seems problematic as it seems to use carriage return (Cr/dec. 13/hex. 0D), line feeds (Lf/dec. 10/hex. 0A), or any combination thereof with the odd vertical tab (Vt/dec. 11/hex. 0B)  thrown in.  To rectify this situation, this implementation assumes a Cr internally and the native platform convention externally.
	The native platform line termination conventions are as follows:

	DOS/Windows on x86		CrLf
	Unix				  Lf
	Mac				Cr

Enhancements:

1. Implementation Version:

	It may be useful to have an implementation version scheme of the SIF version.


2. Annotations:

a. Category:

	The SIF standard has no provision for multiple categories.  Some dialects allow classes or methods to have multiple categories.  It is problematic which is the most appropriate.  One solution I explored was to annotate with a key of category and a value the most appropriate category then add another annotation with a key of categoryList and a value the additional categories.

b. Emphasis:

	The SIF standard has no provision for author, comment, or copyright with emphasis as some dialects do.  One solution I explored was to use a combination of a string annotation plus an emphasis annotation.  For example an author with emphasis would be filed out as an annotation with a key of author and a value the author string then another annotation with a key of authorEmphasis and a value the a store string of say a run array indicating the emphasis attributes.  A platform independent of specifying emphasis should I think be used, possibly HTML.


c. Dialect Missing Annotation:

	It would be nice to add the missing SIF standard annotations to all dialects.
!

!ProgramReaderWriter methodsFor: 'accessing' stamp: 'RAH 9/9/1999 09:15'!
stream
	"Answer a <ReadStream> or a <WriteStream>, the receiver's stream.

Definition: <ProgramReaderWriter>
Return Values
	<ReadStream>		state
	<WriteStream>		state
"		#SIFMod. "Protocol: <ProgramReaderWriter>."
	^stream
! !

!ProgramReaderWriter methodsFor: 'accessing' stamp: 'RAH 9/9/1999 09:15'!
stream: readOrWriteStream
	"Set the receiver's stream to the <ReadStream> or <WriteStream>, readOrWriteStream.

Definition: <ProgramReaderWriter>
Parameters
	readOrWriteStream	<ReadStream> | <WriteStream>	captured
"		#SIFMod. "Protocol: <ProgramReaderWriter>."
	stream := readOrWriteStream
! !


!ProgramReaderV1r0 commentStamp: 'RAH 9/28/1999 09:09' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.3.3 Protocol: <programReader>

Conforms To
	<ProgramReaderWriter>

Description
	Class ProgramReaderV1r0 implements the behavior of reading ANSI Smalltalk Interchange Format (SIF) program element specifications from a stream for version 1 release 0 SIF.  This implementation has SIF enhancements to allow collections of program element specifications in component sets and the nesting of component sets in a tree structure.  Set specifications are embedded in comments so as not to break existing implementations.
	A corresponding <ElementDefinition> subclass instance is created for each program element specification or comment read and a corresponding <ProgramComponentSet> subclass instance is created for each component set read.
	Elements and sets are collected in a program (instance of <ProgramDefinitionSet>) for each stream.  Loading a program only reads the program's components but does not create the Smalltalk objects specified by the components.  This does not assume that prerequisite programs are loaded.

Instance Variables

	chunkString		<readableString>	The current chunk string read from the receiver's stream.

	definedClassNames	<Set>			The list of <readableString> class names defined by the current program.

	fileLocator		<readableString>	The receiver's stream file drive, path, name, and extension.  It may be a dummy file locator if the stream is not a file.

	programComponentSetStack Stack			The receiver's stack of <ProgramComponentSet> sets.

Smalltalk Interchange Format:

	The following is a simplified representation of the ANSI Smalltalk Interchange Format specification (from the proposed standard document):

<interchangeFile>
	Smalltalk interchangeVersion: '1.0'!!
	( <interchangeElement> <annotation>* ) +

<interchangeElement>
	<classDefinition> |
	<classInitialization> |
	<globalDefinition> |
	<globalValueInitialization> |
	<poolDefinition> |
	<poolVariableDefinition> |
	<poolValueInitialization> |
	<methodDefinition> |
	<classMethodDefinition> |
	<programInitialization> |
	comment <elementSeparator>


<classDefinition>
	Class named: 'X' superclass: 'Y'
	indexedInstanceVariables: #'none'
	instanceVariableNames: 'd' classVariableNames: 'e'
	sharedPools: 'Z' classInstanceVariableNames: 'g'!!

<classInitialization>
	X initializer!!
	some code!!

<globalVariableDefinition>
	Global variable: 'Z'!!

<globalConstantDefinition>
	Global constant: 'Z'!!

<globalValueInitialization>
	Z initializer!!
	some code!!

<poolDefinition>
	Pool named: 'Z'!!

<poolValueDefinition>
	Z variable: 'V'!!

<poolConstantDefinition>
	Z constant: 'V'!!

<poolValueInitialization>
	Z initializerFor: 'W'!!
	some code!!

<methodDefinition>
	X method!!
	some code!!

<classMethodDefinition>
	X classMethod!!
	some code!!

<programInitialization>
	Global initializer!!
	some code!!

"comment"


Implementation:

Loading A Program:

	A program is read in two parts in separate invocations of a reader so that prerequisite programs can be determined and loaded before those requiring them.
	First the new program component is requested with the <programReader> newProgramFromDescription message.  This answers a <ProgramDefinitionSet>, a new program without components.  If a program description comment is found the program name and the list of prerequisite programs names are set from it, else a temporary name (the file name or a dummy name) is set.  The program component is also annotated with any annotations following the description comment.
	Next the program's components are requested with the <programReader> loadComponentsIntoProgram: message with the new program component as a parameter.  This reads all the annotations element specifications, comments, and component sets embedded in comments (and any following annotations) and creates their corresponding program components.
	The mechanism for capturing the element specification information seems clumsy but is the best I could think of that did not involve creating and destroying global, class, and pool Smalltalk objects while reading the file (I tried this, or creating a parser to read it (and this).  It also eliminates a number of ambiguous situations and undefined object errors.
	The  read chunk/create component loop tests as follows:

	is the chunk a comment then
	   has it an embedded macro directive then
	      evaluate it as a directive message to the reader
	   else
	    create the comment component
	else
	   evaluate it as an element specification message to the reader

A directive message is evaluate with 'SIF currentReader ' appended to it.  For example:

	SIF currentReader newClassAllSet: 'AClass'

An element specification message is evaluate with 'SIF currentReader specOf: #' appended to it,  For example:

	SIF currentReader specOf: #Class named: 'X' superclass: 'Y'
		indexedInstanceVariables: #'none'
		instanceVariableNames: 'd' classVariableNames: 'e'
		sharedPools: 'Z' classInstanceVariableNames: 'g'

This require the globally accessible ProgramReaderWriter class instance variable currentReader be set to the instance of the current reader prior to evaluation.  I set it to nil immediately after evaluation.  This really seems clumsy and likely to break.
	It does have the advantage of keeping the processing isolated to the program reader except for two element specification message:

	<methodDefinition> and	<classMethodDefinition>
		X method!!		X classMethod!!
		some code!!		some code!!

Resulting in the evaluation of:

	SIF currentReader specOf: #AClass method
	SIF currentReader specOf: #AClass classMethod

I implemented Symbol>>#method and  Symbol>>#classMethod methods to return new <MethodDefinitionElement> components to the <programReader> specOf: message.
	After I have sorted out what is what in the various specOf: . . . methods, read any additional chunks required by an element specification, read any following annotations, and created its corresponding program component, I add the component to the current set.


Global Or Class Initializer Unknown:

	I couldn't figure out how to detect if 'X initializer!!' was a <classInitialization> or <globalValueInitialization> element other than keeping a list of class definitions and check 'X' against the list.  If 'X' was previously defined as a class this must be a <classInitialization> else a <globalValueInitialization>.  This seems kind of clumsy.


Program Description Enhancement:

	I implemented a kind of program description with the  <programAndPrerequisites> directive to indicate the program name and the prerequisite program names for the program being read.
	Note that a program with this directive will consider the annotations following the comment in which it is imbedded to apply to the program component as no comment component is created.


Macro Facility Enhancement:

	I added a rudimentary macro facility enhancement to the ANSI SIF specification embedded in comments so as not to break existing implementations.  A macro format is:

	doubleQuote dollarSign whiteSpace <directive> whiteSpace doubleQuote

It is currently only used for a <programAndPrerequisites> directive to add program component sets.  The format is:

<programAndPrerequisites>
	programName: 'ProgramName'
	prerequisiteProgramNames: 'PrereqPgmName1 PrereqPgmName2'

<setDefinition>
	<setDirective>
	(( <interchangeElement> <annotation>* )*  | <setDefinition>* )
	<setEnd>

<setDirective>
	<classAllSet> |
	<globalAllSet> |
	<groupingSet> |
	<poolAllSet>

<classAllSet>
	newClassAllSet: 'AClass'

<globalAllSet>
	newGlobalAllSet: 'AGlobal'

<groupingSet>
	newGroupingSet: 'AGroup'

<poolAllSet>
	newPoolAllSet: 'APool'

<setEnd>
	setEnd: 'ASetName'


I also have been considering the following macro directives:

	define: 'id'

	define: 'value' as: '$$100.50'

	test: 'id >= aNum' ifTrue: [
		. . .
	] ifFalse: [
		. . .
	]

	undefine: 'id'
!

!ProgramReaderV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/19/1999 07:43'!
loadComponentsIntoProgram: aProgramDefinitionSet
	"Load all the ANSI SIF program components of aProgramDefinitionSet from the receiver's stream.
	Note: This does NOT assume that prerequisite programs are loaded.

Definition: <programReader>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	captured
"		#SIFMod. "Protocol: <programReader>."
	self programComponentSetStack push: aProgramDefinitionSet.

	"Bypass SIF version."
	self nextSIFChunk.
	aProgramDefinitionSet hasDescription ifTrue: [
		"Bypass program description."
		self nextElementChunk.
	].

	[self chunkString isEmpty] whileFalse: [
		self isCommentChunk ifTrue: [
			self isProgramDirectiveChunk ifTrue: [
				self evaluateProgramDirective.
			] ifFalse: [
				self newProgramComment.
			].
		] ifFalse: [
			self evaluateElementSpec.
		].
		"Next chunk is fetched as part of the above messages
		checking for following annotations."
	].
! !

!ProgramReaderV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/24/1999 07:03'!
newProgramFromDescription
	"Answer a <ProgramDefinitionSet>, a new program without components from the receiver's ANSI SIF program stream.  Set the program name and the list of prerequisite programs names from the program description comment chunk if found, else a temporary name from the file name.  Annotate the program component with any following description comment annotations read from the stream.
	Note: This does NOT assume that prerequisite programs are loaded.

Definition: <programReader>
"	| program tmpName |
		#SIFMod. "Protocol: <programReader>."
	self portFunc
		fileLocatorSplit: self fileLocator
		to: [ :p :flNameString :e | tmpName := flNameString. ].
	program := PGM newProgramNamed: tmpName.
	self programComponentSetStack push: program.

	self nextSIFChunk.
	"Check for program and prerequisite names program directive."
	self isCommentChunk ifTrue: [
		self isProgramDirectiveChunk ifTrue: [
			self evaluateProgramDirective.
	]	].

	^program
! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/20/1999 13:32'!
evaluateElementSpec
	"Private - Evaluate the ANSI SIF program element specification in the current chunk appended to 'SIF currentReader specOf: #'.

	Note: This mechanism for capturing the element specification information seems clumsy but the best I could think of that did not involve creating and destroying global, class, and pool Smalltalk objects while reading the file, or creating a parser.  It eliminated a number of ambiguous situations and undefined object errors.
"		#SIFMod.
	[	SIF currentReader: self.
		^self evaluate: ('SIF currentReader specOf: #', self chunkString trimBlanks).
	] ensure: [
		SIF currentReader: nil.
	].
! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/20/1999 13:35'!
evaluateProgramDirective
	"Private - Evaluate the program directive embedded in the current chunk (a comment).  If the current chunk is not a comment then get the next chunk.

	Note: This is intended to support a macro facility enhancement to the ANSI SIF specification embedded in comments so as not to break existing implementations.  It is currently only used to add program component sets.
"	| directiveString tempString |
		#SIFMod.
	"$ directive. "
	tempString := self chunkString trimBlanks.
	tempString size < 4 ifTrue: [
			self currentSet errorMessage:  ('Invalid program directive : "',
					self chunkString, '".').
			"Move on to next component."
			^self nextElementChunk.
	].

	directiveString := tempString
		copyFrom: 3
		to: (tempString size - 1).

	[ 	SIF currentReader: self.
		self evaluate: ('SIF currentReader ', directiveString).
	] ensure: [
		SIF currentReader: nil.
	].! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 10:06'!
newClassAllSet: classNameString
	"Private - Create a new class all component set for class named classNameString, to hold the following class components read from the stream up to the next set end.  Annotate the set component with any annotations read from the stream.  Push the set component on the stack of sets.

Parameters
	classNameString	<readableString>		captured
"	| annotationDictionary component |
		#SIFMod.
	component := PGM newClassAllSet: classNameString.
	annotationDictionary := self loadAnnotations.
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.

	self programComponentSetStack push: component! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 13:05'!
newGroupingSet: groupingNameString
	"Private - Create a new grouping component set for grouping named groupingNameString, to hold the following components read from the stream up to the next set end.  Annotate the set component with any annotations read from the stream.  Push the set component on the stack of sets.

Parameters
	groupingNameString		<readableString>		captured
"	| annotationDictionary component |
		#SIFMod.
	component := PGM newGroupingSet: groupingNameString.
	annotationDictionary := self loadAnnotations.
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.

	self programComponentSetStack push: component! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 13:13'!
newProgramComment
	"Private - Create a new program comment component from the current chunk.  Annotate the component with any annotations read from the stream.  Add the component to the current set.
"	| annotationDictionary commentString component |
		#SIFMod.
	commentString := self chunkString.

	annotationDictionary := self loadAnnotations.

	component := PGM newProgramComment: commentString.
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/24/1999 07:04'!
programName: programNameString
prerequisiteProgramNames: prerequisiteNamesString
	"Private - Set program name to programNameString and its prerequisite program names to those in prerequisiteNamesString.  Annotate the program component with any annotations read from the stream.
	Note: A program with this program and prerequisite names program directive will consider the annotations following the comment in which it is imbedded to apply to the program component as no comment component is created.

Parameters
	programNameString		<readableString>		captured
	prerequisiteNamesString	<readableString>		captured
"	| annotationDictionary program |
		#SIFMod.
	program := self currentSet.
	programNameString trimBlanks isEmpty ifTrue: [
		self portFunc promptInform: 'An empty program name in directive encountered - using ', program programName.
	].

	program programName: programNameString.
	(prerequisiteNamesString subStrings) do: [ :programName |
		program addPrerequisiteProgramName: programName
	].
	program hasDescription: true.

	annotationDictionary := self loadAnnotations.
	self annotateComponent: program
		with: annotationDictionary.
! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/20/1999 13:59'!
setEnd: setNameString
	"Private - Pop the current <ProgramComponentSets> set named <readableString> setNameString off the receiver's stack of sets, and get the next SIF chunk from the receiver's stream.

Parameters
	setNameString	<readableString>		uncaptured
Errors
	The current set name does not match the set end name setNameString indicating the set and set end pair is no longer in synchronization.  Most likely the reader is confused and further processing is discontinued.
"	| component |
		#SIFMod.
	component := self programComponentSetStack pop.
	component setName = setNameString ifFalse: [
		self currentSet fatalMessage: 'Set end: "',
			setNameString,
			'" does not match current set: "',
			component setName,
			'" - stopping load.'.
		self stream setToEnd.
	].

	self nextSIFChunk.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:01'!
specOf: anObject
	"Private - Create or complete the new component from the next ANSI SIF program element specification read and evaluated from the stream.  This includes getting the code from the following chunk.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF elements <methodDefinition>, <classMethodDefinition>, <programInitialization>, <classInitialization>, and <globalValueInitialization>.

Parameters
	anObject	<MethodDefinitionElement>|<symbol>	captured
Errors
	Couldn't find the method selector in the method code from the following chunk.
 "	| annotationDictionary component methodSelector objectName sourceString |
		#SIFMod.
	self nextSIFChunk.
	sourceString := self chunkString.
	annotationDictionary := self loadAnnotations.

	(anObject isKindOf: PGM methodDefinitionElement) ifTrue: [
	"<methodDefinition> or	<classMethodDefinition>
		X method!!				X classMethod!!
		some code!!			some code!!"
		component := anObject.
		methodSelector := SIF methodSelector: sourceString.
		methodSelector isNil ifTrue: [
			component errorMessage: ('Couldn''t find the method selector in "',
				sourceString, '".').
			methodSelector := '?not found?'.
		].
		component methodName: methodSelector.
		component source: sourceString.
	] ifFalse: [
		objectName := anObject asString.

		(objectName = 'Global' ) ifTrue: [
			"<programInitialization>
				Global initializer!!
				some code!!"
			component := PGM newProgramInitializer: sourceString.
		] ifFalse: [
			(self definedClassNames includes: objectName) ifTrue: [
				"No way to tell for certain if this is a class or global
				initializer unless it was previously defined as a class.
				<classInitialization>
					X initializer!!
					some code!!"
				component := PGM
					newClass: objectName
					initializer: sourceString
			] ifFalse: [
				"<globalValueInitialization>
					Z initializer!!
					some code!!"
				component := PGM
					newGlobal: objectName
					initializer: sourceString
	]	]	].

	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:02'!
specOf: aSymbol constant: constantNameString
	"Private - Create a new component from the next ANSI SIF program element specification read and evaluated from the stream.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF elements <globalConstantDefinition> and <poolConstantDefinition>.

Parameters
	aSymbol				<symbol>			unspecified
	constantNameString	<readableString>	captured
"	| annotationDictionary component objectName |
		#SIFMod.
	annotationDictionary := self loadAnnotations.

	objectName := aSymbol asString.

	objectName = 'Global' ifTrue: [
		"<globalConstantDefinition>
			Global constant: 'Z'!!"
		component := PGM
			newGlobalDefinitionOf: constantNameString
			constant: true.
	] ifFalse: [
		"<poolConstantDefinition>
			Z constant: 'V'!!"
		component := PGM
			newPoolValueDefinitionOf: constantNameString
			in: objectName
			constant: true.
	].
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:08'!
specOf: aSymbol initializerFor: variableNameString
	"Private - Create a new component from the next ANSI SIF program element specification read and evaluated from the stream.  This includes getting the code from the following chunk.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF element <poolValueInitialization>.

Parameters
	aSymbol				<symbol>			captured
	variableNameString	<readableString>	captured
"	| annotationDictionary component sourceString |
		#SIFMod.
	"<poolValueInitialization>
		Z initializerFor: 'W'!!
		some code!!"
	self nextSIFChunk.
	sourceString := self chunkString.
	annotationDictionary := self loadAnnotations.

	component := PGM
		newPoolValue: variableNameString
		in: aSymbol asString
		initializer: sourceString.
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:10'!
specOf: aSymbol named: poolNameString
	"Private - Create a new component from the next ANSI SIF program element specification read and evaluated from the stream.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF element <poolDefinition>.

Parameters
	aSymbol				<symbol>			uncaptured
	poolNameString	<readableString>	captured
"	| annotationDictionary component |
		#SIFMod.
	"<poolDefinition> 
		Pool named: 'Z'!!"
	annotationDictionary := self loadAnnotations.

	component := PGM newPoolDefinitionOf: poolNameString.
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:21'!
specOf: aSymbol
named: classNameString
superclass: superclassNameString
indexedInstanceVariables: indexableInstVarType
instanceVariableNames: instanceVariableList
classVariableNames: classVariableList
sharedPools: poolList
classInstanceVariableNames: classInstVariableList
	"Private - Create a new component from the next ANSI SIF program element specification read and evaluated from the stream.  Add the class name to the list of defined class names.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF element <classDefinition>.

Parameters
	aSymbol				<symbol>		uncaptured
	classNameString			<readableString>	captured
	superclassNameString	<readableString>	captured
	indexableInstVarType	<symbol>		captured
	instanceVariableList		<readableString>	captured
	classVariableList		<readableString>	captured
	poolList					<readableString>	captured
	classInstVariableList		<readableString>	captured
"	| annotationDictionary component |
		#SIFMod.
	"<classDefinition>
		Class named: 'X' superclass: 'Y' indexedInstanceVariables: #'none'
		instanceVariableNames: 'd' classVariableNames: 'e' sharedPools: 'Z'
		classInstanceVariableNames: 'g'!!"
	annotationDictionary := self loadAnnotations.

	component := PGM
		newClassDefinitionOf: classNameString
		superclass: superclassNameString
		indexedInstanceVariables: indexableInstVarType
		instanceVariableNames: instanceVariableList
		classVariableNames: classVariableList
		sharedPools: poolList
		classInstanceVariableNames: classInstVariableList.
	self	annotateComponent: component
		with: annotationDictionary.

	self definedClassNames add:  classNameString.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'program-element specifying' stamp: 'RAH 9/10/1999 15:24'!
specOf: aSymbol variable: variableNameString
	"Private - Create a new component from the next ANSI SIF program element specification read and evaluated from the stream.  Annotate the component with any following annotations read from the stream.  Add the component to the current set.
	Note: This method handles SIF element <globalVariableDefinition> and <poolValueDefinition>.

Parameters
	aSymbol				<symbol>			uncaptured
	variableNameString		<readableString>		captured
"	| annotationDictionary component objectName |
		#SIFMod.
	annotationDictionary := self loadAnnotations.

	objectName := aSymbol asString.

	objectName = 'Global' ifTrue: [
		"<globalVariableDefinition>
			Global variable: 'Z'!!"
		component := PGM
			newGlobalDefinitionOf: variableNameString
			constant: false.
	] ifFalse: [
		"<poolValueDefinition>
			Z variable: 'V'!!"
		component := PGM
			newPoolValueDefinitionOf: variableNameString
			in: objectName
			constant: false.
	].
	self	annotateComponent: component
		with: annotationDictionary.

	self currentSet addComponent: component.! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/10/1999 08:15'!
chunkString
	"Private - Answer a <readableString>, the receiver's code chunk string.  It may be empty.

Return Values
	<readableString>		state
"		#SIFMod.
	^chunkString! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/20/1999 13:03'!
chunkString: aString
	"Private - Answer a <readableString>, the receiver's code chunk string after setting chunkString to it.  It may be empty.

Parameters
	aString	<readableString>		captured
Return Values
	<readableString>		state
"		#SIFMod.
	^chunkString := aString! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/10/1999 08:23'!
currentSet
	"Private - Answer a <ProgramComponentSet>, the receiver's current set to add components read.

Return Values
	<ProgramComponentSet>		state
"		#SIFMod.
	^programComponentSetStack top! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/20/1999 13:06'!
definedClassNames
	"Private - Answer a <Set>, the list of <readableString> class names defined by this program.

Return Values
	<Set>		state
"		#SIFMod.
#todoSIFMod."This is used to determine if an element refers to a class or global.  It should be changed to ask the program being read, then ask the program manager to ask all the other programs."
	^definedClassNames
! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/10/1999 09:32'!
fileLocator
	"Private - Answer a <readableString>, the receiver's stream file drive, path, name, and extension.  It may be a dummy file locator.

Return Values
	<readableString>		state
"		#SIFMod.
	^fileLocator! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/20/1999 13:37'!
fileLocator: aString
	"Private - Answer a <readableString>, the receiver's stream file drive, path, name, and extension after setting fileLocator.  It may be a dummy file locator.

Parameters
	aString	<readableString>		captured
Return Values
	<readableString>		state
"		#SIFMod.
	^fileLocator:= aString! !

!ProgramReaderV1r0 methodsFor: 'accessing' stamp: 'RAH 9/10/1999 13:54'!
programComponentSetStack
	"Private - Answer a Stack, the receiver's stack of <ProgramComponentSets>."
		#SIFMod.
	^programComponentSetStack! !

!ProgramReaderV1r0 methodsFor: 'private' stamp: 'RAH 9/10/1999 05:55'!
annotateComponent: aComponent with: annotationDictionary 
	"Private - Annotate the <ProgramAbstractComponent>, aComponent, with each key and value in the <Dictionary>, annotationDictionary.

Parameters
	aComponent			<ProgramAbstractComponent>	uncaptured
	annotationDictionary	<Dictionary>			?uncaptured?
"		#SIFMod.
	annotationDictionary keysAndValuesDo: [ :aKey :aValue |
		aComponent
			annotationAt: aKey
			put: aValue
	].! !

!ProgramReaderV1r0 methodsFor: 'private' stamp: 'RAH 9/20/1999 13:09'!
evaluate: codeString
	"Private - Answer an <Object>, the Smalltalk object created by evaluating the Smalltalk code <readableString>, codeString.  Normally the object returned is not used unless it is an annotation for one ANSI SIF program elements read.

Parameters
	codeString		<readableString>	uncaptured
Return Values
	<Object>		state
"		#SIFMod.
	[ ^Compiler
			evaluate: codeString
			logged: false
	] 	on: PGM compilerNotification do: [ :except |
		self currentSet errorMessage:  ('Error evaluating component : "',
					codeString, '" - ', except text).
		"Move on to next component."
		self nextElementChunk.
		^except return
	].
! !

!ProgramReaderV1r0 methodsFor: 'private' stamp: 'RAH 9/10/1999 09:53'!
loadAnnotations
	"Private - Answer a <Dictionary>, the annotations for one ANSI SIF program element read and evaluated from the stream.

Return Values
	<Dictionary>		new
"	| annotations anAssociation |
		#SIFMod.
	annotations := PGM defaultAnnotationCollection.
	self nextSIFChunk.
	[self isAnnotationChunk] whileTrue: [
		anAssociation := self evaluate: self chunkString.
		(anAssociation isMemberOf: Association) ifTrue: [
			annotations add: anAssociation.
		].
		self nextSIFChunk.
	].
	^annotations! !

!ProgramReaderV1r0 methodsFor: 'private' stamp: 'RAH 9/20/1999 13:53'!
nextElementChunk
	"Private - Get chunks from the receiver's stream until a chunk not an annotation is found.
	Note: This to re-synchronize the read loop leaving current chunk containing an element specification in case of an error."
		#SIFMod. 
	self nextSIFChunk.
	[self isAnnotationChunk] whileTrue: [
		self nextSIFChunk.
	].
! !

!ProgramReaderV1r0 methodsFor: 'private' stamp: 'RAH 9/20/1999 13:55'!
nextSIFChunk
	"Private - Set receiver's current chunk to the characters up to the next chunk marker from the receiver's ANSI SIF program stream, un-doubling embedded chunk markers.  Trailing line termination is skipped (Dialect Specific)."
	| chunkTerminator chunkStream lineTermedStream |
		#SIFMod.
	chunkTerminator := $!!.
	"String concatenation is more efficient here because most
		chunks do not include embedded chunk markers."
	self chunkString: (self stream upTo: chunkTerminator).
	[ self stream atEnd or: [(self stream peekFor: chunkTerminator) not ]
	] whileFalse: [
		self chunkString: (self chunkString,
			(String with: chunkTerminator),
			(self stream upTo: chunkTerminator))
	].
	self stream skipLineTermination.

	self chunkString trimBlanks isEmpty ifTrue: [
		self chunkString: String new
	].

	"Fix chunk line termination to be only cr."
	chunkStream := ReadStream on: self chunkString.	"Chged SQ"
	lineTermedStream := WriteStream on: (String  new: 100).
	[ chunkStream atEnd] whileFalse: [
		lineTermedStream
			nextPutAll: chunkStream nextLine; 
			cr.
	].
	self chunkString: lineTermedStream contents.


! !

!ProgramReaderV1r0 methodsFor: 'testing' stamp: 'RAH 9/10/1999 09:54'!
isAnnotationChunk
	"Private - Answer a <boolean>, true if the receiver's current code chunk string is an ANSI SIF program element specification annotation, false if not.

Return Values
	<boolean>		unspecified
"	| tempString |
		#SIFMod.
	tempString := self chunkString trimBlanks.
	tempString isEmpty ifFalse: [
		tempString size >= 10 ifTrue: [
			(tempString
				copyFrom: 1
				to: 10
			) = 'Annotation' ifTrue: [
				^true
	]	]	].
	^false! !

!ProgramReaderV1r0 methodsFor: 'testing' stamp: 'RAH 9/12/1999 10:50'!
isCommentChunk
	"Private - Answer a <boolean>, true if the receiver's current code chunk string is an ANSI SIF program comment, false if not.

Return Values
	<boolean>		unspecified
"	| tempString |
		#SIFMod.
	tempString := self chunkString trimBlanks.
	tempString isEmpty ifFalse: [
		(tempString at: 1) = $" ifTrue: [
			^true
	]	].
	^false
! !

!ProgramReaderV1r0 methodsFor: 'testing' stamp: 'RAH 9/12/1999 10:51'!
isProgramDirectiveChunk
	"Private - Answer a <boolean>, true if the receiver's current code chunk string is a program directive, false if not.

Return Values
	<boolean>		unspecified
"	| ndx |
		#SIFMod.
	ndx := self chunkString indexOf: $".
	ndx > 0 ifTrue: [
		ndx < self chunkString size ifTrue: [
			(self chunkString at: (ndx + 1)) = SIF directiveInd ifTrue: [
				^true
	]	]	].
	^false
! !

!ProgramReaderV1r0 methodsFor: 'initializing' stamp: 'RAH 9/10/1999 09:45'!
initialize
	"Private - Initialize the instance."
		#SIFMod.
	definedClassNames := Set new.
	"May be a dummy file locator, if not a file stream."
	self fileLocator: (self class fileLocatorOf: self stream).
	programComponentSetStack := Stack new.
	^self! !


!ProgramReaderWriter class methodsFor: 'private' stamp: 'RAH 9/9/1999 09:38'!
fileLocatorOf: aStream
	"Private - Answer a <readableString>, if aStream is a FileStream then the file locator (dir., path, name and ext.), else a dummy locator."
		#SIFMod.
	(aStream isKindOf: FileStream) ifTrue: [
		^aStream name
	].
	^aStream class name asString, 'FileIn'! !

!ProgramReaderWriter class methodsFor: 'private' stamp: 'RAH 9/20/1999 06:20'!
programReaderClassForVersion: versionString
	"Private - Answer a <classDescription>, an appropriate program reader class for a SIF file with version <readableString>, versionString."
	| versionNumber versionReaderDict |
		#SIFMod.
#todoSIFMod."Implement this as a class variable maybe?"
	versionReaderDict := Dictionary new.
	versionReaderDict
		at: 1.0
		put: (Smalltalk at: #'ProgramReaderV1r0').
"
	versionNumber := Number fromString: versionString. Dolphin"
	versionNumber := Number readFrom: versionString.	"Chg SQ"
	^versionReaderDict
		at: versionNumber
		ifAbsent: [
			SIF sifVersionError signal: 'No SIF reader for version "',
				versionString, '".'.
		]! !

!ProgramReaderWriter class methodsFor: 'formatting' stamp: 'RAH 9/9/1999 09:41'!
formatAnnotationKey: keyString value: valueString
	"Private - Answer a <readableString>, an annotation in SIF format with the key the <readableString>, keyString, and the value the <readableString>, valueString."
	| aStream |
		#SIFMod.
	aStream := WriteStream on: (String new: 36).
	aStream nextPutAll: 'Annotation'; cr.

	aStream tab; nextPutAll: 'key: '.
	keyString asString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'value: '.
	valueString asString printOn: aStream.

	^aStream contents! !

!ProgramReaderWriter class methodsFor: 'formatting' stamp: 'RAH 9/9/1999 09:50'!
formatClassDefinitionOf: classNameString
superclass: superclassNameString
indexedInstanceVariables: typeSymbol
instanceVariableNames: instanceVariableNameString
classVariableNames: classVariableNameString
sharedPools: sharedPoolNameString
classInstanceVariableNames: classInstanceVariableNameString
	"Private - Answer a <readableString>, the class definition in SIF format of the class named <readableString>, classNameString, with the superclass named <readableString>, superclassNameString, the indexed instance variables type <symbol>, typeSymbol, the instance variables named <readableString>, instanceVariableNameString, the class variables named <readableString>, classVariableNameString, the pools named <readableString>, sharedPoolNameString, and the class instance variables named <readableString>, classInstanceVariableNameString."
	|  aStream |
		#SIFMod.
	aStream := WriteStream on: (String new:  200).

	aStream nextPutAll: 'Class named: '.
	classNameString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'superclass: '.
	superclassNameString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'indexedInstanceVariables: #'.
	typeSymbol asString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'instanceVariableNames: '.
	instanceVariableNameString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'classVariableNames: '.
	classVariableNameString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'sharedPools: '.
	sharedPoolNameString printOn: aStream.
	aStream cr.

	aStream tab; nextPutAll: 'classInstanceVariableNames: '.
	classInstanceVariableNameString printOn: aStream.
	aStream cr.

	^aStream contents! !

!ProgramReaderWriter class methodsFor: 'instance creation' stamp: 'RAH 9/9/1999 10:05'!
newReaderOn: readStream
	"Answer a <programReader>, a SIF reader that can read the SIF version text stream, readStream.

Definition: <ProgramReaderWriter factory>
Parameters
	readStream	<ReadStream>		captured
Return Values
	<programReader>		new
"	| versionString |
		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	versionString := self getSIFVersion: readStream.
	versionString isNil ifTrue: [
		^SIF sifVersionError signal: 'File has no SIF version.'.
	].
	^(self programReaderClassForVersion: versionString)
		newOn: readStream
! !

!ProgramReaderWriter class methodsFor: 'instance creation' stamp: 'RAH 9/9/1999 10:09'!
newWriterOn: aStream
	"Answer a <programWriter>, a SIF writer that can write a SIF version text stream, writeStream.

Definition: <ProgramReaderWriter factory>
Parameters
	writeStream	<WriteStream>		captured
Return Values
	<programWriter>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^(Smalltalk at: #'ProgramWriterV1r0') new
		stream: aStream;
		initialize! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/9/1999 09:20'!
authorKey
	"Answer a <readableString>, the ANSI SIF author annotation key.

Definition: <ProgramReaderWriter factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^'author' copy! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/9/1999 09:20'!
categoryKey
	"Answer a <readableString>, the ANSI SIF category annotation key.

Definition: <ProgramReaderWriter factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^'category' copy! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/9/1999 09:21'!
commentKey
	"Answer a <readableString>, the ANSI SIF comment annotation key.

Definition: <ProgramReaderWriter factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^'comment' copy! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/9/1999 09:21'!
copyrightKey
	"Answer a <readableString>, the ANSI SIF copyright annotation key.

Definition: <ProgramReaderWriter factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^'copyright' copy! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/25/1999 08:03'!
getSIFVersion: aStream
	"Private - Answer a <readableString>, if aStream begins with a SIF interchange version specification then the version, else <nil>."
	| tokenList |
		#SIFMod.
	"'Smalltalk' 'interchangeVersion:' <versionId> <elementSeparator>
				<versionId> ::= quotedString"
	tokenList := (aStream upTo: $!!) subStrings.
	tokenList size = 3 ifTrue: [
		(((tokenList at: 1)  = 'Smalltalk')
		and: [(tokenList at: 2)  = 'interchangeVersion:']) ifTrue: [
			(tokenList at: 3) size > 2 ifTrue: [
				^(tokenList at: 3)		"Drop quote marks:"
					copyFrom: 2		"Chg SQ."
					to: ((tokenList at: 3) size - 1).
	]	]	].
	aStream reset.
	^nil! !

!ProgramReaderWriter class methodsFor: 'constants-annotation keys' stamp: 'RAH 9/23/1999 06:52'!
methodSelector: methodDefinitionString 
	"Private - Answer a <readableString>, the method selector extracted from the instance or class method definition methodDefinitionString if found, or <nil> if not found.
	Note: This is not bullet proof.

Parameters
	methodDefinitionString	<readableString>	uncaptured
Return Values
	<readableString>		new
	<nil>				state
"	| sourceStream selectorStream | 
		#SIFMod.
	sourceStream := ReadStream on: methodDefinitionString trimBlanks.
	selectorStream := WriteStream on: (String  new: 20).	"Chged SQ"

	sourceStream skipSeparators.
	sourceStream peek isBinarySelectorChar ifTrue: [		"Binary selector."
		[(sourceStream peek) isBinarySelectorChar] whileTrue: [
			selectorStream nextPut: sourceStream next.
		].
		^selectorStream contents
	].
		"Find initial unary or keyword selector token."
	[sourceStream peek isUnaryOrKeywordSelectorChar] whileTrue: [
			selectorStream nextPut: sourceStream next.
	].

	sourceStream peek = $: ifFalse: [		"Unary selector."
		^selectorStream contents
	].

	selectorStream nextPut: sourceStream next.	"Add $:."
	sourceStream nextToken.					"Bypass parm."

		"Extract remaining keyword selector tokens."
	[	sourceStream skipSeparators.
		[sourceStream peek isUnaryOrKeywordSelectorChar] whileTrue: [
				selectorStream nextPut: sourceStream next.
		].
		sourceStream peek = $: ifFalse: [	"Found keyword selector."
			^selectorStream contents
		].
		selectorStream nextPut: sourceStream next.	"Add $:."
		sourceStream nextToken isEmpty not.			"Bypass parm."
	] whileTrue.
	^nil! !

!ProgramReaderWriter class methodsFor: 'constants-SIF ReaderWriter version' stamp: 'RAH 9/9/1999 10:18'!
sifReaderWriterVersion
	"Private - Answer a new <readableString>, the current SIF ReaderWriter version."
		#SIFMod.
	^'1.0' copy! !

!ProgramReaderWriter class methodsFor: 'initializing' stamp: 'RAH 9/9/1999 09:55'!
initialize
	"Class initialization.  Example:

		ProgramReaderWriter initialize

Definition: <ProgramReaderWriter factory>
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	"Initialize the global SIF to be the receiver:"
	SIF := self
! !

!ProgramReaderWriter class methodsFor: 'constants-exceptions' stamp: 'RAH 9/9/1999 10:22'!
sifVersionError
	"Answer an <Error class>, a no-SIF-version error class object.

Definition: <ProgramReaderWriter factory>
Return Values
	<Error class>		state
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^Smalltalk at: #'SIFVersionError'! !

!ProgramReaderWriter class methodsFor: 'accessing' stamp: 'RAH 9/9/1999 09:23'!
currentReader
	"Private - Answer a <programReader>, the singleton program reader."
		#SIFMod.
	^currentReader! !

!ProgramReaderWriter class methodsFor: 'accessing' stamp: 'RAH 9/20/1999 06:12'!
currentReader: aProgramReader
	"Private - Set the singleton current program reader to the <programReader>, aProgramReader, so as to be globally accessible."
		#SIFMod.
	currentReader := aProgramReader! !

!ProgramReaderWriter class methodsFor: 'constants-directive' stamp: 'RAH 9/9/1999 09:34'!
directiveCommentStart
	"Private - Answer a <readableString>, the comment start directive."
		#SIFMod.
	^String with: $" with: SIF directiveInd with: Character space! !

!ProgramReaderWriter class methodsFor: 'constants-directive' stamp: 'RAH 9/20/1999 06:13'!
directiveInd
	"Private - Answer a <Character>, the directive indicator."
		#SIFMod.
	^$$! !

!ProgramReaderWriter class methodsFor: 'constants' stamp: 'RAH 9/9/1999 10:23'!
sifVersionTag
	"Answer a <readableString>, a signaled no SIF version error tag.

Definition: <ProgramAbstractComponent factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramAbstractComponent factory>."
	^'SIFVersion' copy! !

!ProgramReaderWriter class methodsFor: 'constants-SIF file types' stamp: 'RAH 9/10/1999 06:51'!
programFileDialogTypes
	"Answer an <Object>, the ANSI SIF program file dialog types suitable for adding to a file dialog.

Definition: <ProgramReaderWriter factory>
Return Values
	<Object>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^self programFileLocatorExtension.
! !

!ProgramReaderWriter class methodsFor: 'constants-SIF file types' stamp: 'RAH 9/27/1999 16:02'!
programFileLocatorExtension
	"Answer a <readableString>, the program file locator extension of this Smalltalk dialect.

Definition: <ProgramReaderWriter factory>
Return Values
	<readableString>		new
"		#SIFMod. "Protocol: <ProgramReaderWriter factory>."
	^'sif' copy

! !


!ProgramReaderV1r0 class methodsFor: 'instance creation' stamp: 'RAH 9/10/1999 15:29'!
newOn: aStream
	"Private - Answer a <programReader>, a new program reader that can read the SIF version text stream, readStream.

Parameters
	readStream	<ReadStream>		captured
Return Values
	<programReader>		new
"		#SIFMod.
	#todoSIFMod. "Add selection of reader based on version."
	^super new
		stream: aStream;
		initialize.! !


!ProgramWriterV1r0 commentStamp: 'RAH 9/28/1999 09:18' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.


1.3.4 Protocol: <programWriter>

Conforms To
	<ProgramReaderWriter>

Description
	Class ProgramWriterV1r0 implements the behavior of writing a program in ANSI Smalltalk Interchange (SIF) version 1 release 0 Format to a stream (normally an external file).
	This implementation has SIF enhancements to allow collections of program element specifications in component sets and the nesting of component sets in a tree structure.  Set specifications are embedded in comments so as not to break existing implementations.  See the ProgrameaderV1r0 class for program element and enhancements format.


Note:

	Squeak implements a variable word subclass that is not supported by ANSI SIF.  This implementation will currently generate an error when the indexedInstanceVariablesType is quiried (filing out to an ANSI SIF file).!

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:00'!
fileOutClassAllSetDefinition: classNameString
annotations: annotationDictionary 
	"Write the class named classNameString ClassAllSet definition macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	classNameString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put set as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		'newClassAllSet: ', classNameString printString, '. "').

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:00'!
fileOutClassDefinitionOf: classNameString
superclass: superclassNameString
indexedInstanceVariables: indexableInstVarTypeSymbol
instanceVariableNames: instanceVariableNamesString
classVariableNames: classVariableNamesString
sharedPools: poolNamesString
classInstanceVariableNames: classInstanceVariableNamesString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the class definition of the class named, classNameString, with superclass named, superclassNameString, indexedInstanceVariables type, indexableInstVarTypeSymbol, instanceVariableNames named, instanceVariableNamesString, classVariableNames named, classVariableNamesString, sharedPools named, poolNamesString, and classInstanceVariableNames named, classInstanceVariableNamesString followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	classNameString						<readableString>	uncaptured
	superclassNameString					<readableString>	uncaptured
	indexableInstVarTypeSymbol			<symbol>		uncaptured
	instanceVariableNamesString			<readableString>	uncaptured
	classVariableNamesString				<readableString>	uncaptured
	poolNamesString						<readableString>	uncaptured
	classInstanceVariableNamesString	<readableString>	uncaptured
	annotationDictionary					<Dictionary>	uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put class def. as chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF
		formatClassDefinitionOf: classNameString
		superclass: superclassNameString
		indexedInstanceVariables: indexableInstVarTypeSymbol
		instanceVariableNames: instanceVariableNamesString
		classVariableNames: classVariableNamesString
		sharedPools: poolNamesString
		classInstanceVariableNames: classInstanceVariableNamesString
	).
	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:01'!
fileOutClassInitializerOf: classNameString
initializer: initializerString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the class initializer of the class named, classNameString, with initialization code, initializerString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	classNameString			<readableString>	uncaptured
	initializerString		<readableString>	uncaptured
	annotationDictionary		<Dictionary>	uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put class identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: (classNameString, ' initializer').

	"Put initialion code as chunk to file."
	self putNextSIFChunk: initializerString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 10:18'!
fileOutGlobalAllSetDefinition: globalNameString
annotations: annotationDictionary 
	"Write the global named globalNameString GlobalAllSet definition macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	globalNameString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put set as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		'newGlobalAllSet: ', globalNameString printString, '. "').

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:02'!
fileOutGlobalDefinitionOf: globalNameString
isConstant: isConstantBoolean
annotations: annotationDictionary 
	"Write in ANSI SIF file format the global definition of the global named, globalNameString, with constant indicator, isConstantBoolean, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	globalNameString			<readableString>	uncaptured
	isConstantBoolean		<boolean>		uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put global identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: ('Global ',
		(isConstantBoolean ifTrue: [
				' constant: '
			] ifFalse: [
				' variable: '
		]), globalNameString printString).

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:02'!
fileOutGlobalInitializerOf: globalNameString
initializer: initializerString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the global initializer of the global named, globalNameString, with initialization code, initializerString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	globalNameString			<readableString>	uncaptured
	initializerString		<readableString>	uncaptured
	annotationDictionary		<Dictionary>	uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put global identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: (globalNameString, ' initializer').

	"Put initialion code as chunk to file."
	self putNextSIFChunk: initializerString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:03'!
fileOutGroupingSetDefinition: groupingNameString
annotations: annotationDictionary 
	"Write the grouping named groupingNameString GroupingSet definition macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	groupingNameString		<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put set as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		'newGroupingSet: ', groupingNameString printString, '. "').

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:03'!
fileOutMethodDefinitionOf: methodNameString
in: classNameString
isClass: isClassBoolean
source: sourceString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the class or instance method definition of the method named, methodNameString, in the class named, classNameString,  with class method indicator, isClassBoolean, with method source code, sourceString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	methodNameString			<readableString>	uncaptured
	classNameString			<readableString>	uncaptured
	isClassBoolean			<boolean>		uncaptured
	sourceString				<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put method class identifier as chunk to file."
	self stream cr.
	isClassBoolean ifTrue: [
		self putNextSIFChunk: classNameString, ' classMethod'.
	] ifFalse: [
		self putNextSIFChunk: classNameString, ' method'.
	].

	"Put method definition source as chunk to file."
	self putNextSIFChunk: sourceString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:04'!
fileOutPoolAllSetDefinition: poolNameString
annotations: annotationDictionary 
	"Write the pool named poolNameString PoolAllSet definition macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	poolNameString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put set as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		'newPoolAllSet: ', poolNameString printString, '. "').

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:04'!
fileOutPoolDefinitionOf: poolNameString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the pool definition of the pool named, poolNameString followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	poolNameString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put pool identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: ('Pool named: ', poolNameString printString).

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:05'!
fileOutPoolValueDefinitionOf: poolValueNameString
in: poolNameString
isConstant: isConstantBoolean
annotations: annotationDictionary 
	"Write in ANSI SIF file format the constant or variable pool value definition of the pool value named, poolValueNameString, in the pool named, poolNameString, with constant indicator, isConstantBoolean, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	poolValueNameString		<readableString>	uncaptured
	poolNameString			<readableString>	uncaptured
	isConstantBoolean		<boolean>		uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put value pool identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: (poolNameString,
		(isConstantBoolean ifTrue: [
				' constant: '
			] ifFalse: [
				' variable: '
		]), poolValueNameString printString).

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:07'!
fileOutPoolValueInitializerOf: poolValueNameString
in: poolNameString
initializer: initializerString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the pool value initializer of the pool value named, poolValueNameString, in the pool named, poolNameString, with initialization code, initializerString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	poolValueNameString		<readableString>	uncaptured
	poolNameString			<readableString>	uncaptured
	initializerString		<readableString>	uncaptured
	annotationDictionary		<Dictionary>	uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put value pool identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: (poolNameString,
		' initializerFor: ', poolValueNameString printString).

	"Put initialion code as chunk to file."
	self putNextSIFChunk: initializerString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:07'!
fileOutProgramComment: commentString
annotations: annotationDictionary 
	"Write in ANSI SIF file format the program comment with content, commentString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	commentString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put comment as chunk to file."
	self stream cr.
	self putNextSIFChunk: commentString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/25/1999 10:23'!
fileOutProgramDescName: programNameString
prerequisiteProgramNames: prerequisiteProgramNameSet
programAnnotations: programAnnotationDictionary
	"Write the description of the program named programNameString with the list of prerequisite program names, prerequisiteProgramNameSet, programAndPrerequisites macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	programNameString			<readableString>	uncaptured
	prerequisiteProgramNameSet	<Set>			uncaptured
	annotationDictionary			<Dictionary>		uncaptured
"	| buildStream |
		#SIFMod. "Protocol: <programWriter>."
	"Build program description directive string."
	buildStream := WriteStream on: (String new: 75).
	buildStream tab; nextPutAll: 'programName: ';
		nextPut: $';
		nextPutAll: programNameString;
		nextPut: $'; cr.
	buildStream tab; nextPutAll: 'prerequisiteProgramNames: '; nextPut: $'.
	prerequisiteProgramNameSet
		do: [ :nameString | buildStream nextPutAll: nameString. ]
		separatedBy: [ buildStream space.].
	buildStream nextPut: $'.

	"Put description as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		buildStream contents, '. "').

	"Put annotations as chunks to file."
	self fileOutAnnotations: programAnnotationDictionary.
! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:08'!
fileOutProgramInitializer: initializerString annotations: annotationDictionary 
	"Write in ANSI SIF file format the program initializer with initialization code, initializerString, followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	initializerString		<readableString>	uncaptured
	annotationDictionary		<Dictionary>	uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put global identifier as chunk to file."
	self stream cr.
	self putNextSIFChunk: ('Global initializer').

	"Put initialion code as chunk to file."
	self putNextSIFChunk: initializerString.

	"Put annotations as chunks to file."
	self fileOutAnnotations: annotationDictionary.! !

!ProgramWriterV1r0 methodsFor: 'program-element filing' stamp: 'RAH 9/23/1999 07:08'!
fileOutSetEnd: setNameString 
	"Write the component set named setNameString <GroupingSet> end macro directive embedded in a comment followed by each key and value pair in annotationDictionary as an annotation to the receiver's stream.

Definition: <programWriter>
Parameters
	setNameString			<readableString>	uncaptured
	annotationDictionary		<Dictionary>		uncaptured
"		#SIFMod. "Protocol: <programWriter>."
	"Put set end as a directive comment chunk to file."
	self stream cr.
	self putNextSIFChunk: (SIF directiveCommentStart,
		'setEnd: ', setNameString printString, '. "').! !

!ProgramWriterV1r0 methodsFor: 'private' stamp: 'RAH 9/23/1999 06:59'!
fileOutAnnotations: aDictionary
	"Private - Write in ANSI SIF file format each key and value pair in aDictionary as an annotation to the receiver's stream.

Parameters
	aDictionary		<Dictionary>		uncaptured
"	| aString sortedKeys  |
		#SIFMod.
	sortedKeys := aDictionary keys asSortedCollection.
	sortedKeys do: [ :key |
		aString := SIF
			formatAnnotationKey: key
			value: (aDictionary at: key).
		self putNextSIFChunk: aString.
	].! !

!ProgramWriterV1r0 methodsFor: 'private' stamp: 'RAH 9/24/1999 08:44'!
putNextSIFChunk: chunkString
	"Private - Append a <readableString>, chunkString, to the receiver's stream, doubling embedded terminators.

Parameters
	chunkString		<readableString>	uncaptured
"	| chunkStream i lineTermedStream remainder terminator |
		#SIFMod.
	"Fix chunk line termination to be crlf."
	chunkStream := ReadStream on: chunkString.	"Chged SQ"
	lineTermedStream := WriteStream on: (String  new: 100).
	[ chunkStream atEnd] whileFalse: [
		lineTermedStream
			nextPutAll: chunkStream nextLine; 
			nextPut: Character cr;	"Chged SQ"
			nextPut: Character lf.
	].
	remainder := lineTermedStream contents.

	terminator := $!!.
	[(i := remainder indexOf: terminator) = 0] whileFalse: [
		self stream nextPutAll: (remainder copyFrom: 1 to: i).
		self stream nextPut: terminator.  "double imbedded terminators"
		remainder := remainder copyFrom: i+1 to: remainder size
	].
	self stream
		nextPutAll: remainder;
		nextPut: terminator;
		nextPut: Character cr;	"Chged SQ"
		nextPut: Character lf.
! !

!ProgramWriterV1r0 methodsFor: 'initializing' stamp: 'RAH 9/24/1999 09:13'!
initialize
	"Private - Initialize the instance and put the ANSI SIF file format version."
		#SIFMod.
	self stream reset.
	"Put interchange version as chunk to file:
		'Smalltalk' 'interchangeVersion:' <versionId> <elementSeparator>
					<versionId> ::= quotedString"
	self putNextSIFChunk: ('Smalltalk interchangeVersion: ''',
		SIF sifReaderWriterVersion, '''').
	^self! !


!SIFVersionError class methodsFor: 'instance creation' stamp: 'RAH 9/19/1999 10:06'!
signal: signalerText
	"Raise a new instance of the receiver, with the <readableString> message text, signalerText and an exception identifying tag.

Definition: <exceptionSignaler>
Refinement: <SIFVersionError factory>
Parameters
	signalerText	<readableString>	captured
Return Value
	<SIFVersionError>		unspecified
"		#SIFMod. "Protocol: <exceptionSignaler>."
	^super new
		tag: SIF sifVersionTag;
		signal: signalerText! !


!String methodsFor: 'converting' stamp: 'RAH 7/13/1999 07:50'!
subStrings
	"Answer an array of the substrings that compose the receiver."
		#SIFMod. "Protocol: ."
	^self substrings

! !

!String methodsFor: 'converting' stamp: 'RAH 9/23/1999 10:23'!
trimBlanks
	"Answer a <readableString>, a copy of the receiver from which leading and trailing separators have been trimmed.

Return Values
	<readableString>		new
"		#SIFMod.
	^self withBlanksTrimmed
! !


!Symbol methodsFor: 'program-element specifying' stamp: 'RAH 9/19/1999 16:48'!
classMethod
	"Private - Answer a <MethodDefinitionElement>, an incomplete new class method component with a class name the same as the receiver and a temporary method name.

	Note: This is a part of kludge for reading and evaluating an ANSI SIF program element specification from a stream.
Return Values
	<MethodDefinitionElement>		new
"		#SIFMod.
	^PGM
		newClassMethodDefinitionOf: '???'
		in: self asString! !

!Symbol methodsFor: 'program-element specifying' stamp: 'RAH 9/19/1999 16:50'!
initializer
	"Private - Answer a <symbol>, the name the same as the receiver of a globally accessible object (a global or a class) specified by an ANSI SIF initializer message.

	Note: This is a part of kludge for reading and evaluating an ANSI SIF program element specification from a stream.
	I could find no way to determined at this point if this initializer message is for a global or a class.
Return Values
	<symbol>		new
"		#SIFMod.
	^self! !

!Symbol methodsFor: 'program-element specifying' stamp: 'RAH 9/19/1999 16:49'!
method
	"Private - Answer a <MethodDefinitionElement>, an incomplete new instance method component with a class name the same as the receiver and a temporary method name.

	Note: This is a part of kludge for reading and evaluating an ANSI SIF program element specification from a stream.
Return Values
	<MethodDefinitionElement>		new
"		#SIFMod.
	^PGM
		newInstanceMethodDefinitionOf: '???'
		in: self asString! !


!TreeModelAbstract commentStamp: 'RAH 9/28/1999 08:29' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.2 Protocol: <abstractTreeModel>

Conforms To
	<model>

Description
	The TreeModelAbstract class is an abstract class and provides the protocol for a client to manipulate and operating on an abstract model of a tree structure with nodes and leaves that contains no state.  Nodes may contain sub-nodes and leaves.
!

!TreeModelAbstract methodsFor: 'enumerating' stamp: 'RAH 8/22/1999 10:39'!
do: operationBlock
	"Answer the <RECEIVER> after evaluating monadic value argument, operationBlock, for each of the elements of the receiver.  The elements are enumerated using a 'pre-order' or 'depth-first' traversal where the parent is visited before the children.

Definition: <abstractTreeModel>
Parameters
	operationBlock	<monadicValuable>	uncaptured
Return Values
	<RECEIVER>	unspecified
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	self roots do: [ :root |
		root preOrderDo: operationBlock
	].
	^self
! !

!TreeModelAbstract methodsFor: 'acessing' stamp: 'RAH 8/21/1999 16:36'!
allParentsOf: anObject
	"Answer an <OrderedCollection>, all the parents of anObject within the receiver.

Definition: <abstractTreeModel>
Parameters
	anObject	<Object>	uncaptured
Return Values
	<OrderedCollection>	new
"	| parent |
		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	parent := self parentOf: anObject.
	parent isNil ifTrue: [
		^OrderedCollection new
	].
	^(self allParentsOf: parent)
		add: parent;
		yourself! !

!TreeModelAbstract methodsFor: 'updating' stamp: 'RAH 8/26/1999 15:02'!
move: anObject asChildOf: aParentObject
	"Answer an <Object>, anObject after moving it as a child of aParentObject.  If aParentObject is nil then anObject is moved to a root of the tree.

Definition: <abstractTreeModel>
	Note: Must be implemented in subclasses.
Parameters
	anObject		<Object>		captured
	aParentObject	<Object>		uncaptured
Return Values
	<Object>		state
Errors
	anObject does not exist in the tree.
	aParentObject does not exist in the tree.
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	^self subclassResponsibility
! !

!TreeModelAbstract methodsFor: 'removing' stamp: 'RAH 8/26/1999 14:57'!
clear
	"Clear the receiver.

Definition: <abstractTreeModel>
	Note: Must be implemented in subclasses.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self subclassResponsibility
! !

!TreeModelAbstract methodsFor: 'removing' stamp: 'RAH 8/26/1999 08:19'!
remove: anObject
	"Answer an <Object>, anObject after removing it from the receiver.  If anObject does not exist in the tree then raise a suitable exception.

Definition: <abstractTreeModel>
Parameters
	anObject	<Object>	uncaptured
Return Values
	<Object>		unspecified
"		#SIFMod. "Protocol: <abstractTreeModel>."
	^self subclassResponsibility
! !

!TreeModelAbstract methodsFor: 'hierarchy' stamp: 'RAH 8/26/1999 10:45'!
childrenOf: anObject
	"Answer an <OrderedCollection>, the children of anObject in the receiver.

Definition: <abstractTreeModel>
	Note: Must be implemented in subclasses.
Parameters
	anObject	<Object>			captured
Return Values
	<OrderedCollection>		unspecified
Errors
	anObject does not exist in the receiver.
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	^self subclassResponsibility
! !

!TreeModelAbstract methodsFor: 'hierarchy' stamp: 'RAH 8/26/1999 10:31'!
includes: anObject
	"Answer a <boolean>, true if the receiver contains anObject, false if not.

Definition: <abstractTreeModel>
Parameters
	anObject	<Object>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <abstractTreeModel>."
	(self
		detect: [ :nodeOrLeaf | anObject == nodeOrLeaf]
		ifNone: []
	) isNil ifTrue: [
		^false
	].
	^true
! !

!TreeModelAbstract methodsFor: 'hierarchy' stamp: 'RAH 8/26/1999 15:08'!
parentOf: anObject
	"Answer an <Object>, the parent of anObject in the receiver, or <nil> if anObject is a root of the hierarchy.

Definition: <abstractTreeModel>
	Note: Must be implemented in subclasses.
Parameters
	anObject	<Object>			uncaptured
Return Values
	<Object>		unspecified
Errors
	anObject does not exist in the receiver.
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	^self subclassResponsibility
! !

!TreeModelAbstract methodsFor: 'hierarchy' stamp: 'RAH 8/26/1999 08:38'!
roots
	"Answer an <OrderedCollection>, the roots of the receiver's hierarchy.

Definition: <abstractTreeModel>
Return Values
	<OrderedCollection>	new
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	^self subclassResponsibility! !

!TreeModelAbstract methodsFor: 'adding' stamp: 'RAH 8/26/1999 10:48'!
add: anObject asChildOf: aParentObject
	"Answer an <Object>, anObject after adding it as a child of aParentObject.  If aParentObject is nil then anObject is added as a root of the tree.

Definition: <abstractTreeModel>
	Note: Must be implemented in subclasses.
Parameters
	anObject		<Object>		captured
	aParentObject	<Object>		uncaptured
Return Values
	<Object>		state
Errors
	anObject already exists in the tree.
	aParentObject does not exist in the tree.
"		#SIFMod. "Protocol: <abstractTreeModel>."		"Chged SQ"
	^self subclassResponsibility
! !


!ProgramAbstractComponent commentStamp: 'RAH 9/28/1999 08:33' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2 ProgramComponent Protocols

	This section includes protocols that define the behavior of the ProgramComponent classes.

	The graphs below shows the conformance relationships between the protocols defined in this section.

	     <Object>
		|
	     <model>
		|
	<abstractTreeModel>
		|
	<ProgramAbstractComponent>
		|
	-------------------------------------------------
	|						|
<ElementDefinition>				<ProgramComponentSet>
|- <CodeDefinitionionElement>				|- <GroupingSet>
|	|- <InitializerElement>				|	|- <ClassAllSet>
|	|	|- <ClassInitializerElement>		|	|- <GlobalAllSet>
|	|	|- <GlobalInitializerElement>		|	|- <PoolAllSet>
|	|	|- <PoolValueInitializerElement>	|- <ProgramDefinitionSet>
|	|	|- <ProgramInitializerElement>
|	|- <MethodDefinitionElement>
|- <ObjectDefinitionElement>
|	|- <ClassDefinitionElement>
|	|- <GlobalDefinitionElement>
|	|- <PoolDefinitionElement>
|- <PoolValueDefinitionElement>
|- <ProgramCommentElement>


	     <Object>
		|
	<ProgramAbstractComponent factory>
		|
	<ProgramComponentSet factory>
		|
	<ProgramDefinitionSet factory>




1.2.1 Protocol: <model>

Conforms To
	<Object>

Description
	Provides the protocol for an abstract model that contains no state.


1.2.2 Protocol: <abstractTreeModel>


	See the TreeModelAbstract class comment.


1.2.3 Protocol: <ProgramAbstractComponent>

Conforms To
	<abstractTreeModel>

Description
	Provides the protocol for a client to manipulate and operating on either individual, or collections of, program elements using the Container pattern.  The component ProgramAbstractComponent may be a leaf, instance of ElementDefinition class, or a composite, instance of ProgramComponentSet class, containing a collection of ProgramComponent components.  A program is a composite, instance of <ProgramDefinitionSet> class, and specifies a group of definition, initialization, and comment elements that are intended to be filed in together, or out in Smalltalk Interchange Format (SIF), to generate all or part of the Smalltalk objects comprising a Smalltalk application or subsystem.
	This protocol is the core interface that contains all messages appropriate to both composites and leafs.  These core messages are defined in terms of a small number of kernel messages. The kernel messages are implemented in ProgramAbstractComponent with #subclassResponsibility, in ElementDefinition by performing the behavior, and in ProgramComponentSet by forwarding the message to each of its components and merging the behavior.
	The ElementDefinition and ProgramComponentSet each have an extended interface that may not be used by ProgramAbstractComponent, except in the case of instance creation or of adding or removing components when it has been determined beforehand that the component is a ProgramComponentSet.  ProgramComponentSet must do suitability checking on set type operations such as adding or removing components.
ProgramComponentSet delegates to its components, but none of its subclasses, ProgramAbstractComponent, or ElementDefinition may delegate.  ProgramComponentSet subclasses defer default behavior to ProgramComponentSet.

Instance Variables

	annotations	<Dictionary> of <readableString> key and <Object> value annotations (author, category, etc.),
			| <nil> if none.

	changed	<boolean>	true if the receiver or any of its components have been changed since this was last reset, else false.

	messages	<collection> of  <readableString> text messages,
			| <nil> if none.

	messageLevel	<integer> highest level of integer code associated with a message sent to the component.
			| 0 if none.

	parent		<ProgramComponentSet> parent of the component,
			| <nil> ProgramDefinitionSet parent of the program.

1.2.4 Protocol: <ProgramAbstractComponent factory>

Conforms To
	<Object>

Description
	Provides the protocol for creating ProgramAbstractComponent component instances, and accessing useful defaults and constants.


Notes:

1. PGM Global:

	Since I find pools to be somewhat difficult to maintain I have instead defined a global, PGM, containing the ProgramAbstractComponent class object that can be conveniently queried for defaults and constants.

2. Programs Containing Programs:

	I intended this implementation to be able to handle the situation of programs containing programs.  I suspect this may break something if tried.

3. Finding Components.

	This needs a consistent and convenient mechanism for finding components.  For example finding all components in a program that reference class method x in class Y.

4. Pre- & Post- Install & Uninstall:

	I added an enhancement for pre-install, post-install, pre-uninstall, and post-uninstall program description annotations in which the value contains Smalltalk code scripts to be evaluated.  I have not fully determined what should be done or when (only when installing from a file or also when evaluating and already installed or loaded program).
!

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 8/27/1999 09:39'!
addMessage: messageString
	"Private - Add the <readableString> text message, messageString, to the list of messages for the receiver."
		#SIFMod.
	messages isNil ifTrue: [
		messages := self class defaultMessageCollection
	].
	messages add: messageString.
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 7/17/1999 09:53'!
annotateObject
	"Private - Annotate the Smalltalk object defined by the receiver with the annotations value objects recognized by this implementation (Dialect Specific)."
	| capitalizedKey handlerSelector |		"Chged SQ"
		#SIFMod.
	self containsErrors ifTrue: [
		^nil.
	].
	self annotations keysAndValuesDo: [ :key :value |
		capitalizedKey := key copy.
		capitalizedKey
			at: 1
			put: ((capitalizedKey at: 1) asUppercase).
		handlerSelector := ('object', capitalizedKey, ':') asSymbol.
		(self class selectors includes: handlerSelector) ifTrue: [	"Chged SQ"
			self perform: handlerSelector
				with: value.
		] ifFalse: [
			self program noteUnhandledAnnotations ifTrue: [
				self warningMessage: ('No handler for ', key, ': ',
										value, '.').
	]	]	].
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 9/20/1999 05:51'!
errorTreeOnly
	"Private - Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>."
		#SIFMod.
	self error: 'This message may only be sent to a <ProgramDefinitionSet> component .'
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 07/08/1999 01:24:30'!
initialize
	"Private - Initialize the receiver."
		#SIFMod.
	messageLevel := 0.
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 8/24/1999 06:37'!
messageLevel
	"Private - Answer an <integer>, the highest level of message sent to the receiver."
		#SIFMod.
	^messageLevel
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 8/24/1999 06:41'!
messageLevel: levelInteger
	"Private - Indicate to the receiver a message of level levelInteger, an <integer>."
		#SIFMod.
	levelInteger > self messageLevel ifTrue: [
		messageLevel := levelInteger
	].
! !

!ProgramAbstractComponent methodsFor: 'private' stamp: 'RAH 07/08/1999 01:24:37'!
resetMessage
	"Private - Reset the receiver's the list of messages to nil, and message level to 0."
		#SIFMod.
	messageLevel := 0.
	messages := nil! !

!ProgramAbstractComponent methodsFor: 'testing' stamp: 'RAH 8/22/1999 09:46'!
containsErrors
	"Answer a <boolean>, true if the receiver or its components contain errors, false if no errors.

Definition: <ProgramAbstractComponent>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self containsMessageLevel: PGM messageErrorLevel! !

!ProgramAbstractComponent methodsFor: 'testing' stamp: 'RAH 8/22/1999 09:52'!
containsMessageLevel: levelInteger
	"Private - Answer a <boolean>, true if the receiver or its components contain messageLevel, levelInteger, false if no errors."
		#SIFMod.
	^self messageLevel >= levelInteger! !

!ProgramAbstractComponent methodsFor: 'testing' stamp: 'RAH 9/20/1999 17:16'!
hasSubcomponents
	"Answer a <boolean>, true if the receiver has sub-components, false if empty or an element.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'testing' stamp: 'RAH 8/24/1999 06:32'!
isComponentSet
	"Answer a <boolean>, whether the receiver is a program component set object.

Definition: <ProgramAbstractComponent>
	Note: This default implementation answers false for element definition objects, so component set classes must re-implement this method to answer true.
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <abstractProgramComponent>."
	^false! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/22/1999 06:53'!
annotationAt: keyString
	"Answer an <Object>, the annotation of the receiver at keyString, a key (author, category, etc.), or nil if none present.

Definition: <ProgramAbstractComponent>
Parameters
	keyString		<readableString>	uncaptured
Return Values
	<Object>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self annotations
		at: keyString
		ifAbsent: [nil]! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/22/1999 06:52'!
annotationAt: keyString put: valueObject
	"Answer an <Object>, valueObject after storing it as the annotation value of the receiver at the key, keyString (author, category, etc.).

Definition: <ProgramAbstractComponent>
Parameters
	keyString		<readableString>	captured
	valueObject	<Object>			captured
Return Values
	<Object>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	annotations isNil ifTrue: [
		annotations := PGM defaultAnnotationCollection.
	].
	^annotations
		at: keyString
		put: valueObject! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/22/1999 08:19'!
annotations
	"Answer a <Dictionary>, the annotation collection of the receiver (author, category, etc.).

Definition: <ProgramAbstractComponent>
Return Values
	<Dictionary>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	annotations isNil ifTrue: [
		^PGM defaultAnnotationCollection.
	].
	^annotations! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:10'!
clearMessages
	"Clear the receiver's messages and set the message level to zero.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:33'!
components
	"Answer a <OrderedCollection>, the receiver's components.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Return Values
	<OrderedCollection>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:52'!
errorMessage: messageString
	"Add an error message, messageString, to the list of messages and indicate an error level message to the receiver, and notify the user of an error using messageString.

Definition: <ProgramAbstractComponent>
Parameters
	messageString	<readableString>	captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self addMessage: messageString.
	self messageLevel: PGM messageErrorLevel.
	self portFunc promptError: messageString.! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:53'!
fatalMessage: messageString
	"Add a fatal error message, messageString, to the list of messages and indicate an fatal level message to the receiver, and notify the user of an error using messageString.

Definition: <ProgramAbstractComponent>
Parameters
	messageString	<readableString>	captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self addMessage: messageString.
	self messageLevel: PGM messageFatalLevel.
	self portFunc promptFatal: messageString.! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:53'!
informMessage: messageString
	"Add an inform message, messageString, to the list of messages and indicate an inform level message to the receiver, and inform the user using messageString.

Definition: <ProgramAbstractComponent>
Parameters
	messageString	<readableString>	captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self addMessage: messageString.
	self messageLevel: PGM messageInformLevel.
	self portFunc promptInform: messageString.! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 06:24'!
isChanged
	"Answer a <boolean>, true if the receiver or any of its components have been changed since their changed flag was last reset (Dialect Specific).

Definition: <ProgramAbstractComponent>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^changed = true		"Chged SQ"! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 07:25'!
isChanged: aBoolean
	"Set the receiver's changed flag to indicate whether the it or any of its components have been changed since their changed flag was last reset (Dialect Specific).

Definition: <ProgramAbstractComponent>
Parameters
	aBoolean	<boolean>		captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	aBoolean ifTrue: [
		changed := true.		"Chged SQ"
		self program wasEvaluated: false.
	] ifFalse: [
		changed := nil.		"Chged SQ"
	].! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 06:47'!
messages
	"Answer a <collection> of  <readableString>, the list of text messages sent to the receiver, or <nil> if none.

Definition: <ProgramAbstractComponent>
Return Values
	<collection>	state
	<nil>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	messages isNil ifTrue: [
		^self class defaultMessageCollection
	].
	^messages! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 07:15'!
parentSetOrNil
	"Answer a <ProgramComponentSet>, the parent of the receiver, or <nil> if the receiver is a <ProgramDefinitionSet>.

Definition: <ProgramAbstractComponent>
Return Values
	<ProgramComponentSet>	state
	<nil>					unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^parent! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 07:15'!
parentSetOrNil: componentOrNil
	"Set parent of the receiver to a <ProgramComponentSet>, componentOrNil, or <nil> to clear parent or the receiver is a <ProgramDefinitionSet>.

Definition: <ProgramAbstractComponent>
Parameters
	componentOrNil		<ProgramComponentSet>	captured
						<nil>				captured
Errors
	componentOrNil is not a <ProgramComponentSet> or <nil> .
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	(componentOrNil isKindOf: PGM programComponentSet) ifFalse: [
		componentOrNil isNil ifFalse: [
			PGM programError signal: 'Parent must be a ProgramComponentSet or nil.'.
	]	].
	parent := componentOrNil! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 08:49'!
program
	"Answer a <ProgramDefinitionSet>, the program containing the receiver.

Definition: <ProgramAbstractComponent>
Return Values
	<collection>	state
	<nil>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self parentSetOrNil isNil ifFalse: [
		^self parentSetOrNil program
	].
	^self! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 8/24/1999 08:55'!
removeAnnotationAt: keyString
	"Answer an <Object>, the value of the annotation at keyString after removing the element in which is stored in the receiver, or nil if none present.

Definition: <ProgramAbstractComponent>
Parameters
	keyString		<readableString>	uncaptured
Return Values
	<Object>		state
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self annotations
		removeKey: keyString
		ifAbsent: [nil]! !

!ProgramAbstractComponent methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:54'!
warningMessage: messageString
	"Add a warning message, messageString, to the list of messages and indicate an warning level message to the receiver, and notify the user of a warning using messageString.

Definition: <ProgramAbstractComponent>
Parameters
	messageString	<readableString>	captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self addMessage: messageString.
	self messageLevel: PGM messageWarningLevel.
	self portFunc promptWarn: messageString.
! !

!ProgramAbstractComponent methodsFor: 'searching' stamp: 'RAH 8/23/1999 09:02'!
detect: discriminator ifNone: exceptionHandler
	"Answer an <ProgramAbstractComponent>, the first element of the receiver's elements (in the #do: order) for which the <monadicValuable> argument, discriminator, evaluates to true, or <Object>, the result of evaluating the <niladicValuable> argument, exceptionHandler, if none evaluates to true.

Definition: <ProgramAbstractComponent>
Parameters
	discriminator		<monadicValuable>	uncaptured
	exceptionHandler	<niladicValuable>	uncaptured
Return Values
	<ProgramAbstractComponent>	state
	<Object>					unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self do: [ :component |
		(discriminator value: component) ifTrue: [
			^component
	]	].
	^exceptionHandler value! !

!ProgramAbstractComponent methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:17'!
storeOn: targetStream
	"Append to targetStream, a <puttableStream>, the ASCII representation of the receiver from which the receiver can be reinstantiated.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	targetStream	<puttableStream>	uncaptured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:23'!
storeString
	"Answer a <readableString>, a representation of the receiver from which it can be reconstructed.

Definition: <ProgramAbstractComponent>
Return Values
	<readableString>	new
"	| aStream |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	aStream := WriteStream on: (String new: 16).
	self storeOn: aStream.
	^aStream contents! !

!ProgramAbstractComponent methodsFor: 'events' stamp: 'RAH 8/24/1999 09:24'!
triggerAddedCmponent: childComponent toParent: parentComponent
	"Private - Inform programManager the receiver has changed.
	Pretty funky - I don't think this is how to do it."
		#SIFMod.	"Chg SQ"
#todoSIFMod."
	self	trigger: #item:addedInParent:
		with: childComponent
		with: parentComponent.
"! !

!ProgramAbstractComponent methodsFor: 'events' stamp: 'RAH 8/24/1999 09:24'!
triggerProgramChanged
	"Private - Inform programManager the receiver has changed.
	Pretty funky - I don't think this is how to do it."
		#SIFMod.	"Chg SQ"
#todoSIFMod."
	self programManager
		trigger: #programChanged:
		with: self
"! !

!ProgramAbstractComponent methodsFor: 'events' stamp: 'RAH 8/24/1999 09:25'!
triggerRemovedCmponent: childComponent fromParent: parentComponent
	"Private - Inform programManager the receiver has changed.
	Pretty funky - I don't think this is how to do it."
		#SIFMod.	"Chg SQ"
#todoSIFMod."
	self	trigger: #item:removedFromParent:
		with: childComponent
		with: parentComponent.
"! !

!ProgramAbstractComponent methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:25'!
printOn: targetStream
	"Append to targetStream, a <puttableStream>, a text representation of the receiver as a developer would want to see it (inspector, etc).

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	targetStream	<puttableStream>	uncaptured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self subclassResponsibility
! !

!ProgramAbstractComponent methodsFor: 'displaying' stamp: 'RAH 8/24/1999 07:22'!
displayOn: targetStream
	"Append to targetStream, a <puttableStream>, a text representation of the receiver as a user would want to see it (program browser, etc).

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	targetStream	<puttableStream>	uncaptured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'displaying' stamp: 'RAH 7/24/1999 09:42'!
displayString
	"Answer a <readableString>, a text representation of the receiver as a user would want to see it (program browser, etc)."
		#SIFMod. "Protocol: <abstractProgramComponent>."
	^String streamContents: [ :stream |	"Chged SQ"
		self displayOn: stream.
	]
! !

!ProgramAbstractComponent methodsFor: 'enumerating' stamp: 'RAH 9/5/1999 17:56'!
collect: transformerBlock
	"Answer a <OrderedCollection>, the values returned by evaluating the <monadicValuable> block, transformerBlock, on each the receiver's components (in the #do: order).

Definition: <ProgramAbstractComponent>
Parameters
	transformerBlock		<monadicValuable>	uncaptured
Return Values
	<OrderedCollection>		new
"	| newCollection |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	newCollection := OrderedCollection new.
	self do: [ :component |
		newCollection add: (transformerBlock value: component)
	].
	^newCollection
! !

!ProgramAbstractComponent methodsFor: 'enumerating' stamp: 'RAH 8/23/1999 10:01'!
endOrderDo: operationBlock
	"Answer the <RECEIVER> after evaluating in 'end-order' traversal where the receiver is visited after the children the <monadicValuable> argument,  operationBlock, for each of it's components.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	operationBlock		<monadicValuable>		uncaptured
Return Values
	<RECEIVER>	unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self subclassResponsibility
! !

!ProgramAbstractComponent methodsFor: 'enumerating' stamp: 'RAH 8/24/1999 08:08'!
preOrderDo: operationBlock
	"Answer the <RECEIVER> after evaluating in 'pre-order' or 'depth-first' traversal where the receiver is visited before the children the <monadicValuable> argument,  operationBlock, for each of it's components.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	operationBlock		<monadicValuable>		uncaptured
Return Values
	<RECEIVER>	unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self subclassResponsibility
! !

!ProgramAbstractComponent methodsFor: 'enumerating' stamp: 'RAH 9/5/1999 17:57'!
select: discriminatorBlock
	"Answer a <OrderedCollection>, the receiver's components (in the #do: order) for which the <monadicValuable> argument, discriminatorBlock, evaluates to true.

Definition: <ProgramAbstractComponent>
Parameters
	discriminatorBlock	<monadicValuable>	uncaptured
Return Values
	<OrderedCollection>		new
"	| newCollection |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	newCollection := OrderedCollection new.
	self do: [ :component |
		(discriminatorBlock value: component) ifTrue: [
			newCollection add: component
	]	].
	^newCollection
! !

!ProgramAbstractComponent methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:39'!
fileOutOnSIFFiler: programFiler
	"File out the receiver and its components on the <programWriter>, programFiler.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
Parameters
	programFiler	<programWriter>	captured
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:59'!
evaluateObjects 
	"Create or initialize the Smalltalk objects specified by the receiver and its components.

Definition: <ProgramAbstractComponent>
	Note: Must be implemented in subclasses.
		This includes annotating the Smalltalk objects to the extent the dialect permits (class comment, method category, etc.).
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self subclassResponsibility! !

!ProgramAbstractComponent methodsFor: 'constants' stamp: 'RAH 8/24/1999 08:52'!
programManager
	"Private - Answer a <ProgramManager>, the current program manager which the receiver is stored in."
		#SIFMod.
	^ProgramManager current! !

!ProgramAbstractComponent methodsFor: 'adding' stamp: 'RAH 9/20/1999 05:48'!
add: anObject asChildOf: aParentObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !

!ProgramAbstractComponent methodsFor: 'hierarchy' stamp: 'RAH 9/20/1999 05:49'!
childrenOf: anObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly! !

!ProgramAbstractComponent methodsFor: 'hierarchy' stamp: 'RAH 9/20/1999 05:54'!
parentOf: anObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !

!ProgramAbstractComponent methodsFor: 'hierarchy' stamp: 'RAH 9/20/1999 05:55'!
roots

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !

!ProgramAbstractComponent methodsFor: 'removing' stamp: 'RAH 9/20/1999 05:49'!
clear

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a ProgramDefinitionSet.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !

!ProgramAbstractComponent methodsFor: 'removing' stamp: 'RAH 9/20/1999 05:55'!
remove: anObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a ProgramDefinitionSet.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !

!ProgramAbstractComponent methodsFor: 'updating' stamp: 'RAH 9/20/1999 05:54'!
move: anObject asChildOf: aParentObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Raise an exception as this is an inappropriate message to any component except a <ProgramDefinitionSet>.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	self errorTreeOnly
! !


!ElementDefinition commentStamp: 'RAH 9/28/1999 08:34' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.5 Protocol: <ElementDefinition>

Conforms To
	<ProgramAbstractComponent>

Description
	Class ElementDefinition is an abstract class implementing the common behavior of all leaf component program elements.  Each concrete subclass roughly corresponds to a Smalltalk Interchange Format section <interchangeElement>, an interchange definition, initialization, or comment element that is intended to be filed in or out in SIF file, and to generate or initialize a Smalltalk object.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ElementDefinition.

Instance Variables:

	None.
!

!ElementDefinition methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:10'!
clearMessages

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self resetMessage.
	self triggerProgramChanged.! !

!ElementDefinition methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:32'!
components

"Definition: <ProgramAbstractComponent>
Refinement: <ElementDefinition>
	Answer a <OrderedCollection>, collection containing only the receiver.
Return Values
	<OrderedCollection>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^self class defaultComponentCollection with: self
! !

!ElementDefinition methodsFor: 'private' stamp: 'RAH 9/20/1999 05:51'!
errorSetOnly
	"Private - Signal an inappropriate-message-to-an-element error."
		#SIFMod.
	PGM programError signal: 'This Set message is inappropriate for an element'! !

!ElementDefinition methodsFor: 'private' stamp: 'RAH 8/27/1999 13:15'!
objectAuthor: aString
	"Private - Set the receiver's Smalltalk object author to aString, a <readableString>.
	Do nothing as this object does not support the annotation.  This default behavior must be overridden for those elements that support this annotation."
		#SIFMod.
	self program noteUnhandledAnnotations ifTrue: [
		self warningMessage: ('No handler for author: "', aString, '".').
	].! !

!ElementDefinition methodsFor: 'private' stamp: 'RAH 8/27/1999 13:15'!
objectCategory: aString
	"Private - Set the receiver's Smalltalk object category to aString, a <readableString>.
	Do nothing as this object does not support a category annotation.  This default behavior must be overridden for those elements that support this annotation."
		#SIFMod.
	self program noteUnhandledAnnotations ifTrue: [
		self warningMessage: ('No handler for category: "', aString, '".').
	].! !

!ElementDefinition methodsFor: 'private' stamp: 'RAH 8/27/1999 13:15'!
objectComment: aString
	"Private - Set the receiver's Smalltalk object comment to aString, a <readableString>.
	Do nothing as this object does not support a comment annotation.  This default behavior must be overridden for those elements that support this annotation."
		#SIFMod.
	self program noteUnhandledAnnotations ifTrue: [
		self warningMessage: ('No handler for comment: "', aString, '".').
	].! !

!ElementDefinition methodsFor: 'private' stamp: 'RAH 8/27/1999 13:15'!
objectCopyright: aString
	"Private - Set the receiver's Smalltalk object copyright to aString, a <readableString>.
	Do nothing as this object does not support a copyright annotation.  This default behavior must be overridden for those elements that support this annotation."
		#SIFMod.
	self program noteUnhandledAnnotations ifTrue: [
		self warningMessage: ('No handler for copyright: "', aString, '".').
	].! !

!ElementDefinition methodsFor: 'testing' stamp: 'RAH 8/24/1999 06:15'!
hasSubcomponents

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^false! !

!ElementDefinition methodsFor: 'enumerating' stamp: 'RAH 8/22/1999 10:48'!
endOrderDo: operationBlock

"Definition: <ProgramAbstractComponent>
Refinement: <ElementDefinition>
	Answer the <RECEIVER> after evaluating the <monadicValuable> argument,  operationBlock, for the receiver.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	operationBlock value: self.
	^self! !

!ElementDefinition methodsFor: 'enumerating' stamp: 'RAH 8/24/1999 08:09'!
preOrderDo: operationBlock

"Definition: <ProgramAbstractComponent>
Refinement: <ElementDefinition>
	Answer the <RECEIVER> after evaluating the <monadicValuable> argument,  operationBlock, for the receiver.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	operationBlock value: self.
	^self
! !


!CodeDefinitionionElement commentStamp: 'RAH 9/28/1999 08:36' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <CodeDefinitionionElement>

Conforms To
	<ElementDefinition>

Description
	Class CodeDefinitionionElement is an abstract class implementing the common behavior of all program interchange elements that require Smalltalk source code to generate or initialize the Smalltalk object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a CodeDefinitionionElement.

Instance Variables:

	source	<readableString>	Program element source code. 
!

!CodeDefinitionionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 06:49'!
source
	"Answer a <readableString>, the receiver's source code.

Definition: <CodeDefinitionionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <CodeDefinitionionElement>."
	^source
! !

!CodeDefinitionionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 06:54'!
source: aString
	"Set the receiver's source code to aString, a <readableString>.

Definition: <CodeDefinitionionElement>
Parameters
	aString	<readableString>	captured
Errors
	aString is not a <readableString>.
"		#SIFMod. "Protocol: <CodeDefinitionionElement>."
	(aString isKindOf: String) ifFalse: [
		PGM programError signal: 'Source code not a String.'.
	].
	source := aString
! !


!InitializerElement commentStamp: 'RAH 9/28/1999 08:36' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <InitializerElement>

Conforms To
	<CodeDefinitionionElement>

Description
	Class InitializerElement is an abstract class implementing the common behavior of all program interchange elements that require Smalltalk source code to initialize the Smalltalk object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a InitializerElement.

Instance Variables:

	None 
!

!ClassInitializerElement commentStamp: 'RAH 9/28/1999 08:37' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <ClassInitializerElement>

Conforms To
	<InitializerElement>

Description
	Class ClassInitializerElement implements the behavior of a Smalltalk Interchange Format section <classInitialization>, a class initializer program interchange elements that initialize the Smalltalk class object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ClassInitializerElement.

Instance Variables:

	className	<readableString>	Class initializer program element class name.
 !

!ClassInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:09'!
className
	"Answer a <readableString>, the receiver's class name.

Definition: <ClassInitializerElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassInitializerElement>."
	^className
! !

!ClassInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:09'!
className: nameString
	"Set the receiver's class name to nameString, a <readableString>.

Definition: <ClassInitializerElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <ClassInitializerElement>."
	className := nameString
! !

!ClassInitializerElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:22'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self className printOn: targetStream.
	targetStream nextPutAll: ' Class Initializer'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!ClassInitializerElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:50'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
	Note: ANSI specifies the result of evaluating a class initializer is discarded.
Errors
	Class is not defined.
	Native initialization code evaluation errors.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	Smalltalk
		at: (self className asSymbol)
		ifAbsent: [
			^self errorMessage: ('class "', self className, '" not defined.').
		].
	[	Compiler
			evaluate: self source
			logged: true.
	] on: (Error, PGM compilerNotification) do: [ :except |
		^self errorMessage: ('Error evaluating class initializer of : "',
				self className, '" - ', except description).
	].
! !

!ClassInitializerElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:10'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newClass: '.
	self className printOn: targetStream.
	targetStream nextPutAll: ' initializer: '.
	self source storeOn: targetStream.
	targetStream nextPutAll: ')'.! !

!ClassInitializerElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:36'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutClassInitializerOf: self className
		initializer: self source
		annotations: self annotations! !

!ClassInitializerElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:16'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ClassInit: ';
		nextPutAll: self className.! !


!GlobalInitializerElement commentStamp: 'RAH 9/28/1999 08:38' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <GlobalInitializerElement>

Conforms To
	<InitializerElement>

Description
	Class GlobalInitializerElement implements the behavior of a Smalltalk Interchange Format section <globalValueInitialization>, a global initializer program interchange elements that initialize the Smalltalk global object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a GlobalInitializerElement.

Instance Variables:

	globalName	<readableString>	Global initializer program element class name. 
!

!GlobalInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:38'!
globalName
	"Answer a <readableString>, the receiver's global name.

Definition: <GlobalInitializerElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <GlobalInitializerElement>."
	^globalName! !

!GlobalInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:40'!
globalName: nameString
	"Set the receiver's global name to nameString, a <readableString>.

Definition: <GlobalInitializerElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <GlobalInitializerElement>."
	globalName := nameString
! !

!GlobalInitializerElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:12'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newGlobal: '.
	self globalName printOn: targetStream.
	targetStream nextPutAll: ' initializer: '.
	self source storeOn: targetStream.
	targetStream nextPutAll: ')'.! !

!GlobalInitializerElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutGlobalInitializerOf: self globalName
		initializer: self source
		annotations: self annotations! !

!GlobalInitializerElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:23'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self globalName printOn: targetStream.
	targetStream nextPutAll: ' Global Initializer'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!GlobalInitializerElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:20'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'GlobalInit: ';
		nextPutAll: self globalName.! !

!GlobalInitializerElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:49'!
evaluateObjects 
 
"Definition: <ProgramAbstractComponent>
Errors
	Global is not defined.
	Native initialization code evaluation errors.
"	| globalNameSymbol globalValue |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	globalNameSymbol := self globalName asSymbol.
	Smalltalk
		at: globalNameSymbol
		ifAbsent: [
			^self errorMessage: ('Global "', self globalName, '" not defined.').
		].

	globalValue := [ Compiler
		evaluate: self source
		logged: true
	] on: (Error, PGM compilerNotification) do: [ :except |
		^self errorMessage: ('Error evaluating global initializer of : "',
				self globalName, '" - ', except description).
	].
	Smalltalk
		at: globalNameSymbol
		put: globalValue.! !


!MethodDefinitionElement commentStamp: 'RAH 9/28/1999 08:41' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.7 Protocol: <MethodDefinitionElement>

Conforms To
	<CodeDefinitionionElement>

Description
	Class MethodDefinitionElement implements the behavior of a Smalltalk Interchange Format section methodDefinition> or <classMethodDefinition>, a instance or class method definition program interchange elements that specify the Smalltalk class object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a MethodDefinitionElement.

Instance Variables:

	className	<readableString>	Method definition program element class name. 

	isClassMethod	<boolean>		Indicates a class or instance method definition.

	methodName	<readableString>	Method definition program element class name.
!

!MethodDefinitionElement methodsFor: 'private' stamp: 'RAH 9/23/1999 10:07'!
classOrMetaclassName
	"Private - Answer a <readableString>, the method's Metaclass name if the receiver specifies a class method, else the method's Class name."
		#SIFMod.
	self isClassMethod ifTrue: [
		^self className, ' class'
	].
	^self className! !

!MethodDefinitionElement methodsFor: 'private' stamp: 'RAH 9/23/1999 10:07'!
classThenSelectorString 
	"Private - Answer a <readableString>, the receiver specified method's Class or Metaclass name and method selector."
		#SIFMod.
	^self classOrMetaclassName, '>>#', self methodName! !

!MethodDefinitionElement methodsFor: 'private' stamp: 'RAH 9/23/1999 10:07'!
getClassOrMetaclassIfAbsent: aBlock
	"Private - Answer a <classDescription>, the method's Metaclass if the receiver specifies a class method, else the method's Class."
	| classObj |
		#SIFMod.
	classObj := Smalltalk
		at: self className asSymbol
		ifAbsent: [
			^aBlock value.
		].
	self isClassMethod ifTrue: [
		^classObj class
	].
	^classObj! !

!MethodDefinitionElement methodsFor: 'private' stamp: 'RAH 8/27/1999 13:28'!
objectCategory: aString
	"Private - Set the method's category to aString, a <readableString> (Dialect Specific)."
	| classOrMetaclassObj selector |
		#SIFMod. "Protocol: <abstractElementDefinition>"
	classOrMetaclassObj := self getClassOrMetaclassIfAbsent: [
		^self errorMessage: ('Class "', self className, '" not defined.').
	].
	selector := self methodName asSymbol.
	(classOrMetaclassObj includesSelector: selector) ifTrue: [
		classOrMetaclassObj organization		"Chged SQ"
			classify: selector
			under: aString.
	].! !

!MethodDefinitionElement methodsFor: 'private' stamp: 'RAH 9/23/1999 10:08'!
updateElementSpecification
	"Private - Update the element specification from the Smalltalk object."
	| methodCategory classOrMetaclassObj methodSelector sourceString |
		#SIFMod.
	classOrMetaclassObj := self getClassOrMetaclassIfAbsent: [
		^self errorMessage: ('Class "', self className, '" not defined.').
	].

	"Update the method source."
	methodSelector := methodName asSymbol.		"Chged SQ"
	sourceString := ((classOrMetaclassObj compiledMethodAt: methodSelector)
		getSourceFor: methodSelector
		in: classOrMetaclassObj
		) asString.
	self source: sourceString.

	"Update the category annotation."
	"???This is problematic in the scenario:
		Method specification filed in with a list of categories.
		Method function changed.
		The single method object category allowed by a dialect changed.
		Now update the list of categories with what?.
			Replace the list with the single?
			Add the single to the list?"
	methodCategory := classOrMetaclassObj organization 	"Chg SQ"
		categoryOfElement: methodSelector.
	methodCategory = ClassOrganizer default ifTrue: [
		self removeAnnotationAt: SIF categoryKey.
	] ifFalse: [
		self
			annotationAt: SIF categoryKey
			put: methodCategory.
	].! !

!MethodDefinitionElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:14'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self isClassMethod ifTrue: [
		targetStream nextPutAll: '(PGM newClassMethodDefinitionOf: '.
	] ifFalse: [
		targetStream nextPutAll: '(PGM newInstanceMethodDefinitionOf: '.
	].
	self methodName printOn: targetStream.
	targetStream nextPutAll: ' in: '.
	self className printOn: targetStream.
	targetStream nextPutAll: ')'.! !

!MethodDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 09:11'!
className
	"Answer a <readableString>, the receiver's class name.

Definition: <MethodDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	^className
! !

!MethodDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 09:12'!
className: nameString
	"Set the receiver's class name to nameString, a <readableString>.

Definition: <MethodDefinitionElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	className := nameString! !

!MethodDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 13:27'!
isClassMethod: aBoolean
	"Set the receiver's is-class-method indicator to a <boolean>, aBoolean, to specify a class or instance method.

Definition: <MethodDefinitionElement>
Parameters
	aBoolean		<boolean>	captured
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	isClassMethod := aBoolean! !

!MethodDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 09:27'!
methodName
	"Answer a <readableString>, the receiver's method name.

Definition: <MethodDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	^methodName! !

!MethodDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 09:28'!
methodName: nameString
	"Set the receiver's method name to nameString, a <readableString>.

Definition: <MethodDefinitionElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	methodName := nameString! !

!MethodDefinitionElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:24'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self className printOn: targetStream.
	targetStream nextPutAll: ' >> #'.
	self methodName printOn: targetStream.
	self isClassMethod ifTrue: [
		targetStream nextPutAll: ' Class'.
	].
	targetStream nextPutAll: ' Method'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].


		! !

!MethodDefinitionElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	"The method object may differ from the receiver specification, so update."
	"???Not required in declarative model SDE,
		or if the specification and the object are kept in sync."
	self updateElementSpecification.

	programFiler
		fileOutMethodDefinitionOf: self methodName
		in: self className
		isClass: self isClassMethod
		source: self source
		annotations: self annotations! !

!MethodDefinitionElement methodsFor: 'testing' stamp: 'RAH 8/27/1999 13:25'!
isClassMethod
	"Answer a <boolean>, true if the receiver specifies a class method, else false.

Definition: <MethodDefinitionElement>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <MethodDefinitionElement>."
	^isClassMethod! !

!MethodDefinitionElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:47'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
Errors
	Class is not defined.
	Native initialization code evaluation errors.
"	| classOrMetaclassObj |	"Chged SQ"
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	classOrMetaclassObj := self getClassOrMetaclassIfAbsent: [
		^self errorMessage: ('Class "', self className, '" not defined.').
	].

	[	classOrMetaclassObj compile: self source
	] on: PGM compilerNotification do: [ :except |
		^self errorMessage: ('Error compiling source: "',
				self classThenSelectorString, '" - ', except text, '.').
	].
	self annotateObject.! !

!MethodDefinitionElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:22'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'MethodDef: ';
		nextPutAll: self classThenSelectorString.! !


!ObjectDefinitionElement commentStamp: 'RAH 9/28/1999 08:42' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.8 Protocol: <ObjectDefinitionElement>

Conforms To
	<CodeDefinitionionElement>

Description
	Class ObjectDefinitionElement is an abstract class implementing the common behavior of all globally accessible object (class|pool|global) definition program interchange elements.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ObjectDefinitionElement.

Instance Variables:

	objectName	<readableString>	Globally accessible object (class|pool|global) definition program element name.
!

!ObjectDefinitionElement methodsFor: 'accessing' stamp: 'RAH 9/21/1999 05:03'!
objectName
	"Answer a <readableString>, the receiver's globally accessible object name (class|pool|global).

Definition: <ObjectDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ObjectDefinitionElement>."
	^objectName
! !

!ObjectDefinitionElement methodsFor: 'accessing' stamp: 'RAH 9/21/1999 05:03'!
objectName: nameString
	"Set the receiver's a globally accessible object (class|pool|global) name to nameString, a <readableString>.

Definition: <ObjectDefinitionElement>
Parameters
	nameString	<readableString>		captured
"		#SIFMod. "Protocol: <ObjectDefinitionElement>."
	objectName := nameString! !


!ClassDefinitionElement commentStamp: 'RAH 9/28/1999 08:43' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.9 Protocol: <ClassDefinitionElement>

Conforms To
	<ObjectDefinitionElement>

Description
	Class ClassDefinitionElement implements the behavior of a Smalltalk Interchange Format section <classDefinition>, a class definition program interchange elements that specify the Smalltalk class object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ClassDefinitionElement.

Instance Variables:

	superclassName	<readableString>		Class definition program element superclass name. 

	indexedInstanceVariablesType	<symbol>	Indicates a class definition program element indexed instance variable type.  Must be #'byte'. #'object'. or #'none' 

	instanceVariableNames	<readableString>	Class definition program element instance variable names separated by whiteSpace. 

	classVariableNames	<readableString>	Class definition program element class variable names separated by whiteSpace. 

	sharedPoolNames	<readableString>	Class definition program element shared pool names separated by whiteSpace. 

	classInstanceVariableNames	<readableString>	Class definition program element class instance variable names separated by whiteSpace.

Implementation Note:

	Without a Declarative Model some aspects of a class object may differ from the its definition program element.  This is a problem in this implementation, so before filing out to a SIF file I update the class specification by substituting the class object attributes such as instance variable names, category, etc.  This would not be required in declarative model SDE, or if the specification and the object are kept synchronised.
!

!ClassDefinitionElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:22'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self objectName printOn: targetStream.
	targetStream nextPutAll: ' Class Definition'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:14'!
classInstanceVariableNames
	"Answer a <readableString>, the receiver's class instance variable names.

Definition: <ClassDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^classInstanceVariableNames! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:15'!
classInstanceVariableNames: aString
	"Set the receiver's class instance variable names to aString, a <readableString>.

Definition: <ClassDefinitionElement>
Parameters
	aString	<readableString>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	classInstanceVariableNames := aString
! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:16'!
classVariableNames
	"Answer a <readableString>, the receiver's class variable names.

Definition: <ClassDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^classVariableNames! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:18'!
classVariableNames: aString
	"Set the receiver's class variable names to aString, a <readableString>.

Definition: <ClassDefinitionElement>
Parameters
	aString	<readableString>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	classVariableNames := aString
! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:20'!
indexedInstanceVariablesType
	"Answer a <symbol>, the receiver's indexed instance variable type.

Definition: <ClassDefinitionElement>
Return Values
	<symbol>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^indexedInstanceVariablesType! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:22'!
indexedInstanceVariablesType: typeSymbol
	"Set the receiver's indexed instance variable type to typeSymbol, a <symbol>.

Definition: <ClassDefinitionElement>
Parameters
	typeSymbol	<symbol>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	indexedInstanceVariablesType := typeSymbol! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:23'!
instanceVariableNames
	"Answer a <readableString>, the receiver's instance variable names.

Definition: <ClassDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^instanceVariableNames! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:25'!
instanceVariableNames: aString
	"Set the receiver's instance variable names to aString, a <readableString>.

Definition: <ClassDefinitionElement>
Parameters
	aString	<readableString>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	instanceVariableNames := aString! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:32'!
sharedPoolNames
	"Answer a <readableString>, the receiver's shared pool names.

Definition: <ClassDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^sharedPoolNames! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:33'!
sharedPoolNames: aString
	"Set the receiver's shared pool names to aString, a <readableString>.

Definition: <ClassDefinitionElement>
Parameters
	aString	<readableString>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	sharedPoolNames := aString! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:34'!
superclassName
	"Answer a <readableString>, the receiver's superclass name.

Definition: <ClassDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	^superclassName! !

!ClassDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 14:34'!
superclassName: aString
	"Set the receiver's superclass name to aString, a <readableString>.

Definition: <ClassDefinitionElement>
Parameters
	aString	<readableString>		captured
"		#SIFMod. "Protocol: <ClassDefinitionElement>."
	superclassName := aString! !

!ClassDefinitionElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:53'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
Errors
	Superclass is not defined.
	Indexed instance variable is not a valid type.
	Native class definition errors.
"	| classObj classNameSymbol creatorSelector superclassObj |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	"???I'm not sure it is a good idea to compile the class def. unless if it is
	a new class or the class def. is 	different than that of the existing class."
	superclassObj := Smalltalk
		at: (self superclassName asSymbol)
		ifAbsent: [^self errorMessage: ('Superclass not defined: "',
						self superclassName, '".')].
	creatorSelector := self class subclassCreatorMethodSelectorList
		at: self indexedInstanceVariablesType
		ifAbsent: [^self errorMessage: (
				'Invalid indexed instance variables type: "#',
				self indexedInstanceVariablesType, '".')].
	classNameSymbol := self objectName asSymbol.
	classObj := [superclassObj
			perform: creatorSelector
			withArguments: (Array
				with: classNameSymbol
				with: self instanceVariableNames
				with: self classVariableNames
				with: self sharedPoolNames
				with: ClassOrganizer nullCategory)	"Chged SQ"
		] on: PGM compilerNotification do: [ :except |
			self errorMessage: ('Error compiling class definition of : "',
					self objectName, '" - ', except text).
			^except return
		].

	self classInstanceVariableNames isEmpty ifFalse: [
		classObj class instanceVariableNames: self classInstanceVariableNames.
	].

	self annotateObject.
! !

!ClassDefinitionElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:09'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newClassDefinitionOf: '.
	self objectName printOn: targetStream.
	targetStream nextPutAll: ' superclass: '.
	self superclassName printOn: targetStream.
	targetStream nextPutAll: ' indexedInstanceVariables: '.
	self indexedInstanceVariablesType printOn: targetStream.
	targetStream nextPutAll: ' instanceVariableNames: '.
	self instanceVariableNames printOn: targetStream.
	targetStream nextPutAll: ' classVariableNames: '.
	self classVariableNames printOn: targetStream.
	targetStream nextPutAll: ' sharedPools: '.
	self sharedPoolNames printOn: targetStream.
	targetStream nextPutAll: ' classInstanceVariableNames: '.
	self classInstanceVariableNames printOn: targetStream.
	targetStream nextPutAll: ')'.! !

!ClassDefinitionElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:36'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	"The class object may differ from the receiver specification, so update."
	"???Not required in declarative model SDE,
		or if the specification and the object are kept in sync."
	self updateElementSpecification.

	programFiler
		fileOutClassDefinitionOf: self objectName
		superclass: self superclassName
		indexedInstanceVariables: self indexedInstanceVariablesType
		instanceVariableNames: self instanceVariableNames
		classVariableNames: self classVariableNames
		sharedPools: self sharedPoolNames
		classInstanceVariableNames: self classInstanceVariableNames
		annotations: self annotations! !

!ClassDefinitionElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:16'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ClassDef: ';
		nextPutAll: self objectName.! !

!ClassDefinitionElement methodsFor: 'private' stamp: 'RAH 8/27/1999 14:29'!
objectCategory: aString
	"Private - Set the class's category to aString, a <readableString> (Dialect Specific)."
	| classObj |
		#SIFMod.
	classObj := Smalltalk at: (self objectName asSymbol).
	classObj category: aString.	"Chg SQ"! !

!ClassDefinitionElement methodsFor: 'private' stamp: 'RAH 8/27/1999 14:30'!
objectComment: aString
	"Private - Set the class's comment to aString, a <readableString> (Dialect Specific)."
	| classObj |
		#SIFMod.
	classObj := Smalltalk at: (self objectName asSymbol).
	classObj comment: aString.! !

!ClassDefinitionElement methodsFor: 'private' stamp: 'RAH 9/23/1999 10:06'!
updateElementSpecification
	"Private - Update the element specification from the Smalltalk object."
	| classCategory classObj |
		#SIFMod.		"Chged SQ"
	classObj := Smalltalk at: (self objectName asSymbol).

	self superclassName: classObj superclass name asString.
	self indexedInstanceVariablesType: classObj indexedInstanceVariablesType.
	self instanceVariableNames: classObj instanceVariablesString. 	"Chg SQ"
	self classVariableNames: classObj classVariablesString.
	self sharedPoolNames: classObj sharedPoolsString.
	self classInstanceVariableNames: classObj class instanceVariablesString.

	"Update the category annotation."
	classCategory := classObj category.	"Chg SQ"
	classCategory = ClassOrganizer default ifTrue: [
		self removeAnnotationAt: SIF categoryKey.
	] ifFalse: [
		self
			annotationAt: SIF categoryKey
			put: classCategory.
	].

	"Update the class comment."
	classObj comment isEmpty ifTrue: [
		self removeAnnotationAt: SIF commentKey.
	] ifFalse: [
		self
			annotationAt: SIF commentKey
			put: classObj comment.
	].! !


!GlobalDefinitionElement commentStamp: 'RAH 9/28/1999 08:44' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.10 Protocol: <GlobalDefinitionElement>

Conforms To
	<ObjectDefinitionElement>

Description
	Class GlobalDefinitionElement implements the behavior of a Smalltalk Interchange Format section <globalDefinition>, a global definition program interchange elements that specify the Smalltalk global object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a GlobalDefinitionElement.

Instance Variables:

	isConstant	<boolean>	Global definition program element flag specifying a constant (true) or variable (false) global. 
!

!GlobalDefinitionElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:23'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self objectName printOn: targetStream.
	targetStream nextPutAll: ' Global'.
	self isConstant ifTrue: [
		targetStream nextPutAll: ' Constant'.
	] ifFalse: [
		targetStream nextPutAll: ' Variable'.
	].
	targetStream nextPutAll: ' Definition'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!GlobalDefinitionElement methodsFor: 'accessing' stamp: 'RAH 9/21/1999 05:35'!
isConstant: isConstantBoolean
	"Set the is-constant flag to a <boolean>, isConstantBoolean, whether the receiver specifies a constant or variable global.

Definition: <GlobalDefinitionElement>
Parameters
	aBoolean		<boolean>		captured
"		#SIFMod. "Protocol: <GlobalDefinitionElement>."
	isConstant := isConstantBoolean! !

!GlobalDefinitionElement methodsFor: 'testing' stamp: 'RAH 9/21/1999 05:36'!
isConstant
	"Answer a <boolean>, true if the receiver specifies a constant global, or false if a variable global.

Definition: <GlobalDefinitionElement>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <GlobalDefinitionElement>."
	^isConstant! !

!GlobalDefinitionElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:53'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	Smalltalk
		at: (self objectName asSymbol)
		put: nil.! !

!GlobalDefinitionElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:11'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newGlobalDefinitionOf: '.
	self objectName printOn: targetStream.
	targetStream nextPutAll: ' constant: '.
	self isConstant printOn: targetStream.
	targetStream nextPutAll: ')'.! !

!GlobalDefinitionElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutGlobalDefinitionOf: self objectName
		isConstant: self isConstant
		annotations: self annotations! !

!GlobalDefinitionElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:19'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'GlobalDef: ';
		nextPutAll: self objectName.! !


!PoolDefinitionElement commentStamp: 'RAH 9/28/1999 08:45' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.11 Protocol: <PoolDefinitionElement>

Conforms To
	<ObjectDefinitionElement>

Description
	Class PoolDefinitionElement implements the behavior of a Smalltalk Interchange Format section <poolDefinition>, a pool definition program interchange elements that specify the Smalltalk pool object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a PoolDefinitionElement.

Instance Variables:

	None
!

!PoolDefinitionElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:54'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	Smalltalk
		at: (self objectName asSymbol)
		put: Dictionary new.! !

!PoolDefinitionElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:15'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newPoolDefinitionOf: '.
	self objectName printOn: targetStream.
	targetStream nextPutAll: ')'.! !

!PoolDefinitionElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:23'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'PoolDef: ';
		nextPutAll: self objectName.! !

!PoolDefinitionElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutPoolDefinitionOf: self objectName
		annotations: self annotations! !

!PoolDefinitionElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:24'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self objectName printOn: targetStream.
	targetStream nextPutAll: ' Pool Definition'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !


!PoolValueDefinitionElement commentStamp: 'RAH 9/28/1999 08:46' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.12 Protocol: <PoolValueDefinitionElement>

Conforms To
	<ElementDefinition>

Description
	Class PoolValueDefinitionElement implements the behavior of a Smalltalk Interchange Format section <poolVariableDefinition>, a pool value definition program interchange elements that specifies the Smalltalk pool value object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a PoolValueDefinitionElement.

Instance Variables:

	isConstant	<boolean>		Pool value definition program element flag specifying a constant (true) or variable (false) pool value. 

	poolName	<readableString>	Pool value definition program element pool name.

	poolValueName	<readableString>	Pool value definition program element pool value name. 
!

!PoolValueDefinitionElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:24'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self poolName printOn: targetStream.
	targetStream nextPutAll: ' Pool '.
	self poolValueName printOn: targetStream.
	self isConstant ifTrue: [
		targetStream nextPutAll: ' Constant'.
	] ifFalse: [
		targetStream nextPutAll: ' Variable'.
	].
	targetStream nextPutAll: ' Definition'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!PoolValueDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/28/1999 06:53'!
isConstant: isConstantBoolean
	"Set the is-constant flag to a <boolean>, isConstantBoolean, whether the receiver specifies a constant or variable pool value.

Definition: <PoolValueDefinitionElement>
Parameters
	aBoolean		<boolean>		captured
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	isConstant := isConstantBoolean
! !

!PoolValueDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/28/1999 06:59'!
poolName
	"Answer a <readableString>, the receiver's pool name.

Definition: <PoolValueDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	^poolName
! !

!PoolValueDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/28/1999 06:58'!
poolName: nameString
	"Set the receiver's pool name to nameString, a <readableString>.

Definition: <PoolValueDefinitionElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	poolName := nameString
! !

!PoolValueDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/28/1999 07:01'!
poolValueName
	"Answer a <readableString>, the receiver's pool value name.

Definition: <PoolValueDefinitionElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	^poolValueName! !

!PoolValueDefinitionElement methodsFor: 'accessing' stamp: 'RAH 8/28/1999 07:02'!
poolValueName: nameString
	"Set the receiver's pool value name to nameString, a <readableString>.

Definition: <PoolValueDefinitionElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	poolValueName := nameString! !

!PoolValueDefinitionElement methodsFor: 'testing' stamp: 'RAH 9/21/1999 05:47'!
isConstant
	"Answer a <boolean>, true if the receiver specifies a constant pool value, or false if a variable pool value.

Definition: <PoolValueDefinitionElement>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <PoolValueDefinitionElement>."
	^isConstant
! !

!PoolValueDefinitionElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:46'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
Errors
	Pool is not defined.
"	| poolNameSymbol |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	poolNameSymbol := self poolName asSymbol.
	Smalltalk
		at: poolNameSymbol
		ifAbsent: [
			^self errorMessage: ('Pool "', self poolName, '" not defined.').
		].
	(Smalltalk at: poolNameSymbol)
		at: self poolValueName
		put: nil.! !

!PoolValueDefinitionElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutPoolValueDefinitionOf: self poolValueName
		in: self poolName
		isConstant: self isConstant
		annotations: self annotations! !

!PoolValueDefinitionElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:15'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newPoolValueDefinitionOf: '.
	self poolValueName printOn: targetStream.
	targetStream nextPutAll: ' in: '.
	self poolName printOn: targetStream.
	targetStream nextPutAll: ')'.! !

!PoolValueDefinitionElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:23'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'PoolValueDef: ';
		nextPutAll: self poolValueName;
		nextPutAll: ' in: ';
		nextPutAll: self poolName.! !


!PoolValueInitializerElement commentStamp: 'RAH 9/28/1999 08:39' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <PoolValueInitializerElement>

Conforms To
	<InitializerElement>

Description
	Class PoolValueInitializerElement implements the behavior of a Smalltalk Interchange Format section <poolValueInitialization>, a pool value initializer program interchange elements that initialize the Smalltalk pool value object they represent.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a PoolValueInitializerElement.

Instance Variables:

	poolName	<readableString>	Pool value initializer program element pool name.

	poolValueName	<readableString>	Pool value initializer program element pool value name. 
!

!PoolValueInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:45'!
poolName
	"Answer a <readableString>, the receiver's pool name.

Definition: <PoolValueInitializerElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <PoolValueInitializerElement>."
	^poolName
! !

!PoolValueInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:46'!
poolName: nameString
	"Set the receiver's pool name to nameString, a <readableString>.

Definition: <PoolValueInitializerElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <PoolValueInitializerElement>."
	poolName := nameString
! !

!PoolValueInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:47'!
poolValueName
	"Answer a <readableString>, the receiver's pool value name.

Definition: <PoolValueInitializerElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <PoolValueInitializerElement>."
	^poolValueName! !

!PoolValueInitializerElement methodsFor: 'accessing' stamp: 'RAH 8/27/1999 08:48'!
poolValueName: nameString
	"Set the receiver's pool value name to nameString, a <readableString>.

Definition: <PoolValueInitializerElement>
Parameters
	nameString	<readableString>	captured
"		#SIFMod. "Protocol: <PoolValueInitializerElement>."
	poolValueName := nameString! !

!PoolValueInitializerElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:51'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
Errors
	Pool is not defined.
	Native initialization code evaluation errors.
"	| poolNameSymbol poolValueValue |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	poolNameSymbol := self poolName asSymbol.
	Smalltalk
		at: poolNameSymbol
		ifAbsent: [
			^self errorMessage: ('Pool "', self poolName, '" not defined.').
		].

	poolValueValue := [ Compiler
		evaluate: self source
		logged: true
	] on: (Error, PGM compilerNotification) do: [ :except |
		^self errorMessage: ('Error evaluating pool value initializer of: "',
			self poolValueName, '" in: "', self poolName, '" - ',
			except description).
	].
	(Smalltalk at: poolNameSymbol)
		at: self poolValueName
		put: poolValueValue.! !

!PoolValueInitializerElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:24'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self poolName printOn: targetStream.
	targetStream nextPutAll: ' Pool '.
	self poolValueName printOn: targetStream.
	targetStream nextPutAll: ' Value Initializer'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!PoolValueInitializerElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:16'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newPoolValue: '.
	self poolValueName printOn: targetStream.
	targetStream nextPutAll: ' in: '.
	self poolName printOn: targetStream.
	targetStream nextPutAll: ' initializer: '.
	self source storeOn: targetStream.
	targetStream nextPutAll: ')'.! !

!PoolValueInitializerElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:38'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutPoolValueInitializerOf: self poolValueName
		in: self poolName
		initializer: self source
		annotations: self annotations! !

!PoolValueInitializerElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:24'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'PoolValueInit: ';
		nextPutAll: self poolValueName;
		nextPutAll: ' in: ';
		nextPutAll: self poolName.! !


!ProgramCommentElement commentStamp: 'RAH 9/28/1999 08:46' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.13 Protocol: <ProgramCommentElement>

Conforms To
	<ElementDefinition>

Description
	Class ProgramCommentElement implements the behavior of a Smalltalk Interchange Format section comment, program interchange comment elements.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ProgramCommentElement.

Instance Variables:

	comment	<readableString>	Program comment element content. 
!

!ProgramCommentElement methodsFor: 'accessing' stamp: 'RAH 8/29/1999 09:29'!
comment
	"Answer a <readableString>, the receiver's comment.

Definition: <ProgramCommentElement>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ProgramCommentElement>."
	^comment! !

!ProgramCommentElement methodsFor: 'accessing' stamp: 'RAH 8/29/1999 09:35'!
comment: commentString
	"Set the receiver's comment to commentString, a <readableString>.

Definition: <ProgramCommentElement>
Parameters
	commentString	<readableString>	captured
Errors
	commentString is not bracketed with double quote characters.
"	| tmpString |
		#SIFMod. "Protocol: <ProgramCommentElement>."
	(commentString isKindOf: String) ifFalse: [
		self errorMessage: 'Not a String.'.
	].
	tmpString := commentString trimBlanks.
	tmpString size >= 2 ifTrue: [
		(tmpString first = $" and: [tmpString last = $"]) ifFalse: [
			^self error: 'Not bracketed with ".'.
		].
		((tmpString
			copyFrom: 2
			to: (tmpString size - 1)
		) includes: $") ifTrue: [
			^self error: 'contains internal ".'.
	]	].

	comment := commentString
! !

!ProgramCommentElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:26'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ProgramComment'.! !

!ProgramCommentElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:26'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: self comment.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!ProgramCommentElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:54'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."! !

!ProgramCommentElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:18'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newProgramComment: '.
	self comment storeOn: targetStream.
	targetStream nextPutAll: ')'.! !

!ProgramCommentElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:38'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutProgramComment: self comment
		annotations: self annotations! !


!ProgramComponentSet commentStamp: 'RAH 9/28/1999 08:52' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.14 Protocol: <ProgramComponentSet>

Conforms To
	<ProgramAbstractComponent>

Description
	Class ProgramComponentSet is an abstract class implementing the common behavior of all node (composite) component program elements containing collections of leaf or sub-node components.  Each concrete subclass allows a specific kind of group to be handled collectively.
	The concrete subclass, ProgramDefinitionSet, corresponds to all or part of a <<Smalltalk program>> from the section Smalltalk Abstract Program Grammar.  The other concrete subclasses enable component grouping.  They are implementation enhancements to the Smalltalk Interchange Format (SIF) and are filed in and out embedded in program comments so as not to break other implementations of SIF.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ProgramComponentSet.

Instance Variables:

	components	<OrderedCollection>	A collection of program elements or sub-component components. 
!

!ProgramComponentSet methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:11'!
clearMessages

"Definition: <ProgramAbstractComponent>
Refinement: <ProgramComponentSet>
	Clear the receiver's and it's components messages.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self resetMessage.
	self components do: [ :component |
		component clearMessages
	].
	self triggerProgramChanged.
! !

!ProgramComponentSet methodsFor: 'accessing' stamp: 'RAH 8/22/1999 09:33'!
components

"Definition: <ProgramAbstractComponent>
Return Values
	<OrderedCollection>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^components
! !

!ProgramComponentSet methodsFor: 'accessing' stamp: 'RAH 8/24/1999 06:52'!
messages

"Definition: <ProgramAbstractComponent>
Refinement: <ProgramComponentSet>
	Override simply retrieving the receiver's messages to instead answer the set's messages plus its components' messages.
Return Values
	<collection>	state
	<nil>		unspecified
"	| collection |
		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	collection := super messages.
	self components do: [ :aComponent |
			collection addAll: aComponent messages
	].
	^collection! !

!ProgramComponentSet methodsFor: 'accessing' stamp: 'RAH 8/29/1999 09:23'!
setName
	"Answer an <readableString>, the name of the receiver.

Definition: <ProgramComponentSet>
Return Values
	<Object>		state
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	^self subclassResponsibility! !

!ProgramComponentSet methodsFor: 'adding' stamp: 'RAH 8/29/1999 09:07'!
addAllComponents: componentCollection
	"Answer a <collection>, componentCollection, after adding each of its components to the receiver as a child.

Definition: <ProgramComponentSet>
Parameters
	componentCollection	<collection>	unspecified
Return Values
	<collection>		unspecified
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	components addAll: componentCollection.
	componentCollection do: [ :component |
		component parentSetOrNil: self.
	
		"Inform dependents"
		self	triggerAddedCmponent: component	"Chg SQ"
			toParent: self.
	].
	^componentCollection! !

!ProgramComponentSet methodsFor: 'adding' stamp: 'RAH 8/29/1999 09:08'!
addComponent: newComponent
	"Answer a <ProgramAbstractComponent>, newComponent, after adding it to the receiver as a child.

Definition: <ProgramComponentSet>
Parameters
	newComponent		<ProgramAbstractComponent>	captured
Return Values
	<ProgramAbstractComponent>		captured
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	newComponent parentSetOrNil: self.
	self components add: newComponent.

	"Inform dependents"
	self	triggerAddedCmponent: newComponent	"Chg SQ"
		toParent: self.

	^newComponent! !

!ProgramComponentSet methodsFor: 'adding' stamp: 'RAH 8/29/1999 09:08'!
addComponent: newComponent after: targetComponent
	"Answer a <ProgramAbstractComponent>, newComponent, after adding it after targetComponent, to the receiver as a child.

Definition: <ProgramComponentSet>
Parameters
	newComponent		<ProgramAbstractComponent>	captured
Return Values
	<ProgramAbstractComponent>		captured
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	newComponent parentSetOrNil: self.
	components
		add: newComponent
		after: targetComponent.

	"Inform dependents"
	self	triggerAddedCmponent: newComponent	"Chg SQ"
		toParent: self.

	^newComponent
! !

!ProgramComponentSet methodsFor: 'adding' stamp: 'RAH 8/29/1999 09:10'!
addComponent: newComponent before: targetComponent
	"Answer a <ProgramAbstractComponent>, newComponent, after adding it before targetComponent, to the receiver as a child.

Definition: <ProgramComponentSet>
Parameters
	newComponent		<ProgramAbstractComponent>	captured
Return Values
	<ProgramAbstractComponent>		captured
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	newComponent parentSetOrNil: self.
	components
		add: newComponent
		before: targetComponent.

	"Inform dependents"
	self	triggerAddedCmponent: newComponent	"Chg SQ"
		toParent: self.
! !

!ProgramComponentSet methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:55'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self components do: [ :aComponent |
		aComponent evaluateObjects.
	].! !

!ProgramComponentSet methodsFor: 'enumerating' stamp: 'RAH 8/22/1999 10:55'!
endOrderDo: operationBlock

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self components do: [:aComponent |
		aComponent endOrderDo: operationBlock
	].
	operationBlock value: self.
	^self
! !

!ProgramComponentSet methodsFor: 'enumerating' stamp: 'RAH 8/24/1999 08:10'!
preOrderDo: operationBlock

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	operationBlock value: self.
	self components do: [:aComponent |
		aComponent preOrderDo: operationBlock
	].
	^self
! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:06'!
addClass: classNameString classMethod: methodNameString
	"Answer a <MethodDefinitionElement>, a new class method definition program element for class method named methodNameString in the class named classNameString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	classNameString	<readableString>	captured
	methodNameString	<readableString>	captured
Return Values
	<MethodDefinitionElement>		state
Errors
	Class named classNameString does not exist in the SDE environment.
	Class method named methodNameString does not exist in class named classNameString.
"	| component classObj |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	classObj := Smalltalk
		at: classNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Class - "', classNameString, '" doesn''t exist.'
		].
	(classObj class includesSelector: methodNameString asSymbol) ifFalse: [
		^self fatalMessage: '"', classNameString, ' class>>#', methodNameString,
					'" doesn''t exist.'
	].
	component := PGM
		newClassMethodDefinitionOf: methodNameString
		in: classNameString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:07'!
addClass: classNameString initializer: initializerString
	"Answer a <ClassInitializerElement>, a new class initializer program element for class named classNameString with initialization code, initializerString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	classNameString		<readableString>	captured
	initializerString	<readableString>	captured
Return Values
	<ClassInitializerElement>		state
Errors
	Class named classNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: classNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Class - "', classNameString, '" doesn''t exist.'
		].
	component := PGM
		newClass: classNameString
		initializer: initializerString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:08'!
addClass: classNameString method: methodNameString
	"Answer a <MethodDefinitionElement>, a new instance method definition program element for instance method named methodNameString in the class named classNameString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	classNameString	<readableString>	captured
	methodNameString	<readableString>	captured
Return Values
	<MethodDefinitionElement>		state
Errors
	Class named classNameString does not exist in the SDE environment.
	Instance method named methodNameString does not exist in class classNameString.
"	| component classObj |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	classObj := Smalltalk
		at: classNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Class - "', classNameString, '" doesn''t exist.'
		].
	(classObj includesSelector: methodNameString asSymbol) ifFalse: [
		^self fatalMessage: '"', classNameString, '>>#', methodNameString,
					'" doesn''t exist.'
	].
	component := PGM
		newInstanceMethodDefinitionOf: methodNameString
		in: classNameString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 9/21/1999 05:54'!
addClassAll: classNameString
	"Answer a <ClassAllSet>, a new class all grouping set component containing all necessary program elements for the entire class named classNameString after adding component to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	classNameString		<readableString>	captured
Return Values
	<ClassAllSet>		state
Errors
	Class named classNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: classNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Class - "', classNameString, '" doesn''t exist.'
		].
	component := PGM newClassAllSet: classNameString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:09'!
addClassDef: classNameString
	"Answer a <ClassDefinitionElement>, a new class definition program element for class named classNameString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	classNameString		<readableString>	captured
Return Values
	<ClassDefinitionElement>		state
Errors
	Class named classNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: classNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Class - "', classNameString, '" doesn''t exist.'
		].
	component := PGM
		newClassDefinitionOf: classNameString
		superclass: nil
		indexedInstanceVariables: nil
		instanceVariableNames: nil
		classVariableNames: nil
		sharedPools: nil
		classInstanceVariableNames: nil.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 06:42'!
addComment: commentString
	"Answer a <ProgramCommentElement>, a new program comment element with content, commentString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	commentString		<readableString>	captured
Return Values
	<ProgramCommentElement>		state
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	component := PGM newProgramComment: commentString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:10'!
addGlobal: globalNameString initializer: initializerString
	"Answer a <GlobalInitializerElement>, a new global initializer program element for global named globalNameString with initialization code, initializerString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	globalNameString		<readableString>	captured
	initializerString	<readableString>	captured
Return Values
	<GlobalInitializerElement>		state
Errors
	Global named globalNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: globalNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Global - "', globalNameString, '" doesn''t exist.'
		].
	component := PGM
		newGlobal: globalNameString
		initializer: initializerString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:11'!
addGlobalDef: globalNameString constant: isConstantBoolean
	"Answer a <GlobalDefinitionElement>, a new global constant or variable definition program element for global named globalNameString with constant indicator set to isConstantBoolean, after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	globalNameString		<readableString>	captured
	isConstantBoolean	<boolean>		captured
Return Values
	<GlobalDefinitionElement>		state
Errors
	Global named globalNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: globalNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Global - "', globalNameString, '" doesn''t exist.'
		].
	component := PGM
		newGlobalDefinitionOf: globalNameString
		constant: isConstantBoolean.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 9/21/1999 05:58'!
addGrouping: groupingNameString
	"Answer a <GroupingSet>, a new grouping set component named groupingNameString containing program elements, after adding component to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	groupingNameString		<readableString>	captured
Return Values
	<GroupingSet>		state
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	component := PGM newGroupingSet: groupingNameString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:18'!
addPool: poolNameString val: poolValueNameString initializer: initializerString
	"Answer a <PoolValueInitializerElement>, a new pool value initialization program element for the pool value named, poolValueNameString, in the pool named, poolNameString, with initialization code, initializerString, after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	poolNameString		<readableString>	captured
	poolValueNameString	<readableString>	captured
	initializerString	<readableString>	captured
Return Values
	<PoolValueInitializerElement>		state
Errors
	Pool named poolNameString does not exist in the SDE environment.
	Pool value named poolValueNameString does not exist in pool named poolNameString.
"	| component poolObject |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	poolObject := Smalltalk
		at: poolNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Pool - "', poolNameString, '" doesn''t exist.'
		].
	(poolObject includesKey: poolValueNameString) ifFalse: [
		^self fatalMessage: 'Pool - "', poolNameString, ' value - "', poolNameString,
			'" doesn''t exist.'
	].
	component := PGM
		newPoolValue: poolValueNameString
		in: poolNameString
		initializer: initializerString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:43'!
addPool: poolNameString valDef: poolValueNameString constant: isConstantBoolean
	"Answer a <PoolValueDefinitionElement>, a new pool value definition program element for the pool value named, poolValueNameString, in the pool named, poolNameString, and constant indicator set to isConstantBoolean, after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	poolNameString		<readableString>	captured
	poolValueNameString	<readableString>	captured
	isConstantBoolean	<boolean>		captured
Return Values
	<PoolValueDefinitionElement>		state
Errors
	Pool named poolNameString does not exist in the SDE environment.
	Pool value named poolValueNameString does not exist in pool named poolNameString.
"	| component poolObject |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	poolObject := Smalltalk
		at: poolNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Pool - "', poolNameString, '" doesn''t exist.'
		].
	(poolObject includesKey: poolValueNameString) ifFalse: [
		^self fatalMessage: 'Pool - "', poolNameString, ' value - "', poolNameString,
			'" doesn''t exist.'
	].
	component := PGM
		newPoolValueDefinitionOf: poolValueNameString
		in: poolNameString
		constant: isConstantBoolean.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:46'!
addPoolDef: poolNameString 
	"Answer a <PoolValueDefinitionElement>, a new pool definition program element for pool named poolNameString after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	poolNameString		<readableString>	captured
Return Values
	<PoolValueDefinitionElement>		state
Errors
	Pool named poolNameString does not exist in the SDE environment.
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	Smalltalk
		at: poolNameString asSymbol
		ifAbsent: [
			^self fatalMessage: 'Pool - "', poolNameString, '" doesn''t exist.'
		].
	component := PGM newPoolDefinitionOf: poolNameString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'hand editing' stamp: 'RAH 8/29/1999 08:50'!
addProgramInitializer: initializerString
	"Answer a <ProgramInitializerElement>, a new program initialization element with initialization code, initializerString, after adding it to the receiver.

Definition: <ProgramComponentSet>
	Note: This message is intended to be used only in hand editing a program.
Parameters
	initializerString		<readableString>	captured
Return Values
	<ProgramInitializerElement>		state
"	| component |
		#SIFMod. "Protocol: <ProgramComponentSet>."
	component := PGM newProgramInitializer: initializerString.
	self addComponent: component.
	^component! !

!ProgramComponentSet methodsFor: 'initializing' stamp: 'RAH 07/08/1999 01:24:51'!
initialize
	"Private - Initialize the state of the receiver."
		#SIFMod.
	super initialize.
	components := self class defaultComponentCollection.! !

!ProgramComponentSet methodsFor: 'private' stamp: 'RAH 07/08/1999 01:24:51'!
reinitializeComponents
	"Private - Reinitialize the components to an empty collection."
		#SIFMod.
	components := self class defaultComponentCollection.
! !

!ProgramComponentSet methodsFor: 'removing' stamp: 'RAH 8/29/1999 09:19'!
removeComponent: component
	"Answer a <ProgramAbstractComponent>, component, after removing it from the receiver as a child.

Definition: <ProgramComponentSet>
Parameters
	component		<ProgramAbstractComponent>	captured
Return Values
	<ProgramAbstractComponent>		captured
"		#SIFMod. "Protocol: <ProgramComponentSet>."
"trigger does not like this->	component parent: nil."
	components remove: component.

	"Inform dependents"
	self	triggerRemovedCmponent: component		"Chg SQ"
		fromParent: self.

	^component! !

!ProgramComponentSet methodsFor: 'testing' stamp: 'RAH 9/21/1999 06:02'!
containsErrors

"Definition: <ProgramAbstractComponent>
Refinement: <ProgramComponentSet>
	Answer a <boolean>, true if the receiver or it's components contain an error, false if no errors.
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self components do: [ :component |
		component containsErrors ifTrue: [
			^true
	]	].
	^false! !

!ProgramComponentSet methodsFor: 'testing' stamp: 'RAH 8/24/1999 06:16'!
hasSubcomponents

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	^components size > 0! !

!ProgramComponentSet methodsFor: 'testing' stamp: 'RAH 8/24/1999 06:33'!
isComponentSet

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <abstractProgramComponent>."
	^true
! !


!GroupingSet commentStamp: 'RAH 9/28/1999 08:53' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.15 Protocol: <GroupingSet>

Conforms To
	<ProgramComponentSet>

Description
	Class GroupingSet is an abstract class implementing the common behavior of node (composite) component program element grouping.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a GroupingSet.

Instance Variables:

	groupingName	<readableString>	The grouping name. 
!

!GroupingSet methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:13'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '((PGM new', self class name, ': '.
	self groupingName printOn: targetStream.
	targetStream nextPutAll: ')'.
	(self isMemberOf: GroupingSet) ifTrue: [
		targetStream cr.
		self components do: [ :component |
			targetStream nextPutAll: 'addComponent: '.
			component storeOn: targetStream.
		].
		targetStream nextPutAll: 'yourself)';
			cr.
	] ifFalse: [
		targetStream nextPutAll: ')';
			cr.
	].! !

!GroupingSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutGroupingSetDefinition: self groupingName
		annotations: self annotations.

	self components do: [ :aComponent |
		aComponent fileOutOnSIFFiler: programFiler.
	].

	programFiler fileOutSetEnd: self groupingName.! !

!GroupingSet methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:21'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'GroupingSet: ';
		nextPutAll: self groupingName.! !

!GroupingSet methodsFor: 'accessing' stamp: 'RAH 8/29/1999 10:28'!
groupingName
	"Answer a <readableString>, the receiver's grouping name.

Definition: <GroupingSet>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <GroupingSet>."
	^groupingName
! !

!GroupingSet methodsFor: 'accessing' stamp: 'RAH 8/29/1999 10:28'!
groupingName: nameString
	"Set the receiver's grouping name to nameString, a <readableString>.

Definition: <GroupingSet>
Parameters
	nameString	<readableString>		captured
"		#SIFMod. "Protocol: <GroupingSet>."
	groupingName := nameString! !

!GroupingSet methodsFor: 'accessing' stamp: 'RAH 8/29/1999 09:25'!
setName

"Definition: <ProgramComponentSet>
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	^self groupingName! !

!GroupingSet methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:23'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self groupingName printOn: targetStream.
	targetStream nextPutAll: ' Grouping'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!GroupingSet methodsFor: 'private' stamp: 'RAH 9/21/1999 06:07'!
ensurePoolDefined: poolNameString
	"Private - Answer an <Object>, the pool object, after ensuring the program contains a pool definition component.  If the pool definition component does not exist, create it.

Errors
	The pool object does not exist in the SDE environment.
"	| component poolObj |
		#SIFMod.
	poolObj := Smalltalk
		at: poolNameString asSymbol
		ifAbsent: [
			^self errorMessage: ('Pool "', poolNameString,
					'" not defined.').
		].

	#todoSIFMod."This should check prerequisite programs for pool definition."
	self program
		detect: [ :tmpComponent |
			(tmpComponent isMemberOf: PGM poolDefinitionElement)
			and: [tmpComponent objectName = poolNameString]
		]
		ifNone: [
			component := PGM newPoolAllSet: poolNameString.
			component createPoolAllComponents.
			self addComponent: component.
		].
	^poolObj
! !


!ClassAllSet commentStamp: 'RAH 9/28/1999 08:54' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.16 Protocol: <ClassAllSet>

Conforms To
	<GroupingSet>

Description
	Class ClassAllSet implements the common behavior of a node (composite) component program element grouping convenient short-cut to add to a program all the components a class may require.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ClassAllSet.

Instance Variables:

	None

Notes:

1. Class And Pool Definitions:

	The prerequisite programs should be checked for class and pool definitions but seemed too involved to implement at this point.

	This also requires that if a class is referenced by other components the definition should be able to be moved to a position earlier in the program but retain an association with the class all grouping.
!

!ClassAllSet methodsFor: 'updating' stamp: 'RAH 8/31/1999 14:51'!
createClassAllComponents
	"Create and add to the receiver all components that are required by an existing class.

Definition: <ClassAllSet>
	Note: This is intended as a convenient short-cut to add to a program all the components a class may require.
Errors
	The class object does not exist in the SDE environment.
	The pool object does not exist in the SDE environment.
"	| classObj selectors |
		#SIFMod. "Protocol: <ClassAllSet>."
	#todoSIFMod."This looses any previous unsupported annotations,
		and should be reworked."
	self reinitializeComponents.

	classObj := self ensureClassDefined.

	#todoSIFMod."This is incredably clumsy and should be reworked."
	classObj sharedPoolsString subStrings do: [ :poolNameString | "Chged SQ"
		self ensurePoolDefined: poolNameString.
	].

	"Define all class method."
	selectors := classObj class selectors.
	selectors do: [ :classMethodSelector |
		self addComponent: (PGM
				newClassMethodDefinitionOf: classMethodSelector asString
				in: self groupingName).
	].

	"Define all instance method."
	selectors := classObj selectors.
	selectors do: [ :classMethodSelector |
		self addComponent: (PGM
				newInstanceMethodDefinitionOf: classMethodSelector asString
				in: self groupingName).
	].! !

!ClassAllSet methodsFor: 'private' stamp: 'RAH 9/21/1999 06:11'!
ensureClassDefined
	"Private - Answer an <Object class>, the receiver's class' class object, after ensuring the program contains a class definition component.  If the class definition component does not exist, create it.

Errors
	The class object does not exist in the SDE environment.
"
	| classObj |
		#SIFMod.
	classObj := Smalltalk
		at: (self groupingName asSymbol)
		ifAbsent: [
			^self errorMessage: ('Class "', self groupingName,
					'" not defined.').
		].
	"May be an early definition."
	#todoSIFMod."This should check prerequisite programs for class definition."
	self program
		detect: [ :component |
			(component isMemberOf: PGM classDefinitionElement)
			and: [component objectName = self groupingName]
		]
		ifNone: [
			"It will be updated when filed out."
			self addComponent: (PGM
				newClassDefinitionOf: self groupingName
				superclass: nil
				indexedInstanceVariables: nil
				instanceVariableNames: nil
				classVariableNames: nil
				sharedPools: nil
				classInstanceVariableNames: nil).
		].
	^classObj! !

!ClassAllSet methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:22'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self groupingName printOn: targetStream.
	targetStream nextPutAll: ' Class All'.! !

!ClassAllSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/30/1999 05:17'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	"The components may differ from the previous specification (previous file in/out),
	so recreate the components."
	"???Not required in declarative model SDE (I think),
		or if the components and the previous specification are kept in sync."
	self createClassAllComponents.

	programFiler
		fileOutClassAllSetDefinition: self groupingName
		annotations: self annotations.
	self components do: [ :aComponent |
		aComponent fileOutOnSIFFiler: programFiler.
	].
	programFiler fileOutSetEnd: self groupingName.! !

!ClassAllSet methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:14'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ClassAllSet: ';
		nextPutAll: self groupingName.! !


!GlobalAllSet commentStamp: 'RAH 9/28/1999 08:55' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.17 Protocol: <GlobalAllSet>

Conforms To
	<GroupingSet>

Description
	Class GlobalAllSet implements the common behavior of a node (composite) component program element grouping convenient short-cut to add to a program all the components a class may require.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a GlobalAllSet.

Instance Variables:

	None

Notes:

1. Global Definitions:

	The prerequisite programs should be checked for the global definition but seemed too involved to implement at this point.
!

!GlobalAllSet methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:18'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'GlobalAllSet: ';
		nextPutAll: self groupingName.! !

!GlobalAllSet methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:23'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self groupingName printOn: targetStream.
	targetStream nextPutAll: ' Global All'.! !

!GlobalAllSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:36'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutGlobalAllSetDefinition: self groupingName
		annotations: self annotations.

	self components do: [ :aComponent |
		aComponent fileOutOnSIFFiler: programFiler.
	].

	programFiler fileOutSetEnd: self groupingName.! !

!GlobalAllSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 06:13'!
createGlobalAllComponents: constantBoolean
	"Create and add to the receiver all components that are required by an existing global.

Definition: <GlobalAllSet>
	Note: This is intended as a convenient short-cut to add to a program all the components a global may require.
"		#SIFMod. "Protocol: <GlobalAllSet>."
	#todoSIFMod."This should check prerequisite programs for global definition."
	self program
		detect: [ :component |
			(component isMemberOf: PGM globalDefinitionElement)
			and: [component objectName = self groupingName]
		]
		ifNone: [
			self addComponent: (PGM
				newGlobalDefinitionOf: self groupingName
				constant: constantBoolean).
		].

	self program
		detect: [ :component |
			(component isMemberOf: PGM globalInitializerElement)
			and: [component globalName = self groupingName]
		]
		ifNone: [
			self addComponent: (PGM
			newGlobal: self groupingName
			initializer: String new).
		].
! !


!PoolAllSet commentStamp: 'RAH 9/28/1999 08:56' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.17 Protocol: <PoolAllSet>

Conforms To
	<GroupingSet>

Description
	Class PoolAllSet implements the common behavior of a node (composite) component program element grouping convenient short-cut to add to a program all the components a pool may require.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a PoolAllSet.

Instance Variables:

	None

Notes:

1. Pool Definitions:

	The prerequisite programs should be checked for the pool definition but seemed too involved to implement at this point.
!

!PoolAllSet methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:22'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'PoolAllSet: ';
		nextPutAll: self groupingName.! !

!PoolAllSet methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:24'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self groupingName printOn: targetStream.
	targetStream nextPutAll: ' Pool All'.! !

!PoolAllSet methodsFor: 'updating' stamp: 'RAH 9/24/1999 06:52'!
createPoolAllComponents
	"Create and add to the receiver all components that are required by an existing pool.

Definition: <PoolAllSet>
	Note: This is intended as a convenient short-cut to add to a program all the components a pool may require.
	Pool values initializers are created with their current values as strings.
Errors
	The pool object does not exist in the SDE environment.
"	| isConstant poolObj poolValueNameString | 
		#SIFMod. "Protocol: <PoolAllSet>."
	#todoSIFMod."This is incredibly clumsy and should be reworked."
	poolObj := self ensurePoolDefined: self groupingName.

	isConstant := self portFunc promptYesNo: ('Define all of pool "',
			self groupingName, '" pool values as constants?').

	poolObj keysAndValuesDo: [ :aKey :aValue |
		poolValueNameString := aKey asString.
		self addComponent: (PGM
				newPoolValueDefinitionOf: poolValueNameString
				in: self groupingName
				constant: isConstant).

		self addComponent: (PGM
			newPoolValue: poolValueNameString
			in: self groupingName
			initializer: aValue printString).
	].
! !

!PoolAllSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:37'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutPoolAllSetDefinition: self groupingName
		annotations: self annotations.

	self components do: [ :aComponent |
		aComponent fileOutOnSIFFiler: programFiler.
	].

	programFiler fileOutSetEnd: self groupingName.! !


!ProgramDefinitionSet commentStamp: 'RAH 9/28/1999 08:59' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.18 Protocol: <ProgramDefinitionSet>

Conforms To
	<ProgramComponentSet>

Description
	Class ProgramDefinitionSet implements the behavior of the program node (composite) component grouping that corresponds to all or part of a <<Smalltalk program>> from the section Smalltalk Abstract Program Grammar.
	A program is initially created in a program editor by adding program elements.  It can then be filed out in Smalltalk Interchange Format (SIF), or evaluated to create or initialize the Smalltalk objects in the environment.  A program is also created when a SIF file is installed (filed in and evaluated), or loaded (filed in but not evaluated).
	Program hand editing messages are also available for creating a program through Smalltalk code when the program editor is not sufficient or convenient.
	Programs are managed (created, removed, edited, etc.) by the program manager through the program browser.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ProgramDefinitionSet.

Instance Variables:

	fileLocator	<readableString>	The associated file drive, path, name, and extension of the program.  It may be an empty string.

	hasDescription	<boolean>		The flag indicating whether the program was loaded from a file that contained a program description comment.

	noteUnhandledAnnotations <boolean>	The flag indicating whether the program should note unhandled annotations.

	prerequisiteProgramNames	<Set>	The list of the name strings of prerequisite programs required by the program.  It may be an empty string.

	programName	<readableString>	The program name.

	wasEvaluated	<boolean>		The flag indicating whether the program was evaluated.


Implementation:

	This is a transitional implementation that I hope will serve as a bridge between a Smalltalk development environment (SDE) that assumes an imperative model and one that assumes a declarative model.
	I picture a fully declarative SDE as presenting a program editor for defining Smalltalk objects in terms of their components, or their initializers that the SDE then creates.  It presents component source in ANSI format and may allow text formatting.  The source is viewed as a stack of version with a granularity at the sub-component level (one can add, change, or delete versions at the method level).
!

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 09:09'!
addPrerequisiteProgramName: programNameString
	"Answer a <readableString>, the name of a program after adding it to the list of prerequisites of the receiver.

Definition: <ProgramDefinitionSet>
Parameters
	programNameString	<readableString>		captured
Return Values
	<readableString>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	prerequisiteProgramNames isNil ifTrue: [
		prerequisiteProgramNames := self class defaultPrerequisiteProgramNameCollection
	].
	prerequisiteProgramNames add: programNameString.

	self isChanged: true.

	^programNameString! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 09:41'!
fileLocator
	"Answer a <readableString>, the file drive, path, name, and extension.

Definition: <ProgramDefinitionSet>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^fileLocator
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 09:43'!
fileLocator: fileDrivePathNameExtString
	"Set the receiver's file drive, path, name, and extension to fileDrivePathNameExtString, a <readableString>.

Definition: <ProgramDefinitionSet>
Parameters
	nameString	<readableString>		captured
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	fileLocator := fileDrivePathNameExtString
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 09:52'!
hasDescription
	"Answer a <boolean>, true if the receiver was loaded from a file that contained a program description comment.

Definition: <ProgramDefinitionSet>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^hasDescription
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 09:54'!
hasDescription: hasDescriptionBoolean
	"Set the receiver's hasDescription flag to hasDescriptionBoolean indicating whether the receiver was loaded from a file that contained a program description comment.

Definition: <ProgramDefinitionSet>
Parameters
	hasDescriptionBoolean	<boolean>	captured
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	hasDescription := hasDescriptionBoolean
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 11:27'!
noteUnhandledAnnotations
	"Answer a <boolean>, true if the receiver should note unhandled annotations, false if not.

Definition: <ProgramDefinitionSet>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^noteUnhandledAnnotations
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 11:30'!
noteUnhandledAnnotations: noteUnhandledBoolean
	"Set the receiver's noteUnhandledAnnotations flag to noteUnhandledBoolean indicating whether the receiver should note unhandled annotations.

Definition: <ProgramDefinitionSet>
Parameters
	noteUnhandledBoolean	<boolean>	captured
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	noteUnhandledAnnotations := noteUnhandledBoolean
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 11:36'!
prerequisiteProgramNames
	"Answer a <Set>, a list of the name strings of prerequisite programs required by the receiver.

Definition: <ProgramDefinitionSet>
Return Values
	<Set>	unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^prerequisiteProgramNames
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 11:39'!
programName
	"Answer a <readableString>, the receiver's program name.

Definition: <ProgramDefinitionSet>
Return Values
	<readableString>		state
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^programName
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 11:40'!
programName: nameString
	"Set the receiver's program name to nameString, a <readableString>.

Definition: <ProgramDefinitionSet>
Parameters
	nameString	<readableString>		captured
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	programName := nameString
! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 8/29/1999 09:25'!
setName

"Definition: <ProgramComponentSet>
"		#SIFMod. "Protocol: <ProgramComponentSet>."
	^self programName! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 12:10'!
wasEvaluated
	"Answer a <boolean>, true if the receiver was evaluated, false if not.

Definition: <ProgramDefinitionSet>
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^wasEvaluated! !

!ProgramDefinitionSet methodsFor: 'accessing' stamp: 'RAH 9/5/1999 12:11'!
wasEvaluated: wasEvaluatedBoolean
	"Set the receiver's wasEvaluated flag to wasEvaluatedBoolean indicating whether the receiver was evaluated.

Definition: <ProgramDefinitionSet>
Parameters
	wasEvaluatedBoolean	<boolean>	captured
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	wasEvaluated := wasEvaluatedBoolean
! !

!ProgramDefinitionSet methodsFor: 'adding' stamp: 'RAH 8/27/1999 05:41'!
add: anObject asChildOf: aParentObject

"Definition: <abstractTreeModel>
"		#SIFMod. "Protocol: <abstractTreeModel>."
	(self includes: anObject) ifTrue: [
		^self error: 'Can''t add - already contains child ', anObject printString
	].
	(self includes: aParentObject) ifFalse: [
		^self error: 'Can''t add - dose not contain parent', aParentObject printString
	].
	aParentObject isNil ifTrue: [
		^self program addComponent: anObject
	].
	^aParentObject addComponent: anObject
! !

!ProgramDefinitionSet methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:26'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: self programName.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
	self wasEvaluated ifFalse: [
		targetStream nextPutAll: ' (not evaluated)'.
	].
	self isChanged ifTrue: [
		targetStream nextPutAll: ' - changed'.
	].! !

!ProgramDefinitionSet methodsFor: 'enumerating' stamp: 'RAH 9/21/1999 07:40'!
componentsReferingToClassNamed: classNameString
	"Answer a <collection>, all components referring to class named classNameString in the receiver or its components.

Definition: <ProgramDefinitionSet>
Parameters
	classNameString	<readableString>	uncaptured
Return Values
	<collection>		new
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^self select: [ :component |
		((component isMemberOf: PGM classInitializerElement)
		and: [component className = classNameString]
		) or: [
			((component isMemberOf: PGM classAllSet)
			and: [component groupingName = classNameString]
			) or: [
				((component isMemberOf: PGM classDefinitionElement)
				and: [component objectName = classNameString]
				) or: [
					(component isMemberOf: PGM methodDefinitionElement)
					and: [component className = classNameString]
	]	]	]	]! !

!ProgramDefinitionSet methodsFor: 'enumerating' stamp: 'RAH 9/21/1999 07:40'!
componentsReferingToGlobalNamed: globalNameString
	"Answer a <collection>, all components referring to global named globalNameString in the receiver or its components.

Definition: <ProgramDefinitionSet>
Parameters
	globalNameString	<readableString>	uncaptured
Return Values
	<collection>		new
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^self select: [ :component |
		((component isMemberOf: PGM globalAllSet)
		and: [component groupingName = globalNameString]
		) or: [
			((component isMemberOf: PGM globalDefinitionElement)
			and: [component objectName = globalNameString]
			) or: [
				(component isMemberOf: PGM globalInitializerElement)
				and: [component globalName = globalNameString]
	]	]	]
! !

!ProgramDefinitionSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 9/27/1999 06:48'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	self fileOutInSIFProgramDescOnFiler: programFiler.
	self components do: [ :component |
		component fileOutOnSIFFiler: programFiler.
	].
! !

!ProgramDefinitionSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 9/24/1999 06:59'!
installProgramComponents
	"Load then evaluate all the program components owned by the receiver.
	Note: This assumes that prerequisite programs are loaded.

Definition: <ProgramDefinitionSet>
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	#todoSIFMod."?have components inform set it has errors,
				?or do it by set checking components." 
	self loadProgramComponents.
	self	noteUnhandledAnnotations: (self portFunc
		promptYesNo: 'Warn on unhandled annotations?').
	self	evaluateInstallUninstallScript: (self annotationAt: PGM preinstallKey)
		type: 'pre-install'.
	self components do: [ :aComponent |
		aComponent evaluateObjects.
	].
	self	evaluateInstallUninstallScript: (self annotationAt: PGM postinstallKey)
		type: 'post-install'.

	self program wasEvaluated: true.
	#todoSIFMod. "Handle errors."
! !

!ProgramDefinitionSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 9/24/1999 07:00'!
loadProgramComponents
	"Load but do NOT evaluate all the program components owned by the receiver.
	Note: This assumes that prerequisite programs are loaded.

Definition: <ProgramDefinitionSet>
"	| aStream |
		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	self portFunc promptInform: 'Loading in ', self programName,
		' program from ', self fileLocator.
	aStream := FileStream read: self fileLocator.
	[	(SIF newReaderOn: aStream)
			loadComponentsIntoProgram: self.
	] ensure: [aStream close]
! !

!ProgramDefinitionSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 9/24/1999 09:01'!
save
	"Save a definition of the receiver and all the components it owns to a single ANSI SIF file stream.
	Note: Cyclic prerequisites must be dealt with by the user.

Definition: <ProgramDefinitionSet>
"	| aFileStream programFiler |
		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	aFileStream := FileStream write: self fileLocator.
	[	programFiler := SIF newWriterOn: aFileStream.
		self fileOutOnSIFFiler: programFiler.
	] ensure: [aFileStream close]
! !

!ProgramDefinitionSet methodsFor: 'filing-ANSI SIF' stamp: 'RAH 9/5/1999 12:08'!
uninstallUnload: removeProgramBlock
	"Uninstall/unload the receiver making sure removeProgramBlock is evaluated afterward.

Definition: <ProgramDefinitionSet>
Parameters
	removeProgramBlock	<niladicBlock>	uncaptured
"	| aStream |
		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	[self
		evaluateInstallUninstallScript: (self annotationAt: PGM preuninstallKey)
		type: 'pre-uninstall'.
	] ensure: [
		removeProgramBlock value.
	].
	self
		evaluateInstallUninstallScript: (self annotationAt: PGM postuninstallKey)
		type: 'post-uninstall'.! !

!ProgramDefinitionSet methodsFor: 'hierarchy' stamp: 'RAH 9/5/1999 09:15'!
childrenOf: anObject

"Definition: <abstractTreeModel>
Refinement: <ProgramAbstractComponent>
	Return only the immediate children.
"		#SIFMod. "Protocol: <abstractTreeModel>."
	anObject hasSubcomponents ifTrue: [
		^anObject components
	].
	^self class defaultComponentCollection
! !

!ProgramDefinitionSet methodsFor: 'hierarchy' stamp: 'RAH 8/27/1999 06:00'!
parentOf: anObject

"Definition: <abstractTreeModel>
"		#SIFMod. "Protocol: <abstractTreeModel>."
	^anObject parentSetOrNil
! !

!ProgramDefinitionSet methodsFor: 'hierarchy' stamp: 'RAH 8/27/1999 06:14'!
roots

"Definition: <abstractTreeModel>
"		#SIFMod. "Protocol: <abstractTreeModel>."
	^self components
! !

!ProgramDefinitionSet methodsFor: 'initializing' stamp: 'RAH 9/5/1999 09:16'!
clear

"Definition: <abstractTreeModel>
"		#SIFMod. "Protocol: <abstractTreeModel>."
	components := self class defaultComponentCollection.
! !

!ProgramDefinitionSet methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:27'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ProgramDef: ';
		nextPutAll: self programName.! !

!ProgramDefinitionSet methodsFor: 'private' stamp: 'RAH 9/5/1999 09:37'!
evaluateInstallUninstallScript: scriptStringOrNil type: typeString
	"Private - Evaluate the scriptStringOrNil Smalltalk code script of the type typeString (pre-install, post-install, etc.) before or after installing (creating or initializing) or uninstalling (removing) the receiver's elements (Dialect Specific).

Parameters
	scriptStringOrNil	<readableString>	uncaptured
						<nil>			uncaptured
	typeString			<readableString>	uncaptured
"		#SIFMod.
	scriptStringOrNil isNil ifTrue: [
		^nil
	].
	[	Compiler
			evaluate: scriptStringOrNil
			logged: true.
	] on: PGM compilerNotification do: [ :except |
		^self errorMessage: ('Error evaluating ', typeString, ' script', except text).
	].
! !

!ProgramDefinitionSet methodsFor: 'private' stamp: 'RAH 9/24/1999 06:57'!
fileOutInSIFProgramDescOnFiler: programFiler
	"Private - File out the receiver's description in ANSI SIF format on the programFiler.

Parameters
	programFiler		<programWriter>		uncaptured
"		#SIFMod.
	self annotationAt: PGM createdByAppKey
		put: self portFunc dialectNameVersionString.
	self annotationAt: PGM createdDateTimeKey
		put: self portFunc currentDateTimeString.

	programFiler
		fileOutProgramDescName: self programName
		prerequisiteProgramNames: self prerequisiteProgramNames
		programAnnotations: self annotations! !

!ProgramDefinitionSet methodsFor: 'private' stamp: 'RAH 9/5/1999 09:56'!
initialize
	"Private - Initialize the state of the receiver."
		#SIFMod.
	super initialize.
	fileLocator := String new.
	hasDescription := false.
	prerequisiteProgramNames := PGM defaultPrerequisiteProgramNameCollection.
	programName := String new.
	wasEvaluated := false.! !

!ProgramDefinitionSet methodsFor: 'removing' stamp: 'RAH 8/27/1999 06:06'!
remove: anObject

"Definition: <abstractTreeModel>
"	| oldParent |
		#SIFMod. "Protocol: <abstractTreeModel>."
	(self includes: anObject) ifFalse: [
		^self error: 'Can''t remove - dose not contain', anObject printString
	].

	oldParent := anObject parentSetOrNil.
	oldParent isNil ifTrue: [
		self removeComponent: anObject.
	] ifFalse: [
		oldParent removeComponent: anObject.
	].
	^anObject
! !

!ProgramDefinitionSet methodsFor: 'searching' stamp: 'RAH 9/7/1999 07:31'!
includesClassAllNamed: classNameString
	"Answer a <boolean>, true if the receiver or its components contain class all set referencing the class named classNameString, false if not.

Definition: <ProgramDefinitionSet>
Parameters
	classNameString	<readableString>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	self detect: [ :component |
		(component isMemberOf: PGM classAllSet)
		and: [component groupingName = classNameString]
	]
	ifNone: [
		^false
	].
	^true
! !

!ProgramDefinitionSet methodsFor: 'searching' stamp: 'RAH 9/2/1999 15:43'!
includesClassNamed: classNameString
	"Answer a <boolean>, true if the receiver or its components contain elements referencing the class named classNameString, false if not.

Definition: <ProgramDefinitionSet>
Parameters
	classNameString	<readableString>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	self detect: [ :component |
		((component isMemberOf: PGM classInitializerElement)
		and: [component className = classNameString]
		) or: [
			((component isMemberOf: PGM classAllSet)
			and: [component groupingName = classNameString]
			) or: [
				((component isMemberOf: PGM classDefinitionElement)
				and: [component objectName = classNameString]
				) or: [
					(component isMemberOf: PGM methodDefinitionElement)
					and: [component className = classNameString]
	]	]	]	]
	ifNone: [
		^false
	].
	^true
! !

!ProgramDefinitionSet methodsFor: 'searching' stamp: 'RAH 9/4/1999 05:53'!
includesGlobalNamed: globalNameString
	"Answer a <boolean>, true if the receiver or its components contain elements referencing the global named globalNameString, false if not.

Definition: <ProgramDefinitionSet>
Parameters
	globalNameString	<readableString>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	self detect: [ :component |
		((component isMemberOf: PGM globalAllSet)
		and: [component groupingName = globalNameString]
		) or: [
			((component isMemberOf: PGM globalDefinitionElement)
			and: [component objectName = globalNameString]
			) or: [
				(component isMemberOf: PGM globalInitializerElement)
				and: [component globalName = globalNameString]
	]	]	]
	ifNone: [
		^false
	].
	^true! !

!ProgramDefinitionSet methodsFor: 'searching' stamp: 'RAH 9/5/1999 08:00'!
includesMethodNamed: methodNameString in: classNameString isClassMethod: isClassMethodBoolean
	"Answer a <boolean>, true if the receiver or its components contain elements referencing the method named methodNameString in class named classNameString with aBoolean indicating if it is a class method, false if not.

Definition: <ProgramDefinitionSet>
Parameters
	methodNameString			<readableString>	uncaptured
	classNameString			<readableString>	uncaptured
	isClassMethodBoolean		<boolean>		uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	self detect: [ :component |
		(component isMemberOf: PGM methodDefinitionElement)
		and: [component isClassMethod = isClassMethodBoolean
			and: [component className = classNameString
				and: [component methodName = methodNameString]
	]	]	]
	ifNone: [
		^false
	].
	^true
! !

!ProgramDefinitionSet methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:18'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '((PGM newProgramNamed: '.
	self programName printOn: targetStream.
	targetStream nextPutAll: ')';
		cr.
	self components do: [ :component |
		targetStream nextPutAll: 'addComponent: '.
		component storeOn: targetStream.
		targetStream nextPut: $;.
		targetStream cr.
	].
	targetStream nextPutAll: 'yourself)';
		cr.! !

!ProgramDefinitionSet methodsFor: 'testing' stamp: 'RAH 9/5/1999 10:07'!
isPrerequisiteProgram: aProgramDefinitionSet
	"Answer a <boolean>, true if aProgramDefinitionSet is a prerequisite program of the receiver, false if not.

Definition: <ProgramDefinitionSet>
Parameters
	aProgramDefinitionSet	<ProgramDefinitionSet>	uncaptured
Return Values
	<boolean>		unspecified
"		#SIFMod. "Protocol: <ProgramDefinitionSet>."
	^self prerequisiteProgramNames
		includes: aProgramDefinitionSet programName
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 8/27/1999 05:56'!
move: anObject asChildOf: aParentObject

"Definition: <abstractTreeModel>
"	| oldParent |
		#SIFMod. "Protocol: <abstractTreeModel>."
	(self includes: anObject) ifFalse: [
		^self error: 'Can''t move - dose not contain child ', anObject printString
	].
	(self includes: aParentObject) ifFalse: [
		^self error: 'Can''t move - dose not contain parent', aParentObject printString
	].
		"Remove from previous parent:"	
	oldParent := anObject parentSetOrNil.
	oldParent isNil ifTrue: [
		self removeComponent: anObject.
	] ifFalse: [
		oldParent removeComponent: anObject.
	].
		"Add to new parent:"	
	aParentObject isNil ifTrue: [
		^self addComponent: anObject
	].
	aParentObject addComponent: anObject.
	^anObject
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:05'!
sysClassRemovedNamed: classNameString
	"Class named classNameString has been removed from the environment.  Remove all components referring to class classNameString from the receiver and its components.

Definition: <ProgramDefinitionSet>
Parameters
	classNameString	<readableString>	uncaptured
"	| componentChanged componentParent referingComponents |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	referingComponents := self componentsReferingToClassNamed: classNameString.
	"First remove any class All."
	referingComponents do: [ :refComponent |
		(refComponent isMemberOf: PGM classAllSet) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].
	"Next remove any class Def, or Init."
	referingComponents do: [ :refComponent |
		((refComponent isMemberOf: PGM classDefinitionElement)
		or: [refComponent isMemberOf: PGM classInitializerElement]
		) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].

	"Now OK to remove any loose methods of class."
	referingComponents do: [ :refComponent |
		(refComponent isMemberOf: PGM methodDefinitionElement) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].
	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/5/1999 08:57'!
sysClassUpdatedNamed: classNameString
	"Class named classNameString has been updated (pool dictionaries changing, super class changing, etc.) in the environment.  Update any class definition specification from the Smalltalk object.

Definition: <ProgramDefinitionSet>
Parameters
	classNameString	<readableString>	uncaptured
"	| componentChanged |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	"Update any class definition."
	self do: [ :refComponent |					"Find them."
		((refComponent isMemberOf: PGM classDefinitionElement)
		and: [refComponent objectName = classNameString]) ifTrue: [
			refComponent updateElementSpecification.
			componentChanged := true.
	]	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:06'!
sysGlobalRemovedNamed: globalNameString
	"Global named globalNameString has been removed from the environment.  Remove all components referring to global globalNameString from the receiver and its components.

Definition: <ProgramDefinitionSet>
Parameters
	globalNameString	<readableString>	uncaptured
"	| componentChanged componentParent referingComponents |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	referingComponents := self componentsReferingToGlobalNamed: globalNameString.
	"First remove any global All."
	referingComponents do: [ :refComponent |
		(refComponent isMemberOf: PGM globalAllSet) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].
	"Next remove any global Def, or Init."
	referingComponents do: [ :refComponent |
		((refComponent isMemberOf: PGM globalDefinitionElement)
		or: [refComponent isMemberOf: PGM globalInitializerElement]
		) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:07'!
sysGlobalRenamedNamed: oldGlobalNameString to: newGlobalNameString
	"Global named oldGlobalNameString has been renamed to newGlobalNameString in the environment.  Rename all components referring to global oldGlobalNameString in the receiver and its components.

Definition: <ProgramDefinitionSet>
Parameters
	oldGlobalNameString	<readableString>	uncaptured
	newGlobalNameString	<readableString>	uncaptured
"	| componentChanged |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	(self componentsReferingToGlobalNamed: oldGlobalNameString
	) do: [ :refComponent |
		(refComponent isMemberOf: PGM globalAllSet) ifTrue: [
			refComponent groupingName: newGlobalNameString.
		] ifFalse: [
			(refComponent isMemberOf: PGM globalDefinitionElement) ifTrue: [
				refComponent objectName: newGlobalNameString.
			] ifFalse: [		"globalInitializerElement."
				refComponent globalName: newGlobalNameString.
		]	].

		componentChanged := true.
	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:09'!
sysMethodAddedNamed: methodNameString inClassAll: classNameString isClassMethod: isClassMethodBoolean
	"Method named, methodNameString, in class all named, classNameString, with isClassMethodBoolean indicating if it is a class method has been added in the environment.

Definition: <ProgramDefinitionSet>
Parameters
	methodNameString			<readableString>	captured
	classNameString			<readableString>	captured
	isClassMethodBoolean		<boolean>		captured
"	| componentChanged |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	(self select: [ :component |
		(component isMemberOf: PGM classAllSet)
		and: [component groupingName = classNameString]
	]
	)do: [ :refComponent |
		isClassMethodBoolean ifTrue: [
			refComponent addComponent: (PGM
				newClassMethodDefinitionOf: methodNameString
				in: classNameString).
		] ifFalse: [
			refComponent addComponent: (PGM
				newInstanceMethodDefinitionOf: methodNameString
				in: classNameString).
		].
		componentChanged := true.
	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:10'!
sysMethodRemovedNamed: methodNameString in: classNameString isClassMethod: isClassMethodBoolean
	"Method, named methodNameString in class named classNameString with isClassMethodBoolean indicating if it is a class method, has been removed from the environment.  Remove all components referring to method methodNameString from the receiver and its components.

Definition: <ProgramDefinitionSet>
Parameters
	methodNameString			<readableString>	uncaptured
	classNameString			<readableString>	uncaptured
	isClassMethodBoolean		<boolean>		uncaptured
"	| componentChanged componentParent |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	"Remove any method definition."
	self do: [ :refComponent |			"Find them."
		((refComponent isMemberOf: PGM methodDefinitionElement)
		and: [refComponent isClassMethod = isClassMethodBoolean
			and: [refComponent className = classNameString
				and: [refComponent methodName = methodNameString]
		]	]) ifTrue: [
			componentParent := refComponent parentSetOrNil.
			componentParent isNil ifTrue: [
				self removeComponent: refComponent.
			] ifFalse: [
				componentParent removeComponent: refComponent.
			].
			componentChanged := true.
	]	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !

!ProgramDefinitionSet methodsFor: 'updating' stamp: 'RAH 9/21/1999 08:12'!
sysMethodUpdatedNamed: methodNameString in: classNameString isClassMethod: isClassMethodBoolean
	"Method, named methodNameString in class named classNameString with isClassMethodBoolean indicating if it is a class method, has been changed (recompiled, etc.) in the environment.  Update any method definition specification from the Smalltalk object.

Definition: <ProgramDefinitionSet>
Parameters
	methodNameString			<readableString>	uncaptured
	classNameString			<readableString>	uncaptured
	isClassMethodBoolean		<boolean>		uncaptured
"	| componentChanged |
		#SIFMod. "Protocol: <ProgramDefinitionSet>"
	componentChanged := false.
	"Update any method definition."
	self do: [ :refComponent |			"Find them."
		((refComponent isMemberOf: PGM methodDefinitionElement)
		and: [refComponent isClassMethod = isClassMethodBoolean
			and: [refComponent className = classNameString
				and: [refComponent methodName = methodNameString]
		]	]) ifTrue: [
			refComponent updateElementSpecification.
			componentChanged := true.
	]	].

	"Inform program manager that program changed."
	componentChanged ifTrue: [
		self programManager triggerProgramChanged: self
	].
! !


!ProgramInitializerElement commentStamp: 'RAH 9/28/1999 08:40' prior: 0!
The ProgramManager class comment is the starting point for the SIF Model Protocols documentation.

1.2.6 Protocol: <ProgramInitializerElement>

Conforms To
	<InitializerElement>

Description
	Class ProgramInitializerElement implements the behavior of a Smalltalk Interchange Format section <programInitialization>, program initializer interchange program elements that is used to specify an initializer that is executed solely for its side-effects.
	 This protocol provides an extended interface that may only be used when it has been determined beforehand that the component is a ProgramInitializerElement.

Instance Variables:

	None.
!

!ProgramInitializerElement methodsFor: 'displaying' stamp: 'RAH 8/22/1999 10:27'!
displayOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'Program Initializer'.
	self containsErrors ifTrue: [
		targetStream nextPutAll: ' (Errors)'.
	].
! !

!ProgramInitializerElement methodsFor: 'storing' stamp: 'RAH 8/24/1999 09:19'!
storeOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: '(PGM newProgramInitializer: '.
	self source storeOn: targetStream.
	targetStream nextPutAll: ')'.! !

!ProgramInitializerElement methodsFor: 'printing' stamp: 'RAH 8/24/1999 08:28'!
printOn: targetStream

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	targetStream nextPutAll: 'ProgramInit'.! !

!ProgramInitializerElement methodsFor: 'element creating/initializing' stamp: 'RAH 8/23/1999 09:52'!
evaluateObjects 

"Definition: <ProgramAbstractComponent>
	Note: ANSI specifies the result of evaluating a program initializer is discarded.
Errors
	Native initialization code evaluation errors.
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	[ Compiler
		evaluate: self source
		logged: true
	] on: (Error, PGM compilerNotification) do: [ :except |
		^self errorMessage: ('Error evaluating program initializer: ',
				except description).
	].
! !

!ProgramInitializerElement methodsFor: 'filing-ANSI SIF' stamp: 'RAH 8/23/1999 10:38'!
fileOutOnSIFFiler: programFiler

"Definition: <ProgramAbstractComponent>
"		#SIFMod. "Protocol: <ProgramAbstractComponent>."
	programFiler
		fileOutProgramInitializer: self source
		annotations: self annotations! !


" Postinstall script.:"
	"Initialize classes:"
	Transcript cr; show: 'Postinstall:'; cr;
		 show: 'Initialize classes:'; cr; endEntry.
	(Array with: ProgramAbstractComponent
		with: ProgramManager
		with: ProgramReaderWriter
	) do: [ :rootClass |
		((OrderedCollection with: rootClass)
			addAll: rootClass allSubclasses; "Chged SQ"
			yourself
		) do: [ :aClass |
			(aClass class includesSelector: #'initialize') ifTrue: [
				aClass initialize.
				Transcript show: (aClass name printString, ' initialized.');
					cr; endEntry.
	]	]	].!
