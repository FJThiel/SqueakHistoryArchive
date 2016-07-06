'From Squeak3.3alpha of 24 January 2002 [latest update: #4862] on 11 May 2002 at 8:11:46 pm'!"Change Set:		VMMaker32-7part1Date:			7 May 2002Author:			tim@sumeru.stanford.eduPart 1 of 2 of the latest VMMaker; it has to be in two parts because of two bugs relating to fileouts. Filein part 1 first, then part 2.Latest phase of a decent VM source code builder, this version being intended for a 3.2 image of approx update level 4811.This version is a first merge of the version6 changeset with Andreas' variations to support his favoured code layout for Windows.The platforms file tree you need can be downloaded via cvs from squeak.Sourceforge.net.See the VMMaker & VMMakerTool class commnets for details"!InterpreterPlugin class	instanceVariableNames: 'timeStamp '!!InterpreterPlugin class methodsFor: 'accessing' stamp: 'tpr 3/26/2002 15:25'!timeStamp	^timeStamp ifNil:[0]! !!InterpreterPlugin class methodsFor: 'accessing' stamp: 'tpr 3/18/2002 10:49'!touch	timeStamp _ Time totalSeconds! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 4/9/2002 16:13'!buildCodeGeneratorUpTo: aPluginClass	"Build a CCodeGenerator for the plugin"	 | cg theClass |	cg _ self codeGeneratorClass new initialize.	cg pluginName: self moduleName.	"Add an extra declaration for module name"	cg declareModuleName: self moduleNameAndVersion.	theClass _ aPluginClass.	[theClass == Object] whileFalse:[		cg addClass: theClass.		theClass declareCVarsIn: cg.		theClass _ theClass superclass].	^cg! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/23/2001 17:14'!declareHeaderFilesIn: aCCodeGenerator	self hasHeaderFile ifTrue:[		aCCodeGenerator addHeaderFile: '"', self moduleName,'.h"'].! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/23/2001 17:03'!hasHeaderFile	"If there is a single intrinsic header file to be associated with the plugin, here is where you want to flag"	^false! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 7/2/2001 16:33'!requiresCrossPlatformFiles	"default is ok for most, any plugin needing cross platform files aside from a normal header must say so. See SoundCodecPlugin for example"	^self hasHeaderFile! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 11/21/2000 11:53'!requiresPlatformFiles	"default is ok for most, any plugin needing platform specific files must say so"	^false! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 5/14/2001 12:05'!shouldBeTranslated"is this class intended to be translated as a plugin? Most subclasses should answer true, but some such as:-	TestInterpreterPlugin	FlippArrayPlugin2	InflatePlugin	should answer false for various reasons."	^true! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 9/26/2001 07:27'!storeString: s onFileNamed: fileName	"Store the given string in a file of the given name."	| f |	f _ CrLfFileStream forceNewFileNamed: fileName.	f nextPutAll: s.	f close.! !!InterpreterPlugin class methodsFor: 'translation' stamp: 'tpr 4/9/2002 16:14'!translateInDirectory: directory doInlining: inlineFlag"This is the default method for writing out sources for a plugin. Several classes need special handling, so look at all implementors of this message"	| cg fname fstat |	 fname _ self moduleName, '.c'.	"don't translate if the file is newer than my timeStamp"	fstat _ directory entryAt: fname ifAbsent:[nil].	fstat ifNotNil:[self timeStamp < fstat modificationTime ifTrue:[^nil]].	self initialize.	cg _ self buildCodeGeneratorUpTo: self.	cg storeCodeOnFile:  (directory fullNameFor: fname) doInlining: inlineFlag.	^cg exportedPrimitiveNames asArray! !!ObjectMemory class methodsFor: 'translation' stamp: 'tpr 3/27/2002 12:53'!timeStamp	^timeStamp ifNil:[0]! !!ObjectMemory class methodsFor: 'translation' stamp: 'tpr 3/27/2002 12:53'!touch	timeStamp _ Time totalSeconds! !InterpreterPlugin class removeSelector: #headerFile!InterpreterPlugin class removeSelector: #translate!InterpreterPlugin class removeSelector: #translate:all:doInlining:!InterpreterPlugin class removeSelector: #translate:doInlining:!InterpreterPlugin class removeSelector: #translate:doInlining:locally:!InterpreterPlugin class removeSelector: #translateDoInlining:!InterpreterPlugin class removeSelector: #translateLocally!InterpreterPlugin class removeSelector: #translateOn:inlining:to:local:!