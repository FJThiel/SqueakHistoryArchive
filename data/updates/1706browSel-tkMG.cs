'From Squeak2.6 of 11 October 1999 [latest update: #1700] on 9 December 1999 at 3:10:36 pm'!!Browser methodsFor: 'message list' stamp: 'tk 12/7/1999 08:40'!selectedMessage	"Answer a copy of the source code for the selected message selector."	| class selector method tempNames |	contents == nil ifFalse: [^ contents copy].	class _ self selectedClassOrMetaClass.	selector _ self selectedMessageName.	method _ class compiledMethodAt: selector ifAbsent: [		^ ''].	"method deleted while in another project"	currentCompiledMethod _ method.	(Sensor controlKeyPressed		or: [method fileIndex > 0 and: [(SourceFiles at: method fileIndex) == nil]])		ifTrue:		["Emergency or no source file -- decompile without temp names"		contents _ (class decompilerClass new decompile: selector in: class method: method)			decompileString.		^ contents copy].	Sensor leftShiftDown ifTrue:		["Special request to decompile -- get temps from source file"		tempNames _ (class compilerClass new						parse: method getSourceFromFile asString in: class notifying: nil)						tempNames.		contents _ ((class decompilerClass new withTempNames: tempNames)				decompile: selector in: class method: method) decompileString.		contents _ contents asText makeSelectorBoldIn: class.		^ contents copy].	contents _ class sourceCodeAt: selector.	Preferences browseWithPrettyPrint ifTrue:		[contents _ class compilerClass new			format: contents in: class notifying: nil decorated: Preferences colorWhenPrettyPrinting].	self showDiffs ifTrue:		[contents _ self diffFromPriorSourceFor: contents].	contents _ contents asText makeSelectorBoldIn: class.	^ contents copy! !!ChangeSorter methodsFor: 'creation' stamp: 'tk 12/7/1999 12:53'!veryDeepFixupWith: deepCopier	super veryDeepFixupWith: deepCopier.	parent _ deepCopier references at: parent ifAbsent: [parent].	self updateIfNecessary! !!ChangeSorter methodsFor: 'creation' stamp: 'tk 12/7/1999 12:51'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared."super veryDeepInner: deepCopier."parent _ parent.		Weakly copied""myChangeSet _ myChangeSet.		Weakly copied"currentClassName _ currentClassName veryDeepCopyWith: deepCopier."currentSelector _ currentSelector.		Symbol"priorChangeSetList _ priorChangeSetList veryDeepCopyWith: deepCopier.! !!ProjectViewMorph methodsFor: 'copying' stamp: 'tk 12/7/1999 10:54'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  See DeepCopier class comment."	super veryDeepInner: deepCopier.	project _ project.		"Weakly copied"	lastProjectThumbnail _ lastProjectThumbnail veryDeepCopyWith: deepCopier.	currentBorderColor _ currentBorderColor veryDeepCopyWith: deepCopier.	mouseDownTime _ nil.! !!UpdatingStringMorph methodsFor: 'copying' stamp: 'tk 12/7/1999 10:48'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared."	super veryDeepInner: deepCopier.	format _ format veryDeepCopyWith: deepCopier.	target _ target.					"Weakly copied"	lastValue _ lastValue veryDeepCopyWith: deepCopier.	getSelector _ getSelector.			"Symbol"	putSelector _ putSelector.		"Symbol"	floatPrecision _ floatPrecision veryDeepCopyWith: deepCopier.	growable _ growable veryDeepCopyWith: deepCopier.	stepTime _ stepTime veryDeepCopyWith: deepCopier.	autoAcceptOnFocusLoss _ autoAcceptOnFocusLoss veryDeepCopyWith: deepCopier.	minimumWidth _ minimumWidth veryDeepCopyWith: deepCopier.	maximumWidth _ maximumWidth veryDeepCopyWith: deepCopier.! !