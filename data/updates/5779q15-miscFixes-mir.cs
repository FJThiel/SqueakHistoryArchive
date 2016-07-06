'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:54 pm'!
"Change Set:		q15-miscFixes-mir
Date:			29 October 2003
Author:			Michael Rueger

Integrated with 3.7a by Scott Wallace 3/3/04; the one method at issue is ProjectLauncher.startUpAfterLogin, which may still need a tweak.

This change set combines a few fixes contributed by Ted, Bert and myself.
- fix for the Safari workarounds
- enable the notifier when errors during saving occur
- enable relative file names for auto start scripts
- disable the resouce management by never writing out forms separately"!


!HTTPClient class methodsFor: 'testing' stamp: 'mir 8/4/2003 13:44'!
isRunningInBrowser: aBoolean
	"Override the automatic process.
	This should be used with caution.
	One way to determine it without using the primitive is to check for parameters typically only encountered when running as a plugin."

	RunningInBrowser := aBoolean! !


!PasteUpMorph methodsFor: 'initialization' stamp: 'mir 10/29/2003 13:05'!
becomeActiveDuring: aBlock
	"Make the receiver the ActiveWorld during the evaluation of aBlock.
	Note that this method does deliberately *not* use #ensure: to prevent
	re-installation of the world on project switches."
	| priorWorld priorHand priorEvent |
	priorWorld _ ActiveWorld.
	priorHand _ ActiveHand.
	priorEvent _ ActiveEvent.
	ActiveWorld _ self.
	ActiveHand _ self hands first. "default"
	ActiveEvent _ nil. "not in event cycle"
	[aBlock value]
		on: Error
		do: [:ex | 
			ActiveWorld _ priorWorld.
			ActiveEvent _ priorEvent.
			ActiveHand _ priorHand.
			ex pass]! !


!ProjectLauncher methodsFor: 'running' stamp: 'sw 3/4/2004 22:45'!
startUpAfterLogin
	| scriptName loader isUrl |
	self setupFlaps.
	Preferences readDocumentAtStartup ifTrue: [
		HTTPClient isRunningInBrowser ifTrue:[
			self setupFromParameters.
			scriptName _ self parameterAt: 'src'.
			CodeLoader defaultBaseURL: (self parameterAt: 'Base').
		] ifFalse:[
			scriptName _ (SmalltalkImage current getSystemAttribute: 2) ifNil:[''].
			scriptName isEmpty ifFalse:[
				"figure out if script name is a URL by itself"
				isUrl _ (scriptName asLowercase beginsWith:'http://') or:[
						(scriptName asLowercase beginsWith:'file://') or:[
						(scriptName asLowercase beginsWith:'ftp://')]].
				isUrl ifFalse:[scriptName _ 'file:',scriptName]].
		]. ]
	ifFalse: [ scriptName := '' ].

	scriptName isEmptyOrNil
		ifTrue:[^Preferences eToyFriendly ifTrue: [self currentWorld addGlobalFlaps]].
	loader _ CodeLoader new.
	loader loadSourceFiles: (Array with: scriptName).
	(scriptName asLowercase endsWith: '.pr') 
		ifTrue:[self installProjectFrom: loader]
		ifFalse:[loader installSourceFiles].
! !


!ResourceCollector methodsFor: 'accessing' stamp: 'mir 10/29/2003 13:33'!
objectForDataStream: refStream fromForm: aForm
	"Return a replacement for aForm to be stored instead"
	| stub fName copy loc fullSize nameAndSize |

	"First check if the form is one of the intrinsic Squeak forms"
	stub _ internalStubs at: aForm ifAbsent:[nil].
	stub ifNotNil:[
		refStream replace: aForm with: stub. 
		^stub].

	"Now see if we have created the stub already 
	(this may happen if for instance some form is shared)"
	stub _ originalMap at: aForm ifAbsent:[nil].
	stub ifNotNil:[^aForm].
	aForm hibernate.
	aForm bits class == FormStub ifTrue:[^nil].	"something is wrong"
	"too small to be of interest"
	"(aForm bits byteSize < 4096) ifTrue:[^aForm]."
	"We'll turn off writing out forms until we figure out how to reliably deal with resources"
	true ifTrue: [^aForm].

	"Create our stub form"
	stub _ FormStub 
		extent: (aForm width min: 32) @ (aForm height min: 32) 
		depth: (aForm depth min: 8).
	aForm displayScaledOn: stub.
	aForm hibernate.

	"Create a copy of the original form which we use to store those bits"
	copy _ Form extent: aForm extent depth: aForm depth bits: nil.
	copy setResourceBits: aForm bits.

	"Get the locator for the form (if we have any)"
	loc _ locatorMap at: aForm ifAbsent:[nil].

	"Store the resource file"
	nameAndSize _ self writeResourceForm: copy locator: loc.
	fName _ nameAndSize first.
	fullSize _ nameAndSize second.

	ProgressNotification signal: '2:resourceFound' extra: stub.
	stub hibernate.
	"See if we need to assign a new locator"
	(loc notNil and:[loc hasRemoteContents not]) ifTrue:[
		"The locator describes some local resource. 
		If we're preparing to upload the entire project to a
		remote server, make it a remote URL instead."
"		(baseUrl isEmpty not and:[baseUrl asUrl hasRemoteContents])
			ifTrue:[loc urlString: baseUrl, fName].
"
		baseUrl isEmpty not
			ifTrue:[loc urlString: self resourceDirectory , fName]].

	loc ifNil:[
		loc _ ResourceLocator new urlString: self resourceDirectory , fName.
		locatorMap at: aForm put: loc].
	loc localFileName: (localDirectory fullNameFor: fName).
	loc resourceFileSize: fullSize.
	stub locator: loc.

	"Map old against stub form"
	aForm setResourceBits: stub.
	originalMap at: aForm put: copy.
	stubMap at: stub put: aForm.
	locatorMap at: aForm put: loc.
	"note: *must* force aForm in out pointers if 
	in IS or else won't get #comeFullyUpOnReload:"
	refStream replace: aForm with: aForm.
	^aForm! !

