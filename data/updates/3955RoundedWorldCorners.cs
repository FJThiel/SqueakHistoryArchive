'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 28 February 2001 update 3954] on 25 April 2001 at 5:16:32 pm'!"Change Set:		RoundedWorldCornersDate:			25 April 2001Author:			Andreas RaabFixes rounded corners for worlds."!!BorderedMorph methodsFor: 'menu' stamp: 'ar 4/25/2001 17:12'!addCustomMenuItems: aMenu hand: aHandMorph	super addCustomMenuItems: aMenu hand: aHandMorph.	aMenu addList: 		#(('border color...' changeBorderColor:)		('border width...' changeBorderWidth:)).	self couldHaveRoundedCorners ifTrue:		[aMenu addUpdating: #roundedCornersString target: self action: #toggleCornerRounding].	self doesBevels ifTrue:		[borderColor == #raised ifFalse: [aMenu add: 'raised bevel' action: #borderRaised].		borderColor == #inset ifFalse: [aMenu add: 'inset bevel' action: #borderInset]]! !!PasteUpMorph methodsFor: 'accessing' stamp: 'ar 4/25/2001 17:15'!useRoundedCorners	"Somewhat special cased because we do have to fill Display for this"	super useRoundedCorners.	self == World ifTrue:[Display bits primFill: 0]. "done so that we *don't* get a flash"! !!WorldState methodsFor: 'update cycle' stamp: 'ar 4/25/2001 17:01'!displayWorld: aWorld submorphs: submorphs	"Update this world's display."	| deferredUpdateMode worldDamageRects handsToDraw handDamageRects allDamage |	submorphs do: [:m | m fullBounds].  "force re-layout if needed"	self checkIfUpdateNeeded ifFalse: [^ self].  "display is already up-to-date"	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.	deferredUpdateMode ifFalse: [self assuredCanvas].	canvas roundCornersOf: aWorld during:[		worldDamageRects _ self drawWorld: aWorld submorphs: submorphs invalidAreasOn: canvas.  "repair world's damage on canvas"		"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."		handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.		handDamageRects _ handsToDraw collect: [:h | h savePatchFrom: canvas].		allDamage _ worldDamageRects, handDamageRects.		handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"	].	"*make this true to flash damaged areas for testing*"	Preferences debugShowDamage ifTrue: [aWorld flashRects: allDamage color: Color black].	canvas finish.	"quickly copy altered rects of canvas to Display:"	deferredUpdateMode		ifTrue: [self forceDamageToScreen: allDamage]		ifFalse: [canvas showAt: aWorld viewBox origin invalidRects: allDamage].	handsToDraw do: [:h | h restoreSavedPatchOn: canvas].  "restore world canvas under hands"	Display deferUpdates: false; forceDisplayUpdate.! !