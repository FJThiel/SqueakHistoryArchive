'From Squeak3.3alpha of 12 January 2002 [latest update: #4809] on 25 March 2002 at 12:56:03 am'!"Change Set:		slideToTrash-swDate:			25 March 2002Author:			Scott WallaceApplies only when the #preserveTrash preference is true:When dismissing by halo, slide the object being dismissed to the trash-can.  If no trash can is visible on the screen, put one there first.This update also fixes the problem that when haloTransitions and preserveTrash were both true and you dismissed a morph via halo, a spurious momentary fading halo would appear *in the wrong place* at the topleft of the screen."!!Morph methodsFor: 'dropping/grabbing' stamp: 'sw 3/25/2002 00:54'!slideToTrash: evt	"Slide the receiver across the screen to a trash can and make it disappear into it."	| aForm trash startPoint endPoint |	((self == Utilities scrapsBook) or: [self isKindOf: TrashCanMorph]) ifTrue:		[self delete.  ^ self].	aForm _ self imageForm offset: 0@0.	trash _ ActiveWorld findA: TrashCanMorph.	trash ifNil:		[trash _ TrashCanMorph new.		trash position: ActiveWorld bottomLeft - (0 @ (trash extent y + 26)).		trash openInWorld].	endPoint _ trash topRendererOrSelf fullBounds center - (self topRendererOrSelf extent // 2).	startPoint _ self topRendererOrSelf topLeft.	self delete.	ActiveWorld displayWorld.	aForm slideFrom: startPoint to: endPoint nSteps: 12 delay: 15.	Utilities addToTrash: self! !!HaloMorph methodsFor: 'private' stamp: 'sw 3/25/2002 00:55'!maybeDismiss: evt with: dismissHandle	"Ask hand to dismiss my target if mouse comes up in it."	evt hand obtainHalo: self.	(dismissHandle containsPoint: evt cursorPoint)		ifFalse:			[self delete.			target addHalo: evt]		ifTrue:			[Preferences preserveTrash				ifTrue:					[Preferences soundsEnabled ifTrue:						[TrashCanMorph playDeleteSound].					self stopStepping.					super delete.					target slideToTrash: evt]				ifFalse:					[self delete.					target dismissViaHalo]]! !