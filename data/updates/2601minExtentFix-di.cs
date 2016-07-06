'From Squeak2.9alpha of 12 June 2000 [latest update: #2575] on 10 September 2000 at 9:29:35 am'!"Change Set:		minExtentFixDate:			10 September 2000Author:			Dan IngallsThere had been a problem in resizing morphs descended from AlignmentMorph due to special treatment in Halos (for instance MoviePlayerMorph and MonthMorph).  This change simply asks every morph for its minExtent, which is defined as previously for AlignmentMorphs.  However now any subclass can override this method.In the process 5 methods were simplified and Halos no longer need to remember their target's minExtent."!!Morph methodsFor: 'geometry' stamp: 'di 9/10/2000 09:29'!minExtent	"Used as a guide for resizing by HaloMorphs and similar controls."	^ 3 @ 3  "May be overridden in subclasses"! !!Morph methodsFor: 'geometry' stamp: 'di 9/10/2000 09:28'!minHeight	"Return the minimum width for this morph. Ordinary morphs just answer their current height.  This method is particular to the operation of AlignmentMorphs"	^ self fullBounds height! !!Morph methodsFor: 'geometry' stamp: 'di 9/10/2000 09:28'!minWidth	"Return the minimum width for this morph. Ordinary morphs just answer their current width.    This method is particular to the operation of AlignmentMorphs"	^ self fullBounds width! !!AlignmentMorph methodsFor: 'geometry' stamp: 'di 9/10/2000 08:49'!minExtent	^ self minWidth @ self minHeight! !!HaloMorph methodsFor: 'private' stamp: 'di 9/10/2000 08:53'!doGrow: evt with: growHandle	"Called while the mouse is down in the grow handle"	| newExtent extentToUse |	newExtent _ (target pointFromWorld: (target griddedPoint: evt cursorPoint - positionOffset))								- target topLeft.	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].	target renderedMorph extent: (extentToUse _ newExtent max: target minExtent).	growHandle position: evt cursorPoint - (growHandle extent // 2).	self layoutChanged.	(self valueOfProperty: #commandInProgress) doIfNotNil:  		[:cmd | "Update the final extent"		cmd redoTarget: target selector: #extent: argument: extentToUse]! !!HaloMorph methodsFor: 'private' stamp: 'di 9/10/2000 08:53'!doScale: evt with: scaleHandle	"Update the scale of my target if it is scalable."	| newExtent newHandlePos |	newHandlePos _ evt cursorPoint - (scaleHandle extent // 2).	newExtent _ (target pointFromWorld: newHandlePos) - target center * 2.	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].	target scaleToFit: ((newExtent max: target minExtent) min: Display extent).	target scale = 1.0		ifTrue: [scaleHandle color: Color yellow]		ifFalse: [scaleHandle color: Color orange].	scaleHandle position: newHandlePos.	self layoutChanged.! !!HaloMorph methodsFor: 'private' stamp: 'di 9/10/2000 08:53'!startGrow: evt with: growHandle	| botRt |	"Initialize resizing of my target.  Launch a command representing it, to support Undo"	growingOrRotating _ true.	self removeAllHandlesBut: growHandle.  "remove all other handles"	botRt _ target pointInWorld: target bottomRight.	(self world viewBox containsPoint: botRt)		ifTrue: [positionOffset _ evt cursorPoint - botRt]		ifFalse: [positionOffset _ 0@0].	self setProperty: #commandInProgress toValue:		(Command new			cmdWording: 'resizing';			undoTarget: target selector: #extent: argument: target extent)! !!HaloMorph methodsFor: 'private' stamp: 'di 9/10/2000 08:54'!startScale: evt with: scaleHandle	"Initialize scaling of my target."	target isFlexMorph ifFalse: [target addFlexShell].	growingOrRotating _ true.	self removeAllHandlesBut: scaleHandle.  "remove all other handles"	positionOffset _ 0@0.! !!HaloMorph methodsFor: 'copying' stamp: 'di 9/10/2000 08:53'!veryDeepInner: deepCopier	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."	super veryDeepInner: deepCopier.	"target _ target.		Weakly copied"	"innerTarget _ innerTarget.		Weakly copied"	positionOffset _ positionOffset veryDeepCopyWith: deepCopier.	angleOffset _ angleOffset veryDeepCopyWith: deepCopier.	growingOrRotating _ growingOrRotating veryDeepCopyWith: deepCopier.	directionArrowAnchor _ directionArrowAnchor.	simpleMode _ simpleMode.	haloBox _ haloBox! !