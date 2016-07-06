'From Squeak3.1alpha of 5 February 2001 [latest update: #3611] on 15 February 2001 at 10:39:46 pm'!"Change Set:		sketchConstrainDate:			15 February 2001Author:			Bob ArningFixes walkback in sketch editor when drawing an ellipse and changing the state of the shift key during drawing"!!SketchEditorMorph methodsFor: 'actions & preps' stamp: 'RAA 2/15/2001 07:19'!ellipse: evt	"Draw an ellipse from the center. "	| rect oldRect ww ext oldExt cColor sOrigin priorEvt |	sOrigin _ self get: #strokeOrigin for: evt.	cColor _ self getColorFor: evt.	ext _ (sOrigin - evt cursorPoint) abs * 2.	evt shiftPressed ifTrue: [ext _ self shiftConstrainPoint: ext].	rect _ Rectangle center: sOrigin extent: ext.	ww _ (self getNibFor: evt) width.	(priorEvt _ self get: #lastEvent for: evt) ifNotNil: [		oldExt _ (sOrigin - priorEvt cursorPoint) abs + ww * 2.		priorEvt shiftPressed ifTrue: [oldExt _ self shiftConstrainPoint: oldExt].		(oldExt < ext) ifFalse: ["Last draw sticks out, must erase the area"			oldRect _ Rectangle center: sOrigin extent: oldExt.			self restoreRect: oldRect]].	cColor == Color transparent	ifFalse:[	formCanvas fillOval: rect color: cColor 		borderWidth: 0 borderColor: Color transparent.]	ifTrue:[	formCanvas fillOval: rect color: cColor 		borderWidth: ww borderColor: Color black].	self invalidRect: rect.! !!SketchEditorMorph methodsFor: 'actions & preps' stamp: 'RAA 2/15/2001 07:18'!shiftConstrainPoint: aPoint	"answer a point with x and y equal for shift-constrained drawing"	^aPoint max: aPoint transposed! !