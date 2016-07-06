'From Squeak 2.3 of January 14, 1999 on 24 March 1999 at 2:59:37 pm'!"Change Set:		TwoFixes-arDate:			24 March 1999Author:			Andreas RaabTwo small fixes for the Balloon stuff."!!B3DRotation methodsFor: 'interpolating' stamp: 'ar 3/24/1999 14:58'!slerpTo: aRotation at: t extraSpins: spin
	"Sperical Linear Interpolation (slerp).
	Calculate the new quaternion when applying slerp from the receiver (t = 0.0)
	to aRotation (t = 1.0). spin indicates the number of extra rotations to be added.
	The code shown below is from Graphics Gems III"
	| cosT alpha beta flip theta phi sinT |
	alpha := t.
	flip := false.
	"calculate the cosine of the two quaternions on the 4d sphere"
	cosT := self dot: aRotation.
	"if aQuaternion is on the opposite hemisphere reverse the direction
	(note that in quaternion space two points describe the same rotation)"
	cosT < 0.0 ifTrue:[
		flip := true.
		cosT := cosT negated].
	"If the aQuaternion is nearly the same as I am use linear interpolation"
	cosT > 0.99999 ifTrue:[
		"Linear Interpolation"
		beta := 1.0 - alpha
	] ifFalse:[
		"Spherical Interpolation"
		theta := cosT arcCos.
		phi := (spin * Float pi) + theta.
		sinT := theta sin.
		beta := (theta - (alpha * phi)) sin / sinT.
		alpha := (alpha * phi) sin / sinT].

	flip ifTrue:[alpha := alpha negated].
	^B3DRotation 
		a: (alpha * aRotation a) + (beta * self a)
		b: (alpha * aRotation b) + (beta * self b)
		c: (alpha * aRotation c) + (beta * self c)
		d: (alpha * aRotation d) + (beta * self d)! !!OSMesaPlugin class methodsFor: 'translation' stamp: 'ar 3/24/1999 14:58'!declareCVarsIn: cg	"Add the necessary header files"	cg addHeaderFile:'<GL/gl.h>'.	cg addHeaderFile:'<GL/osmesa.h>'.! !