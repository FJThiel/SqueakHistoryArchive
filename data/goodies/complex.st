"      NAME	Complex number class
       AUTHOR	amann@acofi.edu (Allen L. Mann)
       URL	(none)
       FUNCTION	Handles numbers of the form a + ib, where a and b are real numbers.
       KEYWORDS	complex number
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.1
       DATE	12-Nov-99

SUMMARY

Adds a Complex subclass to the Number class. Has methods: +, -, *, /, negated, conjugate, abs, arg, raisedTo:, exp, print. 

				Allen L. Mann
"!
'From Squeak2.6 of 11 October 1999 [latest update: #1559] on 11 November 1999 at 11:06:26 pm'!
Number subclass: #Complex
	instanceVariableNames: 'real imaginary '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!
!Complex commentStamp: '<historical>' prior: 0!
Complex numbers of the form a + ib, where a and b are real numbers.!


!Complex methodsFor: 'modifying' stamp: 'alm 11/6/1999 18:33'!
imaginary: aNumber

imaginary _ aNumber.! !

!Complex methodsFor: 'modifying' stamp: 'alm 11/6/1999 18:26'!
real: aNumber

real _ aNumber.! !

!Complex methodsFor: 'modifying' stamp: 'alm 11/6/1999 18:34'!
real: aNumber imaginary: anotherNumber

real _ aNumber.
imaginary _ anotherNumber.! !


!Complex methodsFor: 'mathematical functions' stamp: 'alm 11/7/1999 15:37'!
exp
"Raises e to the power of the receiver."

| a b |
a _ self real.
b _ self imaginary.
^ Complex real: (a exp) * (b cos) imaginary: (a exp) * (b sin).! !

!Complex methodsFor: 'mathematical functions' stamp: 'alm 11/7/1999 15:47'!
raisedTo: n
"Answer the receiver raised to the power of n."

| r theta |
r _ self abs.
theta _ self arg.

^ Complex real: ((r raisedTo: n) * theta cos) imaginary: ((r raisedTo: n) * theta sin).! !


!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 19:28'!
* z
"Multiply two complex numbers."

| a b c d |
a _ self real.
b _ self imaginary.
c _ z real.
d _ z imaginary.
^ Complex real: ((a * c) - (b * d)) imaginary: ((a * d) + (b * c)).! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 19:28'!
+ z
"Add two complex numbers."

^ Complex real: (self real + z real) imaginary: (self imaginary + z imaginary).! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 19:28'!
- z
"Subtract the argument fromthe receiver."

^ Complex real: (self real - z real) imaginary: (self imaginary - z imaginary).! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 23:11'!
/ z
"Divide two complex numbers."

| a b c d e f |
(z abs = 0.0)
	ifTrue: [ self error: 'division by zero']
	ifFalse: [
		a _ self real.
		b _ self imaginary.
		c _ z real.
		d _ z imaginary.
		
		e _ ((a * c) + (b * d)) / ((c * c) + (d * d)).
		f _ ((b * c) - (a * d)) / ((c * c) + (d * d)).
		^ Complex real: e imaginary: f].! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 19:27'!
abs
"Return the absolute value of the receiver."

| a b | 
a _ self real.
b _ self imaginary.
^ ((a * a) + (b * b)) sqrt. ! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/11/1999 11:33'!
arg
"Return the argument (angle in radians) of a complex number."

| a b |
a _ self real.
b _ self imaginary.

(a > 0.0)
	ifTrue: [ ^ (b / a) arcTan ].
(a = 0.0)
	ifTrue: [ (b = 0.0) 
				ifTrue: [ ^ 0.0 ]
				ifFalse: [ ^ (b / b abs) arcSin ]].
(a < 0.0)
	ifTrue: [ (b >= 0.0)
				ifTrue: [ ^ (b / a) arcTan + (-1 arcCos) ]
				ifFalse: [ ^ (b / a) arcTan - (-1 arcCos) ]].! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/6/1999 23:07'!
conjugate
"Return the complex conjugate of the receiver."

^ Complex real: (self real) imaginary: (self imaginary negated).


! !

!Complex methodsFor: 'arithmetic' stamp: 'alm 11/11/1999 23:05'!
negated
"Return the complex number which is the additive inverse of the receiver."

^ Complex real: (self real negated) imaginary: (self imaginary negated).! !


!Complex methodsFor: 'printing' stamp: 'alm 11/6/1999 20:03'!
print

^ Array with: (self real) with: (self imaginary).! !


!Complex methodsFor: 'accessing' stamp: 'alm 11/6/1999 18:20'!
imaginary

^ imaginary.! !

!Complex methodsFor: 'accessing' stamp: 'alm 11/6/1999 18:20'!
real

^ real.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Complex class
	instanceVariableNames: ''!

!Complex class methodsFor: 'instance creation' stamp: 'alm 11/6/1999 18:42'!
real: a imaginary: b

^ self new real: a imaginary: b.! !

