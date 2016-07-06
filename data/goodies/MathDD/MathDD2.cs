'From Squeak 1.3 of Jan 16, 1998 on 30 January 1998 at 12:53:38 am'!

!Float methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:57'!
* aNumber 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	"get the argument to actually carry out the operation since it can tune the operation based on my type, if it's the same class then the primitive failure is raised there"
	^aNumber productFromFloat: self! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:57'!
+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	"get the argument to actually carry out the operation since it can tune the operation based on my type, if it's the same class then the primitive failure is raised there"
	^aNumber sumFromFloat: self! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:57'!
- aNumber 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	"get the argument to actually carry out the operation since it can tune the operation based on my type, if it's the same class then the primitive failure is raised there"
	^aNumber differenceFromFloat: self! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:58'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 50>
	"get the argument to actually carry out the operation since it can tune the operation based on my type, if it's the same class then the primitive failure is raised there, as well as zero divide detection"
	^aNumber quotientFromFloat: self! !


!Fraction methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:40'!
* aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber productFromFraction: self! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:41'!
+ aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber sumFromFraction: self! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:46'!
- aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber differenceFromFraction: self! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 1/27/98 23:46'!
/ aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber quotientFromFraction: self! !


!Integer methodsFor: 'arithmetic' stamp: 'TAG 1/28/98 09:16'!
* aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber productFromInteger: self! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 1/28/98 0-10:'!
+ aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber sumFromInteger: self! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 1/28/98 09:17'!
- aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber differenceFromInteger: self! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 1/28/98 09:15'!
/ aNumber
	"get the argument to actually carry out the operation since it can tune the operation based on my type"
	^aNumber quotientFromInteger: self! !



