'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:22 pm'!

!Float methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:33'!
* aNumber 
	"Primitive. Answer the result of multiplying the receiver by 
	aNumber.
	Fail if the argument is not a Float. Essential. See Object 
	documentation
	whatIsAPrimitive. "
	<primitive: 49>
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type, if it's the same class then the primitive 
	failure is raised there"
	^ aNumber productFromFloat: self!
]style[(2 7 3 168 19 174 4 7 23)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:33'!
+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail 
	if the argument is not a Float. See Object 
	documentation
	whatIsAPrimitive. "
	<primitive: 41>
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type, if it's the same class then the primitive 
	failure is raised there"
	^ aNumber sumFromFloat: self!
]style[(2 7 3 154 19 174 4 7 19)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:33'!
- aNumber 
	"Primitive. Answer the difference between the receiver and 
	aNumber.
	Fail if the argument is not a Float. Essential. See Object 
	documentation
	whatIsAPrimitive. "
	<primitive: 42>
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type, if it's the same class then the primitive 
	failure is raised there"
	^ aNumber differenceFromFloat: self!
]style[(2 7 3 166 19 174 4 7 26)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:33'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if 
	the argument is not a Float. Essential. See Object 
	documentation
	whatIsAPrimitive. "
	<primitive: 50>
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type, if it's the same class then the primitive 
	failure is raised there, as well as zero divide detection"
	^ aNumber quotientFromFloat: self!
]style[(2 7 3 161 19 208 4 7 24)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !


!Fraction methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:33'!
* aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber productFromFraction: self!
]style[(2 7 3 105 4 7 26)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
+ aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber sumFromFraction: self!
]style[(2 7 3 105 4 7 22)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
- aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber differenceFromFraction: self!
]style[(2 7 3 105 4 7 29)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
/ aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber quotientFromFraction: self!
]style[(2 7 3 105 4 7 27)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !


!Integer methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
* aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber productFromInteger: self!
]style[(2 7 3 105 4 7 25)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
+ aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber sumFromInteger: self!
]style[(2 7 3 105 4 7 21)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
- aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber differenceFromInteger: self!
]style[(2 7 3 105 4 7 28)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:34'!
/ aNumber 
	"get the argument to actually carry out the operation since it can tune 
	the operation based on my type"
	^ aNumber quotientFromInteger: self!
]style[(2 7 3 105 4 7 26)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !



