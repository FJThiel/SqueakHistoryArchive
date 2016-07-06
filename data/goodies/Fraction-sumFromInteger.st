"      NAME	Patch to TAG's Squeak double-dispatching
       AUTHOR	Stephen Pope (stp@create.ucsb.edu)
       URL	(none)
       FUNCTION	Fix for (1 + (1/4)) mixed-mode math
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 1.3
       CONFLICTS	(none)
       DISTRIBUTION	world
       VERSION	1.3.0  (first version for Squeak 1.3)
       DATE	2/18/98

SUMMARY

There are a few methods missing from TAG's excellent double-dispatching code--here's one of them that fixes (int + fraction). 

"!

'From Squeak 1.31 of Feb 4, 1998 on 18 February 1998 at 12:14:19 am'!

!Fraction methodsFor: 'arithmetic-DD' stamp: 'stp 02/17/98 0-11:'!
sumFromInteger: anInteger
	^anInteger * denominator + numerator / denominator! !
