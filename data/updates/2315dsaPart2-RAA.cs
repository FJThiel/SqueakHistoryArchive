'From Squeak2.8alpha of 13 January 2000 [latest update: #2210] on 31 May 2000 at 1:21:06 pm'!"Change Set:		dsaPart2Date:			31 May 2000Author:			Bob ArningThis is the second of two change sets for DSA which will:- Replace the DSA primitives for LargeInteger arithmetic with corresponding Squeak ones. Now that we have the LargeIntegers plugin, this will allow us to verify signatures more quickly while getting rid of DSA-specific primitives.After filing this in, you can:- (DigitalSignatureAlgorithm timeDecode: 20) to see how signature verification times compare to the previous implementation- (DigitalSignatureAlgorithm testExamplesFromDisk) to read some messages from a file created by the previous implementation. This will help to validate that we can still verify messages correctly."!!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'raa 5/30/2000 15:47'!computeSignatureForMessageHash: hash privateKey: privateKey	"Answer the digital signature of the given message hash using the given private key. A signature is a pair of large integers. The private key is an array of four large integers: (p, q, g, x)."	| p q g x r s k tmp |	p _ privateKey first.	q _ privateKey second.	g _ privateKey third.	x _ privateKey fourth.	r _ s _ 0.	[r = 0 or: [s = 0]] whileTrue: [		k _ self nextRandom160 \\ q.		r _ (self raise: g to: k mod: p) \\ q.		tmp _ (hash + (x * r)) \\ q.		s _ ((self inverseOf: k mod: q) * tmp) \\ q].	^ Array with: r with: s! !!DigitalSignatureAlgorithm methodsFor: 'public' stamp: 'raa 5/30/2000 15:49'!verifySignature: aSignature ofMessageHash: hash publicKey: publicKey	"Answer true if the given signature is the authentic signature of the given message hash. That is, if the signature must have been computed using the private key set corresponding to the given public key. The public key is an array of four large integers: (p, q, g, y)."	| p q g y r s w u1 u2 v0 v |	p _ publicKey first.	q _ publicKey second.	g _ publicKey third.	y _ publicKey fourth.	r _ aSignature first.	s _ aSignature last.	((r > 0) and: [r < q]) ifFalse: [^ false].  "reject"	((s > 0) and: [s < q]) ifFalse: [^ false].  "reject"	w _ self inverseOf: s mod: q.	u1 _ (hash * w) \\ q.	u2 _ (r * w) \\ q.	v0 _ (self raise: g to: u1 mod: p) * (self raise: y to: u2 mod: p).	v _ ( v0 \\ p) \\ q.	^ v = r! !!DigitalSignatureAlgorithm methodsFor: 'large integer arithmetic' stamp: 'raa 5/30/2000 15:47'!isProbablyPrime: p	"Answer true if p is prime with very high probability. Such a number is sometimes called an 'industrial grade prime'--a large number that is so extremely likely to be prime that it can assumed that it actually is prime for all practical purposes. This implementation uses the Rabin-Miller algorithm (Schneier, p. 159)."	| iterations factor pMinusOne b m r a j z couldBePrime |	iterations _ 50.  "Note: The DSA spec requires >50 iterations; Schneier says 5 are enough (p. 260)"	"quick elimination: check for p divisible by a small prime"	SmallPrimes ifNil: [  "generate list of small primes > 2"		SmallPrimes _ Integer primesUpTo: 2000.		SmallPrimes _ SmallPrimes copyFrom: 2 to: SmallPrimes size].	factor _ SmallPrimes detect: [:f | (p \\ f) = 0] ifNone: [nil].	factor ifNotNil: [^ p = factor].	pMinusOne _ p - 1.	b _ self logOfLargestPowerOfTwoDividing: pMinusOne.	m _ pMinusOne // (2 raisedTo: b).	"Assert: pMinusOne = m * (2 raisedTo: b) and m is odd"	Transcript show: '      Prime test pass '.	r _ Random new.	1 to: iterations do: [:i |		Transcript show: i printString; space.		a _ (r next * 16rFFFFFF) truncated.		j _ 0.		z _ (self raise: a to: m mod: p) normalize.		couldBePrime _ z = 1.		[couldBePrime] whileFalse: [			z = 1 ifTrue: [Transcript show: 'failed!!'; cr. ^ false].  "not prime"			z = pMinusOne				ifTrue: [couldBePrime _ true]				ifFalse: [					(j _ j + 1) < b						ifTrue: [z _ (z * z) \\ p]						ifFalse: [Transcript show: 'failed!!'; cr. ^ false]]]].  "not prime"	Transcript show: 'passed!!'; cr.	^ true  "passed all tests; probably prime"! !!DigitalSignatureAlgorithm methodsFor: 'large integer arithmetic' stamp: 'RAA 5/31/2000 08:54'!raise: x to: y mod: n	"Answer ((x raisedTo: y) \\ n) for integers x, y and n, but computed efficiently when x, y, and n are very large positive integers. From Schneier, p. 244."	| s t u |	s _ 1.	t _ x.	u _ y.	[u = 0] whileFalse: [		u odd ifTrue: [			s _ s * t.			s >= n ifTrue: [s _ s \\\ n]].		t _ t * t.		t >= n ifTrue: [t _ t \\\ n].		u _ u bitShift: -1].	^ s! !!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'raa 5/30/2000 15:47'!generateQandP	"Generate the two industrial-grade primes, q (160-bits) and p (512-bit) needed to build a key set. Answer the array (q, p, s), where s is the seed that from which q and p were created. This seed is normally discarded, but can be used to verify the key generation process if desired."	| pBits halfTwoToTheP chunkCount sAndq q twoQ n c w x p s |	pBits _ 512.  "desired size of p in bits"	halfTwoToTheP _ 2 raisedTo: (pBits - 1).	chunkCount _ pBits // 160.	Transcript show: 'Searching for primes q and p...'; cr.	[true] whileTrue: [		sAndq _ self generateSandQ.		Transcript show: '  Found a candidate q.'; cr.		s _ sAndq first.		q _ sAndq last.		twoQ _ q bitShift: 1.		n _ 2.		c _ 0.		[c < 4096] whileTrue: [			w _ self generateRandomLength: pBits s: s n: n.			x _ w + halfTwoToTheP.			p _ (x - ( x \\ twoQ)) + 1.			p highBit = pBits ifTrue: [				Transcript show: '    Testing potential p ', (c + 1) printString, '...'; cr.				(self isProbablyPrime: p) ifTrue: [					Transcript show: '  Found p!!'; cr.					^ Array with: q with: p with: s]].			n _ n + chunkCount + 1.			c _ c + 1]].! !!DigitalSignatureAlgorithm methodsFor: 'private' stamp: 'raa 5/30/2000 15:46'!generateSandQ	"Generate a 160-bit random seed s and an industrial grade prime q."	| hasher s sPlusOne u q |	hasher _ SecureHashAlgorithm new.	[true] whileTrue: [		s _ self nextRandom160.		sPlusOne _ s + 1.		sPlusOne highBit > 160 ifTrue: [sPlusOne _ sPlusOne \\ (2 raisedTo: 160)].		u _ (hasher hashInteger: s) bitXor: (hasher hashInteger: sPlusOne).		q _ u bitOr: ((1 bitShift: 159) bitOr: 1).		(self isProbablyPrime: q) ifTrue: [^ Array with: s with: q]].! !!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'RAA 5/31/2000 13:21'!\\\ anInteger 	"a faster modulo method for use in DSA. Be careful if you try to use this elsewhere"	^(self digitDiv: anInteger neg: false) second! !DigitalSignatureAlgorithm class removeSelector: #exerciseDivide:!DigitalSignatureAlgorithm class removeSelector: #exerciseMultiply:!DigitalSignatureAlgorithm class removeSelector: #exerciseRareCaseDivides!DigitalSignatureAlgorithm class removeSelector: #exerciseRemainder:!DigitalSignatureAlgorithm removeSelector: #asLargePositiveInteger:!DigitalSignatureAlgorithm removeSelector: #divide:by:!DigitalSignatureAlgorithm removeSelector: #multiply:by:!DigitalSignatureAlgorithm removeSelector: #normalize:!DigitalSignatureAlgorithm removeSelector: #primDigitMultiply:with:into:!DigitalSignatureAlgorithm removeSelector: #primDivideLoopPrimRem:div:quo:!DigitalSignatureAlgorithm removeSelector: #primHighestNonZeroDigitIndex:!DigitalSignatureAlgorithm removeSelector: #remainder:mod:!