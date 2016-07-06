'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 23 June 2003 at 1:03:51 pm'!"Change Set:		mapcanDate:			23 June 2003Author:			Richard A. O'KeefeThis adds a method #concatenation toSequenceableCollection so that a sequenceof collections can be concatenated to forma single Array.  Brian Pinkney needed aSqueak analogue of the Common Lisp#'mapcan function, and(seq collect: aBlock) concatenationseemed like the simplest and most reusableway to do it."!!SequenceableCollection methodsFor: 'converting' stamp: 'raok 6/23/2003 12:51'!concatenation	|result index|	result _ Array new: (self inject: 0 into: [:sum :each | sum + each size]).	index _ 0.	self do: [:each | each do: [:item | result at: (index _ index+1) put: item]].	^result! !