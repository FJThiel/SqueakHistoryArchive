'From Squeak3.4 of 1 March 2003 [latest update: #5170] on 6 May 2003 at 5:13:05 pm'!"Change Set:		KCP-0009-pullUpAllSub
Date:			12 March 2003
Author:			alexandre bergel

Pull up the method allSubclasses from ClassDescription to Behavior"!!Behavior methodsFor: 'accessing class hierarchy' stamp: 'nb 5/6/2003 17:11'!allSubclasses	"Answer a Set of the receiver's and the receiver's descendent's subclasses. "	| scan scanTop |	scan _ OrderedCollection withAll: self subclasses.	scanTop _ 1.	[scanTop > scan size]		whileFalse: [scan addAll: (scan at: scanTop) subclasses.			scanTop _ scanTop + 1].	^ scan asSet! !ClassDescription removeSelector: #allSubclasses!