'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 1 February 2004 at 9:13:59 pm'!"Change Set:		KCP-195-MoveUpZapAllMethodsDate:			1 February 2004Author:			stephane ducasseBehavior is not an abstract class. So redefine and move zapAllMethods from ClassDescription to Behavior.(removed testZapMethods for now because there is no BehaviorTest class. -dew)"!!Behavior methodsFor: 'accessing method dictionary' stamp: 'sd 2/1/2004 19:41'!zapAllMethods	"Remove all methods in this class which is assumed to be obsolete"	methodDict _ MethodDictionary new.	self class isMeta ifTrue: [self class zapAllMethods]! !ClassDescription removeSelector: #zapAllMethods!