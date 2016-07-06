'From Squeak3.1alpha of 28 February 2001 [latest update: #4319] on 5 September 2001 at 9:54:28 am'!"Change Set:		PlayerSibCopy-tkDate:			5 September 2001Author:			Ted KaehlerFix a bug in 4317PlayerDeepCopy.  UnscriptedPlayers are like Morphs and Players, they should be copied weakly when they appear in the inst vars of a Player."!!Player methodsFor: 'copying' stamp: 'tk 9/5/2001 09:43'!veryDeepInner: deepCopier	"Special code that handles user-added instance variables of a uniClass.	Copy all of my instance variables.  Some need to be not copied at all, but shared.  This is special code for the dictionary.  See DeepCopier."	| instVar weak subAss |	super veryDeepInner: deepCopier.	"my own instance variables are completely normal"	costume _ costume veryDeepCopyWith: deepCopier.	costumes _ costumes veryDeepCopyWith: deepCopier.	Player instSize + 1 to: self class instSize do: [:index |		instVar _ self instVarAt: index.		weak _ instVar isMorph | instVar isPlayerLike. 		(subAss _ deepCopier references associationAt: instVar ifAbsent: [nil])				"use association, not value, so nil is an exceptional value"			ifNil: [weak ifFalse: [					self instVarAt: index put: (instVar veryDeepCopyWith: deepCopier)]]			ifNotNil: [self instVarAt: index put: subAss value].		].!]style[(25 205 10 659)f1b,f1,f1LDeepCopier Comment;,f1! !!WonderlandActor methodsFor: 'copying' stamp: 'tk 9/5/2001 09:44'!veryDeepInner: deepCopier	"Special code that handles user-added instance variables of a uniClass.	Copy all of my instance variables.  Some need to be not copied at all, but shared.  This is special code for the dictionary.  See DeepCopier."	| instVar weak subAss |	super veryDeepInner: deepCopier.	"my own instance variables are completely normal"	myName _ myName veryDeepCopyWith: deepCopier.	myWonderland _ myWonderland.		"don't make a new one"	myMesh _ myMesh veryDeepCopyWith: deepCopier.	myTexture _ myTexture veryDeepCopyWith: deepCopier.	myMaterial _ myMaterial veryDeepCopyWith: deepCopier.	myColor _ myColor veryDeepCopyWith: deepCopier.	scaleMatrix _ scaleMatrix veryDeepCopyWith: deepCopier.	hidden _ hidden veryDeepCopyWith: deepCopier.	firstClass _ firstClass veryDeepCopyWith: deepCopier.	myReactions _ myReactions veryDeepCopyWith: deepCopier.	myProperties _ myProperties veryDeepCopyWith: deepCopier.	"may copy too deeply"	WonderlandActor instSize + 1 to: self class instSize do: [:index |		instVar _ self instVarAt: index.		weak _ instVar isMorph | instVar isPlayerLike. 		(subAss _ deepCopier references associationAt: instVar ifAbsent: [nil])				"use association, not value, so nil is an exceptional value"			ifNil: [weak ifFalse: [					self instVarAt: index put: (instVar veryDeepCopyWith: deepCopier)]]			ifNotNil: [self instVarAt: index put: subAss value].		].!]style[(25 205 10 1170)f1b,f1,f1LDeepCopier Comment;,f1! !