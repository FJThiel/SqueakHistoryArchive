'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6540] on 7 January 2005 at 6:18:26 pm'!"Change Set:		Embed-bfDate:			7 January 2005Author:			Andreas Raab / Jerome PeaceFix for bug #591 as suggested by Andreas Raab"!!Morph methodsFor: 'meta-actions' stamp: 'wiz 1/2/2005 01:06'!potentialEmbeddingTargets	"Return the potential targets for embedding the receiver"	| oneUp topRend |	(oneUp _ (topRend _ self topRendererOrSelf) owner) ifNil:[^#()].	^ (oneUp morphsAt: topRend referencePosition behind: topRend unlocked: true) select:		[:m | m  isFlexMorph not]! !