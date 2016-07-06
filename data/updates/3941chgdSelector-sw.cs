'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 28 February 2001 update 3939] on 20 April 2001 at 9:58:28 pm'!"Change Set:		chgdSelector-swDate:			20 April 2001Author:			Scott WallaceProvides a way to find all change sets that contain a change for a given selector, independent of class, whether it be add, delete, or modify; this is particularly handy for tracking down in just which change set a given method got deleted -- which itself is handy because that may be the only place from which you can recapture the lost method via 'versions'.For example, try evaluating (in a 3.1a image):	ChangeSorter browseChangeSetsWithSelector: #setSizeAndMakeResizable:At the moment there is no UI to this feature -- you need to invoke it by evaluating expressions such as the example above in a workspace"!!ChangeSet methodsFor: 'method changes' stamp: 'sw 4/19/2001 19:45'!hasAnyChangeForSelector: aSelector	"Answer whether the receiver has any change under the given selector, whether it be add, change, or remove, for any class"	changeRecords do:		[:aRecord | (aRecord changedSelectors  includes: aSelector)			ifTrue:	[^ true]].	^ false! !!ChangeSorter class methodsFor: 'services' stamp: 'sw 4/19/2001 20:28'!browseChangeSetsWithSelector: aSelector	"Put up a list of all change sets that contain an addition, deletion, or change of any method with the given selector"	| hits index |	hits _ self allChangeSets select: 		[:cs | cs hasAnyChangeForSelector: aSelector].	hits isEmpty ifTrue: [^ PopUpMenu notify: aSelector , 'is not in any change set'].	index _ hits size == 1		ifTrue:	[1]		ifFalse:	[(PopUpMenu labelArray: (hits collect: [:cs | cs name])					lines: #()) startUp].	index = 0 ifTrue: [^ self].	(ChangeSetBrowser new myChangeSet: (hits at: index)) open"ChangeSorter browseChangeSetsWithSelector: #clearPenTrails"! !