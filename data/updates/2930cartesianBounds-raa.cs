'From Squeak2.9alpha of 17 July 2000 [latest update: #2978] on 8 November 2000 at 6:58:12 pm'!"Change Set:		cartesianBoundsDate:			8 November 2000Author:			Bob Arning- change top, left, bottom and right setters and getters in players to use cartesian coordinates relative to their container (as x and y do presently)"!!Morph methodsFor: 'geometry eToy' stamp: 'RAA 11/8/2000 18:29'!cartesianBoundsTopLeft	"Answer the origin of this morph relative to it's container's cartesian origin. 	NOTE: y DECREASES toward the bottom of the screen"	| w container |	w _ self world ifNil: [^ bounds origin].	container _ self referencePlayfield ifNil: [w].	^ (bounds left - container cartesianOrigin x) @		(container cartesianOrigin y - bounds top)! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:34'!getBottom	"cartesian: decreases towards bottom of screen"	^ self costume cartesianBoundsTopLeft y - self costume height! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:34'!getLeft	"cartesian: decreases towards bottom of screen"	^ self costume cartesianBoundsTopLeft x! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:33'!getRight	"cartesian: decreases towards bottom of screen"	^ self costume cartesianBoundsTopLeft x + self costume width! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:24'!getTop	^ self costume cartesianBoundsTopLeft y! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:51'!setBottom: w	| topLeftNow |	topLeftNow _ self costume cartesianBoundsTopLeft.	^ self costume bottom: self costume top + topLeftNow y - w! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:50'!setLeft: w	| topLeftNow |	topLeftNow _ self costume cartesianBoundsTopLeft.	^ self costume left: self costume left - topLeftNow x + w! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:50'!setRight: w	| topLeftNow |	topLeftNow _ self costume cartesianBoundsTopLeft.	^ self costume right: self costume left - topLeftNow x + w! !!Player methodsFor: 'slot getters/setters' stamp: 'RAA 11/8/2000 18:53'!setTop: w	| topLeftNow |	topLeftNow _ self costume cartesianBoundsTopLeft.	^ self costume top: self costume top + topLeftNow y - w! !