'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5278] on 1 July 2003 at 3:17:35 pm'!"Change Set:		WeakIDHash-arDate:			1 July 2003Author:			Andreas RaabThe CS makes weak identity dictionaries hash the same way as IdentityDictionary, spreading out hash values to reduce the number of collisions."!!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'ar 7/1/2003 15:15'!scanFor: anObject	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."	| element start finish hash |	finish _ array size.	finish > 4096		ifTrue: [hash _ anObject identityHash * (finish // 4096)]		ifFalse: [hash _ anObject identityHash].	start _ (hash \\ array size) + 1.	"Search from (hash mod size) to the end."	start to: finish do:		[:index | ((element _ array at: index) == nil or: [element key == anObject])			ifTrue: [^ index ]].	"Search from 1 to where we started."	1 to: start-1 do:		[:index | ((element _ array at: index) == nil or: [element key == anObject])			ifTrue: [^ index ]].	^ 0  "No match AND no empty slot"! !!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'ar 7/1/2003 15:15'!scanForNil: anObject	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."	| start finish hash |	finish _ array size.	finish > 4096		ifTrue: [hash _ anObject identityHash * (finish // 4096)]		ifFalse: [hash _ anObject identityHash].	start _ (hash \\ array size) + 1.	"Search from (hash mod size) to the end."	start to: finish do:		[:index | (array at: index) == nil ifTrue: [^ index ]].	"Search from 1 to where we started."	1 to: start-1 do:		[:index | (array at: index) == nil ifTrue: [^ index ]].	^ 0  "No match AND no empty slot"! !"Postscript:Rehash WeakIDDicts"WeakIdentityKeyDictionary allInstancesDo:[:wid| wid rehash].!