'From Squeak 2.3 of January 14, 1999 on 27 January 1999 at 10:15:19 pm'!"Change Set:		IPaddress-mdrDate:			27 January 1999Author:			Michael Rutenberg (mdr@scn.org)Corrects a small bug that prevented the lookup of host names which begin with a digit, such as 100000jobs.de   Host names lookup will now be fully compliant with RFC 1123 section 2.1"!!NetNameResolver class methodsFor: 'lookups' stamp: 'mdr 1/27/1999 21:47'!addressForName: hostName timeout: secs	"Look up the given host name and return its address. Return nil if the address is not found in the given number of seconds."	"NetNameResolver addressForName: 'create.ucsb.edu' timeout: 30"	"NetNameResolver addressForName: '100000jobs.de' timeout: 30"	"NetNameResolver addressForName: '1.7.6.4' timeout: 30"	"NetNameResolver addressForName: '' timeout: 30 (This seems to return the Squeak machine address)"	| deadline ready success result |	"check if this is a valid numeric host address (e.g. 1.2.3.4)"	result _ self addressFromString: hostName.	result isNil ifFalse: [^result].	"Look up a host name, including ones that start with a digit (e.g. 100000jobs.de or squeak.org)"	deadline _ Time millisecondClockValue + (secs * 1000).	ready _ self waitForResolverReadyUntil: deadline.	ready ifFalse: [^ nil].	self primStartLookupOfName: hostName.	success _ self waitForCompletionUntil: deadline.	success		ifTrue: [^ self primNameLookupResult]		ifFalse: [^ nil].! !"Postscript:"!