'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5548] on 18 November 2003 at 3:02:44 pm'!"Change Set:		initializeProtoFix-mdDate:			18 November 2003Author:			Marcus DenkerThis changeset just moves the empty implementation of'initialize' from Object to ProtoObject. This allows todo a 'ProtoObject new' without getting an error."!!ProtoObject methodsFor: 'initialize-release' stamp: 'md 11/18/2003 10:33'!initialize	"Subclasses should redefine this method to perform initializations on instance creation"! !Object removeSelector: #initialize!