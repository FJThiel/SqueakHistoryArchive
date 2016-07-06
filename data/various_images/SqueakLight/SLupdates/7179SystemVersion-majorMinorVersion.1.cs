'From SqueakLight|II of 31 May 2008 [latest update: #7177] on 25 June 2008 at 7:05:04 am'!!SystemVersion methodsFor: '*smbase-extension' stamp: 'jcg 11/2/2004 10:03'!majorMinorVersion	"Return the major/minor version number of the form X.Y, without any 'alpha' or 'beta' or other suffix."	"(SystemVersion new version: 'Squeak3.7alpha') majorMinorVersion" "  -->  'Squeak3.7' "	"SystemVersion current majorMinorVersion"		| char stream |	stream := ReadStream on: version, 'x'.	stream upTo: $..	char := stream next.	char ifNil: [^ version].	"eg: 'Jasmine-rc1' has no $. in it."	[char isDigit]		whileTrue: [char := stream next].	^ version copyFrom: 1 to: stream position - 1! !