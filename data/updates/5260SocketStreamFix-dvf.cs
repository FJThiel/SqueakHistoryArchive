'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5259] on 11 June 2003 at 11:52:52 pm'!"Change Set:		SocketStreamFixDate:			11 June 2003Author:			Daniel VainsencherCode used direct access where it should have used lazy initializing accessor."!!SocketStream methodsFor: 'testing' stamp: 'dvf 6/11/2003 18:21'!shouldTimeout	^self timeout > 0! !