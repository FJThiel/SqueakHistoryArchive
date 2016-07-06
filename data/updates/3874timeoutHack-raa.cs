'From Squeak3.1alpha of 5 February 2001 [latest update: #3885] on 28 March 2001 at 10:01:34 am'!"Change Set:		timeoutHackDate:			28 March 2001Author:			Bob ArningTry to reduce spurious timeouts on sockets communicating over slow lines."!!Socket methodsFor: 'sending-receiving' stamp: 'RAA 3/28/2001 09:59'!sendData: aStringOrByteArray	"Send all of the data in the given array, even if it requires multiple calls to send it all. Return the number of bytes sent."	"An experimental version use on slow lines: Longer timeout and smaller writes to try to avoid spurious timeouts."	| bytesSent bytesToSend count |	bytesToSend _ aStringOrByteArray size.	bytesSent _ 0.	[bytesSent < bytesToSend] whileTrue: [		(self waitForSendDoneUntil: (Socket deadlineSecs: 60))			ifFalse: [self error: 'send data timeout; data not sent'].		count _ self primSocket: socketHandle			sendData: aStringOrByteArray			startIndex: bytesSent + 1			count: (bytesToSend - bytesSent min: 5000).		bytesSent _ bytesSent + count].	^ bytesSent! !