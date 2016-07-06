'From Squeak2.8alpha of 4 February 2000 [latest update: #2005] on 22 April 2000 at 5:39:27 pm'!"Change Set:		1035RobustCQ-lsDate:			8 April 2000Author:			Lex SpoonWARNING: Untested, just copied --smaTwo changes to the BSD-style listenLoop in ConnectionQueue to deal with unusual circumnstances.First, make the listener socket be an instance variable instead of a temporary variable.  This way, when the CQ is destroy-ed, the listening socket can also be conveniently destroyed.Second, if the listener socket becomes invalid, restart the listenLoop from scratch.  This is important if a ConnectionQueue is saved, because the socket will become invalid when the image starts up again."!!ConnectionQueue methodsFor: 'private' stamp: 'ls 4/8/2000 20:39'!listenLoop	"Private!! This loop is run in a separate process. It will establish up to maxQueueLength connections on the given port."	"Details: When out of sockets or queue is full, retry more frequently, since a socket may become available, space may open in the queue, or a previously queued connection may be aborted by the client, making it available for a fresh connection."	"Note: If the machine is disconnected from the network while the server is running, the currently waiting socket will go from 'isWaitingForConnection' to 'unconnected', and attempts to create new sockets will fail. When this happens, delete the broken socket and keep trying to create a socket in case the network connection is re-established. Connecting and disconnecting was tested under PPP on Mac system 8.1. It is not if this will work on other platforms."	| newConnection |	Socket initializeNetwork.	socket _ Socket newTCP.	"We'll accept four simultanous connections at the same time"	socket listenOn: portNumber backlogSize: 4.	"If the listener is not valid then the we cannot use the	BSD style accept() mechanism."	socket isValid ifFalse: [^self oldStyleListenLoop].	[true] whileTrue: [		socket isValid ifFalse: [			"socket has stopped listening for some reason"			socket destroy.			(Delay forMilliseconds: 10) wait.			^self listenLoop ].		newConnection _ socket waitForAcceptUntil: (Socket deadlineSecs: 10).		(newConnection notNil and:[newConnection isConnected]) ifTrue:			[accessSema critical: [connections addLast: newConnection].			newConnection _ nil].		self pruneStaleConnections]. ! !