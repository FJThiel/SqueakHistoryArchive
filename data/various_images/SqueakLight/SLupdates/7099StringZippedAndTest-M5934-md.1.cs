'From SqueakLight|II of 31 May 2008 [latest update: #7077] on 10 June 2008 at 9:13:20 am'!!String methodsFor: 'converting' stamp: 'dgd 11/26/2005 21:19'!zipped	| stream gzstream |	stream := RWBinaryOrTextStream on: String new.	gzstream := GZipWriteStream on: stream.	gzstream nextPutAll: self.	gzstream close.	stream reset.	^ stream contents.! !