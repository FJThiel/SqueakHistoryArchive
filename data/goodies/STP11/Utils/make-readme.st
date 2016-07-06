
| root out dirs file comment |
root := FileDirectory default pathName, ':SirenSources:STP11:'.
dirs := (FileDirectory newOnPath: root) directoryContents.
dirs := dirs select: [ :d | d at: 4].
dirs := dirs collect: [ :d | d at: 1].
out := FileStream fileNamed: (root, 'README').
out nextPutAll: 'Squeak Goodies README File -- Updated 1997.07.09'; cr; cr;
	nextPutAll: 'The file STP11.build.1.2 files these in to Squeak 1.2.'; cr; cr.
dirs asSortedCollection do:
	[ :dirN |
	out nextPutAll: 			'===========================================================================
',
				dirN; cr; cr.
	(FileDirectory newOnPath: root, dirN) fileNames do:
		[ :in |
		Transcript show: root, dirN, ':', in; cr.
		file := FileStream fileNamed: root, dirN, ':', in.
		comment := file upTo: $!.
		out nextPutAll: 
			(comment copyFrom: 2 to: comment size - 1); cr; cr.
		file close]].
out close

