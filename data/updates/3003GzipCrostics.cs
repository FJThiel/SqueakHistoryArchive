'From Squeak2.9alpha of 12 June 2000 [latest update: #3051] on 30 November 2000 at 10:15:47 am'!"Change Set:		GzipCrosticsDate:			30 November 2000Author:			Dan IngallsLets CrosticPanels open directly from gzipped files.  Smaller, more transportable, and don't give away the solution if you see them in the fileList."!!CrosticPanel class methodsFor: 'as yet unclassified' stamp: 'di 11/30/2000 09:51'!newFromFile: aStream  "World addMorph: CrosticPanel new"	"World addMorph: (CrosticPanel newFromFile: (FileStream readOnlyFileNamed: 'first.crostic'))"	| quoteWithBlanks citation clue numberLine numbers clues answers indexableQuote quotePanel crosticPanel buttonRow quoteWidth |	(aStream next asciiValue = 16r1F) & (aStream next asciiValue = 16r8B) ifTrue:		["It's gzipped..."  aStream skip: -2.		^ self newFromFile: aStream asUnZippedStream ascii].	aStream skip: -2.	quoteWithBlanks _ aStream nextLine.	quoteWithBlanks _ quoteWithBlanks asUppercase select: [:c | c isLetter or: [' -' includes: c]].	indexableQuote _ quoteWithBlanks select: [:c | c isLetter].	citation _ aStream nextLine.	aStream nextLine.	clues _ OrderedCollection new.	answers _ OrderedCollection new.	[aStream atEnd] whileFalse:		[clue _ aStream nextLine.		"Transcript cr; show: clue."		clues addLast: clue.		numberLine _ aStream nextLine.		numbers _ Scanner new scanTokens: numberLine.		answers addLast: numbers].	aStream close.	"Consistency check: "	(citation asUppercase select: [:c | c isLetter]) =		(String withAll: (answers collect: [:a | indexableQuote at: a first]))		ifFalse: [self error: 'mal-formed crostic file'].		crosticPanel _ super new.	quotePanel _ CrosticQuotePanel new quote: quoteWithBlanks answers: answers cluesPanel: crosticPanel.	crosticPanel color: quotePanel firstSubmorph color;		quote: indexableQuote clues: clues answers: answers quotePanel: quotePanel.	buttonRow _ crosticPanel buttonRow.	quoteWidth _ (crosticPanel width + quotePanel firstSubmorph width)					max: buttonRow width.	quotePanel extent: quoteWidth @ 9999.	crosticPanel addMorph: quotePanel.	^ crosticPanel breakColumnAndResizeWithButtons: buttonRow! !!CrosticPanel class methodsFor: 'as yet unclassified' stamp: 'di 11/30/2000 10:15'!sampleFile 	"If you want to enter a new acrostic, follow this format exactly with regard to CRs and the like, and store it in a file.  Do not double the string quotes as here -- that is only because they are embedded in a string.  Finally, compress the file in the fileList (so it will be easy to transport and hard to read), and name it 'yourName.crostic' so that the 'open' button on the panel will recognize it."	^'Men and women do not feel the same way about dirt.  Women for some hormonal reason can see individual dirt molecules, whereas men tend not to notice them until they join together into clumps large enough to support commercial agriculture.Dave Barry''s Guide to MarriageBoccaccio''s collection of tales74 19 175 156 9 122 84 113 104Wooden instrument of Swiss herders67 184 153 103 14 142 148 54 3Evening service76 99 154 171 89 194 69Russian-born American anarchist (2 wds)159 102 177 25 186 134 128 82 50 62 11Apple-polish (2 wds)32 190 129 126 179 157 79 170Visual-gesture means of communication4 178 27 168 150 185 114Postponed contest173 58 77 65 8 124 85Groundbreaking invention98 15 116 162 112 37 92 155 70 187Material used to make English longbows132 195 28Gracile48 191 145 152Have the effrontery; experience a high (2 wds)164 61 137 33 17 45Florentine painter who experimented with perspective91 181 189 2 20 81 167Sondheim opus (3 wds)72 109 147 13 192 165 93 40 115 138 6 63Spanish rake108 56 44 133 193 29 125Emergence  as of an adult butterfly106 149 59 41 24 135 87 68Type of rifle (hyph)111 7 143 73 39 30 105 95 53Free of charge (3 wds)176 107 120 130 160 22 46 34 94 71Pie filling86 75 136 118 43Master filmmaker31 151 174 51 163 144Longtime sportswriter for the NY Herald tribune (2 wds)60 140 12 101 55 188 166 121Birthplace of Erasmus47 64 141 21 10 180 36 80 1Mae West classic (3 wds)127 123 161 110 183 5 139 97 88Element that glows blue in the dark100 90 35 182 146 117 169 26Sturm und Drang writer158 172 119 16 52 23Starfish or sea cucumber18 66 96 83 57 49 78 131 38 42'! !