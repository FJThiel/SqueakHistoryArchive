'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #328] on 6 October 2004 at 9:06:16 pm'!"Change Set:		AtlantaFix-bfDate:			6 October 2004Author:			Bert FreudenbergFix Atlanta font encoding, which still was MacRoman"(TextStyle named: 'Atlanta') ifNotNilDo: [:s |	s fonts do: [:f | 		f migrateSqueakToIso ]]!