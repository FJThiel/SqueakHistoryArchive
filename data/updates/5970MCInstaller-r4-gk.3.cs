'From Squeak3.7beta of ''1 April 2004'' [latest update: #5969] on 13 July 2004 at 3:09:10 am'!"Change Set:		MCInstaller-r4-gkDate:			13 July 2004Author:			G�ran KrampeThis changeset uses SM to upgrade MCInstaller to version 10 (r4)."| map |Cursor wait showWhile:[map _ SMSqueakMap default loadUpdates."Install r4 (autoversion) of MCInstaller if not installed or an older one is installed"v _ map installedVersionOfPackageWithId: (UUID fromString: 'af9d090d-2896-4a4e-82d0-c61cf2fdf40e').(v isNil or: [v < '4' asVersion]) ifTrue: [	map installPackageWithId: 'af9d090d-2896-4a4e-82d0-c61cf2fdf40e' autoVersion: '4']]!