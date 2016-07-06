﻿'From Squeak3.9alpha of 4 July 2005 [latest update: #7028] on 5 May 2006 at 2:30:36 am'!"Change Set:		SMUpgradeFor39-gkDate:			5 May 2006Author:			Göran KrampeThis changeset is a do-it changeset that upgrades a 3.9-7029 image to current SM version and ends with purging the map from the image (but keeping info on installed packages). Everything is in the preamble. This is a simplified version of the SqueakMap loadscript - we already know quite a bit about the 7029 image (like that we have Monticello in it for example) so we can make it much simpler."| installed |installed := SMSqueakMap default installedPackagesDictionary."This installs SMBase autoversion 12"(MCMczReader versionFromStream: (HTTPClient httpGet:	('http://map.squeak.org/package/c4c13ea3-e376-42c7-8d9e-dc23b09f9f29/autoversion/12/downloadurl' asUrl retrieveContents content))) load."Get aninstance and put installed packages info back into it."map := SMSqueakMap default.map installedPackagesDictionary: installed."Remove old SM Package Loader, if present"map clearInstalledPackageWithId: '047a3b12-7e52-4c5d-be8b-d06635fc4f1c'."Remove old SqueakMap Base, if present"map clearInstalledPackageWithId: 'fffa45d3-2459-4b7d-b594-9cfae17c864d'."Remove old SqueakMap loadscript, if present"map clearInstalledPackageWithId: '4f0b9db6-8add-43aa-8d6b-53e6a0ea8442'."Add that we now have SqueakMap2 base"map noteInstalledPackageWithId: 'c4c13ea3-e376-42c7-8d9e-dc23b09f9f29' autoVersion: '12'."Make sure map is updated"map loadUpdates."Install version 2 (bugfixed) of new SMLoader using SM2, hmmm, MCInstaller makes silly changesetnames..."map installPackageWithId: '941c0108-4039-4071-9863-a8d7d2b3d4a3' autoVersion: '8'."Ok, we are done, purge the map from the image. The installation registry is not lost."map purge!