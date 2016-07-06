'From Squeak3.3alpha of 30 January 2002 [latest update: #4769] on 7 April 2002 at 2:58:08 pm'!"Change Set:		ModuleClassComments-hgDate:			30 March 2002Author:			Henrik GedenrydExtended class comments for module-related classes."!!DeltaClass commentStamp: 'hg 3/30/2002 10:07' prior: 0!I hold special information needed for representing classes in DeltaModules, e.g. old and new versions of methods, etc.!!Module commentStamp: 'hg 3/30/2002 11:24' prior: 0!The purpose of Modules is to allow the image to be decomposed into smaller, independent parts, instead of the single, hopelessly tangled monolithic image that makes it hard to adapt Squeak to different purposes. It is also meant to make it equally simple for anyone to use Squeak effectively, without having to rely on a central group to manage things.Importantly, Modules only deal with static, non-running program definitions (roughly classes and other definitions), not dynamic (running) applications or instances of classes. The dynamic side is a much larger problem, whose solution is often called 'components', and can be fruitfully built on top of modules.This divides into two main functions: 1. Being able to isolate a module from others. From this point of view, modules replace the global Smalltalk dictionary where all global names used to be defined. To be independent of other parts of the image (i.e. to avoid name clashes), each module has its own namespace. It is also possible to hide a module's contents from the outside by specifying whether a definition should be exported or not (definedNames contains all definitions, exportedNames only those that should be exported).2. Being able to build larger 'collaborations' involving several modules. This is done by defining dependencies between modules. Strictly speaking, each module must define a dependency on every other module whose definitions it wants to access. A third major functionality is to manage loading and storage of modules, including the automatic handling of dependecies during loading. This is meant to make it easy to share work with others. To keep the implementation of Module reasonable small, it only knows about a module that is in the image, as described above. All functionality related to the external storage of modules is delegated to Repository and ModuleInstaller objects, repositories represent modules stored externally, and installer objects handle the actual installing and storage process.DependenciesThe modules in an image can be regarded as nodes in a module graph, where each module has links (edges) to the modules it depends on; these are its 'neighbor modules'. A module can access definitions in these modules, and they will be loaded as prerequisites of this module. ModuleReference (and subclasses') instances represent the links in this graph, and define the dependencies/connections between modules. Each module has a unique identity, described by its "module path" (and version), analogous to a file system path. The path describes a unique place in a virtual hierarchy of all modules. This unique, gloabl identification system ensures that different Squeak developers can work independently without risking name or module clashes (cf. the class comment for VirtualRootModule). Each module is the submodule of exactly one other module, its parent module. The module path is an Array of symbols, made up attaching the (local) name of the module to path of its parent. The root has no parent and its path is empty, i.e. #(). For example, its submodule Squeak has the path #(Squeak), and that module's submodule Morphic has the path #(Squeak Morphic). A SubmoduleReference is a special kind of ModuleReference.!]style[(1841 10 5 15 421 15 482 17 376 18 22 15 2)f3,f3LRepository Comment;,f3,f3LModuleInstaller Comment;,f3,f3LModuleReference Comment;,f3,f3LVirtualRootModule Comment;,f3,f3LSubmoduleReference Comment;,f3,f3LModuleReference Comment;,f3!!ModuleExplorer commentStamp: 'hg 3/30/2002 11:15' prior: 0!This is a first, simplest tool for working with Modules. It is a basic Explorer slightly adapted to suit Modules.!!ModuleInstaller commentStamp: 'hg 3/30/2002 10:06' prior: 0!I manage downloading, (un)loading, and (de)activating modules. I traverse the graph of modules and their neighbors, compute what operations are needed and perform them in a safe and controlled manner, while collecting information about what is done, so that e.g. an interrupted or failed operation can be gracefully reverted.A downloaded module is present in the local cache. A loaded module is present in the image, unloading it takes it out from the image.A loaded module can be active or inactive: Multiple versions of the same module may be loaded simultaneously, but only one may be active at one time. This is the basis for conflict resolution.An active DeltaModule has its changes installed into its base module, which also causes a different version of the base module to be active. A loaded but inactive DeltaModule does not have its changes installed into the base module at the current time. !!ModuleRefactorer commentStamp: 'hg 3/30/2002 11:17' prior: 0!This class holds code for refactoring a modular image. The non-modular image was mechanically converted to a module format but is still deeply tangled. All system categories from an older image will reside in the Squeak subtree #(Squeak <...>) in the new virtual module hierarchy.Create subclasses of me, residing in the submodule Refactorings, that will take an image from one version of the Squeak module subtree to another. This is to allow the image to evolve in an orderly manner through successive refactorings, contributed by various Squeakers.!!ModuleReference commentStamp: 'hg 3/30/2002 11:20' prior: 0!I and my subclasses are used to define neighbor links from a module to other modules: external modules, submodules, parameter modules and delta modules.module  Module or Array -- the referenced module, or its pathname   Symbol or nil -- an alias used for the module within the present modulespecifiedVersion -- the version specifiedimportNames   Boolean -- should the module's defined names be available as if they were defined in the present module, ie. without requiring qualified references?To make module loading manageable, I may declare a module before it has been loaded. (I am then 'unresolved'.) If so, then my module instvar holds the path of the Module object until that object has been created.!!ModuleStorageTests commentStamp: 'hg 3/30/2002 11:26' prior: 0!I test the storage and loading of modules on a local disk.Note that this class needs to run the storage tests first (to save time) and these need to pass for the other tests to be able to pass.!!RemoteModuleAccess commentStamp: 'hg 3/30/2002 11:25' prior: 0!I represent a remote repository for use by Module loading tests.To run these tests you need to have full-privileges access to the server where modules will be stored. Look at the class- side message #ftp:http: for how you can override the default location and use your own server. !!RemoteModuleStorageTests commentStamp: 'hg 3/30/2002 11:26' prior: 0!Tests for the remote loading and storage of modules.!!Repository commentStamp: 'hg 3/30/2002 10:02' prior: 0!Repositories are used to deal with everything relating to loading and unloading objects and so on, so that Modules only deal with what a module does when it is in the image. Repository is an abstract filesystem-based repository. It knows how to store, access and manage the files for a module, including what files are needed, the file names to use and so on. However, a ModuleInstaller is used to perform complex load/install etc. operations.module  a Module -- a reference to my moduledirectory -- a directory object for the disk- or server-based directory where I store my filesmoduleState  -- indicates the loading state of the module in the imageinstallRecord -- may hold info from the install of this module, used for uninstallisStandalone -- flags whether this repository is stored separately or together with is parentisAbstract -- flags whether this is a repository with no actual module contentsA Module creates its Repository on demand, unless the repository must be preserved to store information that cannot be recomputed. This saves memory but more importantly prevents repositories from getting out of phase from changes elsewhere.Externally, repositories are structured into a virtual repository tree that mirrors the virtual module hierarchy, so that the directory path of a repository directly corresponds to the module path. It is a 'virtual' tree because the location of a repository is not hard-wired--there could be mirrors, and sub-repositories may be located on different servers if desired.A module is only stored in a separate (sub)directory from its parent module if its repository is defined as "standalone". Thus a standalone repository stores its module and any submodules whose repositories are non-standalone. This helps to avoid having very many and very small directories and files.!!FileRepository commentStamp: 'hg 3/30/2002 10:04' prior: 0!I represent a repository that is accessed via the OS file system; my directory is a FileDirectory. I am used to keep local "cached" copies of remote repositories.!]style[(84 13 65)f1,f1LFileDirectory Comment;,f1!!RemoteRepository commentStamp: 'hg 3/30/2002 10:04' prior: 0!I am a server-based repository that accesses files via internet protocols (HTTP, FTP), but also file:. My directory is a ServerDirectory. I also cache all my up- and downloads in a local file-based cache repository. If you only provide an FTP url, you get a pure FTP repository with full repository functionality, with read and write access, users (incl. anonymous), and all the usual stuff. This is typically the best option.If you only provide an HTTP url, you get a pure HTTP repository. This allows no users etc., and no write access (which ought to be more secure).If you provide an FTP url and an HTTP url, you get a repository that can be accessed via each of FTP and HTTP.See the class comment of ServerDirectory for more info on passwords, urls, and more.!]style[(121 15 577 15 44)f1,f1LServerDirectory Comment;,f1,f1LServerDirectory Comment;,f1!!SubmoduleReference commentStamp: 'hg 3/30/2002 11:20' prior: 0!A submodule is considered "part" of its parent module, i.e. as belonging to its parent in a part-whole relation. Every module (except the root module) must be the submodule of another module (its parentModule). This gives all modules a location in the module hierarchy and thereby also a unique identifier, which is its module path.In this way, submodule relations are used to structure the module system.My name instvar cannot be nil, since it is used to give the submodule its name and thereby its path.!!TransitionalSmalltalkModule commentStamp: 'hg 3/30/2002 11:21' prior: 0!I have only one instance, which is the module that wraps the old global Smalltalk dictionary. This is a compatibility measure during the transition to a modular image.!