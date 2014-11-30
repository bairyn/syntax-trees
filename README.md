Notes on repository.

This repository hosts currently a fork of Dominic Orchard's syntax-trees
project residing in a VCS repository, git in particular, with the following
branches:

 * master: Currently the branch for the actual fork, for the
     syntax-trees-fork-bairyn package.  Version tags refer to commits in this
     branch.
 * cherry-picked-build-fixes: Contains the initial import of the syntax-trees
     package with just build fixes cherry picked in.
 * cherry-picked-build-and-warning-fixes: Based on cherry-picked-build-fixes,
     this branch adds fixes to warnings, an addition of the "-Wall" flag for
     GHC so that warnings are emitted during the build process, and the
     correction of "template-haskell" typos.
 * fork-no-rename: Based on master, and adds commits that reverses the renaming
     of the fork and module names.
