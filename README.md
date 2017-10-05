nix-lisp
========

This utility provides a single `nix` binary for managing your Nixpkgs and NixOS installation. It makes it
easy—at least for me—to manage my system, instead of memorizing many commands with different
interfaces.

This program was salvaged from [ebzzry/scripts](https://github.com/ebzzry/scripts), turning it into
a repository of its own, to make it easier to distribute.


Building
--------

```bash
$ make install
```


Commands
--------

### Base

- out-path
- which
- store
- repl
- shell, pure-shell
- impure-shell
- rebuild
- rebuild-switch
- rebuild-switch-upgrade
- instantiate
- eval- grep
- find
- install-package
- install-package-uri
- references
- referrers
- query-root
- closure
- set-flag
- option
- gargage-collect
- garbage-collect-delete


### Channel management

- channel
- channel-list
- channel-add
- channel-remove
- channel-update
- channel-name
- root-channel
- root-channel-list
- root-channel-add
- root-channel-remove
- root-channel-update
- root-channel-name


### Channel commands

- env
- build
- query
- upgrade
- upgrade-always
- install
- Install
- query-available
- compare-versions
- compare-versions-lt
- compare-versions-eq
- compare-versions-gt
- describe-available
- index-available
- search-available
- view-available

### Upstream (git checkout) comands

- upstream-env
- upstream-build
- upstream-query
- upstream-upgrade
- upstream-upgrade-always
- upstream-install
- upstream-Install
- upstream-query-available
- upstream-compare-versions
- upstream-compare-versions-lt
- upstream-compare-versions-eq
- upstream-compare-versions-gt
- upstream-describe-available
- upstream-index-available
- upstream-search-available
- upstream-view-available


### Query about installed packages

- view-installed
- search-installed
- index-installed
- describe-installed
- query-installed


### Common commands

- uninstall
- update
- root-update
- upstream-update
- build-index
- full-update
- full-upgrade
- full-search


## Miscellany

- view-packages
- make
- nix-version
- nixpkgs-version
- nixos-version


### Prefetch commands

- fetch-url
- fetch-file
- fetch-git
- fetch-zip
- fetch-hg
- fetch-svn
- fetch-bzr
- fetch-cvs
