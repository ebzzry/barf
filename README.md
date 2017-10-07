nix-lisp
========

This utility provides a single `nix` binary for managing your Nixpkgs and NixOS installation. It
makes it easier, at least for me, instead of memorizing many commands with different
interfaces. This is not exhaustive and only covers the commands listed [here](#commands).

For the lazy and impatient, click [here](#frombinary). Only Linux x86-64 binaries, for now. You
still need the the items in [system dependencies](#systemdependencies), for the program to work.

This program was salvaged from [ebzzry/scripts](https://github.com/ebzzry/scripts), turning it into
a repository of its own, to make it easier to distribute. In this document, the `$` symbol
represents the user prompt, while the `*` symbol represents the lisp prompt.


Table of contents
-----------------

- [Dependencies](#dependencies)
  + [Nix](#installnix)
  + [Build dependencies](#builddependencies)
  + [Runtime dependencies](#runtimedependencies)
- [Installation](#installation)
  + [From binary](#frombinary)
  + [From source](#fromsource)
  + [Initialize the databases](#initialize)
- [Commands](#commands)
  + [Base commands](#basecommands)
  + [Channel management](#channelmanagementcommands)
  + [Channel commands](#channelcommands)
  + [Upstream commands](#upstreamcommands)
  + [Querying packages](#querycommands)
  + [Common commands](#commoncommands)
  + [Miscellaneous commands](#miscellaneouscommands)
  + [Prefetch commands](#prefetchcommands)
- [Usage](#usage)


<a name="dependencies">Dependencies</a>
---------------------------------------

### <a name="installnix">Nix</a>

If you don’t have Nix, yet, run:

```bash
$ curl https://nixos.org/nix/install | bash
```

### <a name="builddependencies">Build dependencies</a>

- curl
- git
- xz-utils
- bzip2
- make
- cl-launch ≥ 4.1
- sbcl ≥ 1.3.20
- asdf ≥ 3.2.0
- quicklisp ≥ 2017-03-06


### <a name="runtimedependencies">Runtime dependencies</a>

These are the required minimum for running `nix` on a regular basis:

- curl
- git
- xz-utils
- sudo

If you are going to use the `fetch-*` commands, install the following, too:

- subversion
- mercurial
- zip
- bzr
- cvs


<a name="installation">Installation</a>
---------------------------------------

### <a name="frombinary">From binary</a>

Download the latest release:

```bash
$ mkdir ~/bin
$ curl -SLo ~/bin/nix https://github.com/ebzzry/nix-lisp/releases/download/v0.0.1/nix
```


### <a name="fromsource">From source</a>

First, install Quicklisp:

```bash
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
* (quit)
```

Then, upgrade ASDF to the latest version:

```bash
$ mkdir ~/common-lisp
$ cd ~/common-lisp
$ git clone https://gitlab.common-lisp.net/asdf/asdf.git
```

While still in `~/common-lisp/`, clone this repo:

```bash
$ git clone https://github.com/ebzzry/nix-lisp
```

Finally, build the binary, then install it to `~/bin/`:

```bash
$ mkdir ~/bin
$ cd nix-lisp
$ make install
```


### <a name="initialize">Initialize the databases</a>

On your first run, initialize the databases for the upstream nixpkgs checkout and index database:

```bash
$ nix init
```

Periodically, run the following command to update the databases for the aforementioned databases,
plus the channels for the user and root:

```bash
$ nix full-update
```


<a name="commands">Commands</a>
-------------------------------

Below are the currently available commands. When an option looks like `<package>` it means it
accepts at least one *package* argument. When an option looks like `<package?>`it means it accepts
zero ore more *package* arguments. When a command doesn’t have an argument, it means it doesn’t take
any.

For convenience, the commands—aside from the `fetch-*` ones—can be shortened to its initials, for example:

```bash
$ nix rebuild-switch-upgrade
```

can be shortened to

```bash
$ nix r-s-u
```

### <a name="basecommands">Base commands</a>

- `out-path <package>`
- `which <binary>`
- `store <options>`
- `repl`
- `shell`
- `impure-shell`
- `rebuild`
- `rebuild-switch`
- `rebuild-switch-upgrade`
- `instantiate`
- `eval <expression>`
- `grep <string>`
- `find <package>`
- `install-package <location>`
- `install-package-uri <location>`
- `references <package>`
- `referrers <package>`
- `query-root`
- `closure <package>`
- `set-flag <options>`
- `option <options>`
- `garbage-collect`
- `garbage-collect-delete`


### <a name="channelmanagementcommands">Channel management</a>

- `channel <options>`
- `channel-list`
- `channel-add <url> <name>`
- `channel-remove <name>`
- `channel-update`
- `channel-name`
- `root-channel <options>`
- `root-channel-list`
- `root-channel-add <url> <name>`
- `root-channel-remove <name>`
- `root-channel-update`
- `root-channel-name`


### <a name="channelcommands">Channel commands</a>

- `env <options>`
- `build <options>`
- `query <package>`
- `upgrade <package?>`
- `upgrade-always <package?>`
- `install <package>`
- `Install <package>`
- `query-available`
- `compare-versions`
- `compare-versions-less-than`
- `compare-versions-equal`
- `compare-versions-greater-than`
- `describe-available`
- `index-available`
- `search-available <package>`
- `view-available`


### <a name="upstreamcommands">Upstream (git checkout) commands</a>

- `upstream-env <options>`
- `upstream-build <options>`
- `upstream-query <package>`
- `upstream-upgrade <package?>`
- `upstream-upgrade-always <package?>`
- `upstream-install <package>`
- `upstream-Install <package>`
- `upstream-query-available`
- `upstream-compare-versions`
- `upstream-compare-versions-less-than`
- `upstream-compare-versions-equal`
- `upstream-compare-versions-greater-than`
- `upstream-describe-available`
- `upstream-index-available`
- `upstream-search-available <package>`
- `upstream-view-available`


### <a name="querycommands">Querying packages</a>

- `view-installed`
- `search-installed <package>`
- `index-installed`
- `describe-installed`
- `query-installed <package>`


### <a name="commoncommands">Common commands</a>

- `uninstall <package>`
- `update`
- `root-update`
- `upstream-update`
- `index <package>`
- `full-update`
- `full-upgrade`
- `full-search <package>`


## <a name="miscellaneouscommands">Miscellaneous commands</a>

- `view-packages`
- `make`
- `nix-version`
- `nixpkgs-version`
- `nixos-version`


### <a name="prefetchcommands">Prefetch commands</a>

- `fetch-url <options>`
- `fetch-file <options>`
- `fetch-git <options>`
- `fetch-zip <options>`
- `fetch-hg <options>`
- `fetch-svn <options>`
- `fetch-bzr <options>`
- `fetch-cvs <options>`


Usage
-----

To install the latest Firefox from upstream:

```bash
$ nix upstream-install firefox
```

To install the latest Firefox from channels:

```bash
$ nix install firefox
```

To uninstall it:

```bash
$ nix uninstall firefox
```

To search for upstream packages with the name `firefox`:

```bash
$ nix upstream-search firefox
```

To search for channel packages with the name `firefox`:

```bash
$ nix search firefox
```

To search for packages from both upstream and channels, with the name `firefox`:

```bash
$ nix full-search firefox
```

To display the version of Nix, Nixpkgs, and NixOS:

```bash
$ for i in nix nixpkgs nixos; do nix $i-version; done
```

To view the list of installed packages, using the index:

```bash
$ nix view-installed
```

To view the list of installed packages, using querying:

```bash
$ nix query-installed
```

To view the list of installed packages, including description:

```bash
$ nix describe-installed
```

To view the Haskell packages:

```bash
$ nix view-packages haskellPackages
```

To search if `firefox` installed:

```bash
$ nix search-installed firefox
```

To know which package has the binary `firefox`:

```bash
$ nix which firefox
```

To get the store path of the `firefox` binary available in your PATH:

```bash
$ nix out-path firefox
```

To look for files in the upstream containing the string `firefox`:

```bash
$ nix find firefox
```

To grep the case insensitive string `firefox` in the upstream, displaying the name of the matching file:

```bash
$ nix grep -iH firefox
```

To garbage collect:

```bash
$ nix garbage-collect
```

To aggressively garbage collect:

```bash
$ nix garbage-collect-delete
```

To subscribe to the `nixos-unstable` channel for the current user:

```bash
$ nix channel-add https://nixos.org/channels/nixos-unstable nixos
$ nix channel-update
```

To subscribe to the `nixos-unstable` channel for root:

```bash
$ nix root-channel-add https://nixos.org/channels/nixos-unstable nixos
$ nix root-channel-update
```

To rebuild NixOS from `/etc/nixos/configuration.nix` then perform switch:

```bash
$ nix rebuild-switch
```

To rebuild NixOS from `/etc/nixos/configuration.nix`, perform switch, and upgrade:

```bash
$ nix rebuild-switch-upgrade
```

To update the user channel, root channel, upstream nixpkgs checkout, and index database:

```bash
$ nix full-update
```

To perform the above, then upgrade the whole NixOS system:

```bash
$ nix full-upgrade
```
