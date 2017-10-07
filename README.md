nix-lisp
========

This utility provides a single `nix` binary for managing your Nixpkgs and NixOS installation. It
makes it easier, at least for me, instead of memorizing many commands with different interfaces.

For the lazy and impatient, click [here](#frombinary). Only Linux x86-64 binaries, for now. You
still need the the items in [system dependencies](#systemdependencies), for the program to work.

This program was salvaged from [ebzzry/scripts](https://github.com/ebzzry/scripts), turning it into
a repository of its own, to make it easier to distribute. In this document, the `$` symbol
represents the user prompt, while the `*` symbol represents the lisp prompt.


Table of contents
-----------------

- [Dependencies](#dependencies)
- [Installation](#installation)
  - [From binary](#frombinary)
  - [From source](#fromsource)
- [Commands](#commands)
  + [Base commands](#basecommands)
  + [Channel management](#channelmanagementcommands)
  + [Channel commands](#channelcommands)
  + [Upstream commands](#upstreamcommands)
  + [Querying packages](#querycommands)
  + [Common commands](#commoncommands)
  + [Miscellaneous commands](#miscellaneouscommands)
  + [Prefetch commands](#prefetchcommands)


<a name="dependencies">Dependencies</a>
---------------------------------------

- nix
- curl
- git
- xz-utils
- bzip2
- make
- cl-launch ≥ 4.1
- sbcl ≥ 1.3.20
- asdf ≥ 3.2.0
- quicklisp ≥ 2017-03-06


<a name="installation">Installation</a>
---------------------------------------


### <a name="frombinary">From binary</a>

Download the latest release:

```bash
$ mkdir ~/bin
$ curl -o ~/bin/nix https://github.com/ebzzry/nix-lisp/releases/download/v0.0.1/nix
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


### Initialize the databases

On your first run, initialize the databases for the upstream nixpkgs checkout and index database:

```bash
$ nix init
```

Peridiocally, run the following command to update the databases for the aforementioned databases,
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

For convenience, most of the commands can be shorted to its initials, for example:

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
- `gargage-collect`
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
- `compare-versions-lt`
- `compare-versions-eq`
- `compare-versions-gt`
- `describe-available`
- `index-available`
- `search-available <package>`
- `view-available`


### <a name="upstreamcommands">Upstream (git checkout) comands</a>

- `upstream-env <options>`
- `upstream-build <options>`
- `upstream-query <package>`
- `upstream-upgrade <package?>`
- `upstream-upgrade-always <package?>`
- `upstream-install <package>`
- `upstream-Install <package>`
- `upstream-query-available`
- `upstream-compare-versions`
- `upstream-compare-versions-lt`
- `upstream-compare-versions-eq`
- `upstream-compare-versions-gt`
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
