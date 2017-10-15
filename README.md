nix-lisp
========

This utility provides a single `nix` binary for managing your Nixpkgs and NixOS installation. It
makes it easier, at least for me, instead of memorizing many commands with different
interfaces. This is not exhaustive and only covers the commands listed [here](#commands).

For the lazy and impatient, click [here](#frombinary). You still need the the items in
[runtime dependencies](#runtimedependencies), for the program to work.

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

Download the latest release for Linux:

```bash
$ mkdir ~/bin
$ curl -SL https://github.com/ebzzry/nix-lisp/releases/download/v0.0.5/nix-0.0.5-linux-x86_64.tar.gz | tar -C ~/bin -xzf -
```

Download the latest release for macOS:

```bash
$ mkdir ~/bin
$ curl -SL https://github.com/ebzzry/nix-lisp/releases/download/v0.0.4/nix-0.0.4-macos-x86_64.tar.gz | tar -C ~/bin -xzf -
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

If you’re on macOS, you need to install cl-launch:

```bash
$ git clone https://github.com/fare/cl-launch.git
$ cd cl-launch
$ make install
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

Periodically, run the following command to update the aforementioned databases, plus the channels
for the user and root:

```bash
$ nix full-update
```


<a name="commands">Commands</a>
-------------------------------

Below are the currently available commands. When an option looks like `<package>` it means it
accepts at least one *package* argument. When an option looks like `<package?>`it means it accepts
zero ore more *package* arguments. When a command doesn’t have an argument, it means it doesn’t take
any. The `|` indicates an alternative, shorter name.

### <a name="basecommands">Base commands</a>

- `out-path|o-p <package>`
- `which|h <binary>`
- `store <options>`
- `repl`
- `pure-shell|shell`
- `impure-shell|i-s`
- `rebuild`
- `rebuild-switch|r-s`
- `rebuild-switch-upgrade|r-s-u`
- `instantiate`
- `eval <expression>`
- `grep <string>`
- `find <package>`
- `install-package|i-p <location>`
- `install-package-uri|i-p-u <location>`
- `references|r <package>`
- `referrers|R <package>`
- `query-root|q-r`
- `closure|c <package>`
- `set-flag|s-f <options>`
- `option|o <options>`
- `garbage-collect|g-c`
- `garbage-collect-delete|g-c-d`


### <a name="channelmanagementcommands">Channel management</a>

- `channel|ch <options>`
- `channel-list|ch-l`
- `channel-add|ch-a <url> <name>`
- `channel-remove|ch-r <name>`
- `channel-update|ch-u`
- `channel-name|ch-n`
- `root-channel|r-ch <options>`
- `root-channel-list|r-ch-l`
- `root-channel-add|r-ch-a <url> <name>`
- `root-channel-remove|r-ch-r <name>`
- `root-channel-update|r-ch-u`
- `root-channel-name|r-ch-n`


### <a name="channelcommands">Channel commands</a>

- `env|e <options>`
- `build|b <options>`
- `query|q <package>`
- `upgrade|U <package?>`
- `upgrade-always|U-a <package?>`
- `install|i <package>`
- `Install|I <package>`
- `query-available|q-a`
- `compare-versions|c-v`
- `compare-versions-less-than|c-v-l-t`
- `compare-versions-equal|c-v-e`
- `compare-versions-greater-than|c-v-g-t`
- `describe-available|d-a`
- `index-available|i-a`
- `search-available|search|s-a|s <package>`
- `view-available|v-a`


### <a name="upstreamcommands">Upstream commands</a>

- `upstream-env|u-e <options>`
- `upstream-build|u-b <options>`
- `upstream-query|u-q <package>`
- `upstream-upgrade|u-U <package?>`
- `upstream-upgrade-always|u-U-a <package?>`
- `upstream-install|u-i <package>`
- `upstream-Install|u-I <package>`
- `upstream-query-available|u-q-a`
- `upstream-compare-versions|u-c-v`
- `upstream-compare-versions-less-than|u-c-v-l-t`
- `upstream-compare-versions-equal|u-c-v-e`
- `upstream-compare-versions-greater-than|u-c-v-g-t`
- `upstream-describe-available|u-d-a`
- `upstream-index-available|u-i-a`
- `upstream-search-available|u-s-a <package>`
- `upstream-view-available|u-v-a`


### <a name="querycommands">Querying packages</a>

- `view-installed|v-i`
- `search-installed|s-i <package>`
- `index-installed|i-i`
- `describe-installed|d-i`
- `query-installed|q-i <package>`


### <a name="commoncommands">Common commands</a>

- `uninstall|remove <package>`
- `build-index|index <package>`
- `upstream-update|u-u`
- `full-update|f-u|complete-update`
- `full-upgrade|f-U|complete-upgrade`
- `full-search|f-s <package>`


## <a name="miscellaneouscommands">Miscellaneous commands</a>

- `view-packages|v-p`
- `make`
- `nix-version`
- `nixpkgs-version`
- `nixos-version`
- `version`


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
$ nix remove firefox
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
$ nix version
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
