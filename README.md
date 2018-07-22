baf 🐶
=====

This utility provides a single `baf` binary for managing your Nixpkgs and NixOS installation. It
makes it easier, at least for me, instead of memorizing many commands with different
interfaces. This is not exhaustive and only covers the commands listed [here](#commands).

This program was salvaged from [ebzzry/scripts](https://github.com/ebzzry/scripts), turning it into
a repository of its own, to make it easier to distribute. In this document, the `$` symbol
represents the user prompt, while the `*` symbol represents the lisp prompt.


Table of contents
-----------------

- [Installation](#installation)
- [Initialization](#initialization)
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
- [Notes](#notes)


<a name="installation">Installation</a>
---------------------------------------

Install the dependencies on Debian and NixOS systems, respectively:

```bash
sudo apt-get install -y git sbcl make curl cl-launch bzip2
```

```bash
nix-env -i git sbcl gnumake curl cl-launch bzip2 nix-prefetch-scripts
```

Then install baf:

```bash
mkdir -p ~/bin ~/common-lisp
git clone https://github.com/fare/asdf ~/common-lisp/asdf
git clone https://github.com/ebzzry/baf ~/common-lisp/baf
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (ql:quickload :cl-launch) (sb-ext:quit))'
make -C ~/common-lisp/baf install
```


Or, in one line:

```bash
mkdir -p ~/bin ~/common-lisp; git clone https://github.com/fare/asdf ~/common-lisp/asdf; git clone https://github.com/ebzzry/baf ~/common-lisp/baf; curl -O https://beta.quicklisp.org/quicklisp.lisp; sbcl --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file)  (ql:quickload :cl-launch) (sb-ext:quit))'; make -C ~/common-lisp/baf install
```


<a name="initialization">Initialization</a>
-------------------------------------------

On your first run, initialize the databases for the upstream nixpkgs checkout and index database:

```bash
baf init
```

Bear in mind that re-running `init` will purge the index and package databases.

Periodically, run the following command to update the aforementioned databases, plus the channels
for the user and root:

```bash
baf full-update
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

- `env <options>`
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
- `profile|p <profile>`


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
- `upstream-profile|u-p <profile>`


### <a name="querycommands">Querying packages</a>

- `view-installed|v-i`
- `search-installed|s-i <package>`
- `index-installed|i-i`
- `describe-installed|d-i`
- `query-installed|q-i <package>`


### <a name="commoncommands">Common commands</a>

- `uninstall|remove|erase|e <package>`
- `build-index|b-i|index <package>`
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


<a name="usage">Usage</a>
-------------------------

To install the latest Firefox from upstream:

    baf u-i firefox

To install the latest Firefox from channels:

    baf i firefox

To uninstall it:

    baf e firefox

To search for upstream packages with the name `firefox`:

    baf u-s firefox

To search for channel packages with the name `firefox`:

    baf s firefox

To search for packages from both upstream and channels, with the name `firefox`:

    baf f-s firefox

To display the version of Nix, Nixpkgs, and NixOS:

    baf version

To view the list of installed packages, using the index:

    baf v-i

To view the list of installed packages, using querying:

    baf q-i

To view the list of installed packages, including description:

    baf d-i

To view the Haskell packages from upstream:

    baf u-v-p haskellPackages

To search if Firefox is installed:

    baf s-i firefox

To install Firefox from upstream on a different profile:

    baf u-p firefox -iA firefox

then, to use this version of Firefox:

    PATH=$PATH/.baf/profiles/firefox/bin firefox

To find out which package has the binary `firefox`:

    baf h firefox

To get the store path of Firefox:

    baf o-p firefox

To display the `share/` subdirectory of Firefox, with some options to ls:

    baf ls --color -FAtrl firefox/share

To look for files in upstream containing the string `firefox`:

    baf find firefox

To grep the case insensitive string `firefox` in the upstream, displaying the name of the matching file:

    baf grep -iH firefox

To garbage collect:

    baf g-c

To aggressively garbage collect:

    baf g-c-d

To subscribe to the `nixos-unstable` channel for the current user:

    baf ch-a https://nixos.org/channels/nixos-unstable nixos
    baf ch-u

To subscribe to the `nixos-unstable` channel for root:

    baf r-ch-a https://nixos.org/channels/nixos-unstable nixos
    baf r-ch-u

To rebuild NixOS from `/etc/nixos/configuration.nix` then perform switch:

    baf r-s

To rebuild NixOS from `/etc/nixos/configuration.nix`, perform switch, and upgrade:

    baf r-s-u

To update the user channel, root channel, upstream nixpkgs checkout, and index database:

    baf f-u

To perform the above, then upgrade the whole NixOS system:

    baf f-U


<a name="notes">Notes</a>
-------------------------

In order for the `which` command to work on NixOS, put this in `/etc/nixos/configuration.nix`:

    programs.command-not-found.enable = true;

To update baf to the latest version:

    cd ~/common-lisp/baf; git pull --rebase origin master; make install
