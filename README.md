# Utility md5

#### Requirements

 - The Haskell Tool Stack tool should be installed (_read how to [install][1]_).

#### Build instructions

To build run following:

```sh
$ stack build
```

_Read more about [stack][2] usage._

#### Installation

Run next command to install `md5`:

```sh
$ stack install
```

_`md5` would be placed in `~/.local/bin` directory. Ensure that it is in your `$PATH`._

#### Usage

```sh
$ md5 Setup.hs
bad3a19586e908114369c22943337063  Setup.hs
$ cat Setup.hs | md5
bad3a19586e908114369c22943337063  -
```

#### What's in plans

 - Document it

[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/ "Install/Upgrade - The Haskell Tool Stack"
[2]: https://docs.haskellstack.org/en/stable/README/ "The Haskell Tool Stack"
