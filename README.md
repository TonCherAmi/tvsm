# <img height="50%" width="50%" src="https://raw.githubusercontent.com/TonCherAmi/tvsm/assets/logo.png"/>

## Introduction

`tvsm` is a simple command-line TV show manager. It is written in GNU Guile.

## Examples

General overview of the functionality:

<img height="65%" width="65%" src="https://raw.githubusercontent.com/TonCherAmi/tvsm/assets/demo.gif"/>

## Dependencies

GNU Guile 2.x
* **NOTE:** you must make sure that you have both `guile` and `guild` executables installed.\
            Some distributions (e.g. *Debian*) provide `guild` separately from `guile`, so simply\
            installing a package called `guile` may not be sufficient.\
            (in *Debian*'s case the proper package should be called `guile-2.x-dev`)

## Installation

```shell
# clone the reposityory:
$ git clone https://github.com/TonCherAmi/tvsm

# change working directory to tvsm:
$ cd tvsm

# compile the source using the Makefile:
$ make

# install tvsm using the Makefile :
# note: it might be necessary to run this as root:
$ make install
```

## Necessary configuration

After installation you will need to set up the configuration file:

```shell
# open the 'config' file with a text editor of your choice, e.g.,
$ vi config

# set 'media-player-command' to a shell command of your choice:
# note: '~a' is substituted for the media file path,
# it is _required_ that you specify it in the command
e.g: (media-player-command . "mpv ~a --quiet")

# copy the configuration file to the required location:
$ mkdir -p ~/.config/tvsm
$ cp config ~/.config/tvsm
```

## Usage

```
Usage: tvsm [--version] [--help] <command> [<options>]

available commands:
    add:      add a show.
    watch:    watch a show.
    ls:       list existing shows.
    rm:       remove shows.
    set:      modify a show.
    
See 'tvsm <command> --help' to learn more about a specific command.
```

## License

`tvsm` is licensed under the GNU Lesser General Public License v3.0. [See LICENSE for more information](https://github.com/TonCherAmi/tvsm/blob/master/LICENSE).
