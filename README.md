## Introduction

`tvsm` is a lightweight TV show manager. It is written in GNU Guile.

## Dependencies

GNU Guile 2.x

## Installation

1. Clone the repository.
    * `git clone https://github.com/TonCherAmi/tvsm` 
  
2. Change working directory to `tvsm`.
    * `cd tvsm`
  
3. Compile the source using the Makefile.
    * `make`
  
4. Install tvsm using the Makefile.
    * `make install`
    * **NOTE:** it might be necessary to run this as root.

## Necessary configuration

After installation you will need to set up the configuration file.

1. Open the `config` file with a text editor of your choice.
    * e.g. `vi config`

2. Set `media-player-command` to a shell command that invokes a media player
   of your choice.
    * e.g. `(media-player-command . "mpv ~a --quiet")`
    * **NOTE:** `~a` is substitued for the media file path,
                it is required that you specify it in the command.

3. Copy the configuration file to the required location.
    * `mkdir -p ~/.config/tvsm`
    * `cp config ~/.config/tvsm`

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

## Examples 

/TODO/

## License

`tvsm` is licensed under the GNU Lesser General Public License v3.0. [See LICENSE for more information](https://github.com/TonCherAmi/tvsm/blob/master/LICENSE).
