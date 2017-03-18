## Introduction

`watch` is a lightweight TV show manager. It is written in GNU Guile.

## Why would you want to use it?

First of all, if when you hear the words 'TV show manager' a question like: 
"Doesn't Netflix already does that for me?" arises in your head then this is most likely not 
for you.

Now on the other hand if you are like me, if you live in the terminal and store your
TV shows/anime series/etc. on your hard drive, there is a chance that you are quite tired of
typing stuff like: 
```
mpv <super-long-path-to-a-directory-containing-a-tv-show>/<name-of-an-episode>
```
every time you want to watch a show or, perhaps, you may have trouble remembering what episode
you've watched last after a particularly busy workweek, `watch` automates that for you.

## Dependencies

GNU Guile 2.x

## Installation

1. Clone the repository.
  * `git clone https://github.com/TonCherAmi/watch` 
2. Change working directory to `watch`.
  * `cd watch`
3. Compile the source using the Makefile.

  * `make`

4. Install watch using the Makefile.

  * `make install`
  * **NOTE:** it might be necessary to run this as root.

## Necessary configuration

After installation you will need to set up the configuration file.

1. Open the `config` file with a text editor of your choice.

  * e.g. `vi config`

2. Set `media-player-command` to a media player of your choice.

  * e.g. `(media-player-command . "mpv")`

3. Copy the configuration file to the required location.

  * `mkdir -p ~/.config/watch`
  * `cp config ~/.config/watch`

## Usage

```
Usage: watch [--version] [--help] <command> [<options>]

available commands:
    add:        add a show.
    play:       play a show.
    list:       list existing shows.
    remove:     remove shows.
    set:        modify a show.
    
See 'watch <command> --help' to learn more about a specific command.
```

## Examples 

/TODO/

## License

/TODO/
