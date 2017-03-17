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

## Installation

### Dependencies

GNU Guile 2.x

### Getting the source

Run `$ git clone https://github.com/TonCherAmi/watch` in a location you'd like to store the 
code in.

### Compiling and Installing

To compile and then install run:

`$ make`
`# make install`

### Necessary configuration

Open the `config` file with your favorite text editor and set `media-player-command`
to your preferred media player e.g. "mpv" or "vlc". 
Then copy the `config` file to `$HOME/.config/watch/config`.

You can find default `config` file at `/usr/share/doc/watch/config`.

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
