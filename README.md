# cellularAutomata

a small application for running a one-dimensional cellular automata from random inputs, using comonads

## usage

the program will default to the size of the window
`-w` and `-g` inputs can be given to determine the width and height, respectively

## requirements

  * `getOpt`
  * `ncurses` (for detecting term width/height)

## building / running

builds using nix

from a local folder:
```
nix build .
./result/bin/cellularAutomata
```

from the repo directly:
```
nix run github:techieAgnostic/cellularAutomata -- -w 40 -g 25
```

it may also be included as a flake input, as one normally would, and added to the package list using the included overlay

## example

`./cellularAutomata -w 40 -g 25`

```
██ ████  █ █████   █  ███   █ ███ ███   
██ █  ███  █   ██ █ ███ ██ █  █ █ █ ██  
 █  ███ ███ █ ███   █ █ ██  ██      ███ 
  ███ █ █ █   █ ██ █    ███████    ██ ██
███ █      █ █  ██  █  ██     ██  ███ ██
  █  █    █   ██████ █████   ██████ █ █ 
██ ██ █  █ █ ██    █ █   ██ ██    █     
██ ██  ██    ███  █   █ ███ ███  █ █   █
 █ ███████  ██ ███ █ █  █ █ █ ███   █ ██
█  █     █████ █ █    ██      █ ██ █  █ 
███ █   ██   █    █  ████    █  ██  ██  
█ █  █ ████ █ █  █ ███  ██  █ █████████ 
   ██  █  █    ██  █ ███████  █       ██
  █████ ██ █  █████  █     ███ █     ███
 ██   █ ██  ███   ███ █   ██ █  █   ██  
████ █  █████ ██ ██ █  █ ███  ██ █ ████ 
   █  ███   █ ██ ██  ██  █ █████   █  ██
█ █ ███ ██ █  ██ ████████  █   ██ █ ███ 
█   █ █ ██  ████ █      ███ █ ███   █ █ 
██ █    █████  █  █    ██ █   █ ██ █    
██  █  ██   ███ ██ █  ███  █ █  ██  █  █
████ █████ ██ █ ██  ███ ███   ██████ ███
   █ █   █ ██   █████ █ █ ██ ██    █ █  
  █   █ █  ███ ██   █     ██ ███  █   █ 
██ █ █   ███ █ ███ █ █   ███ █ ███ █ █  
 █    █ ██ █   █ █    █ ██ █   █ █    ██
```
