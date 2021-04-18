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

## discussion

this program was an exercise in using comonads to simulate cellular automata.

### cellular automata

a cellular automaton is defined by a calculation on a local space, which is applied to a global space of cells.

in a one dimensional cellular automata, each cell has two immediate neighbors, to the left and right of it.

additionally, each cell may be in one of two state - Alive, or Dead.

this program has a hard-coded (for now) xor-based rule that works as follows:

regardless of the state of the current cell, it will become Alive if one of its neighbors is alive, but not both, otherwise it will Die.

this local behaviour, when viewed on a global scale, can provide emergent behaviour, in this case a nice triangular pattern (when viewed over time).

### comonads

a comonad is the dual to a monad.

it is defined by two operations, `extract` and `duplicate`.

`extract` takes a comonad (which wraps some base type) and returns a single item of that type, it can be seen in the semantics of a cellular automata space as `extract`ing the focussed cell from the global space.

`duplicate` takes a comonad space, and wraps it in an extra layer of comonad, essentially creating a comonad of comonads. in semantics of a cellular automata, we can `duplicate` a space into that same space, but focussed on every possible cell in the space.

finally, a comonad is a functor, meaning it can be mapped over. we can use this to our advantage in the semantics of a cellular automata, by creating a function from a comonad space, to a cell state.

this function will be our transition function, and determines the new state of the focussed cell, using the non-focussed parts as its inputs.

by `duplicate`ing the comonad space, and mapping the transition function over this new universe of spaces, we get back a single universe, which represents the next step in time.

### laziness

due to haskell's laziness, the comonad space can (and should) be infinite in both directions, and as long as we remember to bound the space we want to print, our program will work properly.

as such, the functions to change our focus within the space assumes an infinite list, and i have defaulted to "clamping" our focus at the edges if a finite list is given on either direction. im not sure this is semantically the correct choice, but in regular usage (with infinite lists) it should not come up.

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
