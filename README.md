# cl-tetris3d

Yet another 3D tetris.

## How to run:

``` lisp
> (ql:quickload :cl-tetris3d)
> (cl-tetris3d:run)
```

Enjoy!

## Controls:

  * A - move figure left
  * D - move figure right
  * S - move figure down
  * space - immediately land figure
  * Q - rotate figure counterclockwise
  * E - rotate figure clockwise

  * P - pause/unpause
  * Esc - quit
  * left, right, up, down, page up, page down -- rotate and zoom camera


## Make executable (SBCL Only!):

There are two ways to do it:

1. Run interpeter from shell (it doesn't work within Slime),
and run the following commands from REPL:
```
> (ql:quickload :cl-tetris3d)
> (cl-tetris3d:make-executable)
```

2.  Run ```./sbcl-make-executable.run``` script

After that you should get the ```./cl-tetris3d``` binary.


