# cl-tetris3d

Yet another 3D tetris.

## How to run:

```
> (ql:quickload :cl-tetris3d)
> (cl-tetris3d:run)
```

Enjoy!

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


