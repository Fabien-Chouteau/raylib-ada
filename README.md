# raylib-ada
Ada bindings for the Raylib library

# Building the examples
 - First build raylib from source or download a binary release
 - Locate the folder containing libraylib.a

```console
$ cd examples
$ alr build -- -largs -L<PATH_TO_RAYLIB_DIR>
$ alr run
```
