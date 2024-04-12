# raylib-ada
Ada bindings for the Raylib library

# Using raylib-ada

This project is available in the [Alire package manager](https://alire.ada.dev).
To use it, just add `raylib` in your dependencies:
```console
$ alr with raylib
```
The binding and Raylib library will be automatically built.

If you are making an application/executable, you have to add this:
```ada
   package Linker is
      for Switches ("Ada") use ("-lraylib", "-lm");
   end Linker;
```
in your `.gpr` file to link with the Raylib and math libraries.

# Building the examples
Checkout the repository and, using Alire, run the following commands:
```console
$ cd examples
$ alr run examples
$ alr run gui_example
```
