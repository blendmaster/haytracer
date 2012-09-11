haytracer
=========

A ray-tracing renderer in haskell, written by Steven Ruppert for Project 1 of
the Fall 2012 **CSCI 441: Computer Graphics** course at the Colorado School of
Mines.

`haytracer` produces PPM images from a simple input format of spheres,
triangles, a single light source, a screen, and a viewpoint. Radical, dude!

## Compilation

With a copy of the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
installed and accessible as `ghc`, and GNU Make, run

    make

in the root directory to compile the project. There shouldn't be any external
dependencies. Run `make clean` to remove compilation files if you want.

## Usage

    raytrace <infile, default "input.txt"> <outfile, default "output.ppm">

### Example

    $ raytrace testdata/test4.txt
    Reading testdata/test4.txt...
    Rendering to output.ppm...
    done!

