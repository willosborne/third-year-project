Genzai: Declarative Presentations with FRP
---

This repository contains the source code for my third-year project, Genzai. This repository is based upon ElvishJerricco's Reflex Project Skeleton, which can be found at https://github.com/ElvishJerricco/reflex-project-skeleton. The package management system [Nix](https://nixos.org/) is required to compile and run this project. As such, the compiler has only been tested on Linux.

## Key components
 - A compiled version of the project presentation can be found in the `project-presentation` directory.
 - A compiled version of the unit tests can be found in the `built-tests` directory

Almost the entire source code for the library is found in the `frontend` directory. `frontend/src` contains the library itself, while `frontend/exe` contains the presentation source code.

## Notable source files
### The application - in `exe`
 - `Presentation.hs` contains the source code for the presentation
 - `Life.hs` contains an implementation of Conway's Game of Life
 
### The library - in `src`
 - `Content.hs` contains the Content interface, AKA the graphics EDSL 
 - Rendering of Content objects is done in `Render.hs`
 - The FRP framework is found in `Reactive/FRPSimple.hs`
 - The external JavaScript image preloader is found in `utils.js`
 - The slide and slideshow interfaces are defined in `Slide.hs`
 - The animation interface is defined in `Animation.hs`

## Install notes

Either clone this repo with `--recurse-submodules`, or run `git
submodule update --init --recursive` in this directory after cloning
to make sure `reflex-platform` is checked out.

First, run `./reflex-platform/try-reflex` at least once. We won't use
it at all in this project, but it does some extra work to setup your
system requirements automatically, namely installing Nix and
configuring the Reflex binary cache.

Once Nix is installed, everything else is mostly handled for you. 

To build the app, use the `./cabal-ghcjs` script:

```bash
$ ./cabal-ghcjs new-build all
```

After building the app, copy the correct html files in using the `copy-images.sh` script. This script can also be used to copy new images in from the `frontend/exe/images` directory when producing new presentations.
The `output` and `output-tests` directories contain symlinks to the produced results. In the event that these links are broken, the build results can be found in the following (long and painful - sorry!) directories:

 - `./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/frontend-0.1.0.0/c/frontend/build/frontend/frontend.jsexe`
 - `./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/common-0.1.0.0/c/test/build/test/test.jsexe`
 
Run the presentations by opening the `index.html` file in a modern web browser. Test results can be seen in the developer console, typically accessed with F12.
