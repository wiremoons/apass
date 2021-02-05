[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/hyperium/hyper/master/LICENSE) 
![adbt](https://github.com/wiremoons/apass/workflows/apass/badge.svg?branch=main) [![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/alire-badge.json)](https://alire.ada.dev/)
<a href="https://project-types.github.io/#toy">
  <img src="https://img.shields.io/badge/project%20type-toy-blue" alt="Toy Badge"/>
</a>

# What is 'apass'?

Description : A CLI password generator written in Ada.   

A small command line application called '`apass`' which can be used to generate 
a new password.

The program is small, fast, and is free software. It is used on a daily basis by
the author, running it on both Linux and Windows operating systems. It should
also compile and run on BSD Unix too, and Mac OS X as well, although this has
not been tested.


## Status

**INCOMPLETE** : WORK IN PROGRESS


## Usage Examples

Running `adbt` without any parameters, but with a database available will
output the following information:

```
coming soon...
```

Running `adbt -h` displays the help screen which will output the following
information:

```
coming soon...
```

## Building the Application

An Ada language compiler will be needed to build the application.


### Dependencies

Other than a working Ada language compiler and the Ada libraries that are normally 
provided by GNAT, then no other dependencies are needed.

### Install an Ada Compiler and Supporting Libraries

Install an Ada compiler and supporting libraries. More information on installing 
Ada can be found here: [Get Ada](http://www.getadanow.com/).

### Building 'adbt'

To build `apass` from source, the following steps can be used:

1. Once Ada is installed on your system, you can clone this GitHub repo with 
the `git` command: `git clone https://github.com/wiremoons/apass.git`
2. Then in the cloned code directory for `apass` use `gprbuild` to build a 
release version of `apass` with the command: `gprbuild -XBUILD=release`. 
Other `gprbuild` build commands include a debug build: `gprbuild -XBUILD=debug`. 
Alternatively, the new (in beta) [Alire](https://alire.ada.dev/) package manager 
should also support the install and build as well.
3. The compiled binary of `apass` can now be found in the `./bin` sub 
directory. Just copy it somewhere in your path, and it should work when run.

**NOTE:** The build is statically compiled - so the program should run when moved 
to a similar CPU based Linux system, without the Ada standard libraries being 
installed as additional dependencies.


## Todo ideas and Future Development Plans

Below are some ideas that I am considering adding to the program, in no
particular priority order.

TO BE ADDED.


## Known Issues

Below are issues known to affect `apass` currently:

- none captured yet - in development.


## Licenses

The following licenses apply to the `apass` source code, and resulting built
application, as described below.

#### License for 'apass'

This program `apass` is licensed under the **MIT License** see
http://opensource.org/licenses/mit for more details.


