R bindings for Redland
======================

Bind the [Redland librdf library](http://librdf.org) into the R environment, and 
expose the librdf API for R. This is accomplished through the use of SWIG to generate
wrapper code in R that links to the underlying C code from librdf and provides
R functions to call that code.  This creates surrogate functions in R for all
of the librdf* functions.  In addition, there is a higher-level API written 
as a series of S4 classes in R that provide a simpler mechanism to access common
functions from librdf.  This API mirrors the python class structure for 
librdf-bindings.

The main R package is in the `redland` subdirectory, and is all that is required
to build and install the R package.  This current directory contains the supporting
build infrastructure to build the SWIG bindings and prepare the `redland` package.
Most users will simply follow the build instruction in the `redland/README.md` file.

## Build notes

To build the package and generate new interfaces to librdf using SWIG, follow the instructions
below. Again, this is typically not needed by most R users, and is mainly available for
regenerating the R interface when new versions of librdf are released.

1. (Optional) Install docker and load the Dockerfile into a container and run it
    ```bash
    docker build -t redland-bindings .
    docker run --rm -it redland-bindings /bin/bash
    ```

2. Run `autoconf` and `make` to generate the shared libraries and compile the R package code, and startup R
    ```bash
    cd /redland-bindings
    ./autogen.sh --with-R
	make
    cd R/redland
    ```

3. Load devtools in R and load the package
    ```r
    > install.packages("devtools")
    > library(devtools)
    > load_all()
    ```

4. Run a redland command
    ```r
    > library(redland)
    > world <- librdf_new_world()
    > world
    An object of class "_p_librdf_world_s"
    Slot "ref":
    <pointer: 0x38bd290>
    ```

5. Run the existing test suite
    ```r
    > install.packages("testthat")
    > library(testthat)
    > test()
    ```
