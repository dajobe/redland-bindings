## redland
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/redland)](https://cran.r-project.org/package=redland)

- **Authors**: Matthew B. Jones, Peter Slaughter, Jeroen Ooms, Carl Boettiger, and Scott Chamberlain ([NCEAS](http://www.nceas.ucsb.edu))
- [doi:10.5063/F1VM496B](http://doi.org/10.5063/F1VM496B)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/ropensci/redland-bindings)
- [**Submit Bugs and feature requests**](https://github.com/ropensci/redland-bindings/issues)

The R package `redland` provides methods to create, query and write information 
stored in the Resource Description Framework (RDF). An introduction to RDF can be 
found at http://www.w3.org/TR/rdf-primer.  In brief, RDF provides a structured
graph consisting of Statements composed of Subject, Predicate, and Object Nodes.

This package is implemented as R scripts that provide an R interface (aka 
"wrapper") to the Redland RDF C libraries that are described at 
http://librdf.org/docs/api/index.html. Additional information about the software that
provides the connection or 'bindings' between R and the Redland RDF C libraries 
is available at https://github.com/ropensci/redland-bindings/tree/master/R. 

In this redland R package, S4 classes are
used to model RDF data structures.   A `redland::Statement` is composed of 
`redland::Node`s representing the subject, predicate, and object of each triple 
statement.  Statements can be composed into a graph by adding them to a 
`redland::Model`, which in turn can be serialized and deserialized to RDF's text 
formats using `redland::Serializer` and `redland::Parser`, respectively.

See the `redland_overview` vignette for a brief example of usage.

## Installation Notes 

The *redland* R package is now available from CRAN. Before the `redland` R package can be installed, the Redland C libraries may need to be installed, depending on the platform type you are installing to and whether or not you wish to install
from source.

### Installing on Mac OS X

On Mac OS X, the binary build from CRAN can be installed without installing the Redland C libraries by typing the
following commands in the R console:

```
install.packages("redland")
library(redland)
```

The *redland* R package should be available for use at this point.

Alternatively, to install *redland* from source, please continue reading this section.

The required Redland C libraries can be installed with either [Mac Ports](https://www.macports.org) package manager
or the [HomeBrew](http://brew.sh) package manager. The HomeBrew package manager can be significantly faster to install
but either one will work provided the directions shown below are followed.

You can check if you have MacPorts installed by entering the following command in a terminal window:

```
port version
```

#### Installing on Mac OS X with Macports
If you are already using the MacPorts package manager, you can install *redland* with the following commands, 
otherwise, it is recommended that you skip to the next section *Installing with HomeBrew*. To install
the *redland* R package with MacPorts, enter these commands at a terminal window:

```
sudo port install redland
```
Then enter these commands in the R console:
```
install.packages("redland", type="source")
library(redland)
```

Please note that the *install.packages* command specifies a "source" installation. Installing from
source is only necessary if Macports is being used, and is not a requirement if Homebrew is used.

The *redland* R package should be available for use at this point

#### Installing on Mac OS X with HomeBrew
On Mac OS X you can use the package management system [HomeBrew](http://brew.sh) to install the 
necessary libraries. The HomeBrew software can be installed with the following command entered at a terminal window:

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Once HomeBrew has been installed, you can then enter the following command to install the Redland C libraries:

```
brew install redland
```

Next, install the *redland* R package with these commands typed at the R console window:
```
install.packages("redland", type="source")
library(redland)
```
  
The *redland* R package should be available for use at this point.

## Installing on Ubuntu

For ubuntu, install the required Redland C libraries:

```
sudo apt-get update
sudo apt-get install librdf0 librdf0-dev
```

Then install the R packages from the R console:

```
install.packages("redland")
library(redland)
```

## Installing on Windows

For windows, the redland R package is distributed as a binary release, and it is not necessary to install any 
additional system libraries.

To install the R packages from the R console:

```
install.packages("redland")
library(redland)
```

## Example Usage

The `redland` library can be used for a wide variety of RDF parsing and creation tasks.  Some examples
are provided in the `redland_overview` vignette.  As a quick start, here is an example that
creates an RDF graph using an in-memory storage model, adds some triples, and then
serializes the graph to disk.

```r
library(redland)

# World is the redland mechanism for scoping models
world <- new("World")

# Storage provides a mechanism to store models; in-memory hashes are convenient for small models
storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")

# A model is a set of Statements, and is associated with a particular Storage instance
model <- new("Model", world=world, storage, options="")

# Add some Dublin Core properties to the model
dc <- "http://purl.org/dc/elements/1.1/"
stmt <- new("Statement", world=world, 
        subject="http://ropensci.org/", predicate=paste0(dc, "title"), object="ROpenSci")
addStatement(model, stmt)
stmt <- new("Statement", world=world, 
        subject="http://ropensci.org/", predicate=paste0(dc, "language"), object="en")
addStatement(model, stmt)
stmt <- new("Statement", world=world, 
        subject="http://ropensci.org/", predicate=paste0(dc, "license"), 
        object="https://creativecommons.org/licenses/by/2.0/")
addStatement(model, stmt)

# Serialize the model to a TTL file
serializer <- new("Serializer", world, name="turtle", mimeType="text/turtle")
status <- setNameSpace(serializer, world, namespace="http://purl.org/dc/elements/1.1/", prefix="dc")  
filePath <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".ttl")
status <- serializeToFile(serializer, world, model, filePath)
readLines(file(filePath))
```

## Acknowledgments
Work on this package was supported by NSF-ABI grant #1262458 to C. Gries, M. Jones, and S. Collins. 

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
