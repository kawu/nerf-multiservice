Nerf service for the multiservice framework
===========================================

The package provides a nerf-multiserive executable tool which
can be used together with the Polish multiservice framework.

Building nerf-multiservice
==========================

First, you need [Apache Thrift](http://thrift.apache.org/) to generate
Haskell modules which are needed to compile the nerf-multiservice tool.
The current nerf-multiservice version has been tested with the 0.9.0
version of Thrift.

After you install the Thrift framework you can generate the additional
modules from within the `thrift` directory: 

    thrift --gen hs types.thrift
    thrift --gen hs subservices.thrift
    thrift --gen hs multiservice.thrift

Due to a bug in the code generation process you need to replace the line

    import qualified Types_Types

with

    import Types_Types

in the `thrift/gen-hs/AnnotatingService.hs` file.  Then just run

    cabal install

from the `nerf-multiservice` top level directory.

Running the service
===================

To use the `nerf-multiservice` tool you need a pretrained Nerf model.
Visit the Nerf [home page](http://zil.ipipan.waw.pl/Nerf) to acquire
the model trained on the National Corpus of Polish.  Run

    nerf-multiservice --help

to learn about the program arguments and options.
