# The BioCro Documentation Web Site

This site contains documentation for the version of the BioCro code contained in the repository residing at https://github.com/ebimodeling/biocro-dev.  While users can generate this documentation on their own from a source tree of the BioCro package, this site should largely eliminate the need to do so.  Eventually, it will always be automatically updated every time a new version of BioCro is released.

There are three components to this documentation:
1. The [BioCro _pkgdown_ documentation](docs/index.html).  This consists of an HTML version of the BioCro manual, which documents user-accessible data and functions; and the vignettes—longer-form documentation about BioCro.  This is the documentation of most interest to users of BioCro and is the documentation available locally to users when the package is installed.
2. The [BioCro _bookdown_ documentation](bookdown_book/index.html).  This is documentation primarly aimed at developers, module writers, and package maintainers.
3. The _Doxygen_ documentation.  This documents the C++ source code, which is the workhorse of BioCro.  It is certainly of interest to developers, but users also will likely be interested in at least the module-related Doxygen documentation, which gives information about the various models that may be used.

    There are several versions of the Doxygen documentation:
    * [Complete HTML documentation including private class members](doxygen_docs_complete/)
    * [Complete HTML documentation of the BioCro modules](doxygen_docs_modules/)
    * [HTML documentation of the public interface to the BioCro modules](doxygen_docs_modules_public_members_only/)
    * [Complete HTML documentation of the simulation framework, excluding the modules](doxygen_docs_framework/)
