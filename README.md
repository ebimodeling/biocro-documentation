# The BioCro Documentation Web Site

This site contains documentation for the version of the BioCro code
contained in the repository residing at
https://github.com/ebimodeling/biocro.  The landing page for the
GitHub Pages site displaying this documentation is
https://ebimodeling.github.io/biocro-documentation/, It is recommended
that you view this page from there (if you aren't there already).

There are three components to this documentation:

1. The [BioCro User Manual](docs/index.html).  This documents
user-accessible data and functions.  It also contains the
vignettes—longer-form documentation about BioCro.  This is the
documentation of most interest to users of BioCro and is the
documentation available locally to users when the package is
installed.

2. The [BioCro Developer Manual](bookdown_book/index.html).  This is
documentation primarly aimed at developers, module writers, and
package maintainers.

3. The _Doxygen_ documentation.  This documents the C++ source code,
which is the workhorse of BioCro.  It is certainly of interest to
developers, but users also will likely be interested in at least the
module-related Doxygen documentation, which gives information about
the various models that may be used.

    There are several versions of the Doxygen documentation:
    * [Complete documentation including private class members](doxygen_docs_complete/)
    * [Complete documentation of the BioCro modules](doxygen_docs_modules/)
    * [Documentation of the public interface to the BioCro modules](doxygen_docs_modules_public_members_only/)
    * [Complete documentation of the simulation framework, excluding the modules](doxygen_docs_framework/)
