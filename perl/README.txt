Redland 0.9.11 Perl API

The API changed in this release.  The following script will
automatically update perl scripts to the new module names:

  update-perl-api.pl SCRIPT-NAMES...

Details of changes:

 * The names of the modules moved from RDF to RDF::Redland in order
   to distinguish Redland from any other perl RDF modules and allow
   it to be installed in parallel with them.

 * The use of the RDF::Redland::Iterator class is discouraged -
   the methods that returned these objects now return perl lists

 * The following methods in RDF::Redland::Model were deprecated in
   favour of the methods that return perl lists.  The old methods
   that return an RDF::Redland::Iterator object are still available
   but have a new names as follows:

    RDF::Redland::Model

    OLD NAME          NEW NAME
    get_sources       sources_iterator
    get_arcs          arcs_iterator
    get_targets       targets_iterator

   In the next release the old names will be removed

 * The following method in RDF::Redland::Iterator was deprecated
   in order to use the same method names and logical sense as
   RDF::Redland::Stream class.

    RDF::Redland::Iterator

    OLD NAME          NEW NAME
    have_elements     !end

   i.e instead of $iterator->have_elements, use !$iterator->end

   In the next release the old name will be removed
