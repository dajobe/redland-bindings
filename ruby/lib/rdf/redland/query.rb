require 'rdf/redland'
require 'rdf/redland/stream'

module Redland

  # This class provides query language support for RDF models either
  # via an adaptor class or direct by persistant storage.

  class Query

    # Constructor - create a new Query object
    # query - the query string
    # language -  the name of the query language
    # uri -  the URI identifying the query language
    def initialize(query,language=nil,uri=nil)
      @query = query
      @language = language
      @uri = uri
      @query = Redland.librdf_new_query($world.world,language,uri,query)
      return nil if not @query
      ObjectSpace.define_finalizer(self,Query.create_finalizer(@query))
    end

    # Execute a query on a model
    def execute(model)
      return RDF::Redland::librdf_query_run_as_bindings(@query,model.model)
    end

    # You shouldn't use this. Used internally for cleanup.
    def Query.create_finalizer(query)
      proc{|id| "Finalizer on #{id}"
        $log_final.info "closing query"
        Redland::librdf_free_query(query)
      }
    end
    

  end

  class QueryResult

    def initialize()
    end

    # Get an array of bindings (?? confirm)
    def bindings
      results = []
      (0...self.size).each{|i| binding_value( i ) }
    end

    # Get binding name for the current result
    def binding_name(index)
      return Redland.librdf_query_results_get_binding_name(@query_results,index)
    end

    # Get one binding value for the current result
    def binding_value(index)
      node = Redland.librdf_query_results_get_binding_value(@query_results,index)
      return Redland::Node.new_from_object(node)
    end

    # Get number of bindings so far
    def size()
      return Redland.librdf_query_results_get_bindings_count(@query_results)
    end

  end

end #Redland
