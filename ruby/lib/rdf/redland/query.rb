require 'rdf/redland'
require 'rdf/redland/stream'

module Redland

  # This class provides query language support for RDF models either
  # via an adaptor class or direct by persistant storage.

  class Query

    attr_reader :query

    # Constructor - create a new Query object
    # query - the query string
    # language -  the name of the query language
    # uri -  the URI identifying the query language
    def initialize(query,language=nil,uri=nil,base_uri=nil)
      @language = language
      @uri = uri
      @query = Redland.librdf_new_query($world.world,language,uri,query,base_uri)
      return nil if not @query
      ObjectSpace.define_finalizer(self,Query.create_finalizer(@query))
    end

    # Execute a query on a model
    def execute(model)
      results=Redland.librdf_query_execute(@query,model.model)
      if not results
        return nil
      else
        return QueryResults.new(results)
      end
    end

    # You shouldn't use this. Used internally for cleanup.
    def Query.create_finalizer(query)
      proc{|id| "Finalizer on #{id}"
        #$log_final.info "closing query"
        Redland::librdf_free_query(query)
      }
    end
    
  end

end #Redland
