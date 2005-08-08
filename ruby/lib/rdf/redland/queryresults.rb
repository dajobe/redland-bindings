require 'rdf/redland'

module Redland

  # This class provides query language results support
  class QueryResults

    attr_reader :results

    # this is not a public constructor
    def initialize(object)
      @results = object
    end

    # Get the number of results so far
    def size
      return Redland.librdf_query_results_get_count(@results)
    end

    # Get the number of results so far
    def count
      return Redland.librdf_query_results_get_count(@results)
    end

    # Test if the results are finished
    def finished?
      return (Redland.librdf_query_results_finished(@results) != 0)
    end

    # Get binding name for the current result
    def binding_name(index)
      return Redland.librdf_query_results_get_binding_name(@results,index)
    end

    # Get binding name for the current result
    def binding_name(index)
      return Redland.librdf_query_results_get_binding_name(@results,index)
    end

    # Get the names all of the variable bindings as an array
    def binding_names
      count=Redland.librdf_query_results_get_bindings_count(@results)
      names=[]
      for i in 0..count-1
        names << binding_name(i)
      end
      return names
    end

    # Get one binding value for the current result
    def binding_value(index)
      node = Redland.librdf_query_results_get_binding_value(@results,index)
      return Node.new(node)
    end

    # Get an array of all the values of all of the variable bindings in the current query result.
    def binding_values
      count=Redland.librdf_query_results_get_bindings_count(@results)
      values=[]
      for i in 0..count-1
        values << binding_value(i)
      end
      return values
    end

    # Get the value of the variable binding name in the current query result.
    def binding_value_by_name(name)
      node=Redland.librdf_query_results_get_binding_value_by_name(@results,name)
      return Node.new(node)
    end

    # Get an array of binding values
    def bindings
      (0...self.size).each{|i| binding_value( i ) }
    end

    # Move to the next query result 
    def next
      Redland.librdf_query_results_next(@results)
    end

    # Get a new Stream object representing the query results as an RDF Graph.
    def as_stream
      my_stream=Redland.librdf_query_results_as_stream(@results)
      return Stream.new(my_stream, self)
    end

    # Serialize to a string syntax in format_uri using the optional
    # base_uri.  The default format when none is given is determined by
    # librdf_query_results_to_string
    def to_string(format_uri = nil, base_uri = nil)
      if self.is_graph?()
        tmpstorage=Storage.new()
        tmpmodel=Model.new(tmpstorage)
        tmpmodel.add_statements(self.as_stream())
        serializer=Serializer.new()
        return serializer.model_to_string(base_uri, tmpmodel)
      end

      if self.is_boolean?() and not self.is_bindings?()
        raise RedlandError.new("Unknown query result format cannot be written as a string")
      end

      return Redland.librdf_query_results_to_string(@results, format_uri, base_uri)
    end


    # Test if is variable bindings result
    def is_bindings?()
      return (Redland.librdf_query_results_is_bindings(@results) != 0)
    end

    # Test if is a boolean result
    def is_boolean?()
      return (Redland.librdf_query_results_is_boolean(@results) != 0)
    end

    # Test if is an RDF graph result
    def is_graph?()
      return (Redland.librdf_query_results_is_graph(@results) != 0)
    end

    # Get the boolean query result
    def get_boolean?()
      return (Redland.librdf_query_results_get_boolean(@results) != 0)
    end


  end

end #Redland

