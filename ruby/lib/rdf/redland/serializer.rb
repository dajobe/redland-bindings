require 'rdf/redland'

module Redland
  # This module provides RDF graph to syntax serialization support via 
  # factory classes providing one or more particular target syntaxes.
  class Serializer

    # Constructor - create a new Serializer object
    # name -  the serializer factory name
    # mime_type -  the MIME type of the syntax
    # uri -  URI of syntax
    def initialize(name='rdfxml',mime_type="application/rdf+xml",uri=nil)
      uri = uri.uri if uri
      @serializer = Redland.librdf_new_serializer($world.world,name,mime_type,uri)
      raise RedlandError.new("Serializer construction failed") if !@serializer
      ObjectSpace.define_finalizer(self,Serializer.create_finalizer(@serializer))
    end

    # Create a NTriples serializer
    # uri -  URI of syntax
    def Serializer.ntriples(uri=nil)
      return Serializer.new('ntriples','text/plain',uri)
    end

    # You shouldn't use this. Used internally for cleanup.
    def Serializer.create_finalizer(serializer)
      proc {|id| "Finalizer on #{id}"
        log_final.info "closing serializer"
        Redland::librdf_free_serializer(serializer)
      }
    end

    # Serializes a model and stores it in a file
    # name -  the serializer factory name
    # mime_type -  the MIME type of the syntax
    # base_uri -  URI of syntax
    def to_file(name,model,base_uri=nil)
      if base_uri
        base_uri = base_uri.uri
      end
      return Redland.librdf_serializer_serialize_model_to_file(@serializer,name,base_uri,model.model)
    end

    #Get a parser feature.  The feature is named via RDF::Redland::URI
    #I<URI>
    def feature(uri)
      uri = Uri.new(uri) unless uri.class == Uri
      value = Redland::librdf_serializer_get_feature(@serializer,uri.uri)
      if value == "NULL" or value==nil
        return nil
      else
        return Node.new(:from_object=>'value')
      end
    end

    #Set a serializer feature.  The feature is named via RDF::Redland::URI
    #I<URI> and the value is a RDF::Redland::Node.
    def feature=(uri,value)
      uri = Uri.new(uri) unless uri.class == Uri
      if value.class == String then value = Node.new(:literal=>'value') end
      return (Redland::librdf_serializer_set_feature(@serializer,uri.uri,value.node) == 0)
    end
    
    #Set a serializer namepace to the desired prefix
    def set_namespace(prefix, uri)
      uri = Uri.new(uri) unless uri.class == Uri
      return (Redland::librdf_serializer_set_namespace(@serializer,uri.uri,prefix) == 0)
    end
      
    # Serialize the RDF graph in model to a string
    def model_to_string(base_uri, model)
      if base_uri
        base_uri = base_uri.uri
      end
      return Redland.librdf_serializer_serialize_model_to_string(@serializer, base_uri, model.model)
    end
    
  end

end #module Redland
