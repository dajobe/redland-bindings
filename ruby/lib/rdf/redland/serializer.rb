require 'redland'

module Redland
  class Serializer
    
    def initialize(name="",mime_type="application/rdf+xml",uri=nil)
      if uri
        uri = uri.uri
      end
      @serializer = Redland.librdf_new_serializer($world.world,name,mime_type,uri)
      ObjectSpace.define_finalizer(self,Serializer.create_finalizer(@serializer))
    end

    def Serializer.ntriples(uri=nil)
      return Serializer.new('ntriples','text/plain',uri)
    end


    def Serializer.create_finalizer(serializer)
      proc {|id| "Finalizer on #{id}"
        log_final.info "closing serializer"
        Redland::librdf_free_serializer(serializer)
      }
    end

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
    def set_namespace(uri,prefix)
      uri = Uri.new(uri) unless uri.class == Uri
      return (Redland::librdf_serializer_set_namespace(@serializer,uri.uri,prefix) == 0)
    end
      
    
  end

end #module Redland
