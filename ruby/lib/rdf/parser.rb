require 'rdf/uri'
require 'rdf'
require 'rdf/stream'


#module RDF
module Redland
  class Parser
    attr_accessor :context
    
    def initialize(name='raptor',mime_type="application/rdf+xml",uri=nil,context=nil)
      @context = nil
      @idents = []
      uri=uri.uri if uri
      @parser = Redland.librdf_new_parser($world.world,name,mime_type,uri)
      raise RedlandError.new("Parser construction failed") if !@parser
      ObjectSpace.define_finalizer(self,Parser.create_finalizer(@parser))
    end

    def Parser.create_finalizer(parser)
      proc {|id| "Finalizer on #{id}"
        puts "closing parser"
        Redland::librdf_free_parser(parser)
      }
    end

    def Parser.ntriples(uri=nil)
      return Parser.new('ntriples','text/plain',uri)
    end

    def Parser.raptor(uri=nil)
      initialize('raptor','application/rdf+xml',uri)
    end
    
    def parse_as_stream(uri,base_uri=nil)
      if uri.class == String then uri = Uri.new(uri) end
      if base_uri.class == String then base_uri = Uri.new(base_uri) end
      unless base_uri
        my_stream = Redland::librdf_parser_parse_as_stream @parser, uri.uri, nil
      else
        my_stream = Redland::librdf_parser_parse_as_stream @parser, uri.uri ,base_uri.uri
      end
      return nil if !my_stream
      stream = Stream.new(my_stream,self)
      if !block_given?
        return stream
      else
        while not stream.end?
          yield stream.current
          stream.next
        end
      end
    end

    # Parse the syntax at the RDF::Redland::Uri I<SOURCE_URI> with optional base
    # RDF::Redland::Uri I<BASE_URI> into RDF::Redland::Model I<MODEL>.  If the base URI is
    # given then the content is parsed as if it was at the base URI rather
    # than the source URI.  If the optional context is given, the statement is added to the context
    def parse_into_model(model,uri,base_uri=nil,context=@context)
      if not context
        if uri.class == String
          uri = Uri.new(uri)
        end
        if base_uri
          if base_uri.class == String
            base_uri = Uri.new(base_uri)
          end
        else
          base_uri=uri 
        end
        begin
          r = Redland.librdf_parser_parse_into_model(@parser,uri.uri,base_uri.uri,model.model)
        rescue RedlandError => err
          print "caught error"+ err
        end
      else #context
        self.parse_as_stream(uri,base_uri){|s| model.add_statement(s,context)}
      end

    end

    # Parse the syntax in I<STRING> with required base
    # RDF::Redland::URI I<BASE_URI> into RDF::Redfland::Model I<MODEL>.
    #
    # Returns an RDF::Redland::Stream or yields RDF::Redland::Statement objects
    # if block given.  Returns nil on failure.
    def parse_string_as_stream(string,base_uri)
      if base_uri.class == String then base_uri = Uri.new(base_uri) end
      if not base_uri then raise RedlandError("A base URI is required when parsing a string") end
      my_stream = Redland::librdf_parser_parse_string_as_stream(@parser,string,base_uri.uri)
      return nil if !my_stream
      stream = Stream.new(my_stream,self)
      return stream if !block_given?
      while not stream.end?
        yield stream.current
        stream.next
      end
    end

    #Parse the syntax in I<STRING> with required base
    #RDF::Redland::URI I<BASE_URI> into RDF::Redland::Model I<MODEL>.
    def parse_string_into_model(model,string,base_uri=nil,context=@context)
      if base_uri.class == String then base_uri = Uri.new(base_uri)end
      if not base_uri then raise RedlandError.new("A base URI is required when parsing a string") end
      if not context
        return (Redland::librdf_parser_parse_string_into_model(@parser,string,base_uri.uri,model.model)== 0)
      else
        self.parse_string_as_stream(@parser,string,base_uri.uri){|s| model.add_statement(s,context)}
      end        
    end

    #Get a parser feature.  The feature is named via RDF::Redland::URI
    #I<URI>.
    def feature(uri)
      if uri.class == String then uri = Uri.new(uri) end
      value = Redland::librdf_parser_get_feature(@parser,uri.uri)
      if value == "NULL" or value == nil
        return nil
      else
        return Node.new(:from_object=>'value')
      end
    end

    #Set a parser feature.  The feature is named via RDF::Redland::URI
    #I<URI> and the value is a RDF::Redland::Node.
    def feature=(uri,value)
      if uri.class == String then uri = Uri.new(uri) end
      if value.class == String then value = Node.new(:literal=>'value') end
      return (Redland::librdf_parser_set_feature(@parser,uri.uri,value.node) == 0)
    end

    def add_ident(node)
      @idents << node
    end

    def smush_string(string,model,idents=@idents)
      to_change = {}
      temp_model = Model.new()
      self.parse_string_into_model(temp_model,string,'http://xml.com')
      idents.each do |ident|
        temp_model.find(nil,ident,nil){|s,p,o|
          old_id = s
          new_id = model.subject(p,o)
          if new_id
            to_change[old_id.to_s]= new_id if !to_change.key?(old_id.to_s)
          end
        }
      end
      #puts to_change
      temp_model.triples do |s,p,o|
        s = to_change[s.to_s] if to_change.key?(s.to_s)
        model.add(s,p,o)
      end
      
    end

    def smush_file(model,uri,base_uri=nil,context=nil,idents=@idents)
      to_change = {}
      temp_model = Model.new()
      self.parse_into_model(temp_model,uri,base_uri,context)
      idents.each do |ident|
        temp_model.find(nil,ident,nil){|s,p,o|
          old_id = s
          new_id = model.subject(p,o)
          if new_id
            to_change[old_id.to_s]= new_id if !to_change.key?(old_id.to_s)
          end
        }
      end
      #puts to_change
      temp_model.triples do |s,p,o|
        s = to_change[s.to_s] if to_change.key?(s.to_s)
        model.add(s,p,o,context)
      end
      
    end

  end

  class ContextParser
    require 'uri'
    def initialize(file)
      case file
      when String
        file = file.expand_path(file)
        raise RedlandError("File doesn't exist") unless File.file?(file)
        @uri == Uri.new(file)
      when File
      end
      @parser = Redland.librdf_new_parser($world.world,name,mime_type,@uri.uri,@uri.uri)
      raise RedlandError.new("Parser construction failed") if !@parser
      ObjectSpace.define_finalizer(self,Parser.create_finalizer(@parser))
      
    end

    def parse_into_model(model,string,base_uri)
      super(model,@uri,nil,@uri)
    end

    
  end

  
end #module Redland

#end
