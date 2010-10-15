require 'rdf/redland'

module Redland

  #include Redland

  class Resource < Node
    # The internal librdf_node
    attr_reader :node
    
    # The internal model this Resource is connected to
    attr_reader :model

    # Create a Redland::Resource.  Arg can be a String, or a Redland::Uri
    # 
    def initialize(arg=nil,model=nil,new_node=true)
      @context = nil
      case arg
      when nil
        @node = Redland.librdf_new_node_from_blank_identifier($world.world,nil)
      when String
        @node = Redland.librdf_new_node_from_uri_string($world.world,arg)
      when Uri
        @node = Redland.librdf_new_node_from_uri($world.world,arg.uri)
      when Node
        @node = Redland.librdf_new_node_from_node(arg.node)
      when SWIG::TYPE_p_librdf_node_s
        
        @node = new_node ? Redland.librdf_new_node_from_node(arg): arg
      end
      raise RedlandError.new("Node construction failed") if not @node
      ObjectSpace.define_finalizer(self,Node.create_finalizer(@node))

      model ||= Model.new
      @model = model
    end

    # Answer model.find(self,p,nil) in the associated model.  If there are several 
    # such statements, any one of them may be returned
    def get_property(p)
      @model.object(self,p)
    end


    # Answer model.find(self,pred,nil) in the associated model
    # If no predicate or block given, returns and array of
    # statements matching model.find(self,nil,nil)
    # If block_given? yields the predicate and the object
    #   resource.get_properties() do |p,o|
    #     puts "<td>#{p}</td><td>#{o}</td>"
    #   end
    #
    # If pred given, answers model.find(self,pred,nil)
    # if no block return an array of the object values
    #   nicknames = resource.get_properties(FOAF::NICK)
    #
    # if block_given yields the objects which satisfy the predicate
    #   puts "Nicknames are"
    #     resource.get_properties(FOAF_NICK){|o| puts o}
    def get_properties(pred=nil)
      statements = @model.find(self,pred,nil)
      if pred
        if block_given?
          statements.each{|st| yield st.object}
        else
          statements.map{|st| st.object}
        end
      else
        return statements if not block_given?
        statements.each{ |st| yield st.predicate,st.object}
      end
    end
    
    # Finds all objects of a given predicate. Takes an optional block,
    # yielding each statement if given.
    def object_of_predicate(pred = nil)
      # statements = @model.find(nil,pred,self)
      if pred
        if block_given?
          @model.subjects(pred,self){|sub| yield sub}
        else
          return @model.subjects(pred,self)
        end
      else
        statements = @model.find(nil,nil,self)
        return statements if not block_given?
        statements.each { |st| yield st.subject,st.predicate}
      end
    end

    # Adds a statement to the model with the subject as the 
    # resource and p as the predicate and o as the object
    # same as Model.add(this,p,o)
    def add_property(p,o,context=nil)
      @model.add(self,p,o,context)
      return self
    end

    # Removes the property from the model with the predicate
    # pred and optional context
    def delete_property(pred,context=nil)
      @model.delete(self,pred,self.get_property(pred),context)
      return self
    end

    # Removes all of the properties from the model with this
    # resource as the subject
    def delete_properties()
      self.get_properties().each{|st| @model.delete_statement(st)}
    end

    # Determine if this resource is of a given type
    def type?(type)
      self.model.find(self,TYPE,type).size != 0
    end

    # Change the value of the given property
    def update_property(prop,value,context=nil)
      self.delete_property(prop)
      self.add_property(prop,value,context)
      return self
   end
    
    # Sets the RDF type of this Resource where RDF type is
    # <http://www.w3.org/1999/02/22-rdf-syntax-ns#TYPE>
    def type=(type,context=@context)
      self.delete_property(TYPE)
      self.add_property(TYPE,type,context)
      return self
    end
    
    # returns the RDF type of this Resource where RDF type is
    # <http://www.w3.org/1999/02/22-rdf-syntax-ns#TYPE>
    def type
      self.get_property(TYPE)
    end   

    # Set the type of this Resource
    def set_type(type,context=@context)
      self.delete_property(TYPE)
      self.add_property(TYPE,type,context)
      return self
    end

    # Defines a context for this resource
    def set_context(context)
      @context = context
    end

  end
end

