require 'rdf'
require 'rdf/node'
include Redland
module Redland

  class Resource < Node

    attr_accessor :node,:model

    def initialize(arg=nil,model=nil,new_node=true)
      case arg
      when nil
        @node = Redland.librdf_new_node_from_blank_identifier($world.world,nil)
      when String
        @node = Redland.librdf_new_node_from_uri_string($world.world,arg)
      when Uri
        @node = Redland.librdf_new_node_from_uri($world.world,node.uri)
      when SWIG::TYPE_p_librdf_node
        
        @node = new_node ? Redland.librdf_new_node_from_node(arg): arg
      end

      model ||= Model.new
      @model = model
    end
    
    def model
      return @model
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
    # resource.get_properties() do |p,o|
    #   puts "<td>#{p}</td><td>#{o}</td>"
    # end
    # If pred given, answers model.find(self,pred,nil)
    # if no block return an array of the object values
    # if block_given yields the object
    # puts "Nicknames are"
    # resource.get_properties(FOAF_NICK){|o| puts o}
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
    def add_property(p,o)
      @model.add(self,p,o)
      return self
    end

    # Removes the property from the model with the predicate
    # pred.
    def delete_property(pred,context=nil)
      @model.delete(self,pred,self.get_property(pred),context)
      return self
    end

    def delete_properties()
      self.get_properties().each{|st| @model.delete_statement(st)}
    end
    
    def type?(type)
     self.model.find(self,TYPE,type) != nil
   end
   
   def type=(type)
   	self.delete_property(TYPE)
   	self.add_property(TYPE,type)
   	return self
   end
   
   def type
   	self.get_property(TYPE)
   end    

  end
end

