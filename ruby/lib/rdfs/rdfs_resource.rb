require 'rdf'
require 'rdf/constants'

module Redland::RDFS
  include Redland

  RDFSNS = Namespace.new("http://www.w3.org/2000/01/rdf-schema#")
  RDFS_CLASS = RDFSNS["Class"]
  RDFS_RESOURCE = RDFSNS["Resource"]
  RDFS_SUBCLASSOF = RDFSNS["subClassOf"]
  RDFS_SUBPROPERTYOF = RDFSNS["subPropertyOf"]
  RDFS_ISDEFINEDBY = RDFSNS["isDefinedBy"]
  RDFS_LABEL = RDFSNS["label"]
  RDFS_COMMENT = RDFSNS["comment"]
  RDFS_RANGE = RDFSNS["range"]
  RDFS_DOMAIN = RDFSNS["domain"]
  RDFS_LITERAL = RDFSNS["Literal"]
  RDFS_CONTAINER = RDFSNS["Container"]
  RDFS_SEEALSO = RDFSNS["seeAlso"]

  class RDFSResource < Resource
    include Redland
    def comment()
      self.get_property(RDFS_COMMENT).to_s
    end

    def comment=(a_comment)
      if a_comment.class == String
        self.delete_property(RDFS_COMMENT)
        self.add_property(RDFS_COMMENT,a_comment)
      end
      return self
    end

    def add_label(a_label,lang=nil)
      label = Literal.new(a_label,lang)
      labels = self.get_properties(RDFS_LABEL) do |a_label| 
        if a_label.language == lang
          self.model.delete(self,RDFS_LABEL,a_label)
        end       
      end
      self.add_property(RDFS_LABEL,label)
      return self
    end

    def label(lang=nil)
    	  self.get_properties(RDFS_LABEL) do |label|
    	  	if label.language == lang
    	  	  return label
    	  	end
    	  end
      return nil
    end
    
   
   	
   	
    	  
    
    	

  end
end
