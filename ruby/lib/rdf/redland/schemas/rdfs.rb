require 'rdf/redland'
require 'rdf/redland/constants'

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

  # A subclass of Resource that adds convenience methods for 
  # adding RDFS information
  # class RDFSResource < Resource
  #  include Redland

  # get the object with the following predicate:
  # http://www.w3.org/2000/01/rdf-schema#comment'
  def comment()
    self.get_property(RDFS_COMMENT).to_s
  end

  # same as the following
  #  model.add(this,Resource.new('http://www.w3.org/2000/01/rdf-schema#comment','comment')
  def comment=(a_comment)
    if a_comment.class == String
      self.delete_property(RDFS_COMMENT)
      self.add_property(RDFS_COMMENT,a_comment)
    end
    return self
  end

  # add a 'http://www.w3.org/2000/01/rdf-schema#label' to this resource.
  # a label is a literal that can have a language.  If a label for
  # a given language is already defined, it replaces the label.
  # If a label is not defined for a language it adds the label
  #  res.add_label('my label') # label created
  #  res.add_label('change label') #label changed to 'change label'
  #  res.add_label('change again','en') # label added with language English
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

  # return the label for this resource
  # if the lang is defined, return the label for this language
  # if no label found return nil
  def label(lang=nil)
    self.get_properties(RDFS_LABEL) do |label|
      if label.language == lang
        return label
      end
    end
    return nil
  end
  
  
  
  
  
  
  

  # end #end class
end
