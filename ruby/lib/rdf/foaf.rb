require 'rubygems'
require_gem 'rdf'
module FOAF

  NS = Redland::Namespace.new('http://xmlns.com/foaf/0.1/')
  FIRST = NS['first']
  SURNAME = NS['surname']
  PHONE = NS['phone']
  TITLE = NS['title']
#  NAME = NS['name']
  MBOX = NS['mbox']
  MBOXSHAL1 = NS['mbox_shal1sum']
  NAME = NS['name']
  NICK = NS['nick']
  IMAGE = NS['image']
  DEPICTION = NS['depiction']
  DEPICTS = NS['depicts']
  

  def foaf_first(node)
    @model.object(node,FOAF['firstName']).to_s
  end

  def foaf_name(node)
    @model.object(node,NAME)
  end

  def foaf_title(node)
    @model.object(node,FOAF['title']).to_s
  end

  def foaf_surname(node)
    @model.object(node,FOAF['surname']).to_s
  end

  def foaf_phone(node)
    @model.object(node,FOAF['phone']).to_s
  end

  def foaf_namespace
    return FOAF
  end

  def foaf_mbox(node)
    @model.object(node,MBOX).to_s
  end

  def foaf_name(node)
    @model.object(node,NAME).to_s
  end

 

end
