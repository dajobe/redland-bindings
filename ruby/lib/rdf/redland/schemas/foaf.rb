require 'rdf/redland'
require 'rdf/redland/resource'
require 'rdf/redland/constants'


module Redland::FOAF
  include Redland

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
  PERSON = NS['Person']
  KNOWS = NS['knows']

  def first()
    self.get_property(NS['firstName']).to_s
  end

  def first=(first,context=@context)
    self.add_property(NS['firstName'],first,context)
  end

  def name()
    self.get_property(NS['name']).to_s
  end

  def name=(name,context=@context)
    self.add_property(NS['name'],name,context)
  end

  def title()
    self.get_property(NS['title']).to_s
  end

  def title=(title,context=@context)
    self.add_property(NS['title'],title,context)
  end

  def surname()
    self.get_property(NS['surname']).to_s
  end

  def surname=(surname,context=@context)
    self.add_property(NS['surname'],surname,context)
  end

  def phone()
    self.get_property(NS['phone']).to_s
  end

  def phone=(phone,context=@context)
    self.add_property(NS['phone'],phone,context)
  end
  
  def mbox()
    self.get_property(MBOX).to_s
  end 

  def mbox=(mbox,context=@context)
    self.add_property(NS['mbox'],mbox,context)
    mboxshal1()
  end

  def mboxshal1()
    mboxshal = self.get_property(MBOXSHAL1)
    unless mboxshal
      mboxshal1 = "Generated from #{mbox}"
      self.add_property(MBOXSHAL1,mboxshal1,Resource.new('http://generated/'))
    end
  end

  def to_Person(context=@context)
    self.set_type(NS['Person'],context)
  end

  #self.add_property(TYPE,NS['Person'])
  

end
