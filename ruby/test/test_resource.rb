require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/constants'
require 'rdf/redland/resource'

class TestResource < Test::Unit::TestCase
  include Redland

  def setup
    @foaf = Namespace.new('http://xmlns.com/foaf/0.1/')
    @faa = Namespace.new("http://www.faa.gov/faa#")
  end


  def test_intitialize_from_model()
    model = Model.new()
    resource = model.create_resource("http://www.faa.gov/DominicSisneros")
    resource.add_property(@foaf['name'],'Dominic').
             add_property(@foaf['phone'],'801-320-2377').
                                                         add_property(@foaf['surname'],'Sisneros')
    resource.add_property(@foaf['nick'],'Dom')
    resource.add_property(@foaf['nick'],'Domster')
    assert_equal(5,resource.get_properties.size)
    #puts resource.get_properties
    assert_equal('Dominic', resource.get_property(@foaf['name']).to_s)
    #resource.get_properties(@foaf['nick']){|nick| puts "<nickname>#{nick}</nickname>"}
    nicknames = [Node.new('Dom'),Node.new('Domster')]
    assert_equal(nicknames,resource.get_properties(@foaf['nick']))
    #resource.get_properties(){|p,o| puts "#{p}:#{o}" }
  end

  def test_get_resource()
    model = Model.new()
    resource = model.create_resource("http://www.faa.gov/DominicSisneros")
    resource.add_property(@foaf['name'],'Dominic').
             add_property(@foaf['phone'],'801-320-2377').
                                                         add_property(@foaf['surname'],'Sisneros')
    assert_equal(3,resource.get_properties.size)
    resource = model.get_resource("http://www.faa.gov/DominicSisneros")
    assert_instance_of(Resource,resource)
    assert_equal("Dominic" ,resource.get_property(@foaf['name']).to_s)
    assert_equal("801-320-2377",resource.get_property(@foaf['phone']).to_s)
  end

  def load_resource(model)
    resource = model.create_resource("http://www.faa.gov/DominicSisneros")
    resource.add_property(@foaf['name'],'Dominic').
             add_property(@foaf['phone'],'801-320-2377').
                                                         add_property(@foaf['surname'],'Sisneros')
    resource.add_property(@foaf['nick'],'Dom')
    resource.add_property(@foaf['nick'],'Domster')
    return resource
  end

  def test_remove_predicate()
    model = Model.new
    resource = load_resource(model)
    assert_equal(5,resource.get_properties.size)
    assert_equal('Dominic',resource.get_property(@foaf['name']).to_s)
    resource.delete_property(@foaf['name'])
    assert_nil(resource.get_property(@foaf['name']))
    assert_equal(4,resource.get_properties.size)
    resource.delete_properties()
    assert_equal(0,resource.get_properties.size)
  end
  
  def test_type()
    model = Model.new()
    resource = model.create_resource()
    resource.type = @foaf['Person']
    assert_equal(Node.new(:uri_string=>'http://xmlns.com/foaf/0.1/Person'), resource.get_property(TYPE))
    assert_equal(@foaf['Person'],resource.type)
    resource.type = @faa['Project']
    assert_equal(Node.new(:uri_string=>'http://www.faa.gov/faa#Project'),resource.type)
    puts resource.model.triples()
  end
  
  def test_object_of_predicate()
  	model =  Model.new
  	dom = model.create_resource()
  	dom.type = @foaf['Person']
  	dom.add_property(@foaf['name'],'Dominic Sisneros')
  	proj = model.create_resource()
  	proj.add_property(@faa['jcn'],'108269')
  	proj.type = @faa['Project']
  	proj.add_property(@faa['electronic'],dom)
  	proj2 = model.create_resource()
  	proj2.add_property(@faa['jcn'],'10834')
  	proj2.type = @faa['Project']
  	proj2.add_property(@faa['electronic'],dom)
  	p = model.create_resource()
  	p.add_property(@foaf['knows'],dom)
  	assert_equal(3, dom.object_of_predicate().size)
  	assert_equal(2,dom.object_of_predicate(@faa['electronic']).size)
	puts proj.model.triples()
	puts "dom is object of #{dom.object_of_predicate}"
	#assert_equal(proj,dom.object_of_predicate(@faa['electronic']))
	puts "dom.object_of_predicate=> #{dom.object_of_predicate(@faa['electronic'])}"
  end
  
end

