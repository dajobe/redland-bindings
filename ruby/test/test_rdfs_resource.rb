require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/constants'
require 'rdf/redland/schemas/rdfs'

class TestRDFResource < Test::Unit::TestCase
  include Redland::RDFS

  def setup
    @rdfsns = Namespace.new("http://www.w3.org/2000/01/rdf-schema#")
  end


  def test_comment()
    model = Model.new()
    resource = model.create_resource("http://www.faa.gov/DominicSisneros")
    	resource.extend RDFS
    	resource.comment = "This is a new comment"
    assert_equal("This is a new comment",resource.get_property(RDFS_COMMENT).value)
    assert_equal("This is a new comment",resource.comment)	
    	resource.comment ="change comment"
    	assert_equal("change comment",resource.model.object(resource,RDFS_COMMENT).value)
  end
  
  def test_label()
  	model = Model.new()
  	resource = model.create_resource("http://class")
  	resource.extend RDFS
  	resource.add_label('First label')
  	assert_equal('First label',resource.label.value)
  	resource.add_label('A new label')
  	assert_equal('A new label', resource.label.value)
  	assert_equal(1, resource.get_properties(RDFS_LABEL).size)
  	resource.add_label('A new label','en') #should be different
  	assert_equal(2, resource.get_properties(RDFS_LABEL).size)
  end
  
  	def test_get_label_language()
  		model = Model.new()
  		resource = model.create_resource('http://class')
  		resource.extend RDFS
  		resource.add_label('Hello','en')
  		resource.add_label('Hola','sp')
  		assert_equal('Hello',resource.label('en').value)
  		assert_equal('Hola',resource.label('sp').value)
  		assert_nil(resource.label('fr'))
    end
    
    	def test_type
      model = Model.new()
      resource = model.create_resource('http://class')
      resource.extend RDFS
      resource.type = Resource.new('http://faa.gov/#Project')
      assert(resource.type?( Resource.new('http://faa.gov/#Project') ))
      #assert_equal(Resource.new('http://faa.gov/#Project'),resource.type)
      puts resource.type
      puts resource.model.triples
    end
    
  

end

