require 'test/unit'
require 'rdf/redland/node.rb'

class TestNode < Test::Unit::TestCase
  include Redland

  def test_initialize()
    node = Node.new("Dominic")
    assert(node.literal?)
    assert_equal('Dominic',node.literal.value)
  end

  def test_resource()
    node = Node.new(Uri.new("http://example.com/"))
    assert(node.resource?)
    assert_equal(node,Node.new(:uri_string=>"http://example.com/"))
  end

  def test_bnode()
    node = Node.new(:blank=>'dom')
    assert(node.blank?)
    assert_equal('dom',node.blank_identifier)
    assert_equal(node,BNode.new('dom'))
    node2 = BNode.new('dom2')
    assert_equal('dom2',node2.blank_identifier)
    assert(node2.blank?)
    node3 = BNode.new()
    node4 = BNode.new()
    assert(node3.blank?)
    assert(node4.blank?)
    assert_not_equal(node3,node4)
    
  end

def test_equality()
    node = Node.new("Dominic")
    assert_equal(node,Node.new("Dominic"))
    assert_not_equal(node,Node.new("NotDominic"))
    assert_equal(node, Node.new(node))
  end

  def test_namespace()
    foaf = Namespace.new("http://xmlns.com/foaf/0.1/")
    assert(foaf.resource?)
    assert_equal(foaf,Node.new(:uri_string=>"http://xmlns.com/foaf/0.1/"))
    nameinst = foaf['name']
    assert_equal(nameinst,Node.new(:uri_string=>"http://xmlns.com/foaf/0.1/name"))
    assert_same(nameinst,foaf['name'])
    assert_equal(foaf['mbox'],Node.new(:uri_string=>"http://xmlns.com/foaf/0.1/mbox"))
  end

  def test_get_uri()
    node = Node.new("Dominic")
    assert_raises(NodeTypeError){uri = node.uri} # Can't get uri from a literal node
    node = Node.new(Uri.new("http://xmlns.com/"))
    assert_equal(Uri, node.uri.class)
    assert_equal('http://xmlns.com/',node.uri.to_s)
  end

  def test_get_literal()
    node = Node.new("Dominic")
    assert_equal("Dominic", node.literal.value)
    node = Node.new(Uri.new("http://xmlns.com/"))
    assert_raises(NodeTypeError){node.literal}
  end

  def test_literal()
    literal = Literal.new('this is a literal','en')
    assert_equal('this is a literal',literal.value)
    assert_equal('en',literal.language)
    literal = Literal.from_xml('<em>chat<em>')
    assert_equal('<em>chat<em>',literal.value)
  end

end
