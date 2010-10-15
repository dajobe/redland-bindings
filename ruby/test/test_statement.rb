require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/node'


class TestStatement < Test::Unit::TestCase
  include Redland

  def test_initialize()
    st = Statement.new(Uri.new("http://foo"),Uri.new("http://bar"),"baz")
    assert_instance_of(Statement,st)
    #puts st.subject.class
    assert_equal(Node.new(Uri.new("http://foo")),st.subject)
    assert_equal(Node.new(Uri.new("http://bar")),st.predicate)
    assert_equal('baz',st.object.to_s)
    st.subject = Node.new(Uri.new("http://xmlns.com" ))
    st.predicate = Node.new( Uri.new("http://xmlns.com/#name"))
    st.object = Node.new("Dominic")
  end
end
