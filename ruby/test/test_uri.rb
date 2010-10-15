require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/uri'


class TestUri < Test::Unit::TestCase
  include Redland

  def test_initialize()
    uri = Uri.new("http://www.rdf.com")
    assert_equal('http://www.rdf.com',uri.to_s)
  end

  def test_uri_from_uri()
    uri = Uri.new("http://www.rdf.com")
    uri2 = Uri.new(uri)
    assert_equal('http://www.rdf.com',uri2.to_s)
  end

  def test_equality()
    uri = Uri.new("http://www.rdf.com")
    uri2 = Uri.new("http://www.rdf.com")
    assert_equal(uri,uri2)
    uri = nil
    assert_not_equal(uri,uri2)
    uri2 = nil
    assert_equal(uri,uri2)
    uri = 'http://www.rdf.com'
    uri2 = Uri.new('http://www.rdf.com')
    assert_not_equal(uri,uri2)
  end
end
