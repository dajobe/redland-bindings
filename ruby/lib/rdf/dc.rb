require 'rubygems'
require_gem 'rdf'

module DC

  NS = Redland::Namespace.new('http://purl.org/dc/elements/1.1/')
  TITLE = NS['title']
  DATE = NS['date']
  DESCRIPTION = NS['description']
  CREATOR = NS['creator']

end
