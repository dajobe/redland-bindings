require 'rubygems'

spec = Gem::Specification.new do |s|
  s.name = "rdf-redland"
  s.version = "0.5"
  s.platform = Gem::Platform::RUBY
  s.summary = "rdf-redland is a wrapper over the Redland RDF framework"
  s.author = "Dominic Sisneros"
  s.email = "dom@sisna.com"
  s.files = Dir.glob("{lib,doc}/**/*").delete_if {|item| item.include?("CVS")|| item.include?("rdoc")}
  s.require_path = 'lib'
  s.autorequire = 'rdf'
end

if $0 ==__FILE__
  Gem::Builder.new(spec).build
end
  
