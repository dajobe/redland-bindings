require 'rdfs_resource'

class RDFSProperty < RDFSResource

	def domain
		domain = []
		self.get_properties(RDFS_DOMAIN)
		self.super_properties().each{ |p| domain.push p}
		
	end
	
	def super_properties()
		self.get_properties(RDFS_SUBPROPERTYOF)
	end
	
	def sub_properties()
		self.object_of_predicate(RDFS_SUBPROPERTY)
	end
	
	def add_domain(klasses)
		if not klasses return
		klasses.each do |cls|
			

