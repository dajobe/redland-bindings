#!/usr/bin/perl
#
# example.pl - Redland eaxmple Perl program
#
# $Id$
#
# Copyright (C) 2000 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology, University of Bristol.
#
#    This package is Free Software available under either of two licenses
#    (see FAQS.html to see why):
# 
# 1. The GNU Lesser General Public License (LGPL)
# 
#    See http://www.gnu.org/copyleft/lesser.html or COPYING.LIB for the
#    full license text.
#      _________________________________________________________________
# 
#      Copyright (C) 2000 David Beckett, Institute for Learning and
#      Research Technology, University of Bristol. All Rights Reserved.
# 
#      This library is free software; you can redistribute it and/or
#      modify it under the terms of the GNU Lesser General Public License
#      as published by the Free Software Foundation; either version 2 of
#      the License, or (at your option) any later version.
# 
#      This library is distributed in the hope that it will be useful, but
#      WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#      Lesser General Public License for more details.
# 
#      You should have received a copy of the GNU Lesser General Public
#      License along with this library; if not, write to the Free Software
#      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
#      USA
#      _________________________________________________________________
# 
#    NOTE - under Term 3 of the LGPL, you may choose to license the entire
#    library under the GPL. See COPYING for the full license text.
# 
# 2. The Mozilla Public License
# 
#    See http://www.mozilla.org/MPL/MPL-1.1.html or MPL.html for the full
#    license text.
# 
#    Under MPL section 13. I declare that all of the Covered Code is
#    Multiple Licensed:
#      _________________________________________________________________
# 
#      The contents of this file are subject to the Mozilla Public License
#      version 1.1 (the "License"); you may not use this file except in
#      compliance with the License. You may obtain a copy of the License
#      at http://www.mozilla.org/MPL/
# 
#      Software distributed under the License is distributed on an "AS IS"
#      basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#      the License for the specific language governing rights and
#      limitations under the License.
# 
#      The Initial Developer of the Original Code is David Beckett.
#      Portions created by David Beckett are Copyright (C) 2000 David
#      Beckett, Institute for Learning and Research Technology, University
#      of Bristol. All Rights Reserved.
# 
#      Alternatively, the contents of this file may be used under the
#      terms of the GNU Lesser General Public License, in which case the
#      provisions of the LGPL License are applicable instead of those
#      above. If you wish to allow use of your version of this file only
#      under the terms of the LGPL License and not to allow others to use
#      your version of this file under the MPL, indicate your decision by
#      deleting the provisions above and replace them with the notice and
#      other provisions required by the LGPL License. If you do not delete
#      the provisions above, a recipient may use your version of this file
#      under either the MPL or the LGPL License.
#

use RDF;

$test_file="dc.rdf";

warn "Creating storage\n";
my $storage=new RDF::Storage("hashes", "test", 
			     "new='yes',hash-type='bdb',dir='.'");
die "Failed to create RDF::Storage\n" unless $storage;
  
warn "\nCreating model\n";
my $model=new RDF::Model($storage, "");
die "Failed to create RDF::Model for storage\n" unless $model;


warn "\nCreating statement\n";
my $statement=RDF::Statement->new_from_nodes(RDF::Node->new_from_uri_string("http://purl.org/net/dajobe/"),
					     RDF::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator"),
					     RDF::Node->new_from_literal("Dave Beckett", "", 0, 0));
die "Failed to create RDF::Statement\n" unless $statement;

warn "\nAdding statement to model\n";
# after this $statement is owned by $model and should not be used
$model->add_statement($statement);
$statement=undef;

warn "\nParsing URI (file) $test_file\n";
my $uri=new RDF::URI("file:$test_file");

my $parser=new RDF::Parser('repat');
die "Failed to find parser\n" if !$parser;
$parser->feature("http://www.w3.org/1999/02/22-rdf-syntax-ns#aboutEach", "yes");

$stream=$parser->parse_as_stream($uri,$uri);
my $count=0;
while(!$stream->end) {
  $model->add_statement($stream->next);
  $count++;
}
$stream=undef;
warn "Parsing added $count statements\n";

warn "\nPrinting all statements\n";
$stream=$model->serialise;
while(!$stream->end) {
  print "Statement: ",$stream->next->as_string,"\n";
}
$stream=undef;

warn "\nSearching model for statements matching predicate http://purl.org/dc/elements/1.1/creator\n";
$statement=RDF::Statement->new_from_nodes(undef, RDF::Node->new_from_uri_string("http://purl.org/dc/elements/1.1/creator"), undef);
my $stream=$model->find_statements($statement);
while(!$stream->end) {
  my $statement2=$stream->next;
  print "Matching Statement: ",$statement2->as_string,"\n";
  my $subject=$statement2->subject;
  print "  Subject: ",$subject->as_string,"\n";
  print "  Predicate: ",$statement2->predicate->as_string,"\n";
  print "  Object: ",$statement2->object->as_string,"\n";
}
$stream=undef;
