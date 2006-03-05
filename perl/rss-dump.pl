#!/usr/bin/perl -w
#
# rss-view.pl - Redland RSS 1.0 test program
#
# $Id$
#
# Copyright (C) 2000-2003 David Beckett - http://purl.org/net/dajobe/
# Copyright (C) 2000-2003 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL)
#   2. GNU General Public License (GPL)
#   3. Mozilla Public License (MPL)
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 
#

use strict;

use RDF::Redland;
use RDF::Redland::RSS;

my(%namespaces)=(
# Built in modules
  'Dublin Core' => 'http://purl.org/dc/elements/1.1/',
  'Syndication' => 'http://purl.org/rss/1.0/modules/syndication/',

# Proposed modules from http://purl.org/rss/1.0/modules/proposed/  
  'Changed Page' => 'http://my.theinfo.org/changed/1.0/rss/',
  'RSS 0.91'     => 'http://purl.org/rss/1.0/modules/rss091#',
  'Threading'    => 'http://purl.org/rss/1.0/modules/threading/',
  'Slash'        => 'ttp://purl.org/rss/1.0/modules/slash/', 
);


die <<"EOT" if @ARGV < 1 || @ARGV > 2;
Usage: $0 <RSS URI> [BASE URI>]

This program excecises the Redland Perl RDF:Redland::RSS module
which supports the RSS 1.0 specification http://purl.org/rss/1.0/

Further information on this format can be found at the RSS-Dev list
page at http://groups.yahoo.com/group/rss-dev/

EOT

my $uri=$ARGV[0];
my $tmp_file=undef;
my $source_uri=$uri;
my $base_uri=(@ARGV ==2) ? $ARGV[1]: $uri;

if(0) { # use this code if the parser does not do URIs
  if($uri !~ m%^file:%) {
    use URI::URL;
    use URI::file;
    use LWP::Simple;

    $tmp_file="/tmp/$0-$$.rss";

    my $perl_uri;
    eval "\$perl_uri=new URI::URL('$uri')";
    if($@) {
      die "$0: URI $uri is not supported by Perl\n";
    }
    my $rc=getstore($perl_uri, $tmp_file);

    if(!is_success($rc)) {
      die "$0: Failed to fetch URI $uri - HTTP error $rc\n";
      unlink $tmp_file;
    }
    $source_uri=URI::file->new($tmp_file)->as_string;
  } 
}

print "Using source URI $source_uri and base URI $base_uri\n";

my $rss=new RDF::Redland::RSS($source_uri, $base_uri);
die "Failed to create RDF::Redland::RSS for URI $uri\n" unless $rss;

for my $channel ($rss->channels) {
  print "Found channel with URI ",$channel->uri->as_string,"\n";
  print "  title is ",($channel->title ? $channel->title->literal_value_as_latin1 : 'MISSING'),"\n";
  print "  link is ",($channel->link ? $channel->link->as_string : 'MISSING'),"\n";
  print "  desc is ",$channel->description->literal_value_as_latin1,"\n" if $channel->description;

  while(my($ns_label,$ns_prefix)=each %namespaces) {
    my(@props)=$channel->properties_with_ns_prefix($ns_prefix);
    if(@props) {
      print "  $ns_label properties from $ns_prefix found:\n";
      for my $property (@props) {
	my $value=$channel->property($property);
	if($value->is_resource) {
	  print "    ",$property->uri->as_string," : URI ",$value->uri->as_string,"\n";
	} else {
	  print "    ",$property->uri->as_string," : ",$value->literal_value_as_latin1,"\n";
	}
      }
    }
  }

  my(@items)=$channel->items;
  print "  Found ",scalar(@items)," items in channel\n";

  for my $item (@items) {
    print "  Item with URI ",$item->uri->as_string,"\n";
    print "    title is ",($item->title ? $item->title->literal_value_as_latin1 : 'MISSING'),"\n";
    print "    link is ",($item->link ? $item->link->as_string : 'MISSING'),"\n";
    # RSS 1.0 section 5.5 <item> - description is optional
    print "    desc is ",$item->description->literal_value_as_latin1,"\n" if $item->description;
    
    my(@props)=$item->properties;
    print "    All properties: ",join(' ', map {$_->uri->as_string} @props),"\n";

    while(my($ns_label,$ns_prefix)=each %namespaces) {
      my(@props)=$item->properties_with_ns_prefix($ns_prefix);
      if(@props) {
	print "    $ns_label properties from $ns_prefix found:\n";
	for my $property (@props) {
	  my $value=$item->property($property);
	  if($value->is_resource) {
	    print "      ",$property->uri->as_string," : URI ",$value->uri->as_string,"\n";
	  } else {
	    print "      ",$property->uri->as_string," : ",$value->literal_value_as_latin1,"\n";
	  }
	}
      }
    }
   
  }

  my $image=$channel->image;
  if($image) {
    print "  Image with URI ",$image->uri->as_string,"\n";
    
    # RSS 1.0 section 5.4 <image> - If present, nothing optional
    print "    title is ",($image->title ? $image->title->literal_value_as_latin1 : 'MISSING'),"\n";
    print "    link is ",($image->link ? $image->link->as_string : 'MISSING'),"\n";
    print "    url is ",$image->image_url->as_string,"\n" if $image->image_url;
  }

  my $textinput=$channel->textinput;
  if($textinput) {
    print "  Textinput with URI ",$textinput->uri->as_string,"\n";

    # RSS 1.0 section 5.6 <textinput> - If present, nothing optional
    print "    title is ",($textinput->title ? $textinput->title->literal_value_as_latin1 : 'MISSING'),"\n";
    print "    link is ",($textinput->link ? $textinput->link->as_string : 'MISSING'),"\n";
    print "    desc is ",($textinput->description ? $textinput->description->literal_value_as_latin1 : 'MISSING'),"\n";
    print "    name is ",($textinput->name ? $textinput->name->as_string : 'MISSING'),"\n";
  }

}

unlink $tmp_file if $tmp_file;
