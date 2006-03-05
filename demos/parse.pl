#!/usr/bin/perl -Tw
#
# parse.pl - Redland RDF Parsing demo
#
# $Id$
#
# Copyright (C) 2005-2006 David Beckett - http://purl.org/net/dajobe/
# Copyright (C) 2005 University of Bristol - http://www.bristol.ac.uk/
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

# Helps with broken web requests (missing headers)
$ENV{'Content-Length'}||=0;

# Tainting, dontcha know
$ENV{'HTTP_HOST'}='librdf.org';
$ENV{'PATH'}="/bin:/usr/bin";

# Standard perl modules
use CGI;
use LWP::Simple;
use URI::URL;

# Configuration

my(@parser_syntaxes)=qw(rdfxml ntriples turtle rss-tag-soup grddl guess),
my(%parser_syntax_labels)=('rdfxml'  =>'RDF/XML',
			    'ntriples' =>'N-Triples',
			    'turtle' =>'Turtle',
			    'rss-tag-soup' =>'RSS Tag Soup',
			    'grddl' => 'GRDDL',
			    'guess' => 'Guess via MIME type and URI'
);
my $default_parser_syntax='rdfxml';

my(%parser_blurbs)=(
  'rdfxml' => <<EOT
  <p>RDF/XML is defined in the <a href="http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/">RDF/XML (Revised) W3C Recommendation</a></p>
EOT
  ,
  'ntriples' => <<EOT
  <p>N-Triples is defined in the
<a href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"> RDF Test Cases W3C Recommendation</a></p>
EOT
  ,
  'turtle' => <<EOT
  <p>Turtle is the <a href="http://www.dajobe.org/2004/01/turtle/">Terse RDF Triple Language</a>
EOT
  ,
  'rss-tag-soup' => <<EOT
  <p>RSS Tag Soup reads any RSS syntax including Atom.</p>
EOT
  ,
  'grddl' => <<EOT
  <p><a href="http://www.w3.org/2004/01/rdxh/spec">Gleaning Resource Descriptions from Dialects of Languages (GRDDL)</a> from XHTML documents.</p>
EOT
);



my $log_file="/home/dajobe/demo.librdf.org/logs/parser.log";


# see FIXMEs below
my $max_stream_size=200;
my $max_result_size=200;

my(%namespaces)=(
  'rdf' => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs' => 'http://www.w3.org/2000/01/rdf-schema#',
  'dc' => 'http://purl.org/dc/elements/1.1/',
  'owl' => 'http://www.w3.org/2002/07/owl#',
#  'xsd' => 'http://www.w3.org/2001/XMLSchema#',
  'foaf' => 'http://xmlns.com/foaf/0.1/',
  'dcterms' => 'http://purl.org/dc/terms/',
  'bot' => 'http://www.w3.org/2001/sw/Europe/200401/bot/terms\#',
);


# Redland perl modules

use RDF::Redland;

#  $RDF::Debug=1;


######################################################################
# Subroutines

sub log_action ($$$;$) {
  my($host, $db, $message, $now)=@_;
  $now ||= time;
  return unless open (LOG, ">>$log_file");
  my($sec,$min,$hour,$mday,$mon,$year)=gmtime $now;
  my $date=sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",1900+$year,$mon+1,$mday,$hour,$min,$sec);
  $message =~ s/[\n\s]+/ /gs
    if $message;
  print LOG "$host $date $db $message\n";
  close(LOG);
}

sub end_page($) {
  my $q=shift;

  print <<'EOT';
<p>The source code of this demonstration is available in the Redland
bindings distribution as <tt>demos/parse.pl</tt> or from the
<a href="http://librdf.org/">Redland</a> website</p>
EOT

  print qq{<hr />\n\n<p class="copyright"><a href="http://purl.org/net/dajobe/">Dave Beckett</a></p>\n\n</div></body>\n</html>\n};
}

sub html_escape ($) {
  my $str=shift;
  $str =~ s/\&/\&amp;/g;
  $str =~ s/</\&lt;/g;
  $str =~ s/</\&gt;/g;
  $str;
}


sub format_node($) {
  my $node=shift;
  my $node_label;

  if(!defined $node) {
    $node_label="&nbsp;";
  } elsif($node->is_resource) {
    $node_label=$node->uri->as_string;
    $node_label=qq{<a href="$node_label">$node_label</a>};
  } elsif ($node->is_literal) {
    $node_label=$node->literal_value_as_latin1;
    if($node->literal_value_language) {
      $node_label.="@".$node->literal_value_language;
    }
    if($node->literal_datatype && 
       !$node->literal_value_is_wf_xml) {
      $node_label.="^^&lt;".$node->literal_datatype->as_string."&gt;";
    }
  } elsif ($node->is_blank) {
    $node_label="blank node ".$node->blank_identifier;
  } else {
    $node_label=$node ? $node->as_string : "undef";
  } 
  $node_label;
}


sub print_stream($) {
  my $stream=shift;

  print <<"EOT";
<center>
<table align="center" border="1">
<tr>
<th>Subject</th>
<th>Predicate</th>
<th>Object</th>
</tr>
EOT

  my $count=0;
  for(;!$stream->end ;  $stream->next) {
    my $statement=$stream->current;
    
    last if !$statement;
    
    my $subject_label=format_node($statement->subject);
    my $predicate_label=format_node($statement->predicate);
    my $object_label=format_node($statement->object);
    
    print << "EOT";
<tr>
<td>$subject_label</td>
<td>$predicate_label</td>
<td>$object_label</td>
</tr>
EOT

    $count++;
    if ($count >= $max_stream_size) {
      print << "EOT";
<tr>
<td colspan="3">Truncated at $max_stream_size items - sorry, this is just a demonstration.</td>
</tr>
EOT
      last;
    }

  } # while


  print <<"EOT";
</table>
</center>
EOT

  my $pl=($count != 1) ? 's' : '';
  print "\n\n<p>Found $count triple$pl</p>\n";

}




######################################################################
# Main code

my $q = new CGI;

# CGI parameter paranoia
my $val;

my $uri_string;
$val=$q->param('uri');
if(defined $val && $val =~ /^([ -~]+)$/) {
  $uri_string=$1;
} else {
  $uri_string=undef;
}

my $content_string='';
$val=$q->param('content');
if(defined $val && $val =~ /^(.*)$/s) {
  $content_string=$1;
}

my $parser_language=$default_parser_syntax;
$val=$q->param('language');
if(defined $val && grep($_ eq $val, @parser_syntaxes) && $val =~ /^([-a-z]+)$/) {
  $parser_language=$1;
}

my $format_namespaces='';
$val=$q->param('format_namespaces');
if(defined $val) {
  $format_namespaces=($val eq 'yes');
}

# End of parameter decoding


# Used in logging
my $host=$q->remote_host;
$host=$1 if $ENV{'HTTP_USER_AGENT'} &&
            $ENV{'HTTP_USER_AGENT'} =~ /demo proxy for (.+)$/;


######################################################################
# Emit content

print $q->header(-type  =>  'text/html', -charset=>'utf-8');


# Always print header
print <<"EOT";
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <title>Redland Raptor RDF Parser Demonstration</title>
  <!-- HTML STYLE -->
</head>
<body>

<!-- LOGO START -->
<h1>Redland Raptor RDF Parser Demonstration</h1>
<!-- LOGO END -->

<p>This is a demonstration of using
<a href="http://librdf.org/raptor/">Raptor</a>
to parse various syntaxes into RDF triples.
This was written using the
<a href="http://librdf.org/docs/perl.html">Redland Perl</a>
language binding.
</p>

EOT

# use q->url() to get URL of this script without any parser parameters
# since we are using a POST here and don't want them added to the
# submission URL.
#my $action_url="/".$q->url(-relative=>1);
my $action_url="/parse";

my $parser_language_label=$parser_syntax_labels{$parser_language};

print <<"EOT";
<p>Firstly choose a syntax then
either enter a URI of some content 
or put it in the text box and run the parse.</p>

EOT
print $q->start_form(-method=>'GET', -action => $action_url),"\n";
print "<p>Syntax:\n";
print $q->radio_group(-name=>'language', 
			-values=>\@parser_syntaxes,
			-default=>$default_parser_syntax, 
			-labels=>\%parser_syntax_labels);
print "</p>\n\n<p><em>URI of content to parse</em><br/>\n";
print $q->textfield(-name=>'uri',
		      -default=>'',
		      -size=>100,
		      -maxlength=>1024);

print "</p>\n\n<p>or <em>content</em><br/>\n";
print $q->textarea(-name=>'content',
		    -default=>$content_string,
		    -columns=>100,
		    -rows=>10);

print "</p>\n\n<p>";

print $q->submit('Run Parser'),"\n";

print "</p>\n";
print $q->endform,"\n";


if(!$content_string && !$uri_string) {
  print <<"EOT";
<p>No data given.</p>
EOT
} else {
  my $parser_blurb=$parser_blurbs{$parser_language};

  print <<"EOT";
$parser_blurb

EOT

}


# Any parameters?

if(!$q->param) {
  end_page($q);
  exit 0;
}

print "<hr />\n\n<h2>$parser_language_label Parsing Results</h2>\n";

######################################################################

# Validate me

my $uri=undef;
if($uri_string) {
  $uri=new RDF::Redland::URI($uri_string);
} else {
  $uri=new RDF::Redland::URI("http://librdf.org/parse#");
}

my $statement=undef;

my(@context_nodes);

if(!$content_string && !$uri_string) {
  end_page($q);
  exit 0;
}

if($content_string) {
  log_action($host, '-', "Data '$content_string' in language $parser_language");
} else {
  log_action($host, '-', "Data from $uri_string in language $parser_language");
}


my(@errors)=();

my $handler=sub {
  my($code, $level, $facility, $message, $line, $column, $byte, $file, $uri)=@_;
  
  push(@errors, [$message, $line]);

  #print "code $code\n";
  #print "level $level\n";
  #print "facility $facility\n";
  #print "message: $message\n" if defined $message;
  #print "line $line\n";
  #print "column $column\n";
  #print "byte $byte\n";
  #print "file $file\n" if defined $file;
  #print "uri $uri\n" if defined $uri;
};

RDF::Redland::set_log_handler($handler);

my $parser=undef;

eval '$parser=new RDF::Redland::Parser($parser_language);';
if($@ || @errors || !$parser) {
  my $err=join("<br />", map {$_->[0]} @errors);
  print "\n\n<p>$parser_language_label parser construction failed with errors:</b> $err</p>\n";
  end_page($q);
  exit 0;
}

# FIXME
$q->delete('command');
$q->delete('parser');
$q->delete('language');

my $stream=undef;
if($content_string) {
  eval '$stream=$parser->parse_string_as_stream($content_string, $uri);';
} else {
  eval '$stream=$parser->parse_as_stream($uri);';
}
if($@ || @errors || !$stream || $stream->end) {
  my $err=join("<br />", map {$_->[1].":".$_->[0]} @errors);
  #my $err=join("<br />", @errors);
  print "\n\n<p><b>$parser_language_label parser failed with errors:</b><br />$err</p>\n";
  end_page($q);
  exit 0;
}

print_stream($stream);

end_page($q);
exit 0;
