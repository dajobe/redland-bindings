<?php
/* -*- Mode: php; c-basic-offset: 2 -*-
 *
 * test.php - Redland PHP Interface test program
 *
 * $Id$
 *
 * Copyright (C) 2002 David Beckett - http://purl.org/net/dajobe/
 * Institute for Learning and Research Technology - http://www.ilrt.org/
 * University of Bristol - http://www.bristol.ac.uk/
 * 
 * This package is Free Software or Open Source available under the
 * following licenses (these are alternatives):
 *   1. GNU Lesser General Public License (LGPL)
 *   2. GNU General Public License (GPL)
 *   3. Mozilla Public License (MPL)
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * full license terms.
 * 
 * 
 */

/* ------------------------------------------------------------------------ */

global $REDLAND_LOADED__;
if ($REDLAND_LOADED__) return;

$REDLAND_LOADED__ = true;

if (!extension_loaded("redland")) {
  if (!dl("redland.so")){ 
    exit;
  }
}


$world = librdf_new_world();
librdf_world_open($world);

print "Redland opened\n";

librdf_free_world($world);


print "Done\n";

?>
