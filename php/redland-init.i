/* PHP seems happier if this happens at module init time */
if(!librdf_php_world) {
  librdf_php_world=librdf_new_world();
  librdf_world_open(librdf_php_world);
}
