/* Ruby pollutes the #define space with these names */
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT

/* STR2CSTR was deprecated in Ruby 1.8 and removed in 1.9 */
#ifndef STR2CSTR
#define STR2CSTR(x) StringValuePtr(x)
#endif
