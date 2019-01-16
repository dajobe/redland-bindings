module rdf.rasqal.constants;

import std.string;

private extern extern(C) immutable __gshared uint rasqal_version_major;
private extern extern(C) immutable __gshared uint rasqal_version_minor;
private extern extern(C) immutable __gshared uint rasqal_version_release;
private extern extern(C) immutable __gshared uint rasqal_version_decimal;

alias version_major   = rasqal_version_major;
alias version_minor   = rasqal_version_minor;
alias version_release = rasqal_version_release;
alias version_decimal = rasqal_version_decimal;

private extern extern(C) {
    immutable char* rasqal_copyright_string;
    immutable char* rasqal_home_url_string;
    immutable char* rasqal_license_string;
    immutable char* rasqal_short_copyright_string;
    immutable char* rasqal_version_string;
}

string copyright_string() {
    return rasqal_copyright_string.fromStringz;
}

string home_url_string() {
    return rasqal_home_url_string.fromStringz;
}

string license_string() {
    return rasqal_license_string.fromStringz;
}

string short_copyright_string() {
    return rasqal_short_copyright_string.fromStringz;
}

string version_string() {
    return rasqal_version_string.fromStringz;
}

unittest {
    import std.conv;

    string combinedString =
        to!string(version_major) ~ '.' ~ to!string(version_minor) ~ '.' ~ to!string(version_release);
    uint combinedDecimal =
        version_major * 10000 + version_minor * 100 + version_release;
    assert(combinedString == version_string, "Combined version string");
    assert(combinedDecimal == version_decimal, "Combined decimal version");
}
