module rdf.raptor.constants;

private extern extern(C) immutable __gshared uint raptor_version_major;
private extern extern(C) immutable __gshared uint raptor_version_minor;
private extern extern(C) immutable __gshared uint raptor_version_release;
private extern extern(C) immutable __gshared uint raptor_version_decimal;

alias version_major   = raptor_version_major;
alias version_minor   = raptor_version_minor;
alias version_release = raptor_version_release;
alias version_decimal = raptor_version_decimal;

// TODO
