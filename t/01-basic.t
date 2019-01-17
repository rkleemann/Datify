#! /usr/bin/env perl

use strict;
use warnings;

use Test2::V0;
plan 3;

ok require Datify, 'Required Datify';

can_ok 'Datify', qw(
    new get set
    varify
    undefify
    booleanify
    stringify stringify1 stringify2
    numify
    scalarify
    vstringify
    regexpify
    listify arrayify
    keyify pairify hashify
    objectify
    codeify
    refify
    formatify
    globify
);

isa_ok( Datify->new, 'Datify' );

