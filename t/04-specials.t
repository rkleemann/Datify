#! /usr/bin/env perl

use v5.12;

use List::MoreUtils 'natatime';
use Test::More tests => 43;

ok require Datify, 'Required Datify';

no warnings 'qw';
my $datify = join(' ', qw(
    array_ref   => '[$_]',
    assign      => '$var = $value;',
    beautify    => undef,
    code        => 'sub {...}',
    dereference => '$referent->$place',
    encode      =>   {0 => '\\\\0',  7 => '\\\\a',  9 => '\\\\t',
                     10 => '\\\\n', 12 => '\\\\f', 13 => '\\\\r',
                     27 => '\\\\e',
                    255 => '\\\\x%02x', 65535 => '\\\\x{%04x}'},
    false       => "''",
    hash_ref    => '{$_}',
    io          => '*UNKNOWN{IO}',
    keysort     => sub {...},
    keywords    => ['undef'],
    list        => '($_)',
    list_sep    => ', ',
    longstr     => 1_000,
    lvalue      => 'substr($lvalue, 0)',
    name        => '$self',
    nested      => '$referent$place',
    null        => 'undef',
    num_sep     => '_',
    object      => 'bless($data, $class_str)',
    pair        => '$key => $value',
    q1          => 'q',
    q2          => 'qq',
    q3          => 'qr',
    qpairs      => ['()', '<>', '[]', '{}'],
    qquotes     => ['!', '#', '%', '&', '*', '+', ',', '-', '.', '/',
                    ':', ';', '=', '?', '^', '|', '~', '$', '@', '`'],
    quote       => undef,
    quote1      => "'",
    quote2      => '"',
    quote3      => '/',
    reference   => '\\\\$_',
    sigils      => '$@',
    true        => 1,
    vformat     => 'v%vd'
));

my @specials = (
    undef           => undef()          => 'undef',
    '\\8_675_309'   => \867_5309        => 'number ref',
    "\\'scalar'"    => \'scalar'        => 'string ref',

    "['array', 1]"  => [ array => 1 ]   => 'array ref',
    #'TODO'          => ???              => 'format',
    '*::STDOUT'     => *STDOUT          => 'glob',
    '{hash => 1}'   => { hash  => 1 }   => 'hash ref',
    q!bless(*UNKNOWN{IO}, 'IO::File')!
                    => *STDOUT{IO}      => 'IO',
    "bless({$datify}, 'Datify')"
                    => Datify->new()    => 'object',
);
if ( $] >= 5.014 ) {
    push @specials, 'qr/(?^u:\s*)/'    => qr/\s*/ => 'regexp';
} else {
    push @specials, 'qr/(?-xism:\s*)/' => qr/\s*/ => 'regexp';
}
my $iter = natatime(3 => @specials);
while ( my ($string, $special, $desc) = $iter->() ) {
    my $str;

    $str = Datify->scalarify($special);
    is $str, $string, "$desc scalarified";

    $str = Datify->scalarify(\$special);
    is $str, "\\$string", "$desc ref scalarified";

    $str = Datify->varify( '$var' => $special);
    is $str, "\$var = $string;", "$desc varified";

    $str = Datify->varify( var => \$special);
    is $str, "\$var = \\$string;", "$desc ref varified";
}

my $str;
my $circular; $circular = \$circular;

$str = Datify->scalarify($circular);
is $str, '\\$self', "Circular scalarified";

$str = Datify->varify( var => $circular);
is $str, '$var = \\$var;', "Circular varified";

$str = Datify->scalarify(\$circular);
is $str, '\\$self', "Circular ref scalarified";

$str = Datify->varify( var => \$circular);
is $str, '$var = \\$var;', "Circular ref varified";

$str = Datify->scalarify(\\$circular);
is $str, '\\\\$self', "Circular double ref scalarified";

$str = Datify->varify( var => \\$circular);
is $str, '$var = \\\\$var;', "Circular double ref varified";

undef $circular;

