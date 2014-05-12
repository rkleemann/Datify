#! /usr/bin/env perl

use v5.12;

use Test::More tests => 19;

ok require Datify, 'Required Datify';


my @strings = (
    'Hello',
    'Hello "world"',
    'Hello world \o/',
    '',
    #           map({sprintf"%02x:%s",$_,chr} 0 .. 0xff                          ) ,
    #           map({sprintf"%02x:%s",$_,chr} 0x20 .. 0x7e                       ) ,
    #           map({sprintf"%02x:%s",$_,chr} 0 .. 0x1f, 0x22, 0x5c, 0x7f .. 0xff) ,
    #join(', ', map({sprintf"%02x:%s",$_,chr} 0 .. 0xff                          )),
    #join(', ', map({sprintf"%02x:%s",$_,chr} 0 .. 0x1f, 0x22                    )),
    #join(', ', map({sprintf"%02x:%s",$_,chr}                  0x5c, 0x7f .. 0xff)),
    join(', ', map({sprintf"%02x:%s",$_,chr} 0x20 .. 0x7e                       )),
    join(', ', map({sprintf"%02x:%s",$_,chr} 0 .. 0x1f, 0x22, 0x5c, 0x7f .. 0xff)),
);
foreach my $string (@strings) {
    my $escapes;
    my $str;

    $str = Datify->stringify1($string);
    # \x27 = ', \x5c = \
    $escapes = $string =~ tr/\x27\x5c// + $string =~ tr/\x5c//;
    is $str =~ tr/\x5c//, $escapes, "Proper escapes for string";

    $str = Datify->stringify2($string);
    # \x22 = ", \x24 = $, \x40 = @, \x5c = \
    $escapes = $string =~ s/([[:cntrl:]\x22\x24\x40\x5c])/$1/g
             + $string =~ s/([\x5c])/$1/g;
    is $str =~ tr/\x5c//, $escapes, "Proper escapes for string";

    $str = Datify->stringify($string);
    if ( $string =~ /[[:cntrl:]]/ ) {
        # \x22 = ", \x24 = $, \x40 = @, \x5c = \
        $escapes = $string =~ s/([[:cntrl:]\x22\x24\x40\x5c])/$1/g
                 + $string =~ s/([\x5c])/$1/g;
    } else {
        $escapes = $string =~ tr/\x27\x5c// + $string =~ tr/\x5c//;
    }
    is $str =~ tr/\x5c//, $escapes, "Proper escapes for string";
}

