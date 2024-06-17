# Make sure that each regex in JavaScript::Token::Regex matches what it
#   should, and doesn't match what it shouldn't.
use warnings;
use strict;

use Test::More;

# test counts
use constant USE        => 1;
use constant SYNOPSIS   => 4;

use constant NAMESPACE  => "JavaScript";

my %regexen;

BEGIN {
        # list of all the tests specific to any particular regular
        #   expression
        %regexen = (
                "WhiteSpace" => {
                        positive => [
                                " \t\f",
                        ],
                        negative => [
                                "\b ",
                                " . ",
                                "Monkey!",
                                "",
                        ],
                },
                "WhiteSpace_Unicode" => {
                        positive => [
                                " \t\f\x{0009}\x{000B}\x{000C}\x{0020}\x{00A0}",
                        ],
                        negative => [
                                "\x{263a}",
                                "\b ",
                                " . ",
                                "Monkey!",
                                "",
                        ],
                },
                "LineTerminator" => {
                        positive => [
                                "\n",
                                "\r",
                        ],
                        negative => [
                                "\n\r",
                                "\x{263a}",
                                "Monkey!",
                                "",
                        ],
                },
                "LineTerminator_Unicode" => {
                        positive => [
                                "\n",
                                "\r",
                                "\x{000A}",
                                "\x{000D}",
                                "\x{2028}",
                                "\x{2029}",
                        ],
                        negative => [
                                "\n\r\x{000A}\x{000D}\x{2028}\x{2029}",
                                "\x{263a}",
                                "Monkey!",
                                "",
                        ],
                },
                "MultiLineComment" => {
                        positive => [
                                q|/*
                               */|,
                                "/* Monkey! */",
                                "/* Monkey! \x{263a} */",
                                "/* \nflorp */",
                                "/* foo\rbar */",
                                "/* /* /* */",  # this isn't a nested comment!
                        ],
                        negative => [
                                q|/*
                                */
|,
                                "/* Monkey! */\n",
                                "Monkey!",
                                "",
                                "//* *//",
                                "/* /* */ */",
                                "/* /* /* */\n",
                                "/* /* /* */\r",
                        ],
                },
                "SingleLineComment" => {
                        positive => [
                                "// sd\t",
                                "// sd\f",
                                "////\\\///\\\\ ",
                                "<!-- hide from older browsers ",
                                "<!-- hide \x{2028} from older browsers ",
                        ],
                        negative => [
                                "  // alice  ",
                                "// ma\nry   ",
                                "// mary   \n//",
                                "// mary\n",
                                "Monkey!",
                                "",
                        ],
                },
                "SingleLineComment_Unicode" => {
                        positive => [
                                "// sd\t",
                                "// sd\f",
                                "////\\\///\\\\ ",
                                "<!-- hide from older browsers ",
                        ],
                        negative => [
                                "  // alice  ",
                                "// ma\nry   ",
                                "// mary   \n//",
                                "// mary   \n",
                                "<!-- hide \x{2028} from older browsers ",
                                "Monkey!",
                                "",
                        ],
                },
                "ReservedWord" => {
                        positive => [qw|
                                break
                                abstract
                                null
                                true
                        |],
                        negative => [
                                "variable",
                                "Monkey!",
                                "",
                        ],
                        identity => [qw|
                                do
                                double
                                default
                                instanceof
                                interface
                                int
                                synchronized
                        |],
                },
                "Keyword" => {
                        positive => [qw|
                                in
                                instanceof
                                finally
                        |],
                        negative => [
                                "variable",
                                "Monkey!",
                                "",
                        ],
                        identity => [qw|
                                instanceof
                                finally
                        |],
                },
                "FutureReservedWord" => {
                        positive => [qw|
                                abstract
                                interface
                                float
                                goto
                                volatile
                                transient
                        |],
                        negative => [
                                "do",
                                "vol",
                                "variable",
                                "Monkey!",
                                "",
                        ],
                        identity => [qw|
                                abstract
                                interface
                                float
                                goto
                                volatile
                                transient
                        |],
                },
                "Identifier" => {
                        positive => [
                                "enumeration",
                                "abstraction",
                                "reimplements",
                                "falsehood",
                                "Anonymous4",
                        ],
                        negative => [
                                "2LiveCrew",
                                "Monkey!",
                                "",
                        ],
                },
                "IdentifierStart" => {
                },
                "IdentifierPart" => {
                },
                "Punctuator" => {
                        identity => [qw|
                                >=
                                !
                                !=
                                !==
                                =
                                ==
                                ===
                                <<=
                                --
                                -
                                ++
                                +
                                .
                        |],
                },
                "DivPunctuator" => {
                        positive => [
                                "/",
                                "/=",
                        ],
                        negative => [
                                "//",
                                "/=/",
                                "Monkey!",
                                "",
                        ],
                },
                "NullLiteral" => {
                        positive => [
                                "null",
                        ],
                        negative => [
                                "nul",
                                "nullify",
                                "Null",
                                "Monkey!",
                                "",
                        ],
                },
                "BooleanLiteral" => {
                        positive => [
                                "true",
                                "false",
                        ],
                        negative => [
                                "tRue",
                                "falsE",
                                "falsehood",
                                "Monkey!",
                                "",
                        ],
                },
                "DecimalLiteral" => {
                        positive => [
                                "3.14159265369",
                                "6.02E23",
                                "6.02e23",
                                "6.02e-23",
                                ".02e-23",
                                "6.02e+23",
                                ".02e+23",
                                "1.e3",
                                ".0",
                                "0",
                                "0.",
                        ],
                        negative => [
                                ".",
                                "2e2.3",
                                "2e2.",
                                "02",
                                "02.3",
                                "-3",
                                "Monkey!",
                                "",
                        ],
                },
                "HexIntegerLiteral" => {
                        positive => [
                                "0x0",
                                "0X0",
                                "0xAF",
                        ],
                        negative => [
                                "OxO",
                                "x0",
                                "0x",
                                "X0",
                                "0X",
                                "0xG",
                                "Monkey!",
                                "",
                        ],
                },
                "StringLiteral" => {
                        positive => [
                                "''",
                                "\"\"",
                                "\"\\\"\"",
                                "\"\\\\\\\"\"",
                                "'\\x0000'",
                                "'\\uaaaa'",
                        ],
                        negative => [
                                "",
                                " ' '",
                                "' ' ",
                                "'\n'",
                                "'''",
                                "\"\"\"",
                                "\"\n\"",
                                "'\\x1'",
                                "'\\xGG'",
                                "'\\u1'",
                                "'\\u111'",
                                "'\\uagaa'",
                                "Monkey!",
                                "",
                        ],
                },
                "RegularExpressionLiteral" => {
                        positive => [
                                "/   /",
                                "/(foo)*\*/x",
                                "/   /dsfalkjasfdlkjsdf",
                                "/ \  /d",
                        ],
                        negative => [
                                "/\\\n/",
                                "/\n/",
                                "//",
                                "//x",
                                "/*x/",
                                "/   /d;s",
                                "/ /  /d",
                                "Monkey!",
                                "",
                        ],
                },
        );

        # count all the tests in the above hash of hashes of lists.  :-)
        my $candidate_count;
        for my $regex (keys %regexen) {
                for my $sense (keys %{$regexen{$regex}}) {
                        for my $candidate (@{$regexen{$regex}{$sense}}) {
                                ++$candidate_count;
                        }
                }
        }


        plan tests => USE + SYNOPSIS + $candidate_count;
}

#=============================================================================
# Make sure the module can be loaded, and its symbols can be exported.
#-----------------------------------------------------------------------------
BEGIN { use_ok('Regexp::Common', qw|no_defaults JavaScript|) }

#=============================================================================
# Test the SYNOPSIS
#-----------------------------------------------------------------------------
{
        my $alert = 'Window.alert("heffalump");';
        my $math = 'Math.sin(3.14159) + Math.log(6.022e23)/Math.log(10);';

        my $string_literal = $RE{JavaScript}{StringLiteral}{-keep};
        my $decimal_literal = $RE{JavaScript}{DecimalLiteral}{-keep};

        my ($string)            = $alert =~ m/$string_literal/g;
        my ($pi,$avogadro,$ten) = $math =~ m/$decimal_literal/g;

        is($string, '"heffalump"');
        is($pi, '3.14159');
        is($avogadro, '6.022e23');
        is($ten, '10');
}

#=============================================================================
# Go through the %regexen
#-----------------------------------------------------------------------------
for my $regex (keys %regexen) {
        for my $sense (qw|positive negative identity|) {
                $regexen{$regex}{$sense} ||= [];
        }
        my $re;
        $regex =~ /([^_]+)(_Unicode)?/;
        if ($2) {
                $re = $RE{&NAMESPACE}{$1}{-unicode};
        } else {
                $re = $RE{&NAMESPACE}{$1};
        }
        for my $candidate (@{$regexen{$regex}{positive}}) {
                like(
                        $candidate,
                        qr/$re->{-head}{-tail}/,
                        "Is a '$regex'"
                );
        }
        for my $candidate (@{$regexen{$regex}{negative}}) {
                unlike(
                        $candidate,
                        qr/$re->{-head}{-tail}/,
                        "Is not a '$regex'"
                );
        }
        for my $candidate (@{$regexen{$regex}{identity}}) {
                $candidate =~ /$re->{-keep}/;
                is(
                        $candidate,
                        $1,
                        "'$regex' matches exactly"
                );
        }
}
