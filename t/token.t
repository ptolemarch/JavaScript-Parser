# Make sure it's possible to match various tokens in a real input
#   stream.  Ensure that each method of Token and of Token's various
#   desecendants behaves correctly.
use warnings;
use strict;

use Test::More;
use List::Util 'sum';

use constant PACKAGE    => "JavaScript::Tokenizer::Token";

# test counts
use constant USE        => 1;
use constant SYNOPSIS   => 5 * 2;
use constant TYPE       => 3 + 1 + 3 * 3 * 2 + 4;
use constant TYPETYPE   => 2;

my %hierarchy = (
        _AsType => [qw(
                NullLiteral
                BooleanLiteral
                Identifier
                DecimalLiteral
                HexIntegerLiteral
                StringLiteral
                RegularExpressionLiteral
        )],
        _AsLexeme => [qw(
                ReservedWord
                Keyword
                FutureReservedWord
                Punctuator
                DivPunctuator
        )],
        _AsNothing => [qw(
                WhiteSpace
                LineTerminator
                MultiLineComment
                SingleLineComment
        )],
);

plan tests =>
        USE + SYNOPSIS + 
        + keys(%hierarchy) * TYPETYPE
        + sum(map { scalar @{$hierarchy{$_}} } keys %hierarchy) * TYPE;

#=============================================================================
# Make sure the module can be loaded.                   USE: 1
#-----------------------------------------------------------------------------
use_ok(PACKAGE);

#=============================================================================
# Test the SYNOPSIS                                     SYNOPSIS: 5 * 2
#-----------------------------------------------------------------------------
  my @lexemes = qw( x = 2 + y );
  my @tokens;

  for my $lexeme (@lexemes) {
          my $token = JavaScript::Tokenizer::Token->new();
          $token->lexeme($lexeme);

          push @tokens, $token;
  }

#  print join('', @tokens), "\n";

  for (my $i = 0; $i < @lexemes && $i < @tokens; ++$i) {
          is(length $lexemes[$i], length $lexemes[$i], "SYNOPSIS");
          is($lexemes[$i], $tokens[$i], "SYNOPSIS");
  }

#=============================================================================
# new()                                                 TYPE: 3, TYPETYPE: 2
#-----------------------------------------------------------------------------
# This is a bit more detailed than perhaps it has to be.  Since the
#   entire class hierarchy is generated dynamically, and since there
#   was, in the past, some question wehther Class::Struct created
#   subclassable classes, I'm erring on the side of caution.
for my $typetype (keys %hierarchy) {
        my $obj = (PACKAGE . "::$typetype")->new();
        isa_ok(
                $obj,
                PACKAGE . "::$typetype",
        );
        isa_ok(
                $obj,
                PACKAGE,
        );
        for my $type (@{$hierarchy{$typetype}}) {
                my $obj = (PACKAGE . "::$type")->new();
                isa_ok(
                        $obj,
                        PACKAGE . "::$type",
                );
                isa_ok(
                        $obj,
                        PACKAGE . "::$typetype",
                );
                isa_ok(
                        $obj,
                        PACKAGE,
                );
        }
}

#=============================================================================
# terminal()                                            TYPE: 1, TYPETYPE: 0
#-----------------------------------------------------------------------------
for my $typetype (keys %hierarchy) {
        for my $type (@{$hierarchy{$typetype}}) {
                my $obj = (PACKAGE . "::$type")->new();

                if ($typetype eq "_AsType") {
                        is(
                                $obj->terminal(),
                                $type,
                                "terminal() behaves with _AsType"
                        );
                } elsif ($typetype eq "_AsLexeme") {
                        is(
                                $obj->terminal(),
                                $obj->lexeme(),
                                "terminal() behaves with _AsLexeme"
                        );
                } elsif ($typetype eq "_AsNothing") {
                        ok(
                                (not defined $obj->terminal()),
                                "terminal() behaves with _AsNothing"
                        );
                } else {
                        fail("Fell through an elsif I shouldn't've");
                }
        }
}

#=============================================================================
# lexeme(), pos(), newlines()                   TYPE: 3 * 3 * 2, TYPETYPE: 0
#-----------------------------------------------------------------------------
# These tests are almost certainly unnecessary.  Whatever.
{
        my $value;
        for my $typetype (keys %hierarchy) {
                for my $type (@{$hierarchy{$typetype}}) {
                        for my $method (qw(lexeme pos newlines)) {
                                my $obj = (PACKAGE . "::$type")->new();
                                for (1 .. 3) {
                                        $value = rand 1000 * 10 ^ rand 10;
                                        is(
                                                $obj->$method($value),
                                                $value,
                                                "$method(\$arg) returns \$arg"
                                        );
                                        is(
                                                $obj->$method(),
                                                $value,
                                                "$method() returns what was set"
                                        );
                                }
                        }
                }
        }
}

#=============================================================================
# length(), string(), boolean()                         TYPE: 4, TYPETYPE: 0
#-----------------------------------------------------------------------------
{
        my $string = "";
        my $length = 0;
        for my $typetype (keys %hierarchy) {
                for my $type (@{$hierarchy{$typetype}}) {
                        $string .= "x";  ++$length;
                        my $obj = (PACKAGE . "::$type")->new();

                        $obj->lexeme($string);
                        is($obj->length, $length, "length()");
                        is(length($obj), $length, "stringifilengthication");
                        is($obj->lexeme, "$obj", "stringification");
                        ok($obj, "boolification");
                }
        }
}
