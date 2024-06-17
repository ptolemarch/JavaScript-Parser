# Take an inventory of the distribution:  Make sure that all of the
#   packages can be loaded and that all of each package's subroutines
#   exist.  Hey, why not?
# Particularly necessary for JavaScript::Tokenizer::Token (generated
#   from Class::Struct) and its descendants (entire classes built out of
#   a hash).
use warnings;
use strict;

use Test::More;
use List::Util 'sum';

# Data structure holding all of the principal packages of the
#   distribution, and these packages' public methods.
my %modules = (
        'Regexp::Common'     => [qw(
                RE_JavaScript_WhiteSpace
                RE_JavaScript_LineTerminator
                RE_JavaScript_MultiLineComment
                RE_JavaScript_SingleLineComment
                RE_JavaScript_ReservedWord
                RE_JavaScript_LineTerminator
                RE_JavaScript_Keyword
                RE_JavaScript_FutureReservedWord
                RE_JavaScript_Identifier
                RE_JavaScript_IdentifierStart
                RE_JavaScript_IdentifierPart
                RE_JavaScript_Punctuator
                RE_JavaScript_DivPunctuator
                RE_JavaScript_NullLiteral
                RE_JavaScript_BooleanLiteral
                RE_JavaScript_DecimalLiteral
                RE_JavaScript_HexIntegerLiteral
                RE_JavaScript_StringLiteral
                RE_JavaScript_EscapeSequence
                RE_JavaScript_RegularExpressionLiteral
        )],
        'JavaScript::Tokenizer::Token'     => [qw(
                new
                lexeme
                pos
                newlines

                length
                terminal
        )],
        'JavaScript::Tokenizer' => [qw(
                new
                from_string

                token

                state
                pos
                notoken
                unicode
        )],
        'JavaScript::Grammar'   => [qw|
                YYParse
                YYNberr
                YYData
                YYErrok
                YYError
                YYAccept
                YYAbort
                YYCurtok
                YYCurval
                YYExpect
                YYLexer
                YYSemval
        |],
        'JavaScript::Parser'    => [ ],
);

# The various descendants of JavaScript::Token
my @token_types = qw|
        _AsNothing
        _AsType
        _AsLexeme
        WhiteSpace
        LineTerminator
        MultiLineComment
        SingleLineComment
        Keyword
        FutureReservedWord
        NullLiteral
        BooleanLiteral
        Identifier
        DecimalLiteral
        HexIntegerLiteral
        StringLiteral
        Punctuator
        DivPunctuator
        RegularExpressionLiteral
|;

plan tests => 
        # use_ok() each module
        keys(%modules)
        # each method in each module
        + sum(map { scalar @{$modules{$_}} } keys %modules)
        # each method in each descendant of JS::T::T
        + scalar(@{$modules{'JavaScript::Tokenizer::Token'}})*@token_types;

for my $module (keys %modules) {
        next if $module eq 'Regexp::Common';
        use_ok($module);
        for my $sub (@{$modules{$module}}) {
                can_ok($module, $sub);
        }
}

use_ok('Regexp::Common', "JavaScript", @{$modules{'Regexp::Common'}});
for my $sub (@{$modules{'Regexp::Common'}}) {
        can_ok('main', $sub);
}

for my $token_type (@token_types) {
        for my $sub (@{$modules{'JavaScript::Tokenizer::Token'}}) {
                can_ok("JavaScript::Tokenizer::Token::$token_type", $sub);
        }
}
