# Make sure it's possible to match various tokens in a real input
#   stream.  Ensure that each method of Token and of Token's various
#   desecendants behaves correctly.
use warnings;
use strict;

use Test::More;
use File::Find;

use constant PACKAGE    => "JavaScript::Tokenizer";
use constant TOKPACKAGE => "JavaScript::Tokenizer::Token";

# test counts
use constant USE        => 1;
use constant SYNOPSIS   => 2;
use constant REALWORLD  => 2;

my @JS_FILES;
find (sub { /\.js$/ and push @JS_FILES, $File::Find::name }, 't');

my $JS_FILES_TOKENS;
for my $js_file (@JS_FILES) {
        open JS_FILE, "$js_file.tokens" or next;  # why be a hardass?
        ++$JS_FILES_TOKENS while <JS_FILE>;
}

plan tests => USE + SYNOPSIS + $JS_FILES_TOKENS + @JS_FILES * REALWORLD;

# TODO: find some way to test strpos and pos() explicitly

#=============================================================================
# Make sure the module can be loaded
#-----------------------------------------------------------------------------
use_ok('JavaScript::Tokenizer');

#=============================================================================
# Test the SYNOPSIS
#-----------------------------------------------------------------------------

{
  my $code = "for (x = 3; x < 99; x++) { window.alert(x) };";

  my $tokens = JavaScript::Tokenizer->from_string($code);
  my @tokens;

  while (defined(my $token = $tokens->token)) {
        push @tokens, $token;
  }

  is(join('', @tokens), $code, "SYNOPSIS token content");
  is(@tokens, 32, "SYNOPSIS token count");
}

#=============================================================================
# Large real-world examples                             REALWORLD: 2
#-----------------------------------------------------------------------------
# ...that don't use InputElementRegExp at all.
for my $js_file (@JS_FILES) {
        my $noslurp = $/;
        local $/;  # Slurp mode ON
        open JS_FILE, "$js_file" or die "Can't open $js_file: $!";
        my $code = <JS_FILE>;
        close JS_FILE or die "Can't close $js_file: $!";
        $/ = $noslurp;

        my $tokens = JavaScript::Tokenizer->from_string($code);
        isa_ok($tokens, PACKAGE, "from_string");

        my @tokens;

        open JS_TOKS, "$js_file.tokens" or next;  # we didn't count this one
        while (defined(my $token = $tokens->token)) {
                push @tokens, $token;
                my $toktype = <JS_TOKS>; chomp $toktype;
                isa_ok(
                        $token,
                        TOKPACKAGE . "::$toktype",
                        "$js_file: token types"
                );
        }
        close JS_TOKS or die "Can't close $js_file.tokens: $!";

        is(join('', @tokens), $code, "$js_file: join, stringification");
}
