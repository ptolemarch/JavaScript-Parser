use 5.006;
use ExtUtils::MakeMaker;

# If Grammar.yp is newer than Grammar.pm, make will update the latter,
#   if you have Parse::Yapp.  But it creates a standalone parser, for
#   those who don't have it.
sub MY::postamble
{
        my $grammar = "lib/JavaScript/Grammar";
        eval { require Parse::Yapp };
        return if $@;

        "$grammar.pm: $grammar.yp\n\tyapp -s -m JavaScript::Grammar $grammar.yp"
}

WriteMakefile(
        NAME            => 'JavaScript::Parser',
        VERSION_FROM    => 'lib/JavaScript/Parser.pm',
        PREREQ_PM       => {
                'strict'        => 0,
                'warnings'      => 0,
                'Carp'          => 0,
                'constant'      => 0,
                'overload'      => 0,
                'Test::More'    => 0,
                'List::Util'    => 0,
                'File::Find'    => 0,
                'Regexp::Common'=> 1,
                'Class::Struct' => 0.59,
        },
        ABSTRACT_FROM   => 'lib/JavaScript/Parser.pm',
        AUTHOR          => 'David Hand <cogent@cpan.org>',
        clean           => { FILES => 'JavaScript-Parser-*', },
        depend          => { distdir => 'lib/JavaScript/Grammar.pm', },
);
