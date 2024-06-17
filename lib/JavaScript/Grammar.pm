####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package JavaScript::Grammar;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
#Included Parse/Yapp/Driver.pm file----------------------------------------
{
#
# Module Parse::Yapp::Driver
#
# This module is part of the Parse::Yapp package available on your
# nearest CPAN
#
# Any use of this module in a standalone parser make the included
# text under the same copyright as the Parse::Yapp module itself.
#
# This notice should remain unchanged.
#
# (c) Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.05';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
			 YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '');
#Mandatory parameters
my(@params)=('LEX','RULES','STATES');

sub new {
    my($class)=shift;
	my($errst,$nberr,$token,$value,$check,$dotpos);
    my($self)={ ERROR => \&_Error,
				ERRST => \$errst,
                NBERR => \$nberr,
				TOKEN => \$token,
				VALUE => \$value,
				DOTPOS => \$dotpos,
				STACK => [],
				DEBUG => 0,
				CHECK => \$check };

	_CheckParams( [], \%params, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Yapp driver version $VERSION ".
			  "incompatible with version $$self{VERSION}:\n".
			  "Please recompile parser module.";

        ref($class)
    and $class=ref($class);

    bless($self,$class);
}

sub YYParse {
    my($self)=shift;
    my($retval);

	_CheckParams( \@params, \%params, \@_, $self );

	if($$self{DEBUG}) {
		_DBLoad();
		$retval = eval '$self->_DBParse()';#Do not create stab entry on compile
        $@ and die $@;
	}
	else {
		$retval = $self->_Parse();
	}
    $retval
}

sub YYData {
	my($self)=shift;

		exists($$self{USER})
	or	$$self{USER}={};

	$$self{USER};
	
}

sub YYErrok {
	my($self)=shift;

	${$$self{ERRST}}=0;
    undef;
}

sub YYNberr {
	my($self)=shift;

	${$$self{NBERR}};
}

sub YYRecovering {
	my($self)=shift;

	${$$self{ERRST}} != 0;
}

sub YYAbort {
	my($self)=shift;

	${$$self{CHECK}}='ABORT';
    undef;
}

sub YYAccept {
	my($self)=shift;

	${$$self{CHECK}}='ACCEPT';
    undef;
}

sub YYError {
	my($self)=shift;

	${$$self{CHECK}}='ERROR';
    undef;
}

sub YYSemval {
	my($self)=shift;
	my($index)= $_[0] - ${$$self{DOTPOS}} - 1;

		$index < 0
	and	-$index <= @{$$self{STACK}}
	and	return $$self{STACK}[$index][1];

	undef;	#Invalid index
}

sub YYCurtok {
	my($self)=shift;

        @_
    and ${$$self{TOKEN}}=$_[0];
    ${$$self{TOKEN}};
}

sub YYCurval {
	my($self)=shift;

        @_
    and ${$$self{VALUE}}=$_[0];
    ${$$self{VALUE}};
}

sub YYExpect {
    my($self)=shift;

    keys %{$self->{STATES}[$self->{STACK}[-1][0]]{ACTIONS}}
}

sub YYLexer {
    my($self)=shift;

	$$self{LEX};
}


#################
# Private stuff #
#################


sub _CheckParams {
	my($mandatory,$checklist,$inarray,$outhash)=@_;
	my($prm,$value);
	my($prmlst)={};

	while(($prm,$value)=splice(@$inarray,0,2)) {
        $prm=uc($prm);
			exists($$checklist{$prm})
		or	croak("Unknow parameter '$prm'");
			ref($value) eq $$checklist{$prm}
		or	croak("Invalid value for parameter '$prm'");
        $prm=unpack('@2A*',$prm);
		$$outhash{$prm}=$value;
	}
	for (@$mandatory) {
			exists($$outhash{$_})
		or	croak("Missing mandatory parameter '".lc($_)."'");
	}
}

sub _Error {
	print "Parse error.\n";
}

sub _DBLoad {
	{
		no strict 'refs';

			exists(${__PACKAGE__.'::'}{_DBParse})#Already loaded ?
		and	return;
	}
	my($fname)=__FILE__;
	my(@drv);
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
	while(<DRV>) {
                	/^\s*sub\s+_Parse\s*{\s*$/ .. /^\s*}\s*#\s*_Parse\s*$/
        	and     do {
                	s/^#DBG>//;
                	push(@drv,$_);
        	}
	}
	close(DRV);

	$drv[0]=~s/_P/_DBP/;
	eval join('',@drv);
}

#Note that for loading debugging version of the driver,
#this file will be parsed from 'sub _Parse' up to '}#_Parse' inclusive.
#So, DO NOT remove comment at end of sub !!!
sub _Parse {
    my($self)=shift;

	my($rules,$states,$lex,$error)
     = @$self{ 'RULES', 'STATES', 'LEX', 'ERROR' };
	my($errstatus,$nberror,$token,$value,$stack,$check,$dotpos)
     = @$self{ 'ERRST', 'NBERR', 'TOKEN', 'VALUE', 'STACK', 'CHECK', 'DOTPOS' };

#DBG>	my($debug)=$$self{DEBUG};
#DBG>	my($dbgerror)=0;

#DBG>	my($ShowCurToken) = sub {
#DBG>		my($tok)='>';
#DBG>		for (split('',$$token)) {
#DBG>			$tok.=		(ord($_) < 32 or ord($_) > 126)
#DBG>					?	sprintf('<%02X>',ord($_))
#DBG>					:	$_;
#DBG>		}
#DBG>		$tok.='<';
#DBG>	};

	$$errstatus=0;
	$$nberror=0;
	($$token,$$value)=(undef,undef);
	@$stack=( [ 0, undef ] );
	$$check='';

    while(1) {
        my($actions,$act,$stateno);

        $stateno=$$stack[-1][0];
        $actions=$$states[$stateno];

#DBG>	print STDERR ('-' x 40),"\n";
#DBG>		$debug & 0x2
#DBG>	and	print STDERR "In state $stateno:\n";
#DBG>		$debug & 0x08
#DBG>	and	print STDERR "Stack:[".
#DBG>					 join(',',map { $$_[0] } @$stack).
#DBG>					 "]\n";


        if  (exists($$actions{ACTIONS})) {

				defined($$token)
            or	do {
				($$token,$$value)=&$lex($self);
#DBG>				$debug & 0x01
#DBG>			and	print STDERR "Need token. Got ".&$ShowCurToken."\n";
			};

            $act=   exists($$actions{ACTIONS}{$$token})
                    ?   $$actions{ACTIONS}{$$token}
                    :   exists($$actions{DEFAULT})
                        ?   $$actions{DEFAULT}
                        :   undef;
        }
        else {
            $act=$$actions{DEFAULT};
#DBG>			$debug & 0x01
#DBG>		and	print STDERR "Don't need token.\n";
        }

            defined($act)
        and do {

                $act > 0
            and do {        #shift

#DBG>				$debug & 0x04
#DBG>			and	print STDERR "Shift and go to state $act.\n";

					$$errstatus
				and	do {
					--$$errstatus;

#DBG>					$debug & 0x10
#DBG>				and	$dbgerror
#DBG>				and	$$errstatus == 0
#DBG>				and	do {
#DBG>					print STDERR "**End of Error recovery.\n";
#DBG>					$dbgerror=0;
#DBG>				};
				};


                push(@$stack,[ $act, $$value ]);

					$$token ne ''	#Don't eat the eof
				and	$$token=$$value=undef;
                next;
            };

            #reduce
            my($lhs,$len,$code,@sempar,$semval);
            ($lhs,$len,$code)=@{$$rules[-$act]};

#DBG>			$debug & 0x04
#DBG>		and	$act
#DBG>		and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): ";

                $act
            or  $self->YYAccept();

            $$dotpos=$len;

                unpack('A1',$lhs) eq '@'    #In line rule
            and do {
                    $lhs =~ /^\@[0-9]+\-([0-9]+)$/
                or  die "In line rule name '$lhs' ill formed: ".
                        "report it as a BUG.\n";
                $$dotpos = $1;
            };

            @sempar =       $$dotpos
                        ?   map { $$_[1] } @$stack[ -$$dotpos .. -1 ]
                        :   ();

            $semval = $code ? &$code( $self, @sempar )
                            : @sempar ? $sempar[0] : undef;

            splice(@$stack,-$len,$len);

                $$check eq 'ACCEPT'
            and do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Accept.\n";

				return($semval);
			};

                $$check eq 'ABORT'
            and	do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Abort.\n";

				return(undef);

			};

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Back to state $$stack[-1][0], then ";

                $$check eq 'ERROR'
            or  do {
#DBG>				$debug & 0x04
#DBG>			and	print STDERR 
#DBG>				    "go to state $$states[$$stack[-1][0]]{GOTOS}{$lhs}.\n";

#DBG>				$debug & 0x10
#DBG>			and	$dbgerror
#DBG>			and	$$errstatus == 0
#DBG>			and	do {
#DBG>				print STDERR "**End of Error recovery.\n";
#DBG>				$dbgerror=0;
#DBG>			};

			    push(@$stack,
                     [ $$states[$$stack[-1][0]]{GOTOS}{$lhs}, $semval ]);
                $$check='';
                next;
            };

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Forced Error recovery.\n";

            $$check='';

        };

        #Error
            $$errstatus
        or   do {

            $$errstatus = 1;
            &$error($self);
                $$errstatus # if 0, then YYErrok has been called
            or  next;       # so continue parsing

#DBG>			$debug & 0x10
#DBG>		and	do {
#DBG>			print STDERR "**Entering Error recovery.\n";
#DBG>			++$dbgerror;
#DBG>		};

            ++$$nberror;

        };

			$$errstatus == 3	#The next token is not valid: discard it
		and	do {
				$$token eq ''	# End of input: no hope
			and	do {
#DBG>				$debug & 0x10
#DBG>			and	print STDERR "**At eof: aborting.\n";
				return(undef);
			};

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Dicard invalid token ".&$ShowCurToken.".\n";

			$$token=$$value=undef;
		};

        $$errstatus=3;

		while(	  @$stack
			  and (		not exists($$states[$$stack[-1][0]]{ACTIONS})
			        or  not exists($$states[$$stack[-1][0]]{ACTIONS}{error})
					or	$$states[$$stack[-1][0]]{ACTIONS}{error} <= 0)) {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Pop state $$stack[-1][0].\n";

			pop(@$stack);
		}

			@$stack
		or	do {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**No state left on stack: aborting.\n";

			return(undef);
		};

		#shift the error token

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Shift \$error token and go to state ".
#DBG>						 $$states[$$stack[-1][0]]{ACTIONS}{error}.
#DBG>						 ".\n";

		push(@$stack, [ $$states[$$stack[-1][0]]{ACTIONS}{error}, undef ]);

    }

    #never reached
	croak("Error in driver logic. Please, report it as a BUG");

}#_Parse
#DO NOT remove comment

1;

}
#End of include--------------------------------------------------




sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.05',
                                  yystates =>
[
	{#State 0
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'Program' => 15,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'ShiftExpressionNoFb' => 54,
			'Block' => 55,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 40
		}
	},
	{#State 1
		DEFAULT => -1,
		GOTOS => {
			'_ISON_' => 71
		}
	},
	{#State 2
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 80,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 3
		DEFAULT => -216
	},
	{#State 4
		ACTIONS => {
			"[" => 98,
			"(" => 100,
			"." => 101
		},
		DEFAULT => -51,
		GOTOS => {
			'Arguments' => 99
		}
	},
	{#State 5
		DEFAULT => -1,
		GOTOS => {
			'_ISON_' => 102
		}
	},
	{#State 6
		DEFAULT => -222
	},
	{#State 7
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 103,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 8
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"," => 112,
			'Elementlist' => 107,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"]" => 118,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'Elision' => 108,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ElementList' => 115,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 111,
			'AdditiveExpression' => 122
		}
	},
	{#State 9
		DEFAULT => -220
	},
	{#State 10
		DEFAULT => -225
	},
	{#State 11
		ACTIONS => {
			"(" => 123
		}
	},
	{#State 12
		ACTIONS => {
			"%" => 124,
			"*" => 125,
			"/" => 126
		},
		DEFAULT => -106
	},
	{#State 13
		ACTIONS => {
			"-" => 127,
			"+" => 128
		},
		DEFAULT => -113
	},
	{#State 14
		ACTIONS => {
			'BooleanLiteral' => 129,
			'RegularExpressionLiteral' => 134,
			'StringLiteral' => 133,
			'HexIntegerLiteral' => 131,
			'DecimalLiteral' => 132,
			'NullLiteral' => 130
		},
		GOTOS => {
			'Literal' => 136,
			'NumericLiteral' => 135
		}
	},
	{#State 15
		ACTIONS => {
			'' => 137
		}
	},
	{#State 16
		DEFAULT => -190
	},
	{#State 17
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 138,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 18
		ACTIONS => {
			":" => 139
		},
		DEFAULT => -18
	},
	{#State 19
		ACTIONS => {
			"(" => 140
		}
	},
	{#State 20
		DEFAULT => -208
	},
	{#State 21
		DEFAULT => -45
	},
	{#State 22
		ACTIONS => {
			"new" => 81,
			"{" => 84,
			"this" => 95,
			"(" => 93,
			'Identifier' => 79,
			"[" => 8,
			"function" => 91
		},
		DEFAULT => -2,
		GOTOS => {
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'FunctionExpression' => 75,
			'NewExpression' => 141,
			'MemberExpression' => 142,
			'ObjectLiteral' => 90,
			'ArrayLiteral' => 85
		}
	},
	{#State 23
		DEFAULT => -67
	},
	{#State 24
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 143,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 25
		DEFAULT => -302
	},
	{#State 26
		ACTIONS => {
			"(" => 144
		}
	},
	{#State 27
		ACTIONS => {
			"<" => 145,
			">=" => 146,
			"instanceof" => 147,
			"<=" => 148,
			"in" => 150,
			">" => 149
		},
		DEFAULT => -142
	},
	{#State 28
		DEFAULT => -217
	},
	{#State 29
		DEFAULT => -221
	},
	{#State 30
		ACTIONS => {
			";" => 152,
			"," => 151
		}
	},
	{#State 31
		ACTIONS => {
			"|" => 153
		},
		DEFAULT => -172
	},
	{#State 32
		DEFAULT => -219
	},
	{#State 33
		DEFAULT => -299
	},
	{#State 34
		ACTIONS => {
			"typeof" => 42,
			"}" => 154,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 156,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'StatementList' => 155
		}
	},
	{#State 35
		DEFAULT => -85
	},
	{#State 36
		ACTIONS => {
			"(" => 157
		}
	},
	{#State 37
		DEFAULT => -20
	},
	{#State 38
		ACTIONS => {
			"{" => 34
		},
		GOTOS => {
			'Block' => 158
		}
	},
	{#State 39
		DEFAULT => -223
	},
	{#State 40
		ACTIONS => {
			'' => -298,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'SourceElement' => 159,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 41
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 160,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 42
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 161,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 43
		ACTIONS => {
			"^" => 162
		},
		DEFAULT => -166
	},
	{#State 44
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 163,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 45
		ACTIONS => {
			"*=" => 164,
			"|=" => 165,
			"&=" => 172,
			"--" => -1,
			"-=" => 173,
			"/=" => 174,
			"<<=" => 167,
			"%=" => 175,
			"^=" => 168,
			">>=" => 169,
			"++" => -1,
			"=" => 177,
			"+=" => 176,
			">>>=" => 171
		},
		DEFAULT => -72,
		GOTOS => {
			'_ISON_' => 170,
			'AssignmentOperator' => 166
		}
	},
	{#State 46
		ACTIONS => {
			"[" => 178,
			"(" => 100,
			"." => 180
		},
		DEFAULT => -68,
		GOTOS => {
			'Arguments' => 179
		}
	},
	{#State 47
		DEFAULT => -1,
		GOTOS => {
			'_ISON_' => 181
		}
	},
	{#State 48
		ACTIONS => {
			'Identifier' => 182
		}
	},
	{#State 49
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 183,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 50
		DEFAULT => -215
	},
	{#State 51
		ACTIONS => {
			"(" => 184
		}
	},
	{#State 52
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 186,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 53
		DEFAULT => -213
	},
	{#State 54
		ACTIONS => {
			">>>" => 188,
			"<<" => 189,
			">>" => 187
		},
		DEFAULT => -124
	},
	{#State 55
		DEFAULT => -212
	},
	{#State 56
		DEFAULT => -241
	},
	{#State 57
		DEFAULT => -1,
		GOTOS => {
			'_ISON_' => 190
		}
	},
	{#State 58
		DEFAULT => -17
	},
	{#State 59
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 191,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 60
		DEFAULT => -224
	},
	{#State 61
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 192,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 62
		DEFAULT => -214
	},
	{#State 63
		DEFAULT => -99
	},
	{#State 64
		DEFAULT => -301
	},
	{#State 65
		ACTIONS => {
			"||" => 194,
			"?" => 193
		},
		DEFAULT => -184
	},
	{#State 66
		ACTIONS => {
			"!=" => 197,
			"!==" => 196,
			"===" => 198,
			"==" => 195
		},
		DEFAULT => -154
	},
	{#State 67
		ACTIONS => {
			'Identifier' => 199
		},
		GOTOS => {
			'VariableDeclaration' => 201,
			'VariableDeclarationList' => 200
		}
	},
	{#State 68
		DEFAULT => -218
	},
	{#State 69
		ACTIONS => {
			"&&" => 202
		},
		DEFAULT => -178
	},
	{#State 70
		ACTIONS => {
			"&" => 203
		},
		DEFAULT => -160
	},
	{#State 71
		ACTIONS => {
			";" => 204
		}
	},
	{#State 72
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 205,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 73
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 206,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 74
		ACTIONS => {
			"[" => 207,
			"(" => 100,
			"." => 209
		},
		DEFAULT => -66,
		GOTOS => {
			'Arguments' => 208
		}
	},
	{#State 75
		DEFAULT => -41
	},
	{#State 76
		ACTIONS => {
			'BooleanLiteral' => 129,
			'RegularExpressionLiteral' => 134,
			'StringLiteral' => 133,
			'HexIntegerLiteral' => 131,
			'DecimalLiteral' => 132,
			'NullLiteral' => 130
		},
		GOTOS => {
			'Literal' => 210,
			'NumericLiteral' => 135
		}
	},
	{#State 77
		DEFAULT => -40
	},
	{#State 78
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 211,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 79
		DEFAULT => -12
	},
	{#State 80
		DEFAULT => -92
	},
	{#State 81
		ACTIONS => {
			"new" => 81,
			"{" => 84,
			"this" => 95,
			"(" => 93,
			'Identifier' => 79,
			"[" => 8,
			"function" => 91
		},
		DEFAULT => -2,
		GOTOS => {
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'FunctionExpression' => 75,
			'NewExpression' => 212,
			'MemberExpression' => 213,
			'ObjectLiteral' => 90,
			'ArrayLiteral' => 85
		}
	},
	{#State 82
		DEFAULT => -65
	},
	{#State 83
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 214,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 84
		ACTIONS => {
			"}" => 215,
			'Identifier' => 216,
			'StringLiteral' => 218,
			'HexIntegerLiteral' => 131,
			'DecimalLiteral' => 132
		},
		GOTOS => {
			'PropertyNameAndValueList' => 217,
			'NumericLiteral' => 220,
			'PropertyName' => 219
		}
	},
	{#State 85
		DEFAULT => -14
	},
	{#State 86
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 221,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 87
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 222,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 88
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 223,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 89
		ACTIONS => {
			"[" => 224,
			"(" => 100,
			"." => 226
		},
		DEFAULT => -49,
		GOTOS => {
			'Arguments' => 225
		}
	},
	{#State 90
		DEFAULT => -15
	},
	{#State 91
		ACTIONS => {
			"(" => 228,
			'Identifier' => 227
		}
	},
	{#State 92
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 229,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 93
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 230,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 94
		ACTIONS => {
			"--" => -1,
			"++" => -1
		},
		DEFAULT => -69,
		GOTOS => {
			'_ISON_' => 231
		}
	},
	{#State 95
		DEFAULT => -11
	},
	{#State 96
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 232,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 97
		DEFAULT => -75
	},
	{#State 98
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 233,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 99
		DEFAULT => -57
	},
	{#State 100
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 235,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'ArgumentList' => 236,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 234,
			'AdditiveExpression' => 122
		}
	},
	{#State 101
		ACTIONS => {
			'Identifier' => 237
		}
	},
	{#State 102
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			";" => 238,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 239,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 103
		DEFAULT => -94
	},
	{#State 104
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"in" => 245,
			"instanceof" => 242,
			"<=" => 243,
			">" => 244
		},
		DEFAULT => -137
	},
	{#State 105
		ACTIONS => {
			"%" => 246,
			"*" => 247,
			"/" => 248
		},
		DEFAULT => -103
	},
	{#State 106
		ACTIONS => {
			"&&" => 249
		},
		DEFAULT => -176
	},
	{#State 107
		ACTIONS => {
			"," => 250
		}
	},
	{#State 108
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"," => 252,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"]" => 253,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 251,
			'AdditiveExpression' => 122
		}
	},
	{#State 109
		DEFAULT => -188
	},
	{#State 110
		DEFAULT => -95
	},
	{#State 111
		DEFAULT => -27
	},
	{#State 112
		DEFAULT => -31
	},
	{#State 113
		ACTIONS => {
			"||" => 255,
			"?" => 254
		},
		DEFAULT => -182
	},
	{#State 114
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -117
	},
	{#State 115
		ACTIONS => {
			"," => 259,
			"]" => 260
		}
	},
	{#State 116
		ACTIONS => {
			"|" => 261
		},
		DEFAULT => -170
	},
	{#State 117
		ACTIONS => {
			"&" => 262
		},
		DEFAULT => -158
	},
	{#State 118
		DEFAULT => -22
	},
	{#State 119
		ACTIONS => {
			"*=" => 164,
			"|=" => 165,
			"&=" => 172,
			"--" => -1,
			"-=" => 173,
			"/=" => 174,
			"<<=" => 167,
			"%=" => 175,
			"^=" => 168,
			">>=" => 169,
			"++" => -1,
			"=" => 177,
			"+=" => 176,
			">>>=" => 171
		},
		DEFAULT => -69,
		GOTOS => {
			'_ISON_' => 231,
			'AssignmentOperator' => 263
		}
	},
	{#State 120
		ACTIONS => {
			"!=" => 266,
			"!==" => 265,
			"===" => 267,
			"==" => 264
		},
		DEFAULT => -152
	},
	{#State 121
		ACTIONS => {
			"^" => 268
		},
		DEFAULT => -164
	},
	{#State 122
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -109
	},
	{#State 123
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 271,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 124
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 272,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 125
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 273,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 126
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 274,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 127
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 275,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 128
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 276,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 129
		DEFAULT => -5
	},
	{#State 130
		DEFAULT => -4
	},
	{#State 131
		DEFAULT => -10
	},
	{#State 132
		DEFAULT => -9
	},
	{#State 133
		DEFAULT => -6
	},
	{#State 134
		DEFAULT => -8
	},
	{#State 135
		DEFAULT => -7
	},
	{#State 136
		DEFAULT => -3,
		GOTOS => {
			'_IED_' => 277
		}
	},
	{#State 137
		DEFAULT => 0
	},
	{#State 138
		DEFAULT => -87
	},
	{#State 139
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 278,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 140
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 279,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 141
		DEFAULT => -52
	},
	{#State 142
		ACTIONS => {
			"[" => 224,
			"(" => 100,
			"." => 226
		},
		DEFAULT => -49,
		GOTOS => {
			'Arguments' => 280
		}
	},
	{#State 143
		DEFAULT => -91
	},
	{#State 144
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 281,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 145
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 282,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 146
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 283,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 147
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 284,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 148
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 285,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 149
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 286,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 150
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 287,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 151
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 288,
			'AdditiveExpression' => 122
		}
	},
	{#State 152
		DEFAULT => -242
	},
	{#State 153
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'BitwiseXorExpression' => 289,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74
		}
	},
	{#State 154
		DEFAULT => -226
	},
	{#State 155
		ACTIONS => {
			"typeof" => 42,
			"}" => 290,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 291,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 156
		DEFAULT => -228
	},
	{#State 157
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 292,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 158
		ACTIONS => {
			"catch" => 296,
			"finally" => 294
		},
		GOTOS => {
			'Finally' => 293,
			'Catch' => 295
		}
	},
	{#State 159
		DEFAULT => -300
	},
	{#State 160
		DEFAULT => -86
	},
	{#State 161
		DEFAULT => -88
	},
	{#State 162
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 297,
			'CallExpression' => 74
		}
	},
	{#State 163
		DEFAULT => -93
	},
	{#State 164
		DEFAULT => -195
	},
	{#State 165
		DEFAULT => -205
	},
	{#State 166
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 298,
			'AdditiveExpression' => 122
		}
	},
	{#State 167
		DEFAULT => -200
	},
	{#State 168
		DEFAULT => -204
	},
	{#State 169
		DEFAULT => -201
	},
	{#State 170
		ACTIONS => {
			"++" => 300,
			"--" => 299
		}
	},
	{#State 171
		DEFAULT => -202
	},
	{#State 172
		DEFAULT => -203
	},
	{#State 173
		DEFAULT => -199
	},
	{#State 174
		DEFAULT => -196
	},
	{#State 175
		DEFAULT => -197
	},
	{#State 176
		DEFAULT => -198
	},
	{#State 177
		DEFAULT => -194
	},
	{#State 178
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 301,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 179
		DEFAULT => -58
	},
	{#State 180
		ACTIONS => {
			'Identifier' => 302
		}
	},
	{#State 181
		ACTIONS => {
			";" => 304,
			'Identifier' => 303
		}
	},
	{#State 182
		ACTIONS => {
			"(" => 305
		}
	},
	{#State 183
		DEFAULT => -90
	},
	{#State 184
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			";" => 316,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"var" => 318,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 315,
			'AssignmentExpressionNoIn' => 314,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'ExpressionNoIn' => 313,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 185
		DEFAULT => -206
	},
	{#State 186
		ACTIONS => {
			"," => 320,
			")" => 321
		}
	},
	{#State 187
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 322,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 188
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 323,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 189
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 324,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 190
		ACTIONS => {
			";" => 326,
			'Identifier' => 325
		}
	},
	{#State 191
		ACTIONS => {
			"while" => 327
		}
	},
	{#State 192
		DEFAULT => -89
	},
	{#State 193
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 328,
			'AdditiveExpression' => 122
		}
	},
	{#State 194
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 329,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 195
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 330,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 196
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 331,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 197
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 332,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 198
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 333,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 199
		ACTIONS => {
			"=" => 334
		},
		DEFAULT => -235,
		GOTOS => {
			'Initialiser' => 335
		}
	},
	{#State 200
		ACTIONS => {
			";" => 337,
			"," => 336
		}
	},
	{#State 201
		DEFAULT => -231
	},
	{#State 202
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 338,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 203
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 339,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 204
		DEFAULT => -282,
		GOTOS => {
			'@1-3' => 340
		}
	},
	{#State 205
		DEFAULT => -82
	},
	{#State 206
		DEFAULT => -84
	},
	{#State 207
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 341,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 208
		DEFAULT => -54
	},
	{#State 209
		ACTIONS => {
			'Identifier' => 342
		}
	},
	{#State 210
		DEFAULT => -3,
		GOTOS => {
			'_IED_' => 343
		}
	},
	{#State 211
		DEFAULT => -77
	},
	{#State 212
		DEFAULT => -50
	},
	{#State 213
		ACTIONS => {
			"[" => 224,
			"(" => 100,
			"." => 226
		},
		DEFAULT => -49,
		GOTOS => {
			'Arguments' => 344
		}
	},
	{#State 214
		DEFAULT => -81
	},
	{#State 215
		DEFAULT => -33
	},
	{#State 216
		DEFAULT => -37
	},
	{#State 217
		ACTIONS => {
			"}" => 345,
			"," => 346
		}
	},
	{#State 218
		DEFAULT => -38
	},
	{#State 219
		ACTIONS => {
			":" => 347
		}
	},
	{#State 220
		DEFAULT => -39
	},
	{#State 221
		DEFAULT => -76
	},
	{#State 222
		DEFAULT => -78
	},
	{#State 223
		DEFAULT => -83
	},
	{#State 224
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 348,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 225
		DEFAULT => -53
	},
	{#State 226
		ACTIONS => {
			'Identifier' => 349
		}
	},
	{#State 227
		ACTIONS => {
			"(" => 350
		}
	},
	{#State 228
		ACTIONS => {
			'Identifier' => 351,
			")" => 352
		},
		GOTOS => {
			'FormalParameterList' => 353
		}
	},
	{#State 229
		DEFAULT => -80
	},
	{#State 230
		ACTIONS => {
			"," => 320,
			")" => 354
		}
	},
	{#State 231
		ACTIONS => {
			"++" => 356,
			"--" => 355
		}
	},
	{#State 232
		DEFAULT => -79
	},
	{#State 233
		ACTIONS => {
			"," => 320,
			"]" => 357
		}
	},
	{#State 234
		DEFAULT => -63
	},
	{#State 235
		DEFAULT => -61
	},
	{#State 236
		ACTIONS => {
			"," => 358,
			")" => 359
		}
	},
	{#State 237
		DEFAULT => -47
	},
	{#State 238
		DEFAULT => -265
	},
	{#State 239
		ACTIONS => {
			";" => 360,
			"," => 320
		}
	},
	{#State 240
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 361,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 241
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 362,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 242
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 363,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 243
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 364,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 244
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 365,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 245
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 366,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 246
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 367,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 247
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 368,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 248
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 369,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 249
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 370,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 250
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"," => 112,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'Elision' => 371,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 372,
			'AdditiveExpression' => 122
		}
	},
	{#State 251
		DEFAULT => -28
	},
	{#State 252
		DEFAULT => -32
	},
	{#State 253
		DEFAULT => -23
	},
	{#State 254
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 373,
			'AdditiveExpression' => 122
		}
	},
	{#State 255
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 374,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 256
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 375,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 257
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 376,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 258
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 377,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 259
		ACTIONS => {
			"," => 112,
			"]" => 379
		},
		GOTOS => {
			'Elision' => 378
		}
	},
	{#State 260
		DEFAULT => -24
	},
	{#State 261
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'BitwiseXorExpression' => 380,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74
		}
	},
	{#State 262
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 381,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 263
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 382,
			'AdditiveExpression' => 122
		}
	},
	{#State 264
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 383,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 265
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 384,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 266
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 385,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 267
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 386,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 268
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 387,
			'CallExpression' => 74
		}
	},
	{#State 269
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 388,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 270
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 389,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'UnaryExpression' => 110,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 271
		ACTIONS => {
			"," => 320,
			")" => 390
		}
	},
	{#State 272
		DEFAULT => -102
	},
	{#State 273
		DEFAULT => -100
	},
	{#State 274
		DEFAULT => -101
	},
	{#State 275
		ACTIONS => {
			"%" => 246,
			"*" => 247,
			"/" => 248
		},
		DEFAULT => -108
	},
	{#State 276
		ACTIONS => {
			"%" => 246,
			"*" => 247,
			"/" => 248
		},
		DEFAULT => -107
	},
	{#State 277
		DEFAULT => -19
	},
	{#State 278
		DEFAULT => -281
	},
	{#State 279
		ACTIONS => {
			"," => 320,
			")" => 391
		}
	},
	{#State 280
		DEFAULT => -48
	},
	{#State 281
		ACTIONS => {
			"," => 320,
			")" => 392
		}
	},
	{#State 282
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -125
	},
	{#State 283
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -128
	},
	{#State 284
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -129
	},
	{#State 285
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -127
	},
	{#State 286
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -126
	},
	{#State 287
		ACTIONS => {
			">>>" => 257,
			"<<" => 258,
			">>" => 256
		},
		DEFAULT => -130
	},
	{#State 288
		DEFAULT => -209
	},
	{#State 289
		ACTIONS => {
			"^" => 268
		},
		DEFAULT => -167
	},
	{#State 290
		DEFAULT => -227
	},
	{#State 291
		DEFAULT => -229
	},
	{#State 292
		ACTIONS => {
			"," => 320,
			")" => 393
		}
	},
	{#State 293
		DEFAULT => -285
	},
	{#State 294
		ACTIONS => {
			"{" => 34
		},
		GOTOS => {
			'Block' => 394
		}
	},
	{#State 295
		ACTIONS => {
			"finally" => 294
		},
		DEFAULT => -284,
		GOTOS => {
			'Finally' => 395
		}
	},
	{#State 296
		ACTIONS => {
			"(" => 396
		}
	},
	{#State 297
		ACTIONS => {
			"&" => 262
		},
		DEFAULT => -161
	},
	{#State 298
		DEFAULT => -191
	},
	{#State 299
		DEFAULT => -74
	},
	{#State 300
		DEFAULT => -73
	},
	{#State 301
		ACTIONS => {
			"," => 320,
			"]" => 397
		}
	},
	{#State 302
		DEFAULT => -60
	},
	{#State 303
		ACTIONS => {
			";" => 398
		}
	},
	{#State 304
		DEFAULT => -263
	},
	{#State 305
		ACTIONS => {
			'Identifier' => 351,
			")" => 399
		},
		GOTOS => {
			'FormalParameterList' => 400
		}
	},
	{#State 306
		ACTIONS => {
			"||" => 402,
			"?" => 401
		},
		DEFAULT => -186
	},
	{#State 307
		ACTIONS => {
			"&&" => 403
		},
		DEFAULT => -180
	},
	{#State 308
		DEFAULT => -192
	},
	{#State 309
		ACTIONS => {
			"!=" => 406,
			"!==" => 405,
			"===" => 407,
			"==" => 404
		},
		DEFAULT => -156
	},
	{#State 310
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -131
	},
	{#State 311
		ACTIONS => {
			"^" => 408
		},
		DEFAULT => -168
	},
	{#State 312
		ACTIONS => {
			"|" => 409
		},
		DEFAULT => -174
	},
	{#State 313
		ACTIONS => {
			";" => 411,
			"," => 410
		}
	},
	{#State 314
		DEFAULT => -210
	},
	{#State 315
		ACTIONS => {
			"*=" => 164,
			"|=" => 165,
			"&=" => 172,
			"--" => -1,
			"-=" => 173,
			"/=" => 174,
			"<<=" => 167,
			"%=" => 175,
			"^=" => 168,
			">>=" => 169,
			"++" => -1,
			"in" => 413,
			"=" => 177,
			"+=" => 176,
			">>>=" => 171
		},
		DEFAULT => -69,
		GOTOS => {
			'_ISON_' => 231,
			'AssignmentOperator' => 412
		}
	},
	{#State 316
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			";" => 414,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 415,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 317
		ACTIONS => {
			"<" => 416,
			">=" => 417,
			"instanceof" => 418,
			"<=" => 419,
			">" => 420
		},
		DEFAULT => -147
	},
	{#State 318
		ACTIONS => {
			'Identifier' => 421
		},
		GOTOS => {
			'VariableDeclarationNoIn' => 422,
			'VariableDeclarationListNoIn' => 423
		}
	},
	{#State 319
		ACTIONS => {
			"&" => 424
		},
		DEFAULT => -162
	},
	{#State 320
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 425,
			'AdditiveExpression' => 122
		}
	},
	{#State 321
		DEFAULT => -21
	},
	{#State 322
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -115
	},
	{#State 323
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -116
	},
	{#State 324
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -114
	},
	{#State 325
		ACTIONS => {
			";" => 426
		}
	},
	{#State 326
		DEFAULT => -261
	},
	{#State 327
		ACTIONS => {
			"(" => 427
		}
	},
	{#State 328
		ACTIONS => {
			":" => 428
		}
	},
	{#State 329
		ACTIONS => {
			"&&" => 249
		},
		DEFAULT => -179
	},
	{#State 330
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"instanceof" => 242,
			"<=" => 243,
			"in" => 245,
			">" => 244
		},
		DEFAULT => -143
	},
	{#State 331
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"instanceof" => 242,
			"<=" => 243,
			"in" => 245,
			">" => 244
		},
		DEFAULT => -146
	},
	{#State 332
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"instanceof" => 242,
			"<=" => 243,
			"in" => 245,
			">" => 244
		},
		DEFAULT => -144
	},
	{#State 333
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"instanceof" => 242,
			"<=" => 243,
			"in" => 245,
			">" => 244
		},
		DEFAULT => -145
	},
	{#State 334
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 429,
			'AdditiveExpression' => 122
		}
	},
	{#State 335
		DEFAULT => -236
	},
	{#State 336
		ACTIONS => {
			'Identifier' => 199
		},
		GOTOS => {
			'VariableDeclaration' => 430
		}
	},
	{#State 337
		DEFAULT => -230
	},
	{#State 338
		ACTIONS => {
			"|" => 261
		},
		DEFAULT => -173
	},
	{#State 339
		ACTIONS => {
			"!=" => 266,
			"!==" => 265,
			"===" => 267,
			"==" => 264
		},
		DEFAULT => -155
	},
	{#State 340
		ACTIONS => {
			"throw" => 431
		}
	},
	{#State 341
		ACTIONS => {
			"," => 320,
			"]" => 432
		}
	},
	{#State 342
		DEFAULT => -56
	},
	{#State 343
		DEFAULT => -13
	},
	{#State 344
		DEFAULT => -44
	},
	{#State 345
		DEFAULT => -34
	},
	{#State 346
		ACTIONS => {
			'Identifier' => 216,
			'StringLiteral' => 218,
			'HexIntegerLiteral' => 131,
			'DecimalLiteral' => 132
		},
		GOTOS => {
			'NumericLiteral' => 220,
			'PropertyName' => 433
		}
	},
	{#State 347
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 434,
			'AdditiveExpression' => 122
		}
	},
	{#State 348
		ACTIONS => {
			"," => 320,
			"]" => 435
		}
	},
	{#State 349
		DEFAULT => -43
	},
	{#State 350
		ACTIONS => {
			'Identifier' => 351,
			")" => 436
		},
		GOTOS => {
			'FormalParameterList' => 437
		}
	},
	{#State 351
		DEFAULT => -295
	},
	{#State 352
		ACTIONS => {
			"{" => 438
		}
	},
	{#State 353
		ACTIONS => {
			"," => 439,
			")" => 440
		}
	},
	{#State 354
		DEFAULT => -16
	},
	{#State 355
		DEFAULT => -71
	},
	{#State 356
		DEFAULT => -70
	},
	{#State 357
		DEFAULT => -46
	},
	{#State 358
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 441,
			'AdditiveExpression' => 122
		}
	},
	{#State 359
		DEFAULT => -62
	},
	{#State 360
		DEFAULT => -266
	},
	{#State 361
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -118
	},
	{#State 362
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -121
	},
	{#State 363
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -122
	},
	{#State 364
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -120
	},
	{#State 365
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -119
	},
	{#State 366
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -123
	},
	{#State 367
		DEFAULT => -98
	},
	{#State 368
		DEFAULT => -96
	},
	{#State 369
		DEFAULT => -97
	},
	{#State 370
		ACTIONS => {
			"|" => 261
		},
		DEFAULT => -171
	},
	{#State 371
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"," => 252,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 442,
			'AdditiveExpression' => 122
		}
	},
	{#State 372
		DEFAULT => -29
	},
	{#State 373
		ACTIONS => {
			":" => 443
		}
	},
	{#State 374
		ACTIONS => {
			"&&" => 249
		},
		DEFAULT => -177
	},
	{#State 375
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -111
	},
	{#State 376
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -112
	},
	{#State 377
		ACTIONS => {
			"-" => 269,
			"+" => 270
		},
		DEFAULT => -110
	},
	{#State 378
		ACTIONS => {
			"," => 252,
			"]" => 444
		}
	},
	{#State 379
		DEFAULT => -25
	},
	{#State 380
		ACTIONS => {
			"^" => 268
		},
		DEFAULT => -165
	},
	{#State 381
		ACTIONS => {
			"!=" => 266,
			"!==" => 265,
			"===" => 267,
			"==" => 264
		},
		DEFAULT => -153
	},
	{#State 382
		DEFAULT => -189
	},
	{#State 383
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"in" => 245,
			"instanceof" => 242,
			"<=" => 243,
			">" => 244
		},
		DEFAULT => -138
	},
	{#State 384
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"in" => 245,
			"instanceof" => 242,
			"<=" => 243,
			">" => 244
		},
		DEFAULT => -141
	},
	{#State 385
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"in" => 245,
			"instanceof" => 242,
			"<=" => 243,
			">" => 244
		},
		DEFAULT => -139
	},
	{#State 386
		ACTIONS => {
			"<" => 240,
			">=" => 241,
			"in" => 245,
			"instanceof" => 242,
			"<=" => 243,
			">" => 244
		},
		DEFAULT => -140
	},
	{#State 387
		ACTIONS => {
			"&" => 262
		},
		DEFAULT => -159
	},
	{#State 388
		ACTIONS => {
			"%" => 246,
			"*" => 247,
			"/" => 248
		},
		DEFAULT => -105
	},
	{#State 389
		ACTIONS => {
			"%" => 246,
			"*" => 247,
			"/" => 248
		},
		DEFAULT => -104
	},
	{#State 390
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 445,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 391
		ACTIONS => {
			"{" => 447
		},
		GOTOS => {
			'CaseBlock' => 446
		}
	},
	{#State 392
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 448,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 393
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 449,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 394
		DEFAULT => -288
	},
	{#State 395
		DEFAULT => -286
	},
	{#State 396
		ACTIONS => {
			'Identifier' => 450
		}
	},
	{#State 397
		DEFAULT => -59
	},
	{#State 398
		DEFAULT => -264
	},
	{#State 399
		ACTIONS => {
			"{" => 451
		}
	},
	{#State 400
		ACTIONS => {
			"," => 439,
			")" => 452
		}
	},
	{#State 401
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'AssignmentExpressionNoIn' => 454,
			'LeftHandSideExpression' => 453,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 402
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 455,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 403
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'BitwiseXorExpressionNoIn' => 311,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'BitwiseAndExpressionNoIn' => 319,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 456,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 404
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 457,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 405
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 458,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 406
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 459,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 407
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 460,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 408
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'BitwiseAndExpressionNoIn' => 461,
			'RelationalExpressionNoIn' => 317,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 409
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'BitwiseXorExpressionNoIn' => 462,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'BitwiseAndExpressionNoIn' => 319,
			'RelationalExpressionNoIn' => 317,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 410
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'AssignmentExpressionNoIn' => 463,
			'LeftHandSideExpression' => 453,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 411
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			";" => 464,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 465,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 412
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'AssignmentExpressionNoIn' => 466,
			'LeftHandSideExpression' => 453,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 413
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 467,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 414
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 468,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 469,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 415
		ACTIONS => {
			";" => 470,
			"," => 320
		}
	},
	{#State 416
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 471,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 417
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 472,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 418
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 473,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 419
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 474,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 420
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 475,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74
		}
	},
	{#State 421
		ACTIONS => {
			"=" => 477
		},
		DEFAULT => -237,
		GOTOS => {
			'InitialiserNoIn' => 476
		}
	},
	{#State 422
		DEFAULT => -233
	},
	{#State 423
		ACTIONS => {
			";" => 479,
			"," => 478,
			"in" => 480
		}
	},
	{#State 424
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 94,
			'FunctionExpression' => 75,
			'NewExpression' => 82,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'EqualityExpressionNoIn' => 481
		}
	},
	{#State 425
		DEFAULT => -207
	},
	{#State 426
		DEFAULT => -262
	},
	{#State 427
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 482,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 428
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 483,
			'AdditiveExpression' => 122
		}
	},
	{#State 429
		DEFAULT => -239
	},
	{#State 430
		DEFAULT => -232
	},
	{#State 431
		DEFAULT => -1,
		GOTOS => {
			'_ISON_' => 484
		}
	},
	{#State 432
		DEFAULT => -55
	},
	{#State 433
		ACTIONS => {
			":" => 485
		}
	},
	{#State 434
		DEFAULT => -35
	},
	{#State 435
		DEFAULT => -42
	},
	{#State 436
		ACTIONS => {
			"{" => 486
		}
	},
	{#State 437
		ACTIONS => {
			"," => 439,
			")" => 487
		}
	},
	{#State 438
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 489,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 439
		ACTIONS => {
			'Identifier' => 490
		}
	},
	{#State 440
		ACTIONS => {
			"{" => 491
		}
	},
	{#State 441
		DEFAULT => -64
	},
	{#State 442
		DEFAULT => -30
	},
	{#State 443
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 492,
			'AdditiveExpression' => 122
		}
	},
	{#State 444
		DEFAULT => -26
	},
	{#State 445
		DEFAULT => -267
	},
	{#State 446
		DEFAULT => -268
	},
	{#State 447
		ACTIONS => {
			"}" => 493,
			"case" => 494,
			"default" => 495
		},
		GOTOS => {
			'CaseClauses' => 496,
			'DefaultClause' => 497,
			'CaseClause' => 498
		}
	},
	{#State 448
		ACTIONS => {
			"else" => 499
		},
		DEFAULT => -244
	},
	{#State 449
		DEFAULT => -246
	},
	{#State 450
		ACTIONS => {
			")" => 500
		}
	},
	{#State 451
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 501,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 452
		ACTIONS => {
			"{" => 502
		}
	},
	{#State 453
		ACTIONS => {
			"*=" => 164,
			"|=" => 165,
			"&=" => 172,
			"--" => -1,
			"-=" => 173,
			"/=" => 174,
			"<<=" => 167,
			"%=" => 175,
			"^=" => 168,
			">>=" => 169,
			"++" => -1,
			"=" => 177,
			"+=" => 176,
			">>>=" => 171
		},
		DEFAULT => -69,
		GOTOS => {
			'_ISON_' => 231,
			'AssignmentOperator' => 412
		}
	},
	{#State 454
		ACTIONS => {
			":" => 503
		}
	},
	{#State 455
		ACTIONS => {
			"&&" => 403
		},
		DEFAULT => -181
	},
	{#State 456
		ACTIONS => {
			"|" => 409
		},
		DEFAULT => -175
	},
	{#State 457
		ACTIONS => {
			"<" => 416,
			">=" => 417,
			"instanceof" => 418,
			"<=" => 419,
			">" => 420
		},
		DEFAULT => -148
	},
	{#State 458
		ACTIONS => {
			"<" => 416,
			">=" => 417,
			"instanceof" => 418,
			"<=" => 419,
			">" => 420
		},
		DEFAULT => -151
	},
	{#State 459
		ACTIONS => {
			"<" => 416,
			">=" => 417,
			"instanceof" => 418,
			"<=" => 419,
			">" => 420
		},
		DEFAULT => -149
	},
	{#State 460
		ACTIONS => {
			"<" => 416,
			">=" => 417,
			"instanceof" => 418,
			"<=" => 419,
			">" => 420
		},
		DEFAULT => -150
	},
	{#State 461
		ACTIONS => {
			"&" => 424
		},
		DEFAULT => -163
	},
	{#State 462
		ACTIONS => {
			"^" => 408
		},
		DEFAULT => -169
	},
	{#State 463
		DEFAULT => -211
	},
	{#State 464
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 504,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 505,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 465
		ACTIONS => {
			";" => 506,
			"," => 320
		}
	},
	{#State 466
		DEFAULT => -193
	},
	{#State 467
		ACTIONS => {
			"," => 320,
			")" => 507
		}
	},
	{#State 468
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 508,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 469
		ACTIONS => {
			"," => 320,
			")" => 509
		}
	},
	{#State 470
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 510,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 511,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 471
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -132
	},
	{#State 472
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -135
	},
	{#State 473
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -136
	},
	{#State 474
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -134
	},
	{#State 475
		ACTIONS => {
			">>" => 256,
			">>>" => 257,
			"<<" => 258
		},
		DEFAULT => -133
	},
	{#State 476
		DEFAULT => -238
	},
	{#State 477
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'AssignmentExpressionNoIn' => 512,
			'LeftHandSideExpression' => 453,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 478
		ACTIONS => {
			'Identifier' => 421
		},
		GOTOS => {
			'VariableDeclarationNoIn' => 513
		}
	},
	{#State 479
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			";" => 514,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 515,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 480
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 516,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 481
		ACTIONS => {
			"!=" => 406,
			"!==" => 405,
			"===" => 407,
			"==" => 404
		},
		DEFAULT => -157
	},
	{#State 482
		ACTIONS => {
			"," => 320,
			")" => 517
		}
	},
	{#State 483
		DEFAULT => -185
	},
	{#State 484
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 518,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 485
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AssignmentExpression' => 519,
			'AdditiveExpression' => 122
		}
	},
	{#State 486
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 520,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 487
		ACTIONS => {
			"{" => 521
		}
	},
	{#State 488
		ACTIONS => {
			"}" => -297,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'SourceElement' => 159,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 489
		ACTIONS => {
			"}" => 522
		}
	},
	{#State 490
		DEFAULT => -296
	},
	{#State 491
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 523,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 492
		DEFAULT => -183
	},
	{#State 493
		DEFAULT => -269
	},
	{#State 494
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 524,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 495
		ACTIONS => {
			":" => 525
		}
	},
	{#State 496
		ACTIONS => {
			"}" => 526,
			"case" => 494,
			"default" => 495
		},
		GOTOS => {
			'DefaultClause' => 527,
			'CaseClause' => 528
		}
	},
	{#State 497
		ACTIONS => {
			"}" => 529,
			"case" => 494
		},
		GOTOS => {
			'CaseClauses' => 530,
			'CaseClause' => 498
		}
	},
	{#State 498
		DEFAULT => -275
	},
	{#State 499
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 531,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 500
		ACTIONS => {
			"{" => 34
		},
		GOTOS => {
			'Block' => 532
		}
	},
	{#State 501
		ACTIONS => {
			"}" => 533
		}
	},
	{#State 502
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 534,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 503
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'AssignmentExpressionNoIn' => 535,
			'LeftHandSideExpression' => 453,
			'NewExpression' => 82,
			'BitwiseXorExpressionNoIn' => 311,
			'MultiplicativeExpression' => 105,
			'RelationalExpressionNoIn' => 317,
			'BitwiseOrExpressionNoIn' => 312,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'CallExpression' => 74,
			'LogicalOrExpressionNoIn' => 306,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'BitwiseAndExpressionNoIn' => 319,
			'LogicalAndExpressionNoIn' => 307,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 310,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122,
			'ConditionalExpressionNoIn' => 308,
			'EqualityExpressionNoIn' => 309
		}
	},
	{#State 504
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 536,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 505
		ACTIONS => {
			"," => 320,
			")" => 537
		}
	},
	{#State 506
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 538,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 539,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 507
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 540,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 508
		DEFAULT => -247
	},
	{#State 509
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 541,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 510
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 542,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 511
		ACTIONS => {
			"," => 320,
			")" => 543
		}
	},
	{#State 512
		DEFAULT => -240
	},
	{#State 513
		DEFAULT => -234
	},
	{#State 514
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 544,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 545,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 515
		ACTIONS => {
			";" => 546,
			"," => 320
		}
	},
	{#State 516
		ACTIONS => {
			"," => 320,
			")" => 547
		}
	},
	{#State 517
		ACTIONS => {
			";" => 548
		}
	},
	{#State 518
		ACTIONS => {
			";" => 549,
			"," => 320
		}
	},
	{#State 519
		DEFAULT => -36
	},
	{#State 520
		ACTIONS => {
			"}" => 550
		}
	},
	{#State 521
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"function" => 48,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'FunctionDeclaration' => 25,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'FunctionBody' => 551,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 64,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'SourceElement' => 33,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'SourceElements' => 488
		}
	},
	{#State 522
		DEFAULT => -291
	},
	{#State 523
		ACTIONS => {
			"}" => 552
		}
	},
	{#State 524
		ACTIONS => {
			":" => 553,
			"," => 320
		}
	},
	{#State 525
		ACTIONS => {
			"}" => -279,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			"case" => -279,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 156,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'StatementList' => 554
		}
	},
	{#State 526
		DEFAULT => -270
	},
	{#State 527
		ACTIONS => {
			"}" => 555,
			"case" => 494
		},
		GOTOS => {
			'CaseClauses' => 556,
			'CaseClause' => 498
		}
	},
	{#State 528
		DEFAULT => -276
	},
	{#State 529
		DEFAULT => -271
	},
	{#State 530
		ACTIONS => {
			"}" => 557,
			"case" => 494
		},
		GOTOS => {
			'CaseClause' => 528
		}
	},
	{#State 531
		DEFAULT => -243
	},
	{#State 532
		DEFAULT => -287
	},
	{#State 533
		DEFAULT => -289
	},
	{#State 534
		ACTIONS => {
			"}" => 558
		}
	},
	{#State 535
		DEFAULT => -187
	},
	{#State 536
		DEFAULT => -251
	},
	{#State 537
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 559,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 538
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 560,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 539
		ACTIONS => {
			"," => 320,
			")" => 561
		}
	},
	{#State 540
		DEFAULT => -259
	},
	{#State 541
		DEFAULT => -248
	},
	{#State 542
		DEFAULT => -249
	},
	{#State 543
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 562,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 544
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 563,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 545
		ACTIONS => {
			"," => 320,
			")" => 564
		}
	},
	{#State 546
		ACTIONS => {
			"typeof" => 87,
			"-" => 72,
			"new" => 81,
			"~" => 88,
			"+" => 83,
			"this" => 95,
			"++" => 96,
			"!" => 73,
			"function" => 91,
			"[" => 8,
			")" => 565,
			"--" => 92,
			"{" => 84,
			"(" => 93,
			"void" => 78,
			'Identifier' => 79,
			"delete" => 86
		},
		DEFAULT => -2,
		GOTOS => {
			'LeftHandSideExpression' => 119,
			'NewExpression' => 82,
			'RelationalExpression' => 104,
			'BitwiseOrExpression' => 116,
			'MultiplicativeExpression' => 105,
			'EqualityExpression' => 120,
			'LogicalAndExpression' => 106,
			'LogicalOrExpression' => 113,
			'ObjectLiteral' => 90,
			'MemberExpression' => 89,
			'BitwiseAndExpression' => 117,
			'CallExpression' => 74,
			'Expression' => 566,
			'BitwiseXorExpression' => 121,
			'FunctionExpression' => 75,
			'PostfixExpression' => 97,
			'ArrayLiteral' => 85,
			'_IERE_' => 76,
			'PrimaryExpression' => 77,
			'ShiftExpression' => 114,
			'AssignmentExpression' => 185,
			'ConditionalExpression' => 109,
			'UnaryExpression' => 110,
			'AdditiveExpression' => 122
		}
	},
	{#State 547
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 567,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 548
		DEFAULT => -245
	},
	{#State 549
		DEFAULT => -283
	},
	{#State 550
		DEFAULT => -293
	},
	{#State 551
		ACTIONS => {
			"}" => 568
		}
	},
	{#State 552
		DEFAULT => -292
	},
	{#State 553
		ACTIONS => {
			"}" => -277,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			"case" => -277,
			'Identifier' => 18,
			"default" => -277,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 156,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39,
			'StatementList' => 569
		}
	},
	{#State 554
		ACTIONS => {
			"}" => -280,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			"case" => -280,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 291,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 555
		DEFAULT => -273
	},
	{#State 556
		ACTIONS => {
			"}" => 570,
			"case" => 494
		},
		GOTOS => {
			'CaseClause' => 528
		}
	},
	{#State 557
		DEFAULT => -272
	},
	{#State 558
		DEFAULT => -290
	},
	{#State 559
		DEFAULT => -252
	},
	{#State 560
		DEFAULT => -253
	},
	{#State 561
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 571,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 562
		DEFAULT => -250
	},
	{#State 563
		DEFAULT => -255
	},
	{#State 564
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 572,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 565
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 573,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 566
		ACTIONS => {
			"," => 320,
			")" => 574
		}
	},
	{#State 567
		DEFAULT => -260
	},
	{#State 568
		DEFAULT => -294
	},
	{#State 569
		ACTIONS => {
			"}" => -278,
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			"case" => -278,
			'Identifier' => 18,
			"default" => -278,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 291,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 570
		DEFAULT => -274
	},
	{#State 571
		DEFAULT => -254
	},
	{#State 572
		DEFAULT => -256
	},
	{#State 573
		DEFAULT => -257
	},
	{#State 574
		ACTIONS => {
			"typeof" => 42,
			"-" => 2,
			"throw" => 1,
			"~" => 44,
			"return" => 5,
			"break" => 47,
			"!" => 7,
			"[" => 8,
			"--" => 49,
			"with" => 11,
			"for" => 51,
			"(" => 52,
			"void" => 17,
			'Identifier' => 18,
			"switch" => 19,
			"new" => 22,
			"+" => 24,
			";" => 56,
			"continue" => 57,
			"this" => 58,
			"do" => 59,
			"if" => 26,
			"++" => 61,
			"var" => 67,
			"{" => 34,
			"while" => 36,
			"try" => 38,
			"delete" => 41
		},
		DEFAULT => -2,
		GOTOS => {
			'BitwiseXorExpressionNoFb' => 43,
			'IfStatement' => 3,
			'LeftHandSideExpressionNoFb' => 45,
			'MemberExpressionNoFb' => 4,
			'CallExpressionNoFb' => 46,
			'LabelledStatement' => 6,
			'ReturnStatement' => 9,
			'TryStatement' => 10,
			'ExpressionStatement' => 50,
			'MultiplicativeExpressionNoFb' => 12,
			'_IERE_' => 14,
			'AdditiveExpressionNoFb' => 13,
			'VariableStatement' => 53,
			'ConditionalExpressionNoFb' => 16,
			'AssignmentExpressionNoFb' => 20,
			'PrimaryExpressionNoFb' => 21,
			'NewExpressionNoFb' => 23,
			'Block' => 55,
			'ShiftExpressionNoFb' => 54,
			'RelationalExpressionNoFb' => 27,
			'ThrowStatement' => 60,
			'IterationStatement' => 28,
			'EmptyStatement' => 62,
			'UnaryExpressionNoFb' => 63,
			'WithStatement' => 29,
			'ExpressionNoFb' => 30,
			'BitwiseOrExpressionNoFb' => 31,
			'LogicalOrExpressionNoFb' => 65,
			'Statement' => 575,
			'EqualityExpressionNoFb' => 66,
			'BreakStatement' => 32,
			'PostfixExpressionNoFb' => 35,
			'ArrayLiteral' => 37,
			'ContinueStatement' => 68,
			'LogicalAndExpressionNoFb' => 69,
			'BitwiseAndExpressionNoFb' => 70,
			'SwitchStatement' => 39
		}
	},
	{#State 575
		DEFAULT => -258
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 '_ISON_', 0,
sub
#line 60 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_ison}->($_[0]);}
	],
	[#Rule 2
		 '_IERE_', 0,
sub
#line 63 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_iere}->();}
	],
	[#Rule 3
		 '_IED_', 0,
sub
#line 66 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_ied}->();}
	],
	[#Rule 4
		 'Literal', 1,
sub
#line 71 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Literal_1(@_);}
	],
	[#Rule 5
		 'Literal', 1,
sub
#line 72 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Literal_2(@_);}
	],
	[#Rule 6
		 'Literal', 1,
sub
#line 73 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Literal_3(@_);}
	],
	[#Rule 7
		 'Literal', 1,
sub
#line 74 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Literal_4(@_);}
	],
	[#Rule 8
		 'Literal', 1,
sub
#line 75 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Literal_5(@_);}
	],
	[#Rule 9
		 'NumericLiteral', 1,
sub
#line 79 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->NumericLiteral_1(@_);}
	],
	[#Rule 10
		 'NumericLiteral', 1,
sub
#line 80 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->NumericLiteral_2(@_);}
	],
	[#Rule 11
		 'PrimaryExpression', 1,
sub
#line 85 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_1(@_);}
	],
	[#Rule 12
		 'PrimaryExpression', 1,
sub
#line 86 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_2(@_);}
	],
	[#Rule 13
		 'PrimaryExpression', 3,
sub
#line 87 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_3(@_);}
	],
	[#Rule 14
		 'PrimaryExpression', 1,
sub
#line 88 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_4(@_);}
	],
	[#Rule 15
		 'PrimaryExpression', 1,
sub
#line 89 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_5(@_);}
	],
	[#Rule 16
		 'PrimaryExpression', 3,
sub
#line 90 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpression_6(@_);}
	],
	[#Rule 17
		 'PrimaryExpressionNoFb', 1,
sub
#line 94 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpressionNoFb_1(@_);}
	],
	[#Rule 18
		 'PrimaryExpressionNoFb', 1,
sub
#line 95 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpressionNoFb_2(@_);}
	],
	[#Rule 19
		 'PrimaryExpressionNoFb', 3,
sub
#line 96 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpressionNoFb_3(@_);}
	],
	[#Rule 20
		 'PrimaryExpressionNoFb', 1,
sub
#line 97 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpressionNoFb_4(@_);}
	],
	[#Rule 21
		 'PrimaryExpressionNoFb', 3,
sub
#line 99 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PrimaryExpressionNoFb_6(@_);}
	],
	[#Rule 22
		 'ArrayLiteral', 2,
sub
#line 103 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArrayLiteral_1(@_);}
	],
	[#Rule 23
		 'ArrayLiteral', 3,
sub
#line 104 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArrayLiteral_2(@_);}
	],
	[#Rule 24
		 'ArrayLiteral', 3,
sub
#line 105 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArrayLiteral_3(@_);}
	],
	[#Rule 25
		 'ArrayLiteral', 4,
sub
#line 106 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArrayLiteral_4(@_);}
	],
	[#Rule 26
		 'ArrayLiteral', 5,
sub
#line 107 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArrayLiteral_5(@_);}
	],
	[#Rule 27
		 'ElementList', 1,
sub
#line 111 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ElementList_1(@_);}
	],
	[#Rule 28
		 'ElementList', 2,
sub
#line 112 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ElementList_2(@_);}
	],
	[#Rule 29
		 'ElementList', 3,
sub
#line 113 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ElementList_3(@_);}
	],
	[#Rule 30
		 'ElementList', 4,
sub
#line 114 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ElementList_4(@_);}
	],
	[#Rule 31
		 'Elision', 1,
sub
#line 118 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Elision_1(@_);}
	],
	[#Rule 32
		 'Elision', 2,
sub
#line 119 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Elision_2(@_);}
	],
	[#Rule 33
		 'ObjectLiteral', 2,
sub
#line 123 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ObjectLiteral_1(@_);}
	],
	[#Rule 34
		 'ObjectLiteral', 3,
sub
#line 124 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ObjectLiteral_2(@_);}
	],
	[#Rule 35
		 'PropertyNameAndValueList', 3,
sub
#line 128 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PropertyNameAndValueList_1(@_);}
	],
	[#Rule 36
		 'PropertyNameAndValueList', 5,
sub
#line 129 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PropertyNameAndValueList_2(@_);}
	],
	[#Rule 37
		 'PropertyName', 1,
sub
#line 133 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PropertyName_1(@_);}
	],
	[#Rule 38
		 'PropertyName', 1,
sub
#line 134 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PropertyName_2(@_);}
	],
	[#Rule 39
		 'PropertyName', 1,
sub
#line 135 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PropertyName_3(@_);}
	],
	[#Rule 40
		 'MemberExpression', 1,
sub
#line 139 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpression_1(@_);}
	],
	[#Rule 41
		 'MemberExpression', 1,
sub
#line 140 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpression_2(@_);}
	],
	[#Rule 42
		 'MemberExpression', 4,
sub
#line 141 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpression_3(@_);}
	],
	[#Rule 43
		 'MemberExpression', 3,
sub
#line 142 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpression_4(@_);}
	],
	[#Rule 44
		 'MemberExpression', 3,
sub
#line 143 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpression_5(@_);}
	],
	[#Rule 45
		 'MemberExpressionNoFb', 1,
sub
#line 147 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpressionNoFb_1(@_);}
	],
	[#Rule 46
		 'MemberExpressionNoFb', 4,
sub
#line 149 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpressionNoFb_3(@_);}
	],
	[#Rule 47
		 'MemberExpressionNoFb', 3,
sub
#line 150 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpressionNoFb_4(@_);}
	],
	[#Rule 48
		 'MemberExpressionNoFb', 3,
sub
#line 151 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MemberExpressionNoFb_5(@_);}
	],
	[#Rule 49
		 'NewExpression', 1,
sub
#line 155 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->NewExpression_1(@_);}
	],
	[#Rule 50
		 'NewExpression', 2,
sub
#line 156 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->NewExpression_2(@_);}
	],
	[#Rule 51
		 'NewExpressionNoFb', 1,
sub
#line 160 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->newExpressionNoFb_1(@_);}
	],
	[#Rule 52
		 'NewExpressionNoFb', 2,
sub
#line 161 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->newExpressionNoFb_2(@_);}
	],
	[#Rule 53
		 'CallExpression', 2,
sub
#line 165 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpression_1(@_);}
	],
	[#Rule 54
		 'CallExpression', 2,
sub
#line 166 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpression_2(@_);}
	],
	[#Rule 55
		 'CallExpression', 4,
sub
#line 167 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpression_3(@_);}
	],
	[#Rule 56
		 'CallExpression', 3,
sub
#line 168 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpression_4(@_);}
	],
	[#Rule 57
		 'CallExpressionNoFb', 2,
sub
#line 172 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpressionNoFb_1(@_);}
	],
	[#Rule 58
		 'CallExpressionNoFb', 2,
sub
#line 173 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpressionNoFb_2(@_);}
	],
	[#Rule 59
		 'CallExpressionNoFb', 4,
sub
#line 174 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpressionNoFb_3(@_);}
	],
	[#Rule 60
		 'CallExpressionNoFb', 3,
sub
#line 175 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CallExpressionNoFb_4(@_);}
	],
	[#Rule 61
		 'Arguments', 2,
sub
#line 179 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Arguments_1(@_);}
	],
	[#Rule 62
		 'Arguments', 3,
sub
#line 180 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Arguments_2(@_);}
	],
	[#Rule 63
		 'ArgumentList', 1,
sub
#line 184 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArgumentList_1(@_);}
	],
	[#Rule 64
		 'ArgumentList', 3,
sub
#line 185 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ArgumentList_2(@_);}
	],
	[#Rule 65
		 'LeftHandSideExpression', 1,
sub
#line 189 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LeftHandSideExpression_1(@_);}
	],
	[#Rule 66
		 'LeftHandSideExpression', 1,
sub
#line 190 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LeftHandSideExpression_2(@_);}
	],
	[#Rule 67
		 'LeftHandSideExpressionNoFb', 1,
sub
#line 194 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LeftHandSideExpressionNoFb_1(@_);}
	],
	[#Rule 68
		 'LeftHandSideExpressionNoFb', 1,
sub
#line 195 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LeftHandSideExpressionNoFb_2(@_);}
	],
	[#Rule 69
		 'PostfixExpression', 1,
sub
#line 199 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpression_1(@_);}
	],
	[#Rule 70
		 'PostfixExpression', 3,
sub
#line 200 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpression_2(@_);}
	],
	[#Rule 71
		 'PostfixExpression', 3,
sub
#line 201 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpression_3(@_);}
	],
	[#Rule 72
		 'PostfixExpressionNoFb', 1,
sub
#line 205 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpressionNoFb_1(@_);}
	],
	[#Rule 73
		 'PostfixExpressionNoFb', 3,
sub
#line 206 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpressionNoFb_2(@_);}
	],
	[#Rule 74
		 'PostfixExpressionNoFb', 3,
sub
#line 207 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->PostfixExpressionNoFb_3(@_);}
	],
	[#Rule 75
		 'UnaryExpression', 1,
sub
#line 211 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_1(@_);}
	],
	[#Rule 76
		 'UnaryExpression', 2,
sub
#line 212 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_2(@_);}
	],
	[#Rule 77
		 'UnaryExpression', 2,
sub
#line 213 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_3(@_);}
	],
	[#Rule 78
		 'UnaryExpression', 2,
sub
#line 214 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_4(@_);}
	],
	[#Rule 79
		 'UnaryExpression', 2,
sub
#line 215 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_5(@_);}
	],
	[#Rule 80
		 'UnaryExpression', 2,
sub
#line 216 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_6(@_);}
	],
	[#Rule 81
		 'UnaryExpression', 2,
sub
#line 217 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_7(@_);}
	],
	[#Rule 82
		 'UnaryExpression', 2,
sub
#line 218 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_8(@_);}
	],
	[#Rule 83
		 'UnaryExpression', 2,
sub
#line 219 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_9(@_);}
	],
	[#Rule 84
		 'UnaryExpression', 2,
sub
#line 220 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpression_10(@_);}
	],
	[#Rule 85
		 'UnaryExpressionNoFb', 1,
sub
#line 224 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_1(@_);}
	],
	[#Rule 86
		 'UnaryExpressionNoFb', 2,
sub
#line 225 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_2(@_);}
	],
	[#Rule 87
		 'UnaryExpressionNoFb', 2,
sub
#line 226 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_3(@_);}
	],
	[#Rule 88
		 'UnaryExpressionNoFb', 2,
sub
#line 227 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_4(@_);}
	],
	[#Rule 89
		 'UnaryExpressionNoFb', 2,
sub
#line 228 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_5(@_);}
	],
	[#Rule 90
		 'UnaryExpressionNoFb', 2,
sub
#line 229 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_6(@_);}
	],
	[#Rule 91
		 'UnaryExpressionNoFb', 2,
sub
#line 230 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_7(@_);}
	],
	[#Rule 92
		 'UnaryExpressionNoFb', 2,
sub
#line 231 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_8(@_);}
	],
	[#Rule 93
		 'UnaryExpressionNoFb', 2,
sub
#line 232 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_9(@_);}
	],
	[#Rule 94
		 'UnaryExpressionNoFb', 2,
sub
#line 233 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->UnaryExpressionNoFb_10(@_);}
	],
	[#Rule 95
		 'MultiplicativeExpression', 1,
sub
#line 237 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpression_1(@_);}
	],
	[#Rule 96
		 'MultiplicativeExpression', 3,
sub
#line 238 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpression_2(@_);}
	],
	[#Rule 97
		 'MultiplicativeExpression', 3,
sub
#line 239 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpression_3(@_);}
	],
	[#Rule 98
		 'MultiplicativeExpression', 3,
sub
#line 240 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpression_4(@_);}
	],
	[#Rule 99
		 'MultiplicativeExpressionNoFb', 1,
sub
#line 244 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpressionNoFb_1(@_);}
	],
	[#Rule 100
		 'MultiplicativeExpressionNoFb', 3,
sub
#line 245 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpressionNoFb_2(@_);}
	],
	[#Rule 101
		 'MultiplicativeExpressionNoFb', 3,
sub
#line 246 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpressionNoFb_3(@_);}
	],
	[#Rule 102
		 'MultiplicativeExpressionNoFb', 3,
sub
#line 247 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->MultiplicativeExpressionNoFb_4(@_);}
	],
	[#Rule 103
		 'AdditiveExpression', 1,
sub
#line 251 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpression_1(@_);}
	],
	[#Rule 104
		 'AdditiveExpression', 3,
sub
#line 252 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpression_2(@_);}
	],
	[#Rule 105
		 'AdditiveExpression', 3,
sub
#line 253 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpression_3(@_);}
	],
	[#Rule 106
		 'AdditiveExpressionNoFb', 1,
sub
#line 257 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpressionNoFb_1(@_);}
	],
	[#Rule 107
		 'AdditiveExpressionNoFb', 3,
sub
#line 258 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpressionNoFb_2(@_);}
	],
	[#Rule 108
		 'AdditiveExpressionNoFb', 3,
sub
#line 259 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AdditiveExpressionNoFb_3(@_);}
	],
	[#Rule 109
		 'ShiftExpression', 1,
sub
#line 263 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpression_1(@_);}
	],
	[#Rule 110
		 'ShiftExpression', 3,
sub
#line 264 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpression_2(@_);}
	],
	[#Rule 111
		 'ShiftExpression', 3,
sub
#line 265 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpression_3(@_);}
	],
	[#Rule 112
		 'ShiftExpression', 3,
sub
#line 266 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpression_4(@_);}
	],
	[#Rule 113
		 'ShiftExpressionNoFb', 1,
sub
#line 270 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpresionNoFb_1(@_);}
	],
	[#Rule 114
		 'ShiftExpressionNoFb', 3,
sub
#line 271 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpresionNoFb_2(@_);}
	],
	[#Rule 115
		 'ShiftExpressionNoFb', 3,
sub
#line 272 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpresionNoFb_3(@_);}
	],
	[#Rule 116
		 'ShiftExpressionNoFb', 3,
sub
#line 273 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ShiftExpresionNoFb_4(@_);}
	],
	[#Rule 117
		 'RelationalExpression', 1,
sub
#line 277 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_1(@_);}
	],
	[#Rule 118
		 'RelationalExpression', 3,
sub
#line 278 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_2(@_);}
	],
	[#Rule 119
		 'RelationalExpression', 3,
sub
#line 279 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_3(@_);}
	],
	[#Rule 120
		 'RelationalExpression', 3,
sub
#line 280 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_4(@_);}
	],
	[#Rule 121
		 'RelationalExpression', 3,
sub
#line 281 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_5(@_);}
	],
	[#Rule 122
		 'RelationalExpression', 3,
sub
#line 282 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_6(@_);}
	],
	[#Rule 123
		 'RelationalExpression', 3,
sub
#line 283 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpression_7(@_);}
	],
	[#Rule 124
		 'RelationalExpressionNoFb', 1,
sub
#line 287 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_1(@_);}
	],
	[#Rule 125
		 'RelationalExpressionNoFb', 3,
sub
#line 288 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_2(@_);}
	],
	[#Rule 126
		 'RelationalExpressionNoFb', 3,
sub
#line 289 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_3(@_);}
	],
	[#Rule 127
		 'RelationalExpressionNoFb', 3,
sub
#line 290 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_4(@_);}
	],
	[#Rule 128
		 'RelationalExpressionNoFb', 3,
sub
#line 291 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_5(@_);}
	],
	[#Rule 129
		 'RelationalExpressionNoFb', 3,
sub
#line 292 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_6(@_);}
	],
	[#Rule 130
		 'RelationalExpressionNoFb', 3,
sub
#line 293 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoFb_7(@_);}
	],
	[#Rule 131
		 'RelationalExpressionNoIn', 1,
sub
#line 297 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_1(@_);}
	],
	[#Rule 132
		 'RelationalExpressionNoIn', 3,
sub
#line 298 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_2(@_);}
	],
	[#Rule 133
		 'RelationalExpressionNoIn', 3,
sub
#line 299 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_3(@_);}
	],
	[#Rule 134
		 'RelationalExpressionNoIn', 3,
sub
#line 300 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_4(@_);}
	],
	[#Rule 135
		 'RelationalExpressionNoIn', 3,
sub
#line 301 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_5(@_);}
	],
	[#Rule 136
		 'RelationalExpressionNoIn', 3,
sub
#line 302 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->RelationalExpressionNoIn_6(@_);}
	],
	[#Rule 137
		 'EqualityExpression', 1,
sub
#line 307 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpression_1(@_);}
	],
	[#Rule 138
		 'EqualityExpression', 3,
sub
#line 308 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpression_2(@_);}
	],
	[#Rule 139
		 'EqualityExpression', 3,
sub
#line 309 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpression_3(@_);}
	],
	[#Rule 140
		 'EqualityExpression', 3,
sub
#line 310 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpression_4(@_);}
	],
	[#Rule 141
		 'EqualityExpression', 3,
sub
#line 311 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpression_5(@_);}
	],
	[#Rule 142
		 'EqualityExpressionNoFb', 1,
sub
#line 315 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoFb_1(@_);}
	],
	[#Rule 143
		 'EqualityExpressionNoFb', 3,
sub
#line 316 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoFb_2(@_);}
	],
	[#Rule 144
		 'EqualityExpressionNoFb', 3,
sub
#line 317 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoFb_3(@_);}
	],
	[#Rule 145
		 'EqualityExpressionNoFb', 3,
sub
#line 318 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoFb_4(@_);}
	],
	[#Rule 146
		 'EqualityExpressionNoFb', 3,
sub
#line 319 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoFb_5(@_);}
	],
	[#Rule 147
		 'EqualityExpressionNoIn', 1,
sub
#line 323 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoIn_1(@_);}
	],
	[#Rule 148
		 'EqualityExpressionNoIn', 3,
sub
#line 324 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoIn_2(@_);}
	],
	[#Rule 149
		 'EqualityExpressionNoIn', 3,
sub
#line 325 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoIn_3(@_);}
	],
	[#Rule 150
		 'EqualityExpressionNoIn', 3,
sub
#line 326 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoIn_4(@_);}
	],
	[#Rule 151
		 'EqualityExpressionNoIn', 3,
sub
#line 327 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EqualityExpressionNoIn_5(@_);}
	],
	[#Rule 152
		 'BitwiseAndExpression', 1,
sub
#line 331 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpression_1(@_);}
	],
	[#Rule 153
		 'BitwiseAndExpression', 3,
sub
#line 332 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpression_2(@_);}
	],
	[#Rule 154
		 'BitwiseAndExpressionNoFb', 1,
sub
#line 336 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpressionNoFb_1(@_);}
	],
	[#Rule 155
		 'BitwiseAndExpressionNoFb', 3,
sub
#line 337 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpressionNoFb_2(@_);}
	],
	[#Rule 156
		 'BitwiseAndExpressionNoIn', 1,
sub
#line 341 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpressionNoIn_1(@_);}
	],
	[#Rule 157
		 'BitwiseAndExpressionNoIn', 3,
sub
#line 342 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseAndExpressionNoIn_2(@_);}
	],
	[#Rule 158
		 'BitwiseXorExpression', 1,
sub
#line 346 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpression_1(@_);}
	],
	[#Rule 159
		 'BitwiseXorExpression', 3,
sub
#line 347 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpression_2(@_);}
	],
	[#Rule 160
		 'BitwiseXorExpressionNoFb', 1,
sub
#line 351 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpressionNoFb_1(@_);}
	],
	[#Rule 161
		 'BitwiseXorExpressionNoFb', 3,
sub
#line 352 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpressionNoFb_2(@_);}
	],
	[#Rule 162
		 'BitwiseXorExpressionNoIn', 1,
sub
#line 356 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpressionNoIn_1(@_);}
	],
	[#Rule 163
		 'BitwiseXorExpressionNoIn', 3,
sub
#line 357 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseXorExpressionNoIn_2(@_);}
	],
	[#Rule 164
		 'BitwiseOrExpression', 1,
sub
#line 361 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpression_1(@_);}
	],
	[#Rule 165
		 'BitwiseOrExpression', 3,
sub
#line 362 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpression_2(@_);}
	],
	[#Rule 166
		 'BitwiseOrExpressionNoFb', 1,
sub
#line 366 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpressionNoFb_1(@_);}
	],
	[#Rule 167
		 'BitwiseOrExpressionNoFb', 3,
sub
#line 367 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpressionNoFb_2(@_);}
	],
	[#Rule 168
		 'BitwiseOrExpressionNoIn', 1,
sub
#line 371 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpressionNoIn_1(@_);}
	],
	[#Rule 169
		 'BitwiseOrExpressionNoIn', 3,
sub
#line 372 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BitwiseOrExpressionNoIn_2(@_);}
	],
	[#Rule 170
		 'LogicalAndExpression', 1,
sub
#line 376 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpression_1(@_);}
	],
	[#Rule 171
		 'LogicalAndExpression', 3,
sub
#line 377 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpression_2(@_);}
	],
	[#Rule 172
		 'LogicalAndExpressionNoFb', 1,
sub
#line 381 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpressionNoFb_1(@_);}
	],
	[#Rule 173
		 'LogicalAndExpressionNoFb', 3,
sub
#line 382 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpressionNoFb_2(@_);}
	],
	[#Rule 174
		 'LogicalAndExpressionNoIn', 1,
sub
#line 386 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpressionNoIn_1(@_);}
	],
	[#Rule 175
		 'LogicalAndExpressionNoIn', 3,
sub
#line 387 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalAndExpressionNoIn_2(@_);}
	],
	[#Rule 176
		 'LogicalOrExpression', 1,
sub
#line 391 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpression_1(@_);}
	],
	[#Rule 177
		 'LogicalOrExpression', 3,
sub
#line 392 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpression_2(@_);}
	],
	[#Rule 178
		 'LogicalOrExpressionNoFb', 1,
sub
#line 396 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpressionNoFb_1(@_);}
	],
	[#Rule 179
		 'LogicalOrExpressionNoFb', 3,
sub
#line 397 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpressionNoFb_2(@_);}
	],
	[#Rule 180
		 'LogicalOrExpressionNoIn', 1,
sub
#line 401 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpressionNoIn_1(@_);}
	],
	[#Rule 181
		 'LogicalOrExpressionNoIn', 3,
sub
#line 402 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LogicalOrExpressionNoIn_2(@_);}
	],
	[#Rule 182
		 'ConditionalExpression', 1,
sub
#line 406 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpression_1(@_);}
	],
	[#Rule 183
		 'ConditionalExpression', 5,
sub
#line 407 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpression_2(@_);}
	],
	[#Rule 184
		 'ConditionalExpressionNoFb', 1,
sub
#line 411 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpressionNoFb_1(@_);}
	],
	[#Rule 185
		 'ConditionalExpressionNoFb', 5,
sub
#line 412 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpressionNoFb_2(@_);}
	],
	[#Rule 186
		 'ConditionalExpressionNoIn', 1,
sub
#line 416 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpressionNoIn_1(@_);}
	],
	[#Rule 187
		 'ConditionalExpressionNoIn', 5,
sub
#line 417 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ConditionalExpressionNoIn_2(@_);}
	],
	[#Rule 188
		 'AssignmentExpression', 1,
sub
#line 421 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpression_1(@_);}
	],
	[#Rule 189
		 'AssignmentExpression', 3,
sub
#line 422 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpression_2(@_);}
	],
	[#Rule 190
		 'AssignmentExpressionNoFb', 1,
sub
#line 426 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpressionNoFb_1(@_);}
	],
	[#Rule 191
		 'AssignmentExpressionNoFb', 3,
sub
#line 427 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpressionNoFb_2(@_);}
	],
	[#Rule 192
		 'AssignmentExpressionNoIn', 1,
sub
#line 431 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpressionNoIn_1(@_);}
	],
	[#Rule 193
		 'AssignmentExpressionNoIn', 3,
sub
#line 432 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentExpressionNoIn_2(@_);}
	],
	[#Rule 194
		 'AssignmentOperator', 1,
sub
#line 436 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_1(@_);}
	],
	[#Rule 195
		 'AssignmentOperator', 1,
sub
#line 437 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_2(@_);}
	],
	[#Rule 196
		 'AssignmentOperator', 1,
sub
#line 438 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_3(@_);}
	],
	[#Rule 197
		 'AssignmentOperator', 1,
sub
#line 439 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_4(@_);}
	],
	[#Rule 198
		 'AssignmentOperator', 1,
sub
#line 440 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_5(@_);}
	],
	[#Rule 199
		 'AssignmentOperator', 1,
sub
#line 441 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_6(@_);}
	],
	[#Rule 200
		 'AssignmentOperator', 1,
sub
#line 442 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_7(@_);}
	],
	[#Rule 201
		 'AssignmentOperator', 1,
sub
#line 443 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_8(@_);}
	],
	[#Rule 202
		 'AssignmentOperator', 1,
sub
#line 444 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_9(@_);}
	],
	[#Rule 203
		 'AssignmentOperator', 1,
sub
#line 445 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_10(@_);}
	],
	[#Rule 204
		 'AssignmentOperator', 1,
sub
#line 446 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_11(@_);}
	],
	[#Rule 205
		 'AssignmentOperator', 1,
sub
#line 447 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->AssignmentOperator_12(@_);}
	],
	[#Rule 206
		 'Expression', 1,
sub
#line 451 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Expression_1(@_);}
	],
	[#Rule 207
		 'Expression', 3,
sub
#line 452 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Expression_2(@_);}
	],
	[#Rule 208
		 'ExpressionNoFb', 1,
sub
#line 456 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ExpressionNoFb_1(@_);}
	],
	[#Rule 209
		 'ExpressionNoFb', 3,
sub
#line 457 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ExpressionNoFb_2(@_);}
	],
	[#Rule 210
		 'ExpressionNoIn', 1,
sub
#line 461 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ExpressionNoIn_1(@_);}
	],
	[#Rule 211
		 'ExpressionNoIn', 3,
sub
#line 462 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ExpressionNoIn_2(@_);}
	],
	[#Rule 212
		 'Statement', 1,
sub
#line 467 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_1(@_);}
	],
	[#Rule 213
		 'Statement', 1,
sub
#line 468 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_2(@_);}
	],
	[#Rule 214
		 'Statement', 1,
sub
#line 469 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_3(@_);}
	],
	[#Rule 215
		 'Statement', 1,
sub
#line 470 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_4(@_);}
	],
	[#Rule 216
		 'Statement', 1,
sub
#line 471 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_5(@_);}
	],
	[#Rule 217
		 'Statement', 1,
sub
#line 472 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_6(@_);}
	],
	[#Rule 218
		 'Statement', 1,
sub
#line 473 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_7(@_);}
	],
	[#Rule 219
		 'Statement', 1,
sub
#line 474 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_8(@_);}
	],
	[#Rule 220
		 'Statement', 1,
sub
#line 475 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_9(@_);}
	],
	[#Rule 221
		 'Statement', 1,
sub
#line 476 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_10(@_);}
	],
	[#Rule 222
		 'Statement', 1,
sub
#line 477 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_11(@_);}
	],
	[#Rule 223
		 'Statement', 1,
sub
#line 478 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_12(@_);}
	],
	[#Rule 224
		 'Statement', 1,
sub
#line 479 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_13(@_);}
	],
	[#Rule 225
		 'Statement', 1,
sub
#line 480 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Statement_14(@_);}
	],
	[#Rule 226
		 'Block', 2,
sub
#line 484 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Block_1(@_);}
	],
	[#Rule 227
		 'Block', 3,
sub
#line 485 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Block_2(@_);}
	],
	[#Rule 228
		 'StatementList', 1,
sub
#line 489 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->StatementList_1(@_);}
	],
	[#Rule 229
		 'StatementList', 2,
sub
#line 490 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->StatementList_2(@_);}
	],
	[#Rule 230
		 'VariableStatement', 3,
sub
#line 494 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableStatement_1(@_);}
	],
	[#Rule 231
		 'VariableDeclarationList', 1,
sub
#line 498 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationList_1(@_);}
	],
	[#Rule 232
		 'VariableDeclarationList', 3,
sub
#line 499 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationList_2(@_);}
	],
	[#Rule 233
		 'VariableDeclarationListNoIn', 1,
sub
#line 503 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationListNoIn_1(@_);}
	],
	[#Rule 234
		 'VariableDeclarationListNoIn', 3,
sub
#line 504 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationListNoIn_2(@_);}
	],
	[#Rule 235
		 'VariableDeclaration', 1,
sub
#line 508 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclaration_1(@_);}
	],
	[#Rule 236
		 'VariableDeclaration', 2,
sub
#line 509 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclaration_2(@_);}
	],
	[#Rule 237
		 'VariableDeclarationNoIn', 1,
sub
#line 513 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationNoIn_1(@_);}
	],
	[#Rule 238
		 'VariableDeclarationNoIn', 2,
sub
#line 514 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->VariableDeclarationNoIn_2(@_);}
	],
	[#Rule 239
		 'Initialiser', 2,
sub
#line 518 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Initializer_1(@_);}
	],
	[#Rule 240
		 'InitialiserNoIn', 2,
sub
#line 522 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->InitializerNoIn_1(@_);}
	],
	[#Rule 241
		 'EmptyStatement', 1,
sub
#line 526 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->EmptyStatement_1(@_);}
	],
	[#Rule 242
		 'ExpressionStatement', 2,
sub
#line 530 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ExpressionStatement_1(@_);}
	],
	[#Rule 243
		 'IfStatement', 7,
sub
#line 534 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IfStatement_1(@_);}
	],
	[#Rule 244
		 'IfStatement', 5,
sub
#line 535 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IfStatement_2(@_);}
	],
	[#Rule 245
		 'IterationStatement', 7,
sub
#line 539 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_1(@_);}
	],
	[#Rule 246
		 'IterationStatement', 5,
sub
#line 540 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_2(@_);}
	],
	[#Rule 247
		 'IterationStatement', 6,
sub
#line 541 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_3(@_);}
	],
	[#Rule 248
		 'IterationStatement', 7,
sub
#line 542 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_4(@_);}
	],
	[#Rule 249
		 'IterationStatement', 7,
sub
#line 543 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_5(@_);}
	],
	[#Rule 250
		 'IterationStatement', 8,
sub
#line 544 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_6(@_);}
	],
	[#Rule 251
		 'IterationStatement', 7,
sub
#line 545 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_7(@_);}
	],
	[#Rule 252
		 'IterationStatement', 8,
sub
#line 546 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_8(@_);}
	],
	[#Rule 253
		 'IterationStatement', 8,
sub
#line 547 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_9(@_);}
	],
	[#Rule 254
		 'IterationStatement', 9,
sub
#line 548 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_10(@_);}
	],
	[#Rule 255
		 'IterationStatement', 8,
sub
#line 549 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_11(@_);}
	],
	[#Rule 256
		 'IterationStatement', 9,
sub
#line 550 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_12(@_);}
	],
	[#Rule 257
		 'IterationStatement', 9,
sub
#line 551 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_14(@_);}
	],
	[#Rule 258
		 'IterationStatement', 10,
sub
#line 552 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_15(@_);}
	],
	[#Rule 259
		 'IterationStatement', 7,
sub
#line 553 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_16(@_);}
	],
	[#Rule 260
		 'IterationStatement', 8,
sub
#line 554 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->IterationStatement_17(@_);}
	],
	[#Rule 261
		 'ContinueStatement', 3,
sub
#line 558 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ContinueStatement_1(@_);}
	],
	[#Rule 262
		 'ContinueStatement', 4,
sub
#line 559 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ContinueStatement_2(@_);}
	],
	[#Rule 263
		 'BreakStatement', 3,
sub
#line 563 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BreakStatement_1(@_);}
	],
	[#Rule 264
		 'BreakStatement', 4,
sub
#line 564 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->BreakStatement_2(@_);}
	],
	[#Rule 265
		 'ReturnStatement', 3,
sub
#line 568 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ReturnStatement_1(@_);}
	],
	[#Rule 266
		 'ReturnStatement', 4,
sub
#line 569 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ReturnStatement_2(@_);}
	],
	[#Rule 267
		 'WithStatement', 5,
sub
#line 573 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->WithStatement_1(@_);}
	],
	[#Rule 268
		 'SwitchStatement', 5,
sub
#line 577 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->SwitchStatement_1(@_);}
	],
	[#Rule 269
		 'CaseBlock', 2,
sub
#line 581 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_1(@_);}
	],
	[#Rule 270
		 'CaseBlock', 3,
sub
#line 582 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_2(@_);}
	],
	[#Rule 271
		 'CaseBlock', 3,
sub
#line 583 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_3(@_);}
	],
	[#Rule 272
		 'CaseBlock', 4,
sub
#line 584 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_4(@_);}
	],
	[#Rule 273
		 'CaseBlock', 4,
sub
#line 585 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_5(@_);}
	],
	[#Rule 274
		 'CaseBlock', 5,
sub
#line 586 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseBlock_6(@_);}
	],
	[#Rule 275
		 'CaseClauses', 1,
sub
#line 590 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseClauses_1(@_);}
	],
	[#Rule 276
		 'CaseClauses', 2,
sub
#line 591 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseClauses_2(@_);}
	],
	[#Rule 277
		 'CaseClause', 3,
sub
#line 595 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseClause_1(@_);}
	],
	[#Rule 278
		 'CaseClause', 4,
sub
#line 596 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->CaseClause_2(@_);}
	],
	[#Rule 279
		 'DefaultClause', 2,
sub
#line 600 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->DefaultClause_1(@_);}
	],
	[#Rule 280
		 'DefaultClause', 3,
sub
#line 601 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->DefaultClause_2(@_);}
	],
	[#Rule 281
		 'LabelledStatement', 3,
sub
#line 605 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->LabelledStatement_1(@_);}
	],
	[#Rule 282
		 '@1-3', 0,
sub
#line 609 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ThrowStatement_1(@_);}
	],
	[#Rule 283
		 'ThrowStatement', 8,
sub
#line 610 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->ThrowStatement_1(@_);}
	],
	[#Rule 284
		 'TryStatement', 3,
sub
#line 614 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->TryStatement_1(@_);}
	],
	[#Rule 285
		 'TryStatement', 3,
sub
#line 615 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->TryStatement_2(@_);}
	],
	[#Rule 286
		 'TryStatement', 4,
sub
#line 616 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->TryStatement_3(@_);}
	],
	[#Rule 287
		 'Catch', 5,
sub
#line 620 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Catch_1(@_);}
	],
	[#Rule 288
		 'Finally', 2,
sub
#line 624 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Finally_1(@_);}
	],
	[#Rule 289
		 'FunctionDeclaration', 7,
sub
#line 629 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionDeclaration_1(@_);}
	],
	[#Rule 290
		 'FunctionDeclaration', 8,
sub
#line 630 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionDeclaration_2(@_);}
	],
	[#Rule 291
		 'FunctionExpression', 6,
sub
#line 634 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionExpression_1(@_);}
	],
	[#Rule 292
		 'FunctionExpression', 7,
sub
#line 635 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionExpression_2(@_);}
	],
	[#Rule 293
		 'FunctionExpression', 7,
sub
#line 636 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionExpression_3(@_);}
	],
	[#Rule 294
		 'FunctionExpression', 8,
sub
#line 637 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionExpression_4(@_);}
	],
	[#Rule 295
		 'FormalParameterList', 1,
sub
#line 641 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FormalParameterList_1(@_);}
	],
	[#Rule 296
		 'FormalParameterList', 3,
sub
#line 642 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FormalParameterList_2(@_);}
	],
	[#Rule 297
		 'FunctionBody', 1,
sub
#line 646 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->FunctionBody_1(@_);}
	],
	[#Rule 298
		 'Program', 1,
sub
#line 650 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->Program_1(@_);}
	],
	[#Rule 299
		 'SourceElements', 1,
sub
#line 654 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->SourceElements_1(@_);}
	],
	[#Rule 300
		 'SourceElements', 2,
sub
#line 655 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->SourceElements_2(@_);}
	],
	[#Rule 301
		 'SourceElement', 1,
sub
#line 659 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->SourceElement_1(@_);}
	],
	[#Rule 302
		 'SourceElement', 1,
sub
#line 660 "lib/JavaScript/Grammar.yp"
{$_[0]->{js_package}->SourceElement_2(@_);}
	]
],
                                  @_);
    bless($self,$class);
}

#line 663 "lib/JavaScript/Grammar.yp"

#=============================================================================
# Footer
#-----------------------------------------------------------------------------
use warnings;
use strict;

use constant YUCKPACKAGE => "JavaScript::Grammar::Default";
{
        package JavaScript::Grammar::Default;

        sub AUTOLOAD
        {
                # First param is $self, second is the parser, so third
                #   is the first token.  Returning the first token is
                #   pretty lame, but it's what Parse::Yapp does, so hey.
                # Except that we don't want to return undef if we can
                #   avoid it.  (Should always be able to.)
                for (my $x = 2; $x < @_; $x++) {
                        return $_[$x] if defined $_[$x];
                }
                return undef;
        }
}

sub YYParse
{
        my $self = shift;
        my %args;

        {
                my %temp = @_;
                # lowercase keys.  It's not a big hash, so this shouldn't be too bad.
                $args{lc($_)} = $temp{$_} for (keys %temp);
        }

        for (qw(iere ied ison)) {
                $self->{"js_$_"} = $args{$_} || $args{"yy$_"} || sub {};
                delete @args{$_, "yy$_"};
        }

        for (qw(package)) {
                $self->{"js_$_"} = $args{$_} || $args{"yy$_"} || YUCKPACKAGE;
                delete @args{$_, "yy$_"};
        }

        $self->SUPER::YYParse(%args);
}

=head1 NAME

JavaScript::Grammar - Parse::Yapp module for parsing JavaScript

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 METHODS

=head1 STANDARDS COMPLIANCE

=head1 BUGS

See the CPAN Request Tracker for JavaScript::Parser, at
L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JavaScript-Parser>.

=head1 SEE ALSO

L<Parse::Yapp>.

L<JavaScript::Parser>.

I<ECMA-262>:  I<ECMAScript Language Specification>,
L<http://www.ecma.ch/ecma1/STAND/ECMA-262.HTM>

L<http://www.mozilla.org/js/language/>

=head1 AUTHOR

David "cogent" Hand, L<mailto:cogent@cpan.org>.

Copyright (c) 2003.  All rights reserved.  This module is free software;
you may restribute and/or modify it under the same terms as Perl itself.

As distributed, JavaScript::Grammar contains code from Parse::Yapp,
which is copyright (c) 1998-2001 Francois Desarmenien, France.  All
rights reserved.  You may use and distribute this code under the same
terms as Perl itself.

=cut

1;
