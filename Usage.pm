;############################################################
;#  Usage module
;#   Allows autochecking on arguments.
;#   Provides hooks for adding in custom checks
;#
;#   Predefined checks include
;#      INTEGER			an Integer
;#      INTEGER(>,num)		an Integer > num
;#      INTEGER(<,num)		an Integer < num
;#      INTEGER(>=,num)		an Integer >= num
;#      INTEGER(<=,num)		an Integer <= num
;#      INTEGER(==,num)		an Integer == num
;#      INTEGER(!=,num)		an Integer != num
;#      INTEGER(RANGE,n1,n2)	an Integer between n1 and n2
;#				including num1 and num2
;#	INSTANCE(CLASSNAME)	An instance of the class CLASSNAME or one
;#				of its subclasses
;#	CLASS(CLASSNAME)	The Class (package name) CLASSNAME or one
;#				of its subclasses
;#	OBJECT(CLASSNAME)	Either CLASS(CLASSNAME) or INSTANCE(CLASSNAME)
;#	OPEN_HANDLE		A handle which is open (checked with fileno)
;#				If it is not a fully qualified name, this
;#				test will fix the name so that the FileHandle
;#				gets passed to the function correctly.
;#	ANYTHING		Anything. This usually acts as an alias, e.g.
;#				for BOOLEAN, STRING, FUNCTION, etc.
;#
;#
;# Author: Jack Shirazi <js@biu.icnet.uk>
;# Date: 25 November 1994
;# Version: beta2
;#
;# The setUsage and checkUsage functions provide
;# a way to check the actual arguments to the function
;# as well as the number of expected arguments.
;# 
;# A simple example is:
;# sub do_something {
;#     setUsage('INTEGER','OPEN_HANDLE','INSTANCE(MyClass)','OPT_MODE');&checkUsage;
;#     ...
;# }
;# 
;# which would check that the first argument is an integer, the second
;# is a filehandle which is open (and incidentally corrects the name so that
;# if the function was called from main as do_something(1,FILE,...), the
;# do_something function will actually get the second argument as main::FILE
;# if FILE is not an open filehandle, but main::FILE is), the third is a
;# 'MyClass' instance, and the fourth argument an optional argument which
;# if present is a 'mode'. Some tests are pre-defined in this module, but
;# can be overridden in the calling module. Tests not defined in this module
;# must be defined in the calling module.
;# 
;# NOTE: the checkUsage function should be called as
;#    &checkUsage;
;# and you should first have called setUsage(...)
;# 
;# 
;# Tests are defined simply by using the name (where TEST is the test
;# name) 't_TEST_test_usage' as a subroutine name, and return true or
;# false, e.g. the 'INTEGER' test above could be defined as
;# sub t_INTEGER_test_usage {my $int = shift; $int =~ /^[\+\-]\d+$/}
;# 
;# WARNING: Tests should probably not try to change the argument.
;# Tests should not use side effects (like setting a global).
;# If you do these things, the consequences are undefined
;# (you'd better know what you're doing). The OPEN_HANDLE
;# test is an example of one that CAN change its argument.
;# This provides added functionality, correcting an error
;# that is all too common.
;# 
;# 
;# 
;# If the test fails, the test names are printed in a usage statement,
;# and a fatal error is produced.
;# e.g if the above 'do_something' returned false on the 'WIBBLE'
;# test then you would see:
;# 
;# Fatal usage error: argument 2 to 'setUsage' was not a 'WIBBLE'
;# Usage: setUsage(INTEGER,WIBBLE,'MyClass instance' [,MODE])
;# 
;# There is also a removeUsage function that sets all the
;# Usage functions to null. You can do this when you get to
;# production phase if you want to cut out all the overhead.
;# 
;# 
;# There is also the option of using an alias of your own
;# choice for tests. The test 'INTEGER(>,0)' accurately
;# conveys that you want a positive integer, but does
;# not tell you why. You can define an alias for this (and
;# any test) using a global has %USAGE_ALIAS in the package
;# that is using the test. For example
;# 
;# %USAGE_ALIAS = ('POSITIVE'=> 'INTEGER(>,1)');
;# sub do_something {
;#     setUsage('POSITIVE', ...);
;#     ...
;# }
;# 
;# will use the test 'POSITIVE' exactly as if it was 'INTEGER(>,1)'
;# internally, but externally (i.e. in usage messages) it uses
;# 'POSITIVE'. You can also set the long name for the alias
;# (this is the string which displays a long explanation of the test),
;# by using an array in the hash with first element the real test
;# and second element the long name, e.g. for the above alias,
;# you could have
;# 
;# %USAGE_ALIAS = ('POSITIVE'=> ['INTEGER(>,1)','A positive integer.']);
;# 
;# and the usage error will produce lines like
;# 
;# 
;# Usage: do_something(POSITIVE, ...)
;#   POSITIVE - A positive integer.
;#   ...
;# 
;# 
;# NOTE: aliases are assumed not to have prefixes or postfixes,
;# i.e. they are converted internally BEFORE the test is parsed.
;# 
;# 
;# There are more sophistications if you want to use them - skip
;# the rest if you don't want the full specification.
;# 
;# 
;# Syntax for a TYPE test
;# 
;# <PREFIX><TYPE><POSTFIX>
;# 
;# <PREFIX> and <POSTFIX> are optional.
;# 
;# 
;# <TYPE> can be any sequence of characters that you can get to
;# be a subroutine name.
;# 
;# <PREFIX> if present is one of
;#   OPT_
;#   OPT(arg)_
;#   OPT+_
;#   OPT+(arg)_
;#   LIST_OF_
;#   LISTTEST_
;# 
;# <POSTFIX> if present is '(<ARG_TEXT>)' (without quotes)
;# where <ARG_TEXT> are arguments to pass to the test. These
;# arguments are passed exactly as written (there is one mechanism
;# in place which will allow selected arguments to be quoted - see
;# below for more documentation). The args are passed
;# after the argument being tested (though LISTTEST_ prefix
;# is a special case - see below for more documentation).
;# 
;# The TYPE test:
;#   Whatever test is specified, is translated into a straight
;#   call to the subroutine t_<test>_test_usage with the argument
;#   being tested passed as the first argument to t_<test>_test_usage.
;#   E.g. setUsage(TEST1,TEST2) translates into calls to
;# 	t_TEST1_test_usage($_[0]) and
;# 	t_TEST2_test_usage($_[1]).
;# 
;#   NOTE that in order to ensure that the test is available,
;#   it is first called with leading argument undefined, but
;#   others as given. It does not matter what the test returns
;#   to this, as long as it does not produce a fatal error.
;#   (This is because testing for its existence with 
;#   'defined(&t_TEST_test_usage)' is not sufficient,
;#   since it might be an autoloadable function.). If a fatal
;#   error is produced to this it is caught, and the TEST is
;#   just assumed not to be defined.
;# 
;# 
;# PREFIX specifiers mean the following
;# 
;# OPT_		These mean that the argument is optional. The
;# OPT(arg)_	version with (arg) provides a default which will
;# OPT+_	set the argument to arg if it is undefined.
;# OPT+(arg)_	Options are nestable as follows: The first OPT
;# 		obviously implies that all further args are optional.
;# 		All optional arguments MUST have an OPT prefix.
;# 		But some optional arguments may be necessary if the
;# 		previous argument is an optional argument which is present
;#		For example you normally write
;# 			ARG1 [,ARG2,ARG3]
;# 		to mean that ARG1 is necessary, and that ARG2 and ARG3
;# 		are optional, but if ARG2 is present, then ARG3 must be
;# 		present too, whereas
;# 			ARG1 [,ARG2 [,ARG3]]
;# 		usually means that ARG1 is necessary, ARG2 is optional
;# 		and ARG3 is optional even if ARG2 is present. These two
;# 		cases would be specified as
;# 			TEST1,OPT_TEST2,OPT+_TEST3		and
;# 			TEST1,OPT_TEST2,OPT_TEST3 	respectively.
;# 
;# LIST_OF_	This means all further arguments are optional, and
;# 		any present will be tested using the same test. This
;#		must be the last test specified if present.
;# 			LIST_OF_TEST		is like
;# 		'foreach $arg (@remaining_args) {t_TEST_test_usage($arg)}'
;# 
;# LISTTEST_	This is similar to LIST_OF_, but instead of doing the
;# 		looping for you, the remaining arguments are all passed
;# 		to the test. Unlike all the other tests, though, the
;# 		arguments are passed as a reference to an array.
;# 			LISTTEST_MY_TEST
;# 		is like 'MY_TEST(\@remaining_args)'
;# 		This slight difference allows the list to be passed
;# 		as one argument, the first, and so allows optional
;# 		extra arguments to be passed to the test from the
;#		POSTFIX in the same way as any other test (see below).
;# 
;# 
;# The POSTFIX specifier allows extra arguments to be passed
;# to the test. This is useful where you want to have a generic
;# test - e.g. an integer test which checks that the arg is
;# in a specified range. 
;# The call TEST(args) translates directly to a call to
;# 		t_TEST_test_usage(ARG,args)
;# i.e. the argument to be tested is passed as the first argument.
;# For example, 
;# INTEGER_RANGE(1,5) would test for an integer in the range 1 to 5
;# if there is a sub like:
;# 
;# sub t_INTEGER_RANGE_test_usage {
;#     my($arg,$min,$max) = @_;
;#     ($arg =~ /^[\+\-]\d+$/) && ($arg >= $min) && ($arg <= $max);
;# }
;# 
;# In particular, if you want write tests that depend on
;# other arguments as well as the one sent by default, you
;# can include them as $_[index] where you specify the index.
;# For example, 
;#    setUsage('BIGGER($_[1])','SMALLER')
;# 
;# sub t_SMALLER_test_usage {1;}
;# sub t_BIGGER_test_usage {
;#     my($next,$arg) = @_;
;#     $arg > $next;
;# }
;# 
;# The 'SMALLER' test is non-testing because the test is already performed
;# in the first test.
;# 
;# NOTE that if you want to support multiple test types
;# within a test (see for example INTEGER which supports
;# INTEGER,INTEGER(>,1),INTEGER(RANGE,1,5), ...), then
;# you should 'die' out of the test if you don't support
;# a particular type (e.g. INTEGER dies if you say
;# INTEGER(BOB)). This will be caught by the testing mechanism
;# before the testing phase, and produce an error of the form 
;# 'Test does not exist'.
;# 
;# There is one additional mechanism. If a function called
;# q_TEST_quoted_args_usage exists, it is assumed to return
;# an array consisting of argument indexes which need to have
;# quotes added. For example if just the first argument needs
;# to be in quotes when passed on to 't_TEST_test_usage'
;# then the array returned would be (0). The arguments
;# are then checked, and any not surrounded by double
;# or single quotes have single quotes put onto them.
;# (see INTEGER for an example)
;# 
;# BUGS: Every comma within a postfix is assumed to be
;# an argument separator.
;# 
;# 
;# 
;# 
;# Usage Statements
;# The printed out usage statement is constructed as follows (see
;# below for how 'short name' and 'long name' are specified):
;# The first line is:
;# 
;# "Fatal error: argument $ARG to '$METHOD' was not a '$TEST'"
;# 
;# where $ARG is the number of the argument that failed, $METHOD is
;# the method name (as given by the fourth element from caller),
;# and $TEST is the short name of the test that failed.
;# 
;# The second line is constructed by adding to 'Usage: ' first
;# the method name, then the short names of each test separated
;# by commas (and nesting OPT_ tests in square brackets appropriately.
;# 
;# Subsequent lines are one per long name, and consist of
;# "'short name' - 'long name'"
;# 
;# 
;# The short name for a test is specified by the the function
;#     d_TEST_display_string_usage(0,args)
;# and the long name by 
;#     d_TEST_display_string_usage(1,args)
;# where this function is defined. args are any args added in the
;# POSTFIX.  If the function is defined but long name returns false,
;# then there is no long name. If the function is not defined then
;# there is no long name - i.e long names are only available if
;# the function d_TEST_display_string_usage exists, and returns
;# a non-empty string when the first argument is '1'.
;# If the function is defined then whatever it returns when the
;# first argument is '0' is used as the short name (except
;# where the LIST_OF_ prefix is used, where LIST_OF_
;# is prefixed onto whatever is returned for the short name).
;# If the function is not defined then the short name is
;# as follows (POSTFIX ignored completely).
;# 
;# TEST				short name
;# 
;# OPT_${test}			"${test}"
;# OPT(arg)_${test}		"${test}"
;# OPT+_${test}			"${test}"
;# OPT+(arg)_${test}		"${test}"
;# LIST_OF_${test}		"LIST_OF_${test}s"
;# LISTTEST_${test}		"${test}"
;#
;#
;# Setting $Usage::Debug to true will printout the functions
;# that are created to test the arguments when they are first
;# compiled.
;#
;# Calls to setUsage from a function only result in one
;# compilation which is cached. Further calls use the cached
;# function. If the arguments of setUsage are changed between
;# calls, then you should use 'overrideUsage' which will
;# re-compile each time. This is bad practice though - you
;# are better off using some other method to achieve your
;# ends if possible.
;#
;# There is one further option. If the r_<test>_reference_arg_usage
;# can be called and doesn't produce an error, then instead of passing
;# the argument for the test index to the test, an anonymous array
;# with two elements - the index number and the reference to @_ -
;# are passed. I.e. instead of
;#	t_<test>_test_usage($_[$index] ...), 
;# the call is
;#	t_<test>_test_usage([$index,\@_] ...)
;# 


package Usage;
use Exporter;
use AutoLoader;
#use strict			qw(refs subs);

@ISA = qw(Exporter AutoLoader);
@EXPORT = qw(setUsage checkUsage);
@EXPORT_OK = qw(overrideUsage removeUsage);


$Debug = 0;
sub FileHandle::_is_filehandle_usage_test_ {1};

sub _func {
;#    $CALLFUNC;
    (my $callfunc = $CALLFUNC) =~ s/^.*:://;
    $callfunc;
}


sub removeUsage {
    eval 'sub Usage::overrideUsage {} sub Usage::setUsage {}
	sub Usage::checkUsage {}'
}

sub overrideUsage {
    $CALLFUNC = (caller(1))[3];
    delete($DEFINED_TESTS{$CALLFUNC});
    _setUsage(@_);
}

sub setUsage {
    $CALLFUNC = (caller(1))[3];
    if($DEFINED_TESTS{$CALLFUNC}) {return} ;#Cached
    _setUsage(@_)
}

sub _die_settingUsage {
    my($error) = @_;
    my($callpack,$callfile,$callline,$callfunc) = caller(2);
    die "$callfunc error: $error at line $callline in file $callfile\n";
}

sub _setUsage {
    my(@tests) = @_;
    my($test,$error,$error2,$test_body);

    my($callfunc) = _func();

    ;#No arguments
    if ($#tests == -1) {
	$test = "    Usage::_check_arg_number(\$#_,0,-1);\n";
	$error = "${callfunc}() ";
	return _set_test_and_error($test,$error);
    }

    ;#Has arguments
    my($index,$count,@allowed_args,@temp,$opt_on,$comma,$end);
    $error = "${callfunc}(";

    ;#Iterate up to the list arg (if any)
    my $pack = (caller(1))[0];
    $opt_on = 0;
    $count = -1;
    for ($index = 0; $index <= $#tests; $index++) {
	@temp = _split_test($tests[$index],$pack);
	$#temp == 0 && _die_settingUsage($temp[0]. " for argument index $index");

	;#Building error strings
	$temp[5] && ($error2 .= "  " . $temp[4] . " - " . $temp[5] . "\n");
	$error .= (($temp[6] > 1) ? '[' : '') . $comma . $temp[4];
	$end .= ($temp[6] > 1) ? ']' : '';
	$comma = ",";

	;#Building the test body
	if ($temp[6] == 0) {
	    $test_body .= _normal_test_body($temp[1],$temp[2],
			$temp[3],$temp[4],$index,4,$temp[8]);
	} elsif ( ($temp[6] == 1) || ($temp[6] == 2) ) {
	    $test_body .= _opt_test_body($temp[1],$temp[2],
			$temp[3],$temp[4],$index,4,$temp[7],$temp[8]);
	} elsif ($temp[6] == 3) {
	    $test_body .= _list_test_body($temp[1],$temp[2],
			$temp[3],$temp[4],$index,4,$temp[8]);
	} elsif( $temp[6] == 4) {
	    $test_body .= _listtest_test_body($temp[1],$temp[2],
			$temp[3],$temp[4],$index,4,$temp[8]);
	    
	}



	if ($opt_on) {
	    $temp[6] || _die_settingUsage("All args after arg "
		. ($allowed_args[0] + 1) .
		" must be defined as optional, but arg ${index} is not");
	} else {
	    ($temp[6] == 1) && _die_settingUsage(
		" The first optional arg must NOT be specified as 'OPT+'");
	    if ($temp[6]) {$opt_on = 1}
	}
	($temp[6] > 2) && last;
	($temp[6] == 2) && push(@allowed_args,$count);
	$count = $index;
    }
    $error .= $end . ')';

    ;#Is this a list arg, and if so is it the last arg?
    if ($temp[6] > 2) {
	($index == $#tests) || _die_settingUsage(
		"If a list argument is specified, it can\n" .  
		"only be specified once, and MUST be the last argument");

	;#The last arg is a list arg. Add in the previous count.
	push(@allowed_args,$count);
	$test = '    Usage::_check_arg_number($#_,1,';
	$test .= join(',',@allowed_args) . ");\n";
    } else {
	;#Not the list arg - we need to add in last index.
	push(@allowed_args,$index - 1);
	$test = '    Usage::_check_arg_number($#_,0,';
	$test .= join(',',@allowed_args) . ");\n";
    }
    $error .= "\n" . $error2;
    _set_test_and_error($test . $test_body,$error);
}

sub _normal_test_body {
    my($test,$args,$pack,$sname,$i,$indent,$needs_ref) = @_;
    my($test_body);
    $test_body = " " x $indent;
    $test_body .= "${pack}::t_${test}_test_usage(";
    $test_body .= $needs_ref ? "[${i},\\\@_]" : "\$_[${i}]";
    $test_body .= $args ? ("," . $args) : '';
    $test_body .= ") ||\n" . (" " x ($indent + 4));
    $sname =~ s/'/\\'/g;
    $test_body .= 'Usage::_failed_test_usage($_[' . "$i],$i,'$sname');\n";
    $test_body;
}

sub _opt_test_body {
    my($test,$args,$pack,$sname,$i,$indent,$opt_args,$needs_ref) = @_;
    my($test_body);
    $test_body = " " x $indent;
    $test_body .= "if (defined(\$_[$i])) {\n";
    $test_body .= _normal_test_body($test,$args,$pack,$sname,$i,$indent+4,$needs_ref);
    if ($opt_args) {
	$test_body .= (" " x $indent) . "} else {\n" . (" " x ($indent + 4));
	$test_body .= "\$_[$i] = " . $opt_args . ";\n";
    }
    $test_body .= (" " x $indent) . "}\n";
}


sub _list_test_body {
    my($test,$args,$pack,$sname,$i,$indent,$needs_ref) = @_;
    my($test_body);
    $test_body = " " x $indent . "foreach \$i ($i .. \$#_) {\n";
    $test_body .= _normal_test_body($test,$args,$pack,$sname,$i,$indent+4,$needs_ref);
    $test_body =~ s/\$_\[\d+\],\d+/\$_[\$i],\$i/g;
    $test_body =~ s/\$_\[\d+\]/\$_[\$i]/g;
    $test_body .= " " x $indent . "}\n";
}

sub _listtest_test_body {
    my($test,$args,$pack,$sname,$i,$indent,$needs_ref) = @_;
    my($test_body);
    $test_body = _normal_test_body($test,$args,$pack,$sname,$i,$indent,$needs_ref);
    $test_body =~ s/\$_\[\d+\]/[\@_[$i .. \$#_]]/g;
    $test_body;
}

sub _set_test_and_error {
    my($test,$error) = @_;
    $test = "\$DEFINED_TESTS{'${CALLFUNC}'} = sub {\n" . $test . "\n}\n";
    $Debug && warn $test,"\n";
    eval $test;
    if ($@) {
        my($callpack,$callfile,$callline,$callfunc) = caller(2);
	die("Error during compilation of:\n$test\n$@\n"
		."Usage::$callfunc error at line $callline in file $callfile\n");
    } else {
	$ERROR_STRINGS{$CALLFUNC} = "Usage: " . $error;
    }
    1;
}

sub _check_arg_number {
    my($num_args,$list,@allowed_nums) = @_;
    my($n,$error);
    foreach $n (@allowed_nums) {if($num_args == $n) {return}}
    if($list) {
	($num_args > $allowed_nums[$#allowed_nums]) && return;
    } 

    my($callfunc) = _func();
    my($callpack,$callfile,$callline) = caller(4);
    die("\nUsage error at line $callline in file $callfile:\n",
	    "    Incorrect number of arguments passed to function '$callfunc',\n",
	    "    passed ",$num_args + 1," arguments, but expected ",
	     join(' or ',grep($_++,@allowed_nums)),
	    " arguments",($list ? " or more.\n\n" : ".\n\n"),
	    $ERROR_STRINGS{$CALLFUNC},"\nStack was:\n",_dump_stack(4));
}

sub _split_test {
    my($test,$pack) = @_;
    my($prefix,$postfix,$shortname,$sname,$lname,$opt,$opt_arg,$eval);

    $pack || ($pack = 'main');

    my($value);
    local(*hash);
    *hash = \%{"${pack}::USAGE_ALIAS"};
    if ($value = $hash{$test}) {
	$sname = $test;
	if (ref($value) eq 'ARRAY') {
	    $test = $value->[0];
	    $lname = $value->[1];
	} else {
	    $test = $value;
	}
    }

    ;#POSTFIXES
    if ($test =~ s/\(([^\)]+)\)$//) {$postfix = $1}
    if ($postfix =~ /^\s*$/) {$postfix = undef}

    ;#PREFIXES
    if      ($test =~ s/^OPT(\+?)(\([^\)]+\))?_//) {
	$prefix = $&;  $shortname="$test";$opt = ($1 eq '+') ? 1 : 2;
	chop($opt_arg = substr($2,1)); 
	$opt_arg = ($opt_arg =~ /^\s*$/) ? undef : $opt_arg;
    } elsif ($test =~ s/LIST_OF_//) {
	$prefix = $&;  $shortname="LIST_OF_${test}s";$opt = 3;
    } elsif ($test =~ s/LISTTEST_//) {
	$prefix = $&;  $shortname="$test";$opt = 4;
    } else {
	$prefix = ""; $shortname="$test";$opt = 0;
    }

    if ($test eq "") {return ("No test specified");}

    $postfix = _quote_postfix_args($test,$pack,$postfix);

    $eval = $postfix ? 
	"${pack}::t_${test}_test_usage(undef,${postfix})":
	"${pack}::t_${test}_test_usage()";
    eval $eval;
    if ($@) {
	$pack = 'Usage';
    	$eval = $postfix ? 
	    "${pack}::t_${test}_test_usage(undef,${postfix})":
	    "${pack}::t_${test}_test_usage()";
	eval $eval;
	if ($@) {return ("Test '${test}' was not found");}
    }

    $sname = $sname ? $sname : _name(0,$test,$pack,$postfix,$shortname);
    $lname = $lname ? $lname : _name(1,$test,$pack,$postfix,$shortname);
    ($opt == 3) && ($sname = 'LIST_OF_' . $sname);

    ($prefix,$test,$postfix,$pack,$sname,$lname,$opt,$opt_arg,
	 _ref_args_test($test,$pack));
}

sub _ref_args_test {
    my($test,$pack) = @_;
    my($need_ref) = eval "${pack}::r_${test}_reference_arg_usage()";
    if ($@) {
	$pack = 'Usage';
	$need_ref = eval "${pack}::r_${test}_reference_arg_usage()";
	$@ && return 0;
    }
    1;
}

sub _quote_postfix_args {
    my($test,$pack,$postfix) = @_;
    $postfix || return $postfix;
    my(@need_quotes) = eval "${pack}::q_${test}_quoted_args_usage()";
    if ($@) {
	$pack = 'Usage';
	@need_quotes = eval "${pack}::q_${test}_quoted_args_usage()";
	$@ && return $postfix;
    }
    @postfix = split(/,/,$postfix);
    foreach $i (@need_quotes) {
	if(defined($postfix[$i])){
	    ($postfix[$i] =~ /^'.*'$/) || 
		($postfix[$i] =~ /^".*"$/) ||
		($postfix[$i] = "'" . $postfix[$i] . "'");
	}
    }
    join(',',@postfix);
}

sub _name {
    my($long,$test,$pack,$args,$default_sname) = @_;
    my($name);

    $name = $args ?
	eval "${pack}::d_${test}_display_string_usage(${long},${args})" :
	eval "${pack}::d_${test}_display_string_usage(${long})" ;
    if ($@) {
    	$name = $args ?
	    eval "Usage::d_${test}_display_string_usage(${long},${args})":
	    eval "Usage::d_${test}_display_string_usage(${long})" ;
	if (!$@) {return $name;}
    } else {
	return $name;
    }
    ;#Still here? No display string function defined, so use defaults.
    $long && return undef;
    $default_sname;
}

sub checkUsage {&_checkUsage}

sub _checkUsage {
    my($callfunc) = (caller(2))[3];
    ;#sanity check
    ($callfunc eq $CALLFUNC) || _die_settingUsage(
	"'checkUsage' called without first calling 'setUsage'");
    &{$DEFINED_TESTS{"$callfunc"}};
}

sub _failed_test_usage {
    my($arg,$index,$sname) = @_;
    my($callfunc) = _func();
    my($callpack,$callfile,$callline) = caller(4);
    die("\nUsage error at line $callline in file $callfile:\n",
		"    argument at index $index to function '$callfunc'\n",
		"    was '$arg', but should have been a",
		($sname =~ /^[aeiou]/i ? 'n' : ''), " '$sname'\n\n",
		$ERROR_STRINGS{$CALLFUNC},"\nStack was:\n",_dump_stack(4));
}


sub _dump_stack {
    my($level) = @_;
    my($pack,$file,$line,$sub,$stack);
    while (($pack,$file,$line,$sub) = caller($level++)) {
	$stack .= "\t$sub called at $file line $line\n";
    }
    $stack;
}

;############################################################
;#
;#              Support functions for predefined tests
;#
;############################################################
sub _classSpec {
    ;#Returns two element array, first element is the class of the
    ;#argument or false if none, second is true if the argument
    ;#is an instance of the class, false if it is the class itself.
    my $class = ref($_[0]);
    if ($class) {
	;#We have a class - check whether we are that class
	return ( ($class eq $_[0]) ? ($class,0) : ($class,1) );
    } else {
	;#No ref - try the special FileHandle case
	if ($_[0] eq 'FileHandle') {
	    return ('FileHandle',0);
	} else {
	    eval '$_[0]->_is_filehandle_usage_test_()';
	    if ($@) {
		;#Its not anything
		return ($class,0);
	    } else {
		;#Its a FileHandle of some sort
		return ('FileHandle',1);
	    }
	}
    }
}

sub _classType {
    my($class,$ancestor) = @_;
    ($class eq $ancestor) || _has_superclass($class,$ancestor);
}

sub _has_superclass {
    ;#Assumes that the two arguments are already class names
    my($class,$ancestor) = @_;
    ;#Looks to see if $ancestor is defined somewhere in $class's
    ;#@ISA hierarchy. Does a breadth first search, stopping as soon
    ;#as it finds a matching class, and pruning branches that its
    ;#already met (otherwise multiple inheritance means you could
    ;#redundantly scan a superclasses's hierarchy more than once).

    my(@classes,%seen,$cls);
    @classes = ($class);
    $seen{$class} = 1;
    while (@classes) {
	$cls = shift(@classes);
	foreach $superclass ( @{"${cls}::ISA"}) {
	    $seen{$superclass} ? next : $seen{$superclass}++;
	    $superclass eq $ancestor && return 1;
	    push(@classes,$superclass);
	}
    }
    0;
}

;############################################################
;#
;#              PREDEFINED TESTS
;#
;############################################################
sub r_OPEN_HANDLE_reference_arg_usage {}
sub t_OPEN_HANDLE_test_usage {

    ;# WARNING - this test can alter the argument.

    ;# OPEN_HANDLE tests the argument to see if it is open
    ;# (using fileno). It also does something a little naughty
    ;# - if the arg is not open and is not a fully qualified
    ;# handle name, it looks to see if adding the name of
    ;# calling package makes it into an open handle - and
    ;# if so, makes the argument into the fully qualified
    ;# name.
    ($#_ == -1) && return;
    ($#_ == 0) || die(
	"Test OPEN_HANDLE(ARGS) is not defined - it must be just OPEN_HANDLE\n");
    $_[0] || return 0;
    my $index = $_[0]->[0];
    my $arg_ref = $_[0]->[1];
    fileno($$arg_ref[$index]) && return 1;

    ;# If its got a ref, I assume it is passed correctly.
    ref($$arg_ref[$index]) && return 0;
    ($$arg_ref[$index] =~ m/'|::/) && return 0;

    ;#Otherwise, test it and change it if necessary.
    my $pack = (caller(4))[0];
    my $fh = $$arg_ref[$index];
    if (fileno($pack . '::' . $fh)) {
        splice(@$arg_ref,$index,1,$pack . '::' . $fh);
	return 1;
    }
    0;
}
sub d_OPEN_HANDLE_display_string_usage{
    $_[0] ? 
	'Some sort of handle that is open. Tested using "fileno".' :
	'OPEN_HANDLE';
}

sub t_INSTANCE_test_usage {
    ;# INSTANCE: supports the following scheme
    ;#    INSTANCE(CLASSNAME)
    ($#_ == -1) && return;
    ($#_ > 1) && die("Only INSTANCE(Classname) is defined\n");
    my($inst,$class) = @_;
    $class || die("Test INSTANCE is not defined - it must be INSTANCE(CLASSNAME)\n");
    my($instclass,$is_inst) = _classSpec($inst);
    $is_inst && $instclass && _classType($instclass,$class);	
}
sub q_INSTANCE_quoted_args_usage {(0)}
sub d_INSTANCE_display_string_usage{
    $_[0] ? 
	'An instance of ' . $_[1] . ' or one of its subclasses' : 
	$_[1] . '_Instance';
}

sub t_CLASS_test_usage {
    ;# CLASS: supports the following scheme
    ;#    CLASS(CLASSNAME)
    ($#_ == -1) && return;
    ($#_ > 1) && die("Only CLASS(Classname) is defined\n");
    my($inst,$class) = @_;
    $class || die("Test CLASS is not defined - it must be CLASS(CLASSNAME)\n");
    my($instclass,$is_inst) = _classSpec($inst);
    (!$is_inst) && $instclass && _classType($instclass,$class);	
}
sub q_CLASS_quoted_args_usage {(0)}
sub d_CLASS_display_string_usage{
    $_[0] ? 
	'The class (i.e the string) ' . $_[1] . ' or one of its subclasses' : 
	$_[1] . '_Class';
}

sub t_OBJECT_test_usage {
    ;# OBJECT: supports the following scheme
    ;#    OBJECT(CLASSNAME)
    ($#_ == -1) && return;
    ($#_ > 1) && die("Only OBJECT(Classname) is defined\n");
    my($inst,$class) = @_;
    $class || die("Test OBJECT is not defined - it must be OBJECT(CLASSNAME)\n");
    my($instclass) = _classSpec($inst);
    $instclass && _classType($instclass,$class);	
}
sub q_OBJECT_quoted_args_usage {(0)}
sub d_OBJECT_display_string_usage{
    $_[0] ? 
	'Any object of class ' . $_[1] . ' or one of its subclasses' : 
	$_[1] . '_Object';
}

sub t_ANYTHING_test_usage {
    ;# ANYTHING allows anything to be an argument
    ($#_ <= 0) || die(
	"Test ANYTHING(ARGS) is not defined - it must be just ANYTHING\n");
    1;
}
sub d_ANYTHING_display_string_usage{$_[0] ? 'Anything at all' : 'ANYTHING'}


sub t_INTEGER_test_usage {
    ;# INTEGER: supports the following schemes
    ;#    INTEGER
    ;#    INTEGER(>,num)
    ;#    INTEGER(<,num)
    ;#    INTEGER(>=,num)
    ;#    INTEGER(<=,num)
    ;#    INTEGER(==,num)
    ;#    INTEGER(!=,num)
    ;#    INTEGER(RANGE,num1,num2)

    my($int,$type,$other,$max) = @_;
    if ($#_ <= 0) {
	return($int =~ /^[\+\-]?\d+$/);
    } elsif ($#_ == 1) {
	die "Test INTEGER is only defined for INTEGER/" .
	    "INTEGER(</>/>=/<=/==/!=,n)/INTEGER(RANGE,n,n)\n";
    } elsif ($#_ == 2) {
    	if    ($type eq '>' ) {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int >  $other);
    	} elsif ($type eq '<' ) {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int <  $other);
    	} elsif ($type eq '<=') {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int <= $other);
    	} elsif ($type eq '>=') {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int >= $other);
    	} elsif ($type eq '==') {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int == $other);
    	} elsif ($type eq '!=') {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
    	    return($int != $other);
	} else {
	    die "Test INTEGER is only defined for INTEGER/" .
	    	"INTEGER(</>/>=/<=/==/!=,n)/INTEGER(RANGE,n,n)\n";
	}
    } elsif ($#_ == 3) {
	if ($type eq 'RANGE') {
            ($int =~ /^[\+\-]?\d+$/) || return 0;
	    return(($int >= $other) && ($int <= $max))}
	else {
	    die "Test INTEGER is only defined for INTEGER/" .
	    	"INTEGER(</>/>=/<=/==/!=,n)/INTEGER(RANGE,n,n)\n";
	}
    } else {
	die "Test INTEGER is only defined for INTEGER/" .
	    "INTEGER(</>/>=/<=/==/!=,n)/INTEGER(RANGE,n,n)\n";
    }
}
sub q_INTEGER_quoted_args_usage {(0)}
sub d_INTEGER_display_string_usage{
    if ($#_ == 0) {
	return ($_[0] ?
	    'An integer, consisting only of digits (can start with + or -)':
	    'INTEGER');
    } elsif($_[1] eq 'RANGE') {
	return ($_[0] ?
	    ('An integer (optional +/- with digits) between ' .
		$_[2] . ' and ' . $_[3]) :
	    ('INTEGER:' . $_[2] . '-' . $_[3]));
    } else {
	return ($_[0] ?
	    ('An integer (optional +/- with digits) which is ' .
		$_[1] . ' ' . $_[2]):
	    ('INTEGER' . $_[1] . $_[2]) );
    }
}


1;
__END__
BOOLEAN
DEFINED_FUNCTION
DEFINED_FUNCTION
LOCALPORT
HOST
ONE_OF
STRUCTURE (test with unpack?)
SOCK_ADDRESS
