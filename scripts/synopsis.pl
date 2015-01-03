#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack ':constants';

# -----------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new(options => print_warnings);

my($count);
my($result);

for my $text ('n/a* w/a2')
{
	print "Parsing: $text. \n";

	$result = $parser -> parse($text);

	print "Parse result: $result (0 is success)\n";
	print 'Template: ', $parser -> template_report, ". \n";

	$count = 0;

	for my $item (@{$parser -> stack})
	{
		$count++;

		print "$count: ", join(', ', @$item), "\n";
	}

}

print "\n";

$parser -> size_report;
