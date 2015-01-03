#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack ':constants';

# -----------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new(options => print_warnings);
my(@text)   =
(
	q|n/a* # Newline
w/a2|,
	q|n/a* w/a2|,
	q|i9pl|,
);

my($result);

for my $text (@text)
{
	print "Parsing: $text. \n";

	$result = $parser -> parse($text);

	print join("\n", @{$parser -> tree2string}), "\n";
	print "Parse result: $result (0 is success)\n";
	print 'Template: ', $parser -> template_report, ". \n";
}

print "\n";

$parser -> size_report;
