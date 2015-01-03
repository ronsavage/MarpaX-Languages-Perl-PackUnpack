#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack ':constants';

# -----------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new(options => nothing_is_fatal | debug);
my(@text)   =
(
	q|n/a* # Newline
w/a2|,
	q|aaaa|,
	q|i9pl|,
);
my(%count) = (fail => 0, success => 0, total => 0);

my($result);

for my $text (@text)
{
	$count{total}++;

	print "$count{total}: Parsing: $text. \n";

	$result = $parser -> parse($text);

	if ($result == 0)
	{
		$count{success}++;
	}

	print "Parse result: $result (0 is success)\n";
	print 'Template: ', $parser -> template_report, ". \n";
}

print "\n";

$count{fail} = $count{total} - $count{success};

for my $key (sort keys %count)
{
	print sprintf("%-7s: %3d\n", ucfirst $key, $count{$key});
}

print "\n";

$parser -> size_report;
