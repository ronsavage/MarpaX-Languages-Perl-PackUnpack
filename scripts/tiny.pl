#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack;

# -----------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new;
my(@text)   =
(
	q|a|,
	q|a10|,
	q|a[10]|,
);
my(%count) = (fail => 0, success => 0, total => 0);

my($result);

for my $text (@text)
{
	$count{total}++;

	print "Parsing |$text|\n";

	$result = $parser -> parse_pack($text);

	if ($result == 0)
	{
		$count{success}++;
	}

	print join("\n", @{$parser -> tree2string}), "\n";
	print "Parse result: $result (0 is success)\n";
}

$count{fail} = $count{total} - $count{success};

for my $key (sort keys %count)
{
	print sprintf("%-7s: %3d\n", $key, $count{$key});
}
