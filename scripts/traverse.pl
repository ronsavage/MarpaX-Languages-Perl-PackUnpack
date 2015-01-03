#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack ':constants';

# -----------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new(options => print_warnings);
my(@text)   =
(
	q|a3/A A*|,
	q|d[x![d]]|,
	q|%32 W*|,
);
my($format) = "%-20s  %-20s  %-10s  %-10s\n";

my($attributes);
my($event);
my($result);
my($text);

for my $text (@text)
{
	print "Parsing: $text. \n";
	print sprintf($format, 'Node name', 'Event name', 'Depth', 'Text');

	$result = $parser -> parse($text);

	for my $node ($parser -> tree -> traverse)
	{
		next if ($node -> is_root);

		$attributes = $node -> meta;
		$event      = $$attributes{event};
		$text       = $$attributes{text};

		print sprintf($format, $node -> value, $event, $node -> depth, $text);
	}

	print '-' x 66, "\n";
}
