#!/usr/bin/env perl
# -*- coding: utf-8; tab-width: 8 -*-
# vim: fileencoding=UTF-8 shiftwidth=8 softtabstop=8 tabstop=8

#
# Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
# 2003, 2004, 2005  Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.
#

# version 0.4.18 : eel3 : changed shebang, fixed indent, specified emacs/vim coding system.
# version 0.4.17 : eel3 : fixed error occur when print the line matched /%[^%]/g
# version 0.4.16 : eel3 : fixed parse error occur in line 10000 or later
# version 0.4.15 : eel3 : avoided warnings caused by uninitialized variable
# version 0.4.14 : eel3 : fixed line number indent size for over 99999 line
# version 0.4.13 : eel3 : changed warning option
# version 0.4.12 : eel3 : fixed problem that extra tab is output, and changed line number to be right-aligned
# version 0.4.11 : Nick Groesz : fix potential divide by zero
# version 0.4.10 : Nick Groesz : fixed summary in combined coverage, ignore function data, added copyright
# version 0.4.9 : Nick Groesz : added combined reporting in print_summary(), changed usage text
# version 0.4.8 : Nick Groesz : added -c option (combined coverage)
# version 0.4.7 : Nick Groesz : fixed formatting, added comments
# version 0.4.6 : Dickson Patton : fixed tagfile option, right justify counts
# version 0.4.5 : Nick Groesz : list code generated with #define macros
# version 0.4.4 : Dickson Patton : added tagfile option
# version 0.4.3 : Nick Groesz : changed around internal data structures, start of version history

use strict;
use warnings;
use Getopt::Long;

# prototypes
sub read_args();		# read in command line arguments
sub process_files();		# run through each file
sub parse_execution_data($$);	# parse the data from each file
sub print_results();		# print gcov data
sub print_summary();		# print summary (similary to gcov's summary)
sub print_usage();		# print gccr usage text

our $tool_name = 'gccr';			# name of script
our $version = 'gccr (GCC) 0.4.18';		# version of script
our $copyright = 'Copyright (C) 2005 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
';						# copyright notice

# internal data
our @files;			# gcov file data
				#
				# File information:
				# file_number -  ranges from 0 to NUMBER OF FILES SPECIFIED - 1
				# $file[file_number]{'tag'} - user specified file tag
				# $file[file_number]{'name'} - name of file to be read and parsed
				#
				# $file[file_number[{'data'} contains parsed gcov data
				#
				# Line information:
				# line_number - corresponds to the line number of the GCOV file
				# 		and ranges from 1 to NUMBER OF LINES
				# $data{'line'}[line_number]{'type'} - type of line
				#				       Can be set to:
				#						no_ex -  non-executing code (gcov meta-data, header code, code that has been #ifdef'd out)
				#						line - 	 an executable line of code
				#						branch - branch execution information
				#						call -   call execution information
				#
				# $data{'line'}[line_number]{'count'} -    number of times an executable line, branch, or call was executed
				# $data{'line'}[line_number]{'raw'} -      raw data straight from gcov file
				#				           this is only populated in the first file's data structure
				# $data{'line'}[line_number]{'line_num'} - line number in original source code
				# Execution information:
				# $data{'line_count'} - 	number of executable lines
				# $data{'execution_count'} -	number of executed lines

our $file_count = 0;		# number of files read
our $line_count = 0;		# number of lines in gcov data files (should be the same for each file)
our $executable_total = 0;	# number of different executable lines across all files
our $executed_total = 0;	# number of different executable lines across all files that were executed

# options
our $opt_combined = 0;
our $opt_help = 0;		# help option
our $opt_version = 0;		# version option
our @tag_files = ();		# tagfile(s) option
our $opt_nosummary = 0;		# do not print summary

read_args();
process_files();
print_results();
unless($opt_nosummary) {
	print_summary();
}

# Summary: 	read command line arguments:
# Parameters: 	none
# Return: 	none
sub read_args()
{
	unless( GetOptions(
		'combined' => \$opt_combined,
		'help' => \$opt_help,
		'no-summary' => \$opt_nosummary,
		'tagfile|file=s' => \@tag_files,
		'version' => \$opt_version,
			  )
	      ) {

		# if GetOptions returns FALSE, then incorrect options were specified
		# exit with error
		print_usage();
		exit(1);
	}

	if($opt_help) {
		print_usage();
		exit(0);
	}

	if($opt_version) {
		print "$version\n";
		print "$copyright\n";
		exit(0);
	}

	# if any number of tag files were specified, then we read tagfiles instead of command line arugments
	if(scalar(@tag_files)) {
		@files = &read_tagfiles(@tag_files);
	}else {
		# number of file arguments + number of tag arguments should equal an even number
		if(@ARGV % 2 == 1) {
			print("ERROR: file count does not match tag count\n");
			exit(1);
		}
		my $i = 0;
		while($ARGV[$i]) {
			push @files, {name => $ARGV[$i], tag => $ARGV[$i+1]};
			$i+=2;
		}
	}

	$file_count = scalar(@files);
	if($file_count < 2 ) {
		print("ERROR: at least two files must be specified\n");
		exit(1);
	}
}

# Summary: 	read tagfiles specifie dwith the -t option
# Parameters: 	array of names of tagfiles
# Return: 	array of name/tag hashes
sub read_tagfiles($)
{
	my @tag_files = @_;
	my @files;
	my $l = 0;				# count of --tagfile=___ options
 	my $m = 0;				# count of line in the current tagfile

	foreach my $file(@tag_files) {
		$l++;
		open(TAGFILE, $file) || die "ERROR: on open of tagfile $l, $file: ($!)\n";
		while(<TAGFILE>) {
			$m++;
			chomp $_;
			if ( /(^([^ ]+) *(.*)$)/ ) {
				push @files, {name => $2, tag => $3};
			}
			else {
				die "ERROR: invalid file-tag pair on line $m of tagfile $l\n";
			}
		}
		close(TAGFILE);
	}
	return(@files);
}

# Summary: 	run through all the gcov files and call the parsing function
# Parameters: 	none
# Return: 	none
sub process_files()
{
	# the first file is used to gather raw data
	$files[0]{'data'} = parse_execution_data($files[0]{'name'},1);

	for(my $i=1;$i<$file_count;$i++) {
		$files[$i]{'data'} = parse_execution_data($files[$i]{'name'},0);
	}
}

# Summary: 	parse the gcov file, populating the %data structure
# Parameters: 	name of file to parse | boolean indicating whether raw (original gcov) data should be saved
# 		save_raw is set to 1 on the first file parse and set to zero thereafter
# Returns: 	reference to data hash
sub parse_execution_data($$)
{
	my($file,$save_raw) = @_;
	my %data;

	$data{'line_count'} = 0;		# number of executable lines in file
	$data{'execution_count'} = 0;		# number of lines that were executed in file

	stat($file);
	if(!(-r _)) {
		die("ERROR: cannot read file: $file\n");
	}
	if(!(-f _)) {
		die("ERROR: not a plain file: $file\n");
	}
	open(FILE_HANDLE,$file) || die("ERROR: cannot open file $file: $!");

	my $file_line_num = 0;
	while(<FILE_HANDLE>) {
		my $line = $_;
		$file_line_num++;

		chomp $line;

		if($line =~ /^\s+-:\s*(\d+):(.*)/) {

			# line is gcov preamble or non-executing code

			my $line_num = $1;
			my $raw = $2;

			$data{'line'}[$file_line_num]{'type'} = 'no_ex';
			$data{'line'}[$file_line_num]{'line_num'} = $line_num;

			if($save_raw) {
				$data{'line'}[$file_line_num]{'raw'} = $2;
			}
		}elsif($line =~ /^\s+#####:\s*(\d+):(.*)/) {

			# line was not executed

			my $line_num = $1;
			my $raw = $2;

			$data{'line'}[$file_line_num]{'count'} = 0;
			$data{'line'}[$file_line_num]{'type'} = 'code';
			$data{'line'}[$file_line_num]{'line_num'} = $line_num;
			$data{'line_count'}++;
			if($save_raw) {
				$data{'line'}[$file_line_num]{'raw'} = $raw;
			}
		}elsif($line =~ /^\s*(\d+):\s*(\d+):(.*)/) {

			# line was executed

			my $count = $1;
			my $line_num = $2;
			my $raw = $3;

			$data{'line'}[$file_line_num]{'count'} = $count;
			$data{'line'}[$file_line_num]{'type'} = 'code';
			$data{'line'}[$file_line_num]{'line_num'} = $line_num;
			$data{'line_count'}++;
			$data{'execution_count'}++;
			if($save_raw) {
				$data{'line'}[$file_line_num]{'raw'} = $raw;
			}
		}elsif($line =~ /^branch\s+(\d+)/) {

			# line contains branch execution information

			my $branch_num = $1;
			$data{'line'}[$file_line_num]{'num'} = $branch_num;

			if($line =~ /^branch\s+\d+\s+never executed/) {
				$data{'line'}[$file_line_num]{'count'} = 0;
				$data{'line'}[$file_line_num]{'type'} = 'branch';
			}elsif($line =~ /^branch\s+\d+\s+taken (\d+)%/) {
				$data{'line'}[$file_line_num]{'count'} = $1;
				$data{'line'}[$file_line_num]{'type'} = 'branch';
			}
			if($save_raw) {
				$data{'line'}[$file_line_num]{'raw'} = $line;
			}
		}elsif($line =~ /^call\s+(\d+)/) {

			# line contains call execution information

			my $call_num = $1;
			$data{'line'}[$file_line_num]{'num'} = $call_num;

			if($line =~ /^call\s+\d+\s+never executed/) {
				$data{'line'}[$file_line_num]{'count'} = 0;
				$data{'line'}[$file_line_num]{'type'} = 'call';
			}elsif($line =~ /^call\s+\d+\s+returns (\d+)%/) {
				$data{'line'}[$file_line_num]{'count'} = $1;
				$data{'line'}[$file_line_num]{'type'} = 'call';
			}
			if($save_raw) {
				$data{'line'}[$file_line_num]{'raw'} = $line;
			}
		}elsif($line =~ /^function/i) {
			# function data is ignored
		}else {
			# line could not be parsed

			print("ERROR: cannot parse line $file_line_num in file $file\n Is this a valid gcov file?\n");
			exit(1);
		}
	}
	close(FILE_HANDLE);

	# check to see if we should save an overall line count (common to all gcov files)
	if($save_raw) {
		$line_count = $file_line_num;
	}

	return(\%data);
}

# Summary: 	print interpolated gcov information
# Parameters: 	none
# Return: 	none
sub print_results()
{
	my $ftab = '        ';
	my $p_op = ($line_count <= 99999) ? '%5d' : '%13d';

	for(my $line_i=1;$line_i<=$line_count;$line_i++) {

		my $raw_printed = 0;		  # boolean flag to print out the line slurped in from the gcov file

		my $count_sum = 0;		  # sum of executions across all files for this one line- used for combined coverage reporting

		my $never_exec = 1;		  # boolean flag that is set to 0 when the current line is executed or executable

						  # in any file. used in combined coverage reporting.

		my $first_code_line_executed = 0; # boolean flag that indicates whether this is the first unique
						  # executed line of code to be found among the gcov files

		for(my $file_i=0;$file_i<$file_count;$file_i++) {
			# Note that each file is cycled through for every line even if just the raw
			# data from the first file that ends up being printed. This is because the same
			# line may be non-executing in one file and executable in another file (because code
			# may be ifdef'd out).

			my $type = $files[$file_i]{'data'}{'line'}[$line_i]{'type'};

			no warnings 'uninitialized';    # XXX: remove warining for $type
			if($type eq 'no_ex') {
				# non-executing code

				unless($raw_printed || $opt_combined) {
					printf("$ftab-:$p_op:%s\n", $files[$file_i]{'data'}{'line'}[$line_i]{'line_num'}, $files[0]{'data'}{'line'}[$line_i]{'raw'});
					$raw_printed = 1;
				}

				# nothing additional is printed for non-executing code
			}elsif($type eq 'code') {
				# code that is executable
				if($never_exec) {
					# this code is exectuable, so we indicate that in the never_exec flag
					$never_exec = 0;
					# we only want the number of UNIQUE lines across files that are executable
					# in the executable_total flag, so this is only incremented once for all
					# identical lines across each file
					$executable_total++;
				}

				unless($raw_printed || $opt_combined) {
					printf("$ftab  $p_op:%s\n", $files[0]{'data'}{'line'}[$line_i]{'line_num'}, $files[0]{'data'}{'line'}[$line_i]{'raw'});
					$raw_printed = 1;
				}

				my $count = $files[$file_i]{'data'}{'line'}[$line_i]{'count'};
				if($opt_combined) {
					$count_sum += $count;
					if($first_code_line_executed == 0 && $count > 0) {
						$executed_total++;
						$first_code_line_executed = 1;
					}
				}else {
					if($count == 0) {
						print '     ####';
					}else {
						unless($first_code_line_executed) {
							# update the unique count of code lines executed
							# across all gcov files
							$executed_total++;
							$first_code_line_executed = 1;
						}

						my $padding = 9;
						$padding -= length($count);
						printf("%*s%d",$padding,' ',$count);
					}
					printf(":$p_op: ", $files[$file_i]{'data'}{'line'}[$line_i]{'line_num'});
					print "$files[$file_i]{'tag'}\n";
				}
			}elsif($type eq 'branch') {
				# branch information
				my $count = $files[$file_i]{'data'}{'line'}[$line_i]{'count'};
				if($opt_combined) {
					$count_sum += $count;
				}else {
					if($count == 0) {
						print "branch $files[$file_i]{'data'}{'line'}[$line_i]{'num'} never executed:$files[$file_i]{'tag'}\n";
					}else {
						print "branch $files[$file_i]{'data'}{'line'}[$line_i]{'num'} taken $count%:$files[$file_i]{'tag'}\n";
					}
				}
			}elsif($type eq 'call') {
				# call information
				my $count = $files[$file_i]{'data'}{'line'}[$line_i]{'count'};

				if($opt_combined) {
					$count_sum += $count;
				}else {
					if($count == 0) {
						print "call $files[$file_i]{'data'}{'line'}[$line_i]{'num'} never executed:$files[$file_i]{'tag'}\n";
					}else {
						print "call $files[$file_i]{'data'}{'line'}[$line_i]{'num'} returns $count%:$files[$file_i]{'tag'}\n";
					}
				}
			}
		}

		if($opt_combined) {
			# if the combined coverage flag is set then no information is printed in the above for loop
			# count information is summed into $count_sum and printed on a single line

			my $type = $files[0]{'data'}{'line'}[$line_i]{'type'};

			no warnings 'uninitialized';    # XXX: remove warining for $type
			if($type eq 'no_ex' || $type eq 'code') {
				# line is either non-executable or executable code

				if($never_exec) {
					# code line is not executable in any file

					printf("$ftab-:$p_op:%s\n", $files[0]{'data'}{'line'}[$line_i]{'line_num'}, $files[0]{'data'}{'line'}[$line_i]{'raw'});
				}else {

					# line is executable in at least one file

					if($count_sum == 0) {
						print '    #####';
					}else {
						my $padding = 9;
						$padding -= length($count_sum);
						printf("%*s%d",$padding,' ',$count_sum);
					}
					printf(":$p_op:%s\n", $files[0]{'data'}{'line'}[$line_i]{'line_num'}, $files[0]{'data'}{'line'}[$line_i]{'raw'});
				}
			}elsif($type eq 'branch') {
				# branch information
				if($count_sum == 0) {
					print "branch $files[0]{'data'}{'line'}[$line_i]{'num'} never executed\n";
				}else {
					my $percentage = $count_sum / $file_count;
					print "branch $files[0]{'data'}{'line'}[$line_i]{'num'} taken $percentage%\n";
				}
			}elsif($type eq 'call') {
				# call information

				if($count_sum == 0) {
					print "call $files[0]{'data'}{'line'}[$line_i]{'num'} never executed\n";
				}else {
					my $percentage = $count_sum / $file_count;
					print "call $files[0]{'data'}{'line'}[$line_i]{'num'} returns $percentage%\n";
				}
			}
		}
	}
}

# Summary: 	prints the line execution percentages for each file and a percentage for all files combined
# Parameters: 	none
# Return: 	none
sub print_summary()
{
	for(my $file_i=0;$file_i<$file_count;$file_i++) {
		my $file_line_count = $files[$file_i]{'data'}{'line_count'};
		my $percentage;
		if($file_line_count) {
			$percentage = ($files[$file_i]{'data'}{'execution_count'} / $file_line_count) * 100;
		}else {
			$percentage = 0;
		}
		$percentage = sprintf('%.2f',$percentage);
		print "$percentage% of $file_line_count lines executed on target $files[$file_i]{'tag'}\n";
	}
	my $overall_percentage;
	if($executable_total) {
		$overall_percentage = ($executed_total / $executable_total) * 100;
	}else {
		$overall_percentage = 0;
	}
	$overall_percentage = sprintf('%.2f',$overall_percentage);
	print "$overall_percentage% of $executable_total lines executed across all files\n";
}

# Summary: 	print tool usage information
# Parameters: 	none
# Return: 	none
sub print_usage()
{
	print <<END_USAGE;
Usage: ./$tool_name [options] <file name> <target id> <file name> <target id> [file name] [target id]...
Use $tool_name to compare gcov files generated on different platforms or targets.
  -h, --help                      Print this help, then exit
  -v, --version                   Print version number, then exit
Input Options:
  -t, --tagfile                   Take file-tag assignments from a file, not from command-line
Ouput Options:
  -c, --combined		  Print combined coverage
  -n, --no-summmary		  Do not print summary
END_USAGE
}
