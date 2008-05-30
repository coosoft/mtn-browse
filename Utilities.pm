##############################################################################
#
#   File Name    - Utilities.pm
#
#   Description  - The utilities module for the mtn-browse application. This
#                  module contains assorted general purpose routines used
#                  throughout the application.
#
#   Author       - A.E.Cooper.
#
#   Legal Stuff  - Copyright (c) 2007 Anthony Edward Cooper
#                  <aecooper@coosoft.plus.com>.
#
#                  This program is free software; you can redistribute it
#                  and/or modify it under the terms of the GNU General Public
#                  License as published by the Free Software Foundation;
#                  either version 3 of the License, or (at your option) any
#                  later version.
#
#                  This program is distributed in the hope that it will be
#                  useful, but WITHOUT ANY WARRANTY; without even the implied
#                  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#                  PURPOSE. See the GNU General Public License for more
#                  details.
#
#                  You should have received a copy of the GNU General Public
#                  License along with this software; if not, write to the Free
#                  Software Foundation, Inc., 59 Temple Place - Suite 330,
#                  Boston, MA 02111-1307 USA.
#
##############################################################################
#
##############################################################################
#
#   Global Data For This Module
#
##############################################################################



# ***** DIRECTIVES *****

require 5.008;

use strict;

# ***** FUNCTIONAL PROTOTYPES FOR THIS FILE *****

# Public routines.

sub colour_to_string($);
sub create_format_tags($);
sub data_is_binary($);
sub generate_revision_report($$$$;$);
sub generate_tmp_path($);
sub get_branch_revisions($$$$$);
sub get_dir_contents($$$);
sub get_revision_ids($$);
sub glade_signal_autoconnect($$);
sub gtk2_update();
sub hex_dump($);
sub open_database($$$);
sub run_command($@);
sub set_label_value($$);
#
##############################################################################
#
#   Routine      - generate_revision_report
#
#   Description  - Populate the specified Gtk2::TextBuffer with a pretty
#                  printed report on the specified revision.
#
#   Data         - $text_buffer      : The Gtk2::TextBuffer that is to be
#                                      populated.
#                  $revision_id      : The id of the revision being reported
#                                      on.
#                  $certs_list       : A reference to a certs list as returned
#                                      by $mtn->certs().
#                  $colour           : One of "red, "green" or "" depending
#                                      upon the desired colour of the text.
#                  $revision_details : Either a reference to a revision
#                                      details list as returned by
#                                      $mtn->get_revision() if a detailed
#                                      report is to be generated or undef if
#                                      the report is to just be a summary.
#
##############################################################################



sub generate_revision_report($$$$;$)
{

    my($text_buffer, $revision_id, $certs_list, $colour, $revision_details)
	= @_;

    my($bold,
       $change_log,
       $italics,
       $manifest_id,
       $normal,
       @parent_revision_ids,
       %revision_data,
       %seen,
       @unique);
    my @types = (__("Added"),
		 __("Removed"),
		 __("Changed"),
		 __("Renamed"),
		 __("Attributes"));

    # Sort out colour attributes.

    if ($colour ne "")
    {
	$normal = $colour;
	$bold = "bold-" . $colour;
	$italics = "italics-" . $colour;
    }
    else
    {
	$normal = "normal";
	$bold = "bold";
	$italics = "italics";
    }

    # Revision id.

    $text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					   __("Revision id: "),
					   $bold);
    $text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					   $revision_id . "\n\n",
					   $normal);

    # Certs.

    foreach my $cert (@$certs_list)
    {
	if ($cert->{name} eq "changelog")
	{
	    $change_log = $cert->{value};
	    $change_log =~ s/\s+$//os;
	}
	else
	{
	    $cert->{value} =~ s/T/ /o if ($cert->{name} eq "date");
	    $text_buffer->insert_with_tags_by_name
		($text_buffer->get_end_iter(),
		 sprintf("%s:\t", ucfirst($cert->{name})),
		 $bold);
	    $text_buffer->insert_with_tags_by_name
		($text_buffer->get_end_iter(),
		 sprintf("%s\n", $cert->{value}),
		 $normal);
	}
    }

    # Change log.

    $text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					   __("\nChange Log:\n"),
					   $bold);
    $text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					   sprintf("%s", $change_log),
					   $normal);

    # The rest is only provided if it is a detailed report.

    if (defined($revision_details))
    {

	# Revision details.

	$text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					       __("\n\nChanges Made:\n"),
					       $bold);
	foreach my $type (@types)
	{
	    $revision_data{$type} = [];
	}
	foreach my $change (@$revision_details)
	{
	    if ($change->{type} eq "add_dir")
	    {
		push(@{$revision_data{__("Added")}}, $change->{name} . "/");
	    }
	    elsif ($change->{type} eq "add_file")
	    {
		push(@{$revision_data{__("Added")}}, $change->{name});
	    }
	    elsif ($change->{type} eq "delete")
	    {
		push(@{$revision_data{__("Removed")}}, $change->{name});
	    }
	    elsif ($change->{type} eq "patch")
	    {
		push(@{$revision_data{__("Changed")}}, $change->{name});
	    }
	    elsif ($change->{type} eq "rename")
	    {
		push(@{$revision_data{__("Renamed")}},
		     $change->{from_name} . " -> " . $change->{to_name});
	    }
	    elsif ($change->{type} eq "clear")
	    {
		push(@{$revision_data{__("Attributes")}},
		     __x("{name}: {attribute} was cleared",
			 name      => $change->{name},
			 attribute => $change->{attribute}));
	    }
	    elsif ($change->{type} eq "set")
	    {
		push(@{$revision_data{__("Attributes")}},
		     sprintf("%s: %s = %s",
			     $change->{name},
			     $change->{attribute},
			     $change->{value}));
	    }
	    elsif ($change->{type} eq "old_revision")
	    {
		push(@parent_revision_ids, $change->{revision_id});
	    }
	    elsif ($change->{type} eq "new_manifest")
	    {
		$manifest_id = $change->{manifest_id};
	    }
	}
	foreach my $type (@types)
	{
	    if (scalar(@{$revision_data{$type}}) > 0)
	    {
		$text_buffer->insert_with_tags_by_name
		    ($text_buffer->get_end_iter(),
		     "    " . $type . ":\n",
		     $italics);
		%seen = ();
		@unique = sort(grep { ! $seen{$_} ++ }
			       @{$revision_data{$type}});
		foreach my $line (@unique)
		{
		    $text_buffer->insert_with_tags_by_name
			($text_buffer->get_end_iter(),
			 "\t" . $line . "\n",
			 $normal);
		}
	    }
	}

	# Parent revision and manifest ids.

	$text_buffer->insert_with_tags_by_name
	    ($text_buffer->get_end_iter(),
	     __("\nParent revision id(s):\t"),
	     $bold);
	$text_buffer->insert_with_tags_by_name
	    ($text_buffer->get_end_iter(),
	     join(" ", @parent_revision_ids) . "\n",
	     $normal);
	$text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					       __("Manifest id:\t\t"),
					       $bold);
	$text_buffer->insert_with_tags_by_name($text_buffer->get_end_iter(),
					       $manifest_id,
					       $normal);

    }

}
#
##############################################################################
#
#   Routine      - generate_tmp_path
#
#   Description  - Generate a unique and temporary path for the specified file
#                  name. The file name is included in the result and will be
#                  unchanged.
#
#   Data         - $file_name   : The file name component that is to be used.
#                  Return Value : The full, unique, temporary path on success,
#                                 otherwise undef on failure.
#
##############################################################################



sub generate_tmp_path($)
{

    my $file_name = $_[0];

    my($path,
       $i);

    # Loop through looking for a temporary subdirectory not containing the
    # specified file.

    for ($i = 0; ; ++ $i)
    {
	if (-d ($tmp_dir . "/" . $i))
	{
	    if (! -e ($path = $tmp_dir . "/" . $i . "/" . $file_name))
	    {
		return $path;
	    }
	}
	else
	{
	    return unless mkdir($tmp_dir . "/" . $i);
	    return $tmp_dir . "/" . $i . "/" . $file_name;
	}
    }

    return;

}
#
##############################################################################
#
#   Routine      - run_command
#
#   Description  - Run the specified command and return its output.
#
#   Data         - $buffer      : A reference to the buffer that is to contain
#                                 the output from the command.
#                  $args        : A list containing the command to run and its
#                                 arguments.
#                  Return Value : True if the command worked, otherwise false
#                                 if something went wrong.
#
##############################################################################



sub run_command($@)
{

    my($buffer, @args) = @_;

    my(@err,
       $fd_err,
       $fd_in,
       $fd_out,
       $pid,
       $ret_val,
       $status,
       $stop,
       $total_bytes,
       $watcher);

    # Run the command.

    $fd_err = gensym();
    eval
    {
	$pid = open3($fd_in, $fd_out, $fd_err, @args);
    };
    if ($@ ne "")
    {
	my $dialog = Gtk2::MessageDialog->new
	    (undef,
	     ["modal"],
	     "warning",
	     "close",
	     __x("The {name} subprocess could not start,\n",
		 name => Glib::Markup::escape_text($args[0]))
	         . __x("the system gave:\n<b><i>{error_message}</b></i>",
		       error_message => Glib::Markup::escape_text($@)));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Setup a watch handler to get read our data and handle GTK2 events whilst
    # the command is running.

    $stop = $total_bytes = 0;
    $$buffer = "";
    $watcher = Gtk2::Helper->add_watch
	(fileno($fd_out), "in",
	 sub {
	     my $bytes_read;
	     if (($bytes_read = sysread($fd_out,
					$$buffer,
					32768,
					$total_bytes))
		 == 0)
	     {
		 $stop = 1;
	     }
	     else
	     {
		 $total_bytes += $bytes_read;
	     }
	     return TRUE;
	 });
    while (! $stop)
    {
	Gtk2->main_iteration();
    }
    Gtk2::Helper->remove_watch($watcher);

    # Get any error output.

    @err = readline($fd_err);

    close($fd_in);
    close($fd_out);
    close($fd_err);

    # Reap the process and deal with any errors.

    if (($ret_val = waitpid($pid, 0)) == -1)
    {
	if ($! != ECHILD)
	{
	    my $dialog = Gtk2::MessageDialog->new_with_markup
		(undef,
		 ["modal"],
		 "warning",
		 "close",
		 __x("waitpid failed with:\n<b><i>{error_message}</i></b>",
		     error_message => Glib::Markup::escape_text($!)));
	    $dialog->run();
	    $dialog->destroy();
	    return;
	}
    }
    $status = $?;
    if (WIFEXITED($status) && WEXITSTATUS($status) != 0)
    {
	my $dialog = Gtk2::MessageDialog->new_with_markup
	    (undef,
	     ["modal"],
	     "warning",
	     "close",
	     __x("The {name} subprocess failed with an exit status\n",
		 name => Glib::Markup::escape_text($args[0]))
	         . __x("of {exit_code} and printed the following on stderr:\n",
		       exit_code => WEXITSTATUS($status))
	         . __x("<b><i>{error_message}</i></b>",
		       error_message =>
		           Glib::Markup::escape_text(join("", @err))));
	$dialog->run();
	$dialog->destroy();
	return;
    }
    elsif (WIFSIGNALED($status))
    {
	my $dialog = Gtk2::MessageDialog->new
	    (undef,
	     ["modal"],
	     "warning",
	     "close",
	     __x("The {name} subprocess was terminated by signal {number}.",
		 name   => Glib::Markup::escape_text($args[0]),
		 number => WTERMSIG($status)));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    return 1;

}
#
##############################################################################
#
#   Routine      - get_dir_contents
#
#   Description  - Given a path and a Monotone manifest, return a subset of
#                  the manifest that represents the contents of just that
#                  directory along with the directory entry names.
#
#   Data         - $path     : The path to the directory from the top level of
#                              the manifest.
#                  $manifest : A reference to a Monotone manifest.
#                  $result   : A reference to a list that is to contain the
#                              result (a list of records containing the short
#                              directory entry name and a reference to the
#                              related manifest entry).
#
##############################################################################



sub get_dir_contents($$$)
{

    my($path, $manifest, $result) = @_;

    my($entry,
       $extract_re,
       $i,
       $match_re,
       $name);

    $i = 0;
    if ($path eq "")
    {
	$match_re = qr/^[^\/]+$/;
	$extract_re = qr/^([^\/]+)$/;
    }
    else
    {
	$match_re = qr/^${path}\/[^\/]+$/;
	$extract_re = qr/^${path}\/([^\/]+)$/;
    }
    @$result = ();
    foreach $entry (@$manifest)
    {
	if ($entry->{name} =~ m/$match_re/)
	{
	    ($name) = ($entry->{name} =~ m/$extract_re/);
	    $$result[$i ++] = {manifest_entry => $entry,
			       name           => $name};
	}
    }

}
#
##############################################################################
#
#   Routine      - open_database
#
#   Description  - Allows the user to select a Monotone Database and then
#                  opens it, making sure that it is a valid database or
#                  dealing with the consequences if it isn't.
#
#   Data         - $parent      : The parent window for any dialogs that are
#                                 to be displayed.
#                  $mtn         : A reference to a variable that is to contain
#                                 the newly created Monotone::AutomateStdio
#                                 object. This parameter can be undef if the
#                                 object is not required.
#                  $file_name   : A reference to a variable that is to contain
#                                 the full file name of the selected database.
#                                 This parameter can be undef if the file name
#                                 is not required.
#                  Return Value : True on success, otherwise false on
#                                 cancellation.
#
##############################################################################



sub open_database($$$)
{

    my($parent, $mtn, $file_name) = @_;

    my($chooser_dialog,
       $done,
       $ret_val);

    $chooser_dialog = Gtk2::FileChooserDialog->new(__("Open Database"),
						   $parent,
						   "open",
						   "gtk-cancel" => "cancel",
						   "gtk-open" => "ok");

    do
    {
	if ($chooser_dialog->run() eq "ok")
	{

	    my ($err,
		$fh,
		$fname,
		$mtn_obj);

	    $fname = $chooser_dialog->get_filename();

	    # The user has selected a file. First make sure we can open it for
	    # reading (I know I could use the -r test but this takes care of
	    # any other unforeseen access problems as well).

	    if (! defined($fh = IO::File->new($fname, "r")))
	    {
		my $dialog = Gtk2::MessageDialog->new
		    ($parent,
		     ["modal"],
		     "warning",
		     "close",
		     $! . ".");
		$dialog->run();
		$dialog->destroy();
	    }
	    else
	    {

		$fh->close();
		$fh = undef;

		# Ok it is a readable file, try and open it but deal with any
		# errors in a nicer way than normal.

		Monotone::AutomateStdio->register_error_handler("both");
		eval
		{
		    $mtn_obj = Monotone::AutomateStdio->new($fname);
		};
		$err = $@;
		Monotone::AutomateStdio->register_error_handler
		    ("both", \&mtn_error_handler);
		if ($err ne "")
		{
		    my $dialog = Gtk2::MessageDialog->new
			($parent,
			 ["modal"],
			 "warning",
			 "close",
			 __("Not a valid Monotone database."));
		    $dialog->run();
		    $dialog->destroy();
		}
		else
		{

		    # Seems to be ok so tell the caller.

		    $$mtn = $mtn_obj if (defined($mtn));
		    $$file_name = $fname if (defined($file_name));
		    $done = $ret_val = 1;

		}

	    }

	}
	else
	{
	    $done = 1;
	}
    }
    while (! $done);

    $chooser_dialog->destroy();

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - get_branch_revisions
#
#   Description  - Get a list of revision ids or tags for the specified branch
#                  that take into account the user's preferences for ordering
#                  and the maximum number of revisions to display.
#
#   Data         - $mtn       : The Monotone database handle that is to be
#                               used.
#                  $branch    : The name of the branch that revisions are to
#                               be found for.
#                  $tags      : True if the list of revisions are to be tags,
#                               otherwise false if they are to be ids.
#                  $appbar    : If defined, the application progress bar
#                               widget that is to be updated with the progress
#                               of this operation. It is assumed that the
#                               progress is set at 0 and will end up being set
#                               to 1.
#                  $revisions : A reference to a list that is to contain the
#                               resultant list of sorted revision tags or ids.
#
##############################################################################



sub get_branch_revisions($$$$$)
{

    my($mtn, $branch, $tags, $appbar, $revisions) = @_;

    @$revisions = ();

    if ($tags)
    {

	my(@certs,
	   @list,
	   %seen);

	# Get the list of revision tags.

	$mtn->tags(\@list, $branch);
	$appbar->set_progress_percentage(0.5) if (defined($appbar));
	gtk2_update();

	# Dedupe it.

	@list = grep({ ! $seen{$_->{tag}} ++ } @list);

	# Sort it by date if necessary (because it needs to be truncated or
	# that's how the user wants it sorted).

	if (($user_preferences->{query}->{tagged}->{limit} > 0
	     && scalar(@list) > $user_preferences->{query}->{tagged}->{limit})
	    || $user_preferences->{query}->{tagged}->{sort_cronologically})
	{
	    @list = sort({
			     foreach my $rec ($a, $b)
			     {
				 if (! exists($rec->{date}))
				 {
				     $mtn->certs(\@certs,
						 $rec->{revision_id});
				     foreach my $cert (@certs)
				     {
					 if ($cert->{name} eq "date")
					 {
					     $rec->{date} = $cert->{value};
					     last;
					 }
				     }
				 }
			     }
			     $b->{date} cmp $a->{date};
			 }
			 @list);
	}

	# Truncate the list if necessary.

	if ($user_preferences->{query}->{tagged}->{limit} > 0
	    && scalar(@list) > $user_preferences->{query}->{tagged}->{limit})
	{
	    splice(@list, $user_preferences->{query}->{tagged}->{limit});
	}

	# Extract the list of tags.

	@$revisions = map({ $_->{tag} } @list);

	# Sort alphabetically if required.

	@$revisions = sort(@$revisions)
	    if (! $user_preferences->{query}->{tagged}->{sort_cronologically});

    }
    else
    {

	# Get the list of revision ids.

	$mtn->select($revisions, "b:" . $branch);

	# Does it need truncating?

	if ($user_preferences->{query}->{id}->{limit} == 0
	    || scalar(@$revisions)
	        <= $user_preferences->{query}->{id}->{limit})
	{

	    # No so simply sort it.

	    if ($user_preferences->{query}->{id}->{sort_cronologically})
	    {
		$appbar->set_progress_percentage(0.33) if (defined($appbar));
		gtk2_update();
		$mtn->toposort($revisions, @$revisions);
		$appbar->set_progress_percentage(0.66) if (defined($appbar));
		gtk2_update();
		@$revisions = reverse(@$revisions);
	    }
	    else
	    {
		$appbar->set_progress_percentage(0.5) if (defined($appbar));
		gtk2_update();
		@$revisions = sort(@$revisions);
	    }

	}
	else
	{

	    # Yes so truncate and then sort it.

	    $appbar->set_progress_percentage(0.33) if (defined($appbar));
	    gtk2_update();
	    $mtn->toposort($revisions, @$revisions);
	    $appbar->set_progress_percentage(0.66) if (defined($appbar));
	    splice(@$revisions,
		   0,
		   scalar(@$revisions)
		       - $user_preferences->{query}->{id}->{limit});
	    if ($user_preferences->{query}->{id}->{sort_cronologically})
	    {
		@$revisions = reverse(@$revisions);
	    }
	    else
	    {
		@$revisions = sort(@$revisions);
	    }

	}

    }

    $appbar->set_progress_percentage(1) if (defined($appbar));
    gtk2_update();

}
#
##############################################################################
#
#   Routine      - get_revision_ids
#
#   Description  - Return the currently selected revision id, whether this is
#                  specified via a tag or as a revision id.
#
#   Data         - $instance     : The window instance.
#                  $revision_ids : The list of selected revision ids. Normally
#                                  the list will have at most one element but
#                                  may contain more if the tag isn't unique on
#                                  the current branch.
#
##############################################################################



sub get_revision_ids($$)
{

    my($instance, $revision_ids) = @_;

    @$revision_ids=();
    return unless ($instance->{revision_combo_details}->{complete});
    if ($instance->{tagged_checkbutton}->get_active())
    {
	$instance->{mtn}->
	    select($revision_ids,
		   "t:" . $instance->{revision_combo_details}->{value});
    }
    else
    {
	push(@$revision_ids, $instance->{revision_combo_details}->{value});
    }

}
#
##############################################################################
#
#   Routine      - create_format_tags
#
#   Description  - Creates the Gtk2::TextBuffer tags that are used to pretty
#                  print stuff.
#
#   Data         - $text_view : The GTK2::TextBuffer widget that is to have
#                               its tags created.
#
##############################################################################



sub create_format_tags($)
{

    my $text_buffer = $_[0];

    my $colours = $user_preferences->{colours};

    # Normal Black text, assorted styles, on a white background.

    $text_buffer->create_tag("normal", "weight" => PANGO_WEIGHT_NORMAL);

    $text_buffer->create_tag("bold", "weight" => PANGO_WEIGHT_BOLD);
    $text_buffer->create_tag("italics", "style" => "italic");
    $text_buffer->create_tag("bold-italics",
			     "weight" => PANGO_WEIGHT_BOLD,
			     "style" => "italic");

    # Set up the colour and style schemes for file comparison and annotation.

    foreach my $i (1 .. 2)
    {
	my $clr = $user_preferences->{colours}->{"cmp_revision_" . $i};
	$text_buffer->create_tag("compare-" . $i,
				 "foreground" => $clr->{fg});
	$text_buffer->create_tag("bold-compare-" . $i,
				 "weight" => PANGO_WEIGHT_BOLD,
				 "foreground" => $clr->{fg});
	$text_buffer->create_tag("italics-compare-" . $i,
				 "style" => "italic",
				 "foreground" => $clr->{fg});
	$text_buffer->create_tag("bold-italics-compare-" . $i,
				 "weight" => PANGO_WEIGHT_BOLD,
				 "style" => "italic",
				 "foreground" => $clr->{fg});
	$text_buffer->create_tag("compare-file-" . $i,
				 "foreground" => $clr->{fg},
				 "background" => $clr->{bg});
	$text_buffer->create_tag("compare-file-info-" . $i,
				 "weight" => PANGO_WEIGHT_BOLD,
				 "foreground" => $clr->{hl},
				 "background" => "DarkSlateGrey");
	foreach my $prefix ("annotate_prefix_", "annotate_text_")
	{
	    my $tag = $prefix;
	    $tag =~ s/_/-/go;
	    $clr = $user_preferences->{colours}->{$prefix . $i};
	    $text_buffer->create_tag($tag . $i,
				     "foreground" => $clr->{fg},
				     "background" => $clr->{bg});
	}
    }

    # Yellow text on a grey background.

    $text_buffer->create_tag("compare-info",
			     "foreground" => "Yellow",
			     "background" => "LightSlateGrey");

}
#
##############################################################################
#
#   Routine      - hex_dump
#
#   Description  - Generates a hexadecimal dump of the specified data.
#
#   Data         - $data        : A reference to the data that is to be hex
#                                 dumped.
#                  Return Value : A reference to the resultant hex dump as a
#                                 string.
#
##############################################################################



sub hex_dump($)
{

    my $data = $_[0];

    my ($buffer,
	$counter,
	@line);

    $counter = 0;
    foreach my $byte (split(//, $$data))
    {
	++ $counter;
	push(@line, $byte);
	$buffer .= sprintf("%02X ", ord($byte));
	$buffer .= " " if (($counter % 8) == 0);
	if (($counter % 16) == 0)
	{
	    foreach my $byte2 (@line)
	    {
		$buffer .= ($byte2 =~ m/[[:print:]]/) ? (" " . $byte2) : " .";
	    }
	    $buffer .= "\n";
	    @line = ();
	}
    }

    # If the last line is incomplete then finish it off.

    if (scalar(@line) > 0)
    {
	$buffer .= "   " x (16 - scalar(@line));
	$buffer .= " " if (scalar(@line) < 8);
	$buffer .= " ";
	foreach my $byte2 (@line)
	{
	    $buffer .= ($byte2 =~ m/[[:print:]]/) ? (" " . $byte2) : " .";
	}
	$buffer .= "\n";
    }

    return \$buffer;

}
#
##############################################################################
#
#   Routine      - data_is_binary
#
#   Description  - Determines whether the specified string contains binary
#                  data.
#
#   Data         - $data        : A reference to the data that is to be
#                                 tested.
#                  Return Value : True if the data is binary, otherwise false
#                                 if it is predominantly textual.
#
##############################################################################



sub data_is_binary($)
{

    my $data = $_[0];

    my $non_printable;

    $non_printable = grep(/[^[:print:][:space:]]/, split(//, $$data));

    return 1 if (((100 * $non_printable) / length($$data)) > 20);

    return;

}
#
##############################################################################
#
#   Routine      - colour_to_string
#
#   Description  - Returns a string representing the specified
#                  Gtk2::Gdk::Color value.
#
#   Data         - $colour      : A Gtk2::Gdk::Color object.
#                  Return Value : A string containing the colour value.
#
##############################################################################



sub colour_to_string($)
{

    my $colour = $_[0];

    return sprintf("#%02X%02X%02X",
		   ($colour->red() >> 8) & 0xff,
		   ($colour->green() >> 8) & 0xff,
		   ($colour->blue() >> 8) & 0xff);

}
#
##############################################################################
#
#   Routine      - set_label_value
#
#   Description  - Set the text for the given label and the tooltip for the
#                  parent widget, assumed to be an event box, to the specified
#                  text.
#
#   Data         - $widget : The label widget that has an event box as its
#                            parent.
#                  $value  : The text that the label and tooltip are to be set
#                            to.
#
##############################################################################



sub set_label_value($$)
{

    my($widget, $value) = @_;

    $widget->set_text($value);
    $tooltips->set_tip($widget->parent(), $value);

}
#
##############################################################################
#
#   Routine      - glade_signal_autoconnect
#
#   Description  - This routine uses the Glade library to connect up all the
#                  registered signal handlers to their related widgets.
#
#   Data         - $glade       : The Glade object describing the widgets that
#                                 are to have their signal handlers
#                                 registered.
#                  $client_data : The client data that is to be passed into
#                                 each callback routine when it is called.
#
##############################################################################



sub glade_signal_autoconnect($$)
{

    my($glade, $client_data) = @_;

    $glade->signal_autoconnect
	(sub {
	     my($callback_name, $widget, $signal_name, $signal_data,
		$connect_object, $after, $user_data) = @_;
	     my $func = $after ? "signal_connect_after" : "signal_connect";
	     $widget->$func($signal_name,
			    $callback_name,
			    $connect_object ? $connect_object : $user_data); },
	 $client_data);

}
#
##############################################################################
#
#   Routine      - gtk2_update
#
#   Description  - Process all outstanding Gtk2 toolkit events. This is used
#                  to update the GUI whilst the application is busy doing
#                  something.
#
#   Data         - None.
#
##############################################################################



sub gtk2_update()
{

    return if (Gtk2->main_level() == 0);
    while (Gtk2->events_pending())
    {
	Gtk2->main_iteration();
    }

}

1;
