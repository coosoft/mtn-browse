##############################################################################
#
#   File Name    - History.pm
#
#   Description  - The history module for the mtn-browse application. This
#                  module contains all the routines for implementing the
#                  revision and file history windows.
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

require 5.008005;

use locale;
use strict;
use warnings;
no warnings qw(recursion);

# ***** GLOBAL DATA DECLARATIONS *****

# Constants for the columns within the comparison files ListStore widget.

use constant CLS_FILE_NAME_COLUMN => 0;
use constant CLS_LINE_NR_COLUMN   => 1;
use constant CLS_FILE_ID_1_COLUMN => 2;
use constant CLS_FILE_ID_2_COLUMN => 3;

# The translated history strings.

my $__annotate_file         = __("Annotate File");
my $__annotate_file_ttip    = __("Annotate the file in a\n"
                                 . "new annotation window");
my $__browse_file           = __("Browse File");
my $__browse_file_ttip      = __("Browse the file in\na new browser window");
my $__browse_rev            = __("Browse Revision");
my $__browse_rev_ttip       = __("Browse the revision in\n"
                                 . "a new browser window");
my $__full_changelog        = __("Full Change Log");
my $__full_changelog_ttip   = __("View the revision's full change log");
my $__select_id_1           = __("Select As Id 1");
my $__select_id_2           = __("Select As Id 2");
my $__select_id_file_1_ttip = __("Select this file revision for\n"
                                 . "comparison as the first file");
my $__select_id_file_2_ttip = __("Select this file revision for\n"
                                 . "comparison as the second file");
my $__select_id_rev_1_ttip  = __("Select this revision for comparison\n"
                                 . "as the first revision");
my $__select_id_rev_2_ttip  = __("Select this revision for comparison\n"
                                 . "as the second revision");

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_arbitrary_revision_comparison($);
sub display_file_change_history($$$);
sub display_renamed_file_comparison($$$$$$);
sub display_revision_change_history($$$);
sub display_revision_comparison($$$;$);

# Private routines.

sub compare_arbitrary_revision_advanced_find_button_clicked_cb($$);
sub compare_button_clicked_cb($$);
sub comparison_revision_change_log_button_clicked_cb($$);
sub external_diffs($$$$$$);
sub external_diffs_button_clicked_cb($$);
sub file_comparison_combobox_changed_cb($$);
sub generate_history_report($$);
sub get_compare_arbitrary_revisions_window();
sub get_file_history_helper($$$);
sub get_history_window();
sub get_revision_comparison_window($);
sub get_revision_history_helper($$);
sub history_list_button_clicked_cb($$);
sub mtn_diff($$$$$;$);
sub restrict_to_combobox_changed_cb($$);
sub save_differences_button_clicked_cb($$);
#
##############################################################################
#
#   Routine      - display_revision_change_history
#
#   Description  - Display a revision's change history, complete with
#                  selection and comparison buttons.
#
#   Data         - $mtn         : The Monotone::AutomateStdio object that is
#                                 to be used to get the change history.
#                  $tag         : Either a tag name for the specified revision
#                                 that is to be used in the window title
#                                 instead of the revision id or undef if the
#                                 revision id should be used.
#                  $revision_id : The id of the revision that is to have its
#                                 change log displayed.
#
##############################################################################



sub display_revision_change_history($$$)
{

    my ($mtn, $tag, $revision_id) = @_;

    my $instance;
    my $wm = WindowManager->instance();

    $instance = get_history_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{file_name} = undef;
    $instance->{first_revision_id} = "";
    $instance->{second_revision_id} = "";
    $instance->{window}->set_title(__x("Revision History For {rev}",
                                       rev => defined($tag) ?
                                           $tag : $revision_id));
    $instance->{history_label}->set_markup(__("<b>Revision History</b>"));
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();

    # Get the list of file change revisions. Remember to include the current
    # revision in the history.

    $instance->{appbar}->set_status(__("Fetching revision list"));
    $wm->update_gui();
    $instance->{revision_hits} = {};
    get_revision_history_helper($instance, $revision_id);

    # Sort the list.

    $instance->{appbar}->set_progress_percentage(0.5);
    $instance->{appbar}->set_status(__("Sorting revision list"));
    $wm->update_gui();
    $instance->{history} = [];
    $instance->{mtn}->toposort($instance->{history},
                               keys(%{$instance->{revision_hits}}));
    $instance->{revision_hits} = undef;
    @{$instance->{history}} = reverse(@{$instance->{history}});
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Display the file's history.

    $instance->{branch_history} = undef;
    generate_history_report($instance, $instance->{history});

    $instance->{stop_button}->set_sensitive(FALSE);
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - display_file_change_history
#
#   Description  - Display a file's change history, complete with selection
#                  and comparison buttons.
#
#   Data         - $mtn         : The Monotone::AutomateStdio object that is
#                                 to be used to get the change history.
#                  $revision_id : The revision id in which the desired version
#                                 of the file resides.
#                  $file_name   : The name of the file that is to have its
#                                 change log displayed.
#
##############################################################################



sub display_file_change_history($$$)
{

    my ($mtn, $revision_id, $file_name) = @_;

    my $instance;
    my $wm = WindowManager->instance();

    $instance = get_history_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{file_name} = $file_name;
    $instance->{first_revision_id} = "";
    $instance->{second_revision_id} = "";
    $instance->{window}->set_title(__x("File History For {file}",
                                       file => $instance->{file_name}));
    $instance->{history_label}->set_markup(__("<b>File History</b>"));
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();

    # Get the list of file change revisions. Remember that a warning is
    # generated when one goes back beyond a file's addition revision, so
    # temporarily disable the warning handler.

    $instance->{appbar}->set_status(__("Fetching revision list"));
    $wm->update_gui();
    $instance->{revision_hits} = {};
    {
        local $suppress_mtn_warnings = 1;
        get_file_history_helper($instance,
                                $revision_id,
                                \$instance->{file_name});
    }

    # Sort the list.

    $instance->{appbar}->set_progress_percentage(0.5);
    $instance->{appbar}->set_status(__("Sorting revision list"));
    $wm->update_gui();
    $instance->{history} = [];
    $instance->{mtn}->toposort($instance->{history},
                               keys(%{$instance->{revision_hits}}));
    @{$instance->{history}} = reverse(@{$instance->{history}});
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Display the file's history.

    $instance->{branch_history} = undef;
    generate_history_report($instance, $instance->{history});

    $instance->{stop_button}->set_sensitive(FALSE);
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - display_revision_comparison
#
#   Description  - Compares and then displays the differences between the two
#                  specified revisions, optionally restricting it to the
#                  specified file.
#
#   Data         - $mtn           : The Monotone::AutomateStdio object that is
#                                   to be used to do the comparison.
#                  $revision_id_1 : The first revision id that is to be
#                                   compared.
#                  $revision_id_2 : Either the second revision id that is to be
#                                   compared or undef if the current workspace
#                                   is to be used.
#                  $file_name     : Either the name of the file that the
#                                   revision comparison should be restricted
#                                   to or undef for a full revision
#                                   comparison.
#
##############################################################################



sub display_revision_comparison($$$;$)
{

    my ($mtn, $revision_id_1, $revision_id_2, $file_name) = @_;

    my (@files,
        $i,
        $instance,
        $iter);
    my $wm = WindowManager->instance();

    $instance = get_revision_comparison_window($mtn);
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{revision_id_1} = $revision_id_1;
    $instance->{revision_id_2} = $revision_id_2;
    $instance->{window}->
        set_title(__x("{type} Differences Between {rev_1} and {rev_2}",
                      type  => defined($file_name)
                                   ? __("File") : __("Revision"),
                      rev_1 => $revision_id_1,
                      rev_2 => defined($instance->{revision_id_2})
                                   ? $revision_id_2 : __("Workspace")));
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();

    # Get Monotone to do the comparison, later versions can do this via
    # automate stdio.

    $instance->{appbar}->set_status(__("Calculating differences"));
    $wm->update_gui();
    if ($instance->{mtn}->supports(MTN_CONTENT_DIFF_EXTRA_OPTIONS))
    {

        local $instance->{kill_mtn_subprocess} = 1;
        local $pulse_widget = $instance->{appbar}->get_progress();

        # The stop button callback will kill off the mtn subprocess so suppress
        # any resultant errors.

        $instance->{mtn}->suppress_utf8_conversion(1);
        CachingAutomateStdio->register_error_handler
            (MTN_SEVERITY_ALL,
             sub {
                 my ($severity, $message, $instance) = @_;
                 if ($severity == MTN_SEVERITY_WARNING || ! $instance->{stop})
                 {
                     my $dialog;
                     cleanup_mtn_error_message(\$message);
                     $dialog = Gtk2::MessageDialog->new_with_markup
                         ($instance->{window},
                          ["modal"],
                          "warning",
                          "close",
                          __x("There is a problem with your comparison, "
                                      . "Monotone gave:\n"
                                  . "<b><i>{error_message}</i></b>",
                              error_message =>
                                  Glib::Markup::escape_text($message)));
                     busy_dialog_run($dialog);
                     $dialog->destroy();
                     die("Bad comparison");
                 }
             },
             $instance);
        eval
        {
            $instance->{mtn}->content_diff($instance->{diff_output},
                                           ["with-header"],
                                           $revision_id_1,
                                           $revision_id_2,
                                           $file_name);
        };
        CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
                                                     \&mtn_error_handler);
        $instance->{mtn}->suppress_utf8_conversion(0);

        # If we have aborted the comparison by killing off the mtn subprocess
        # then cleanly closedown and restart it, otherwise rethrow any raised
        # exceptions.

        if ($instance->{stop})
        {
            my $dummy;
            $instance->{mtn}->closedown();
            $instance->{mtn}->interface_version(\$dummy);
        }

    }
    else
    {
        local $pulse_widget = $instance->{appbar}->get_progress();
        mtn_diff($instance->{diff_output},
                 \$instance->{stop},
                 $instance->{mtn},
                 $revision_id_1,
                 $revision_id_2,
                 $file_name);
    }

    # Does the user want pretty printed differences output?

    if ($user_preferences->{coloured_diffs})
    {

        my ($char,
            $file_id_1,
            $file_id_2,
            $is_binary,
            $len,
            $line,
            @lines,
            $max_len,
            $name,
            $padding,
            $rest,
            $separator);

        # Yes the user wants pretty printed differences output.

        @lines = @{$instance->{diff_output}};

        # Find the longest line for future padding, having expanded tabs
        # (except for file details lines as tab is used as a separator (these
        # are expanded later)). Please note that the use of unpack un-utf8s the
        # returned strings.

        $max_len = $separator = 0;
        foreach my $line (@lines)
        {
            if ($line =~ m/^==/)
            {
                $separator = 1;
            }
            elsif ($separator)
            {
                $rest = expand(substr($line, 3)) . " ";
                $max_len = $len if (($len = length($rest)) > $max_len);
                $separator = 0 if ($line =~ m/^\+\+\+ .+$/);
            }
            else
            {
                ($char, $rest) = unpack("a1a*", $line);
                eval
                {
                    $rest = decode($file_encoding, $rest, Encode::FB_CROAK);
                };
                $rest =~ s/\s+$//;
                $rest = expand($rest);
                $max_len = $len if (($len = length($rest)) > $max_len);
                $line = $char . $rest;
            }
        }

        # Display the result, highlighting according to the diff output.
        # Remember the first two lines are just empty comment lines.

        $instance->{appbar}->
            set_status(__("Formatting and displaying differences"));
        $wm->update_gui();
        $padding = " " x $max_len;
        $line = substr(" Summary" . $padding, 0, $max_len);
        $instance->{comparison_buffer}->insert_with_tags_by_name
            ($instance->{comparison_buffer}->get_end_iter(),
             $line . "\n",
             "compare-info");
        for ($i = 1; $i < scalar(@lines); ++ $i)
        {

            # Deal with the initial comment lines that summarise the entire set
            # of differences between the revisions.

            if ($lines[$i] =~ m/^\#/)
            {
                $line = substr($lines[$i], 1);
                $instance->{comparison_buffer}->insert
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n");
            }

            # Deal with lines that introduce a new file comparison.

            elsif ($lines[$i] =~ m/^==/)
            {

                # Check for aborts.

                last if ($instance->{stop});

                # Print separator.

                $instance->{comparison_buffer}->
                    insert_pixbuf($instance->{comparison_buffer}->
                                      get_end_iter(),
                                  $line_image);
                $instance->{comparison_buffer}->
                    insert($instance->{comparison_buffer}->get_end_iter(),
                           "\n");

                # Attempt to extract the file name and its id, if this doesn't
                # work then it is probably a comment stating that the file is
                # binary.

                ++ $i;
                if ($lines[$i] =~ m/^--- (.+)\t[0-9a-f]{40}$/)
                {
                    $name = $1;
                }
                elsif (($i + 1) < scalar(@lines)
                       && $lines[$i + 1] =~ m/^\+\+\+ (.+)\t[0-9a-f]{40}$/)
                {
                    $name = $1;
                }
                else
                {
                    $name = undef;
                }
                if (defined($name))
                {
                    $is_binary = 0;
                    if ($lines[$i] =~ m/^--- .+\t([0-9a-f]{40})$/)
                    {
                        $file_id_1 = $1;
                    }
                    else
                    {
                        $file_id_1 = "";
                    }
                    if ($lines[$i + 1] =~ m/^\+\+\+ .+\t([0-9a-f]{40})$/)
                    {
                        $file_id_2 = $1;
                    }
                    else
                    {
                        $file_id_2 = "";
                    }
                }
                else
                {
                    $is_binary = 1;
                    ($name) = ($lines[$i] =~ m/^\# (.+) is binary$/);
                    $file_id_1 = $file_id_2 = "";
                }

                # Print out the details for the first file.

                $line = substr(expand(substr($lines[$i], $is_binary ? 1 : 3))
                                   . $padding,
                               0,
                               $max_len);
                $instance->{comparison_buffer}->insert_with_tags_by_name
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n",
                     "compare-file-info-1");

                # Store the file name and the starting line number so that the
                # user can later jump straight to it using the file combobox.

                $iter = $instance->{comparison_buffer}->get_end_iter();
                $iter->backward_line();
                push(@files, {file_name => $name,
                              line_nr   => $iter->get_line(),
                              file_id_1 => $file_id_1,
                              file_id_2 => $file_id_2});

                # Print out the details for the second file if there is one.

                if (! $is_binary)
                {
                    ++ $i;
                    $line = substr(expand(substr($lines[$i], 3)) . $padding,
                                   0,
                                   $max_len);
                    $instance->{comparison_buffer}->insert_with_tags_by_name
                        ($instance->{comparison_buffer}->get_end_iter(),
                         $line . "\n",
                         "compare-file-info-2");
                }

            }

            # Deal with difference context lines.

            elsif ($lines[$i] =~ m/^@@/)
            {
                $line = substr(substr($lines[$i], 2) . $padding, 0, $max_len);
                $instance->{comparison_buffer}->insert_with_tags_by_name
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n",
                     "compare-info");
            }

            # Deal with - change lines.

            elsif ($lines[$i] =~ m/^-/)
            {
                $line = substr(substr($lines[$i], 1) . $padding, 0, $max_len);
                $instance->{comparison_buffer}->insert_with_tags_by_name
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n",
                     "compare-file-1");
            }

            # Deal with + change lines.

            elsif ($lines[$i] =~ m/^\+/)
            {
                $line = substr(substr($lines[$i], 1) . $padding, 0, $max_len);
                $instance->{comparison_buffer}->insert_with_tags_by_name
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n",
                     "compare-file-2");
            }

            # Print out the rest.

            else
            {
                $line = substr($lines[$i], 1);
                $instance->{comparison_buffer}->insert
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n");
            }

            if (($i % 100) == 0)
            {
                $instance->{appbar}->set_progress_percentage
                    (($i + 1) / scalar(@lines));
                $wm->update_gui();
            }

        }

    }
    else
    {

        my ($file_id_1,
            $file_id_2,
            $is_binary,
            $name);

        # No the user wants the raw differences output.

        # Display the result, storing the locations of the files.

        $instance->{appbar}->set_status(__("Displaying differences"));
        $wm->update_gui();
        for ($i = 0; $i < scalar(@{$instance->{diff_output}}); ++ $i)
        {

            # Deal with lines that introduce a new file comparison.

            if ($instance->{diff_output}->[$i] =~ m/^==/)
            {

                # Check for aborts.

                last if ($instance->{stop});

                # Extract the file name, if this doesn't work then it is
                # probably a comment stating that the file is binary.

                ++ $i;
                if ($instance->{diff_output}->[$i] =~
                    m/^--- (.+)\t[0-9a-f]{40}$/)
                {
                    $name = $1;
                }
                elsif (($i + 1) < scalar(@{$instance->{diff_output}})
                       && $instance->{diff_output}->[$i + 1] =~
                           m/^\+\+\+ (.+)\t[0-9a-f]{40}$/)
                {
                    $name = $1;
                }
                else
                {
                    $name = undef;
                }
                if (defined($name))
                {
                    $is_binary = 0;
                    if ($instance->{diff_output}->[$i] =~
                        m/^--- .+\t([0-9a-f]{40})$/)
                    {
                        $file_id_1 = $1;
                    }
                    else
                    {
                        $file_id_1 = "";
                    }
                    if ($instance->{diff_output}->[$i + 1]
                        =~ m/^\+\+\+ .+\t([0-9a-f]{40})$/)
                    {
                        $file_id_2 = $1;
                    }
                    else
                    {
                        $file_id_2 = "";
                    }
                }
                else
                {
                    ($name) = ($instance->{diff_output}->[$i] =~
                               m/^\# (.+) is binary$/);
                    $is_binary = 1;
                    $file_id_1 = $file_id_2 = "";
                }

                # Print out the separator line that we previously skipped over.

                $instance->{comparison_buffer}->insert
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $instance->{diff_output}->[$i - 1] . "\n");

                # Print out the details for the first file.

                $instance->{comparison_buffer}->insert
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $instance->{diff_output}->[$i] . "\n");

                # Store the file name and the starting line number so that the
                # user can later jump straight to it using the file combobox.

                $iter = $instance->{comparison_buffer}->get_end_iter();
                $iter->backward_line();
                push(@files, {file_name => $name,
                              line_nr   => $iter->get_line(),
                              file_id_1 => $file_id_1,
                              file_id_2 => $file_id_2});

                # Print out the details for the second file if there is one.

                if (! $is_binary)
                {
                    ++ $i;
                    $instance->{comparison_buffer}->insert
                        ($instance->{comparison_buffer}->get_end_iter(),
                         $instance->{diff_output}->[$i] . "\n");
                }

            }

            # Print out the rest.

            else
            {
                my $line = $instance->{diff_output}->[$i];
                eval
                {
                    $line = decode($file_encoding, $line, Encode::FB_CROAK);
                };
                $instance->{comparison_buffer}->insert
                    ($instance->{comparison_buffer}->get_end_iter(),
                     $line . "\n");
            }

            if (($i % 100) == 0)
            {
                $instance->{appbar}->set_progress_percentage
                    (($i + 1) / scalar(@{$instance->{diff_output}}));
                $wm->update_gui();
            }

        }

    }

    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    $instance->{stop_button}->set_sensitive(FALSE);

    # Delete the trailing newline.

    $iter = $instance->{comparison_buffer}->get_end_iter();
    $instance->{comparison_buffer}->delete
        ($iter, $instance->{comparison_buffer}->get_end_iter())
        if ($iter->backward_char());

    # Populate the file combobox.

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status(__("Populating file list"));
    $wm->update_gui();
    @files = sort({ $a->{file_name} cmp $b->{file_name} } @files);
    $i = 1;
    $instance->{file_comparison_combobox}->get_model()->clear();
    $instance->{file_comparison_combobox}->get_model()->set
        ($instance->{file_comparison_combobox}->get_model()->append(),
         CLS_FILE_NAME_COLUMN, __("Summary"),
         CLS_LINE_NR_COLUMN,
             $instance->{comparison_buffer}->get_start_iter()->get_line(),
         CLS_FILE_ID_1_COLUMN, "",
         CLS_FILE_ID_2_COLUMN, "");
    foreach my $file (@files)
    {
        $instance->{file_comparison_combobox}->get_model()->set
            ($instance->{file_comparison_combobox}->get_model()->append(),
             CLS_FILE_NAME_COLUMN, $file->{file_name},
             CLS_LINE_NR_COLUMN, $file->{line_nr},
             CLS_FILE_ID_1_COLUMN, $file->{file_id_1},
             CLS_FILE_ID_2_COLUMN, $file->{file_id_2});
        if (($i % 10) == 0)
        {
            $instance->{appbar}->set_progress_percentage($i / scalar(@files));
            $wm->update_gui();
        }
        ++ $i;
    }
    $instance->{file_comparison_combobox}->set_active(0);
    $instance->{appbar}->set_progress_percentage(1);

    # Make sure we are at the top.

    $instance->{comparison_buffer}->
        place_cursor($instance->{comparison_buffer}->get_start_iter());
    $instance->{comparison_scrolledwindow}->get_vadjustment()->set_value(0);
    $instance->{comparison_scrolledwindow}->get_hadjustment()->set_value(0);
    $wm->update_gui();

    # Move to the file if a file comparison is being done.

    if (defined($file_name) && scalar(@files) > 0)
    {

        # Simply let the combobox's change callback fire after setting its
        # value.

        local $instance->{in_cb} = 0;
        $instance->{file_comparison_combobox}->set_active(1);
        $wm->update_gui();

    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - display_renamed_file_comparison
#
#   Description  - Compares and then displays the differences between two
#                  versions of a file that has either been renamed or moved
#                  somewhere between the specified revisions. The comparison
#                  has to be done with the external helper application.
#
#   Data         - $parent          : The parent window widget for any dialogs
#                                     that may appear.
#                  $mtn             : The Monotone::AutomateStdio object that
#                                     is to be used to get the files' details.
#                  $old_revision_id : The revision id for the older revision.
#                  $old_file_name   : The name of the file on the older
#                  $new_revision_id : The revision id for the newer revision.
#                  $new_file_name   : The name of the file on the newer
#                                     revision.
#
##############################################################################



sub display_renamed_file_comparison($$$$$$)
{

    my ($parent,
        $mtn,
        $old_revision_id,
        $old_file_name,
        $new_revision_id,
        $new_file_name) = @_;

    my ($answer,
        $dialog,
        @manifest,
        $new_file_id,
        $old_file_id);

    $dialog = Gtk2::MessageDialog->new
        ($parent,
         ["modal"],
         "question",
         "yes-no",
         __("The name of the selected file has changed\n"
            . "between the two selected revisions and cannot\n"
            . "be compared internally. Would you like to do\n"
            . "the comparison using the external helper application?"));
    $dialog->set_title(__("External Comparison"));
    $answer = busy_dialog_run($dialog);
    $dialog->destroy();

    # Only continue if asked to do so.

    if ($answer eq "yes")
    {

        # Get the manifests of the two revisions and look for the files in
        # order to get their file ids.

        $mtn->get_manifest_of(\@manifest, $old_revision_id);
        foreach my $entry (@manifest)
        {
            if ($entry->{name} eq $old_file_name)
            {
                $old_file_id = $entry->{file_id};
                last;
            }
        }
        $mtn->get_manifest_of(\@manifest, $new_revision_id);
        foreach my $entry (@manifest)
        {
            if ($entry->{name} eq $new_file_name)
            {
                $new_file_id = $entry->{file_id};
                last;
            }
        }

        # Make sure we have the file ids.

        if (! defined($old_file_id) || ! defined ($new_file_id))
        {
            my $dialog;
            $dialog = Gtk2::MessageDialog->new
                ($parent,
                 ["modal"],
                 "warning",
                 "close",
                 __("The file contents cannot be\n"
                    . "found in the selected revisions.\n"
                    . "This should not be happening."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }

        # Use the external helper application to compare the files.

        external_diffs($parent,
                       $mtn,
                       $old_file_name,
                       $old_file_id,
                       $new_file_name,
                       $new_file_id);

    }

}
#
##############################################################################
#
#   Routine      - display_arbitrary_revision_comparison
#
#   Description  - Display a window that allows the user to select two
#                  arbitrary revisions for comparison.
#
#   Data         - $mtn : The Monotone::AutomateStdio object that is to be
#                         used to do the comparison.
#
##############################################################################



sub display_arbitrary_revision_comparison($)
{

    my $mtn = $_[0];

    my $instance;

    $instance = get_compare_arbitrary_revisions_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{first_revision_id} = "";
    $instance->{second_revision_id} = "";

}
#
##############################################################################
#
#   Routine      - history_list_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on any of the
#                  buttons displayed in the history list in a history window.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $details : A reference to an anonymous hash containing the
#                             window instance, revision and action that is
#                             associated with this widget.
#
##############################################################################



sub history_list_button_clicked_cb($$)
{

    my ($widget, $details) = @_;

    my ($instance,
        $revision_id);

    $instance = $details->{instance};
    $revision_id = $details->{revision_id};

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    if ($details->{button_type} eq "1" || $details->{button_type} eq "2")
    {
        if ($details->{button_type} eq "1")
        {
            $instance->{first_revision_id} = $revision_id;
            set_label_value($instance->{revision_id_1_value_label},
                            $revision_id);
            if ($instance->{first_revision_id}
                eq $instance->{second_revision_id})
            {
                $instance->{second_revision_id} = "";
                set_label_value($instance->{revision_id_2_value_label}, "");
            }
        }
        else
        {
            $instance->{second_revision_id} = $revision_id;
            set_label_value($instance->{revision_id_2_value_label},
                            $revision_id);
            if ($instance->{second_revision_id}
                eq $instance->{first_revision_id})
            {
                $instance->{first_revision_id} = "";
                set_label_value($instance->{revision_id_1_value_label}, "");
            }
        }
        if ($instance->{first_revision_id} ne ""
            && $instance->{second_revision_id} ne "")
        {
            $instance->{compare_button}->set_sensitive(TRUE);
        }
        else
        {
            $instance->{compare_button}->set_sensitive(FALSE);
        }
    }
    elsif ($details->{button_type} eq "browse-revision")
    {

        my ($branch,
            @certs_list);

        # First find out what branch the revision is on (take the first one).

        $instance->{mtn}->certs(\@certs_list, $revision_id);
        $branch = "";
        foreach my $cert (@certs_list)
        {
            if ($cert->{name} eq "branch")
            {
                $branch = $cert->{value};
                last;
            }
        }

        # Get a new browser window preloaded with the desired file.

        get_browser_window($instance->{mtn}, $branch, $revision_id);

    }
    elsif ($details->{button_type} eq "browse-file")
    {

        my ($branch,
            @certs_list,
            $dir,
            $file,
            $path_ref);

        # First find out what branch the revision is on (take the first one).

        $instance->{mtn}->certs(\@certs_list, $revision_id);
        $branch = "";
        foreach my $cert (@certs_list)
        {
            if ($cert->{name} eq "branch")
            {
                $branch = $cert->{value};
                last;
            }
        }

        # Split the file name into directory and file components.

        $path_ref = $instance->{revision_hits}->{$revision_id};
        $dir = dirname($$path_ref);
        $dir = "" if ($dir eq ".");
        $file = basename($$path_ref);

        # Get a new browser window preloaded with the desired file.

        get_browser_window($instance->{mtn},
                           $branch,
                           $revision_id,
                           $dir,
                           $file);

    }
    elsif ($details->{button_type} eq "revision-changelog")
    {

        # Display the full revision change log.

        display_change_log($instance->{mtn}, $revision_id);

    }
    else
    {

        # Annotate the file.

        display_annotation($instance->{mtn},
                           $revision_id,
                           ${$instance->{revision_hits}->{$revision_id}});

    }

}
#
##############################################################################
#
#   Routine      - restrict_to_combobox_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the restrict to combobox by selecting an entry from its
#                  pulldown list in a history window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub restrict_to_combobox_changed_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($branch,
        $iter);
    my $wm = WindowManager->instance();

    # Get the name of the selected branch, treating the first entry of `All
    # Branches' as a special case.

    $iter = $instance->{restrict_to_combobox}->get_active_iter();
    $branch = $instance->{restrict_to_combobox}->get_model()->get($iter, 0);

    # Update the history accordingly.

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();
    if ($instance->{restrict_to_combobox}->get_active() == 0)
    {
        generate_history_report($instance, $instance->{history});
    }
    else
    {
        generate_history_report($instance,
                                $instance->{branch_history}->{$branch});
    }
    $instance->{stop_button}->set_sensitive(FALSE);
    $wm->update_gui();
    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - compare_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  revision comparison button in a history window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub compare_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my @revision_ids;

    # Sort the revisions by date, oldest first.

    @revision_ids = ($instance->{first_revision_id},
                     $instance->{second_revision_id});
    $instance->{mtn}->toposort(\@revision_ids, @revision_ids);

    # If a file is being compared and it has been renamed between the two
    # comparison revisions then we have to fall back on the external helper
    # application, otherwise we can use Monotone's comparison feature.

    if (defined($instance->{file_name})
        && $instance->{revision_hits}->{$revision_ids[0]}
        != $instance->{revision_hits}->{$revision_ids[1]})
    {
        display_renamed_file_comparison($instance->{window},
                                        $instance->{mtn},
                                        $revision_ids[0],
                                        ${$instance->{revision_hits}->
                                          {$revision_ids[0]}},
                                        $revision_ids[1],
                                        ${$instance->{revision_hits}->
                                          {$revision_ids[1]}});
    }
    else
    {

        # Use Monotone's comparison feature.

        display_revision_comparison($instance->{mtn},
                                    $revision_ids[0],
                                    $revision_ids[1],
                                    defined($instance->{file_name})
                                        ? ${$instance->{revision_hits}->
                                            {$revision_ids[0]}}
                                        : undef);

    }

}
#
##############################################################################
#
#   Routine      - compare_arbitrary_revision_advanced_find_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on either of
#                  the advanced find buttons displayed in the compare
#                  arbitrary revisions window.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $details : A reference to an anonymous hash containing the
#                             window instance and the number of the button
#                             pressed.
#
##############################################################################



sub compare_arbitrary_revision_advanced_find_button_clicked_cb($$)
{

    my ($widget, $details) = @_;

    my (@dummy,
        $instance,
        $revision_id);

    $instance = $details->{instance};

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    # Let the user choose the revision (we aren't interested in the branch
    # name(s)) and then store and display the result.

    if (advanced_find($instance, \$revision_id, \@dummy))
    {
        if ($details->{button} == 1)
        {
            $instance->{first_revision_id} = $revision_id;
            set_label_value($instance->{arbitrary_revision_id_1_value_label},
                            $revision_id);
            if ($instance->{first_revision_id}
                eq $instance->{second_revision_id})
            {
                $instance->{second_revision_id} = "";
                set_label_value
                    ($instance->{arbitrary_revision_id_2_value_label}, "");
            }
        }
        else
        {
            $instance->{second_revision_id} = $revision_id;
            set_label_value($instance->{arbitrary_revision_id_2_value_label},
                            $revision_id);
            if ($instance->{second_revision_id}
                eq $instance->{first_revision_id})
            {
                $instance->{first_revision_id} = "";
                set_label_value
                    ($instance->{arbitrary_revision_id_1_value_label}, "");
            }
        }
        if ($instance->{first_revision_id} ne ""
            && $instance->{second_revision_id} ne "")
        {
            $instance->{arbitrary_compare_button}->set_sensitive(TRUE);
        }
        else
        {
            $instance->{arbitrary_compare_button}->set_sensitive(FALSE);
        }
    }

}
#
##############################################################################
#
#   Routine      - file_comparison_combobox_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the file combobox by selecting an entry from its pulldown
#                  list in a revision comparison window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub file_comparison_combobox_changed_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($iter,
        $line_iter,
        $line_nr);

    # Get the line number related to the selected file and then jump to it.

    $iter = $instance->{file_comparison_combobox}->get_active_iter();
    $line_nr = $instance->{file_comparison_combobox}->get_model()->
        get($iter, CLS_LINE_NR_COLUMN);
    if (defined($line_iter = $instance->{comparison_buffer}->
                get_iter_at_line($line_nr)))
    {
        $instance->{comparison_textview}->
            scroll_to_iter($line_iter, 0, TRUE, 0, 0);
    }

    # Only enable the external differences button if an actual file is
    # selected.

    if ($line_nr > 0)
    {
        my $file_id_1 = $instance->{file_comparison_combobox}->get_model()->
            get($iter, CLS_FILE_ID_1_COLUMN);
        my $file_id_2 = $instance->{file_comparison_combobox}->get_model()->
            get($iter, CLS_FILE_ID_2_COLUMN);
        if ($file_id_1 ne "" && $file_id_2 ne "" && $file_id_1 ne $file_id_2)
        {
            $instance->{external_diffs_button}->set_sensitive(TRUE);
        }
        else
        {
            $instance->{external_diffs_button}->set_sensitive(FALSE);
        }
    }
    else
    {
        $instance->{external_diffs_button}->set_sensitive(FALSE);
    }

}
#
##############################################################################
#
#   Routine      - external_diffs_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  external differences button in a revision comparison
#                  window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub external_diffs_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($file_id_1,
        $file_id_2,
        $file_name,
        $iter);

    # Get the details associated with the currently selected file.

    $iter = $instance->{file_comparison_combobox}->get_active_iter();
    $file_name = $instance->{file_comparison_combobox}->get_model()->
        get($iter, CLS_FILE_NAME_COLUMN);
    $file_id_1 = $instance->{file_comparison_combobox}->get_model()->
        get($iter, CLS_FILE_ID_1_COLUMN);
    if (defined($instance->{revision_id_2}))
    {
        $file_id_2 = $instance->{file_comparison_combobox}->get_model()->
            get($iter, CLS_FILE_ID_2_COLUMN);
    }

    # Use the external helper application to compare the files.

    external_diffs($instance->{window},
                   $instance->{mtn},
                   $file_name,
                   $file_id_1,
                   $file_name,
                   $file_id_2);

}
#
##############################################################################
#
#   Routine      - save_differences_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the save
#                  differences button in a revision comparison window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub save_differences_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $data;

    $data = join("\n", @{$instance->{diff_output}}) . "\n";
    save_as_file($instance->{window}, __("unified_diff.patch"), \$data);

}
#
##############################################################################
#
#   Routine      - comparison_revision_change_log_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on either of
#                  the two revision change log buttons in a revision
#                  comparison window.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $details : A reference to an anonymous hash containing the
#                             window instance, revision and action that is
#                             associated with this widget.
#
##############################################################################



sub comparison_revision_change_log_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($colour,
        $revision_id,
        $revision_name);

    # Work out what to do.

    if ($widget == $instance->{revision_change_log_1_button})
    {
        $revision_id = $instance->{revision_id_1};
        if ($user_preferences->{coloured_diffs})
        {
            $colour = "compare-1";
        }
        else
        {
            $revision_name = "- " . $revision_id;
        }
    }
    else
    {
        $revision_id = $instance->{revision_id_2};
        if ($user_preferences->{coloured_diffs})
        {
            $colour = "compare-2";
        }
        else
        {
            $revision_name = "+ " . $revision_id;
        }
    }

    # Display the full revision change log if we have one (we won't if the
    # revision is the workspace).

    if (defined($revision_id))
    {
        display_change_log($instance->{mtn},
                           $revision_id,
                           $colour,
                           $revision_name);
    }
    else
    {
        my $dialog = Gtk2::MessageDialog->new
            (undef,
             ["modal"],
             "info",
             "close",
             __("This revision is infact the current workspace.\n"
                . "As such there is no change log to display."));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

}
#
##############################################################################
#
#   Routine      - generate_history_report
#
#   Description  - Generate a revision history report, complete with buttons,
#                  for the specified revision ids.
#
#   Data         - $instance    : The revision history window instance.
#                  $history     : A reference to a list of revision ids that
#                                 represents the history that is to be
#                                 displayed.
#
##############################################################################



sub generate_history_report($$)
{

    my ($instance, $history) = @_;

    my (@branches,
        $browse_button,
        $browse_button_ttip,
        $browse_button_type,
        $button,
        @certs_list,
        $no_branch_history,
        $select_id_1_ttip,
        $select_id_2_ttip,
        $update_interval);
    my $counter = 0;
    my $wm = WindowManager->instance();

    # Reset the window state before the update.

    $instance->{appbar}->set_progress_percentage(0);
    if (defined($instance->{file_name}))
    {
        $instance->{appbar}->set_status(__("Displaying file history"));
    }
    else
    {
        $instance->{appbar}->set_status(__("Displaying revision history"));
    }
    $instance->{history_buffer}->set_text("");
    set_label_value($instance->{numbers_value_label}, scalar(@$history));
    $wm->update_gui();
    $instance->{stop} = 0;

    # Work out whether we need to generate a branch history as we scan the
    # revisions.

    if (! defined($instance->{branch_history}))
    {
        $no_branch_history = 1;
        $instance->{branch_history} = {};
    }

    # Determine the buttons to display, their tool tips and update interval
    # depending upon whether we are displaying a revision or file history.

    if (defined($instance->{file_name}))
    {
        $select_id_1_ttip = \$__select_id_file_1_ttip;
        $select_id_2_ttip = \$__select_id_file_2_ttip;
        $browse_button_ttip = \$__browse_file_ttip;
        $browse_button = \$__browse_file;
        $browse_button_type = "browse-file";
        $update_interval = 10;
    }
    else
    {
        $select_id_1_ttip = \$__select_id_rev_1_ttip;
        $select_id_2_ttip = \$__select_id_rev_2_ttip;
        $browse_button_ttip = \$__browse_rev_ttip;
        $browse_button = \$__browse_rev;
        $browse_button_type = "browse-revision";
        $update_interval = 100;
    }

    # Display revision details and associated buttons for each revision in the
    # specified list.

    foreach my $revision_id (@$history)
    {

        ++ $counter;

        $instance->{mtn}->certs(\@certs_list, $revision_id);

        # If we haven't done so already, file this revision under the
        # appropriate branch history lists.

        if ($no_branch_history)
        {
            foreach my $cert (@certs_list)
            {
                if ($cert->{name} eq "branch")
                {
                    if (exists($instance->{branch_history}->{$cert->{value}}))
                    {
                        push(@{$instance->{branch_history}->{$cert->{value}}},
                             $revision_id);
                    }
                    else
                    {
                        $instance->{branch_history}->{$cert->{value}} =
                            [$revision_id];
                    }
                }
            }
        }

        # Print out the revision summary.

        generate_revision_report($instance->{history_buffer},
                                 $revision_id,
                                 \@certs_list,
                                 "");
        $instance->{history_buffer}->
            insert($instance->{history_buffer}->get_end_iter(), "\n\n ");

        # Add the buttons.

        $button = Gtk2::Button->new($__select_id_1);
        $button->signal_connect("clicked",
                                \&history_list_button_clicked_cb,
                                {instance    => $instance,
                                 revision_id => $revision_id,
                                 button_type => "1"});
        $tooltips->set_tip($button, $$select_id_1_ttip);
        $instance->{history_textview}->add_child_at_anchor
            ($button,
             $instance->{history_buffer}->
                 create_child_anchor($instance->{history_buffer}->
                                     get_end_iter()));
        $button->show_all();

        $instance->{history_buffer}->
            insert($instance->{history_buffer}->get_end_iter(), " ");
        $button = Gtk2::Button->new($__select_id_2);
        $button->signal_connect("clicked",
                                \&history_list_button_clicked_cb,
                                {instance    => $instance,
                                 revision_id => $revision_id,
                                 button_type => "2"});
        $tooltips->set_tip($button, $$select_id_2_ttip);
        $instance->{history_textview}->add_child_at_anchor
            ($button,
             $instance->{history_buffer}->
                 create_child_anchor($instance->{history_buffer}->
                                     get_end_iter()));
        $button->show_all();

        $instance->{history_buffer}->
            insert($instance->{history_buffer}->get_end_iter(), " ");
        $button = Gtk2::Button->new($$browse_button);
        $button->signal_connect("clicked",
                                \&history_list_button_clicked_cb,
                                {instance    => $instance,
                                 revision_id => $revision_id,
                                 button_type => $browse_button_type});
        $tooltips->set_tip($button, $$browse_button_ttip);
        $instance->{history_textview}->add_child_at_anchor
            ($button,
             $instance->{history_buffer}->
                 create_child_anchor($instance->{history_buffer}->
                                     get_end_iter()));
        $button->show_all();

        if (defined($instance->{file_name}))
        {
            $instance->{history_buffer}->
                insert($instance->{history_buffer}->get_end_iter(), " ");
            $button = Gtk2::Button->new($__annotate_file);
            $button->signal_connect("clicked",
                                    \&history_list_button_clicked_cb,
                                    {instance    => $instance,
                                     revision_id => $revision_id,
                                     button_type => "annotate-file"});
            $tooltips->set_tip($button, $__annotate_file_ttip);
            $instance->{history_textview}->add_child_at_anchor
                ($button,
                 $instance->{history_buffer}->
                     create_child_anchor($instance->{history_buffer}->
                                         get_end_iter()));
            $button->show_all();
        }

        $instance->{history_buffer}->
            insert($instance->{history_buffer}->get_end_iter(), " ");
        $button = Gtk2::Button->new($__full_changelog);
        $button->signal_connect("clicked",
                                \&history_list_button_clicked_cb,
                                {instance    => $instance,
                                 revision_id => $revision_id,
                                 button_type => "revision-changelog"});
        $tooltips->set_tip($button, $__full_changelog_ttip);
        $instance->{history_textview}->add_child_at_anchor
            ($button,
             $instance->{history_buffer}->
                 create_child_anchor($instance->{history_buffer}->
                                     get_end_iter()));
        $button->show_all();

        if (($counter % $update_interval) == 0)
        {
            $instance->{appbar}->set_progress_percentage
                ($counter / scalar(@$history));
            $wm->update_gui();
        }

        # Stop if the user wants to.

        last if ($instance->{stop});

        # If we aren't at the end, print out the revision separator.

        if ($counter < scalar(@$history))
        {
            $instance->{history_buffer}->
                insert($instance->{history_buffer}->get_end_iter(), "\n");
            $instance->{history_buffer}->
                insert_pixbuf($instance->{history_buffer}->get_end_iter(),
                              $line_image);
            $instance->{history_buffer}->
                insert($instance->{history_buffer}->get_end_iter(), "\n");
        }

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # If the user stopped the history generation process then update the
    # counter and truncate the history list to reflect this if this is the
    # initial history gathering stage (which can be determined by the absence
    # of individual branch histories when we first entered this routine).

    if ($instance->{stop})
    {
        set_label_value($instance->{numbers_value_label}, $counter);
        @$history = splice(@$history, 0, $counter) if ($no_branch_history);
    }

    # Make sure we are at the top.

    $instance->{history_buffer}->
        place_cursor($instance->{history_buffer}->get_start_iter());
    $instance->{history_scrolledwindow}->get_vadjustment()->set_value(0);
    $instance->{history_scrolledwindow}->get_hadjustment()->set_value(0);

    # Populate the restrict to combobox if needed.

    if ($no_branch_history)
    {
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->set_status(__("Populating branch list"));
        $wm->update_gui();
        @branches = sort(keys(%{$instance->{branch_history}}));
        $counter = 1;
        $instance->{restrict_to_combobox}->get_model()->clear();
        $instance->{restrict_to_combobox}->get_model()->set
            ($instance->{restrict_to_combobox}->get_model()->append(),
             0, __("All Branches"));
        foreach my $branch (@branches)
        {
            $instance->{restrict_to_combobox}->get_model()->set
                ($instance->{restrict_to_combobox}->get_model()->append(),
                 0, $branch);
            if (($counter % 10) == 0)
            {
                $instance->{appbar}->set_progress_percentage
                    ($counter / scalar(@branches));
                $wm->update_gui();
            }
            ++ $counter;
        }
        $instance->{restrict_to_combobox}->set_active(0);
        $instance->{appbar}->set_progress_percentage(1);
        $wm->update_gui();
    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - get_history_window
#
#   Description  - Creates or prepares an existing history window for use.
#
#   Data         - Return Value : A reference to the newly created or unused
#                                 history instance record.
#
##############################################################################



sub get_history_window()
{

    my $instance;
    my $window_type = "history_window";
    my $wm = WindowManager->instance();

    # Create a new history window if an unused one wasn't found, otherwise
    # reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

        my ($glade,
            $height,
            $renderer,
            $width);

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($window_type);
        foreach my $widget ("appbar",
                            "history_label",
                            "history_textview",
                            "history_scrolledwindow",
                            "numbers_value_label",
                            "restrict_to_combobox",
                            "stop_button",
                            "revision_id_1_value_label",
                            "revision_id_2_value_label",
                            "compare_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $window_type);

        # Setup the history callbacks.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 hide_find_text_and_goto_line($instance->{history_textview});
                 $widget->hide();
                 $instance->{history_buffer}->set_text("");
                 $instance->{restrict_to_combobox}->get_model()->clear();
                 $instance->{branch_history} = undef;
                 $instance->{history} = undef;
                 $instance->{revision_hits} = undef;
                 $instance->{mtn} = undef;
                 return TRUE;
             },
             $instance);
        $instance->{stop_button}->signal_connect
            ("clicked", sub { $_[1]->{stop} = 1; }, $instance);

        # Setup the history viewer.

        $instance->{history_buffer} =
            $instance->{history_textview}->get_buffer();
        create_format_tags($instance->{history_buffer});
        $instance->{history_textview}->modify_font($mono_font);

        # Display the window (it needs to be realised before we get any widget
        # sizes).

        $instance->{window}->show_all();
        $instance->{window}->present();

        # Lock the width of the numbers value label so that the combobox button
        # to its right does not keep shrinking as this label's value increases.
        # This label has had its value set to 8888888 in Glade (so as to allow
        # room for a number in the millions) and has been dynamically sized
        # accordingly by the layout manager. So now take its width and lock its
        # size to that. Doing it this way neatly avoids issues of font sizes
        # etc. Once done set the lable's value to be blank.

        ($width, $height) =
            ($instance->{numbers_value_label}->window()->get_geometry())[2, 3];
        $instance->{numbers_value_label}->set_size_request($width, $height);
        set_label_value($instance->{numbers_value_label}, "");

        # Setup the restrict to combobox.

        $instance->{restrict_to_combobox}->
            set_model(Gtk2::ListStore->new("Glib::String"));
        $renderer = Gtk2::CellRendererText->new();
        $instance->{restrict_to_combobox}->pack_start($renderer, TRUE);
        $instance->{restrict_to_combobox}->add_attribute($renderer,
                                                         "text" => 0);
        $instance->{restrict_to_combobox}->get_model()->clear();
        $instance->{restrict_to_combobox}->get_model()->set
            ($instance->{restrict_to_combobox}->get_model()->append(),
             0, __("All Branches"));
        $instance->{restrict_to_combobox}->set_active(0);

        # Register the window for management and set up the help callbacks.

        $wm->manage($instance,
                    $window_type,
                    $instance->{window},
                    $instance->{stop_button});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => "restrict_to_combobox",
              help_ref => __("mtnb-lachc-history-buttons")},
             {widget   => "stop_button",
              help_ref => __("mtnb-lachc-history-buttons")},
             {widget   => "compare_button",
              help_ref => __("mtnb-lachc-history-buttons")},
             {widget   => undef,
              help_ref => __("mtnb-lachc-the-revision-and-file-history-"
                             . "windows")});
    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        set_window_size($instance->{window}, $window_type);
        $instance->{restrict_to_combobox}->get_model()->clear();
        $instance->{restrict_to_combobox}->get_model()->set
            ($instance->{restrict_to_combobox}->get_model()->append(),
             0, __("All Branches"));
        $instance->{restrict_to_combobox}->set_active(0);
        $instance->{stop_button}->set_sensitive(FALSE);
        $instance->{compare_button}->set_sensitive(FALSE);
        set_label_value($instance->{numbers_value_label}, "");
        set_label_value($instance->{revision_id_1_value_label}, "");
        set_label_value($instance->{revision_id_2_value_label}, "");
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->clear_stack();

    }

    local $instance->{in_cb} = 1;

    $instance->{stop} = 0;

    # Empty out the contents.

    $instance->{history_buffer}->set_text("");

    return $instance;

}
#
##############################################################################
#
#   Routine      - get_file_history_helper
#
#   Description  - Recursive routine for getting the revisions in a file's
#                  change history.
#
#   Data         - $instance      : The file history window instance.
#                  $revision_id   : The revision id from where the search is
#                                   to commence.
#                  $file_name_ref : A reference to the name of the file that
#                                   is to have its history returned.
#
##############################################################################



sub get_file_history_helper($$$)
{

    my ($instance, $revision_id, $file_name_ref) = @_;

    return if ($instance->{stop});

    my @changed_ancestors;

    $instance->{mtn}->get_content_changed(\@changed_ancestors,
                                          $revision_id,
                                          $$file_name_ref);
    foreach my $chg_ancestor (@changed_ancestors)
    {
        if (! exists($instance->{revision_hits}->{$chg_ancestor}))
        {

            my @parents;

            # Track file renames in the changed ancestor revisions (no need to
            # bother if the ancestor revision is the same as the current one as
            # we were passed the file name).

            if ($chg_ancestor ne $revision_id)
            {
                my $file_name;
                $instance->{mtn}->
                    get_corresponding_path(\$file_name,
                                           $revision_id,
                                           $$file_name_ref,
                                           $chg_ancestor);
                $file_name_ref = \$file_name
                    if ($file_name ne $$file_name_ref);
            }

            # Store a reference to the current file name in the revision hit
            # hash, this can be used later to refer to the correct file name
            # for a given changed ancestor revision.

            $instance->{revision_hits}->{$chg_ancestor} = $file_name_ref;

            set_label_value($instance->{numbers_value_label},
                            scalar(keys(%{$instance->{revision_hits}})));
            WindowManager->update_gui();

            # Now repeat for each parent, remembering to track file renames.

            $instance->{mtn}->parents(\@parents, $chg_ancestor);
            foreach my $parent (@parents)
            {
                my $file_name;
                $instance->{mtn}->get_corresponding_path(\$file_name,
                                                         $chg_ancestor,
                                                         $$file_name_ref,
                                                         $parent);
                $file_name_ref = \$file_name
                    if ($file_name ne $$file_name_ref);
                get_file_history_helper($instance, $parent, $file_name_ref);
            }

        }
    }

}
#
##############################################################################
#
#   Routine      - get_revision_history_helper
#
#   Description  - Recursive routine for getting the revisions in a revision's
#                  change history.
#
#   Data         - $instance    : The revision history window instance.
#                  $revision_id : The revision id from where the search is to
#                                 commence.
#
##############################################################################



sub get_revision_history_helper($$)
{

    my ($instance, $revision_id) = @_;

    return if ($instance->{stop});

    my @parents;

    $instance->{revision_hits}->{$revision_id} = 1;
    set_label_value($instance->{numbers_value_label},
                    scalar(keys(%{$instance->{revision_hits}})));
    WindowManager->update_gui();
    $instance->{mtn}->parents(\@parents, $revision_id);
    foreach my $parent (@parents)
    {
        get_revision_history_helper($instance, $parent)
            if (! exists($instance->{revision_hits}->{$parent}));
    }

}
#
##############################################################################
#
#   Routine      - get_compare_arbitrary_revisions_window
#
#   Description  - Creates or prepares an existing compare arbitrary revisions
#                  window for use.
#
#   Data         - Return Value : A reference to the newly created or unused
#                                 compare arbitrary revisions instance record.
#
##############################################################################



sub get_compare_arbitrary_revisions_window()
{

    my $instance;
    my $window_type = "compare_arbitrary_revisions_window";
    my $wm = WindowManager->instance();

    # Create a new file history window if an unused one wasn't found, otherwise
    # reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

        my $glade;

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($window_type);
        foreach my $widget ("arbitrary_revision_id_1_value_label",
                            "arbitrary_revision_id_2_value_label",
                            "arbitrary_revision_1_advanced_find_button",
                            "arbitrary_revision_2_advanced_find_button",
                            "arbitrary_compare_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Setup the arbitrary compare callbacks.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $widget->hide();
                 $instance->{mtn} = undef;
                 return TRUE;
             },
             $instance);
        foreach my $button (1 .. 2)
        {
            my $widget =
                "arbitrary_revision_" . $button . "_advanced_find_button";
            $instance->{$widget}->signal_connect
                ("clicked",
                 \&compare_arbitrary_revision_advanced_find_button_clicked_cb,
                 {instance => $instance,
                  button   => $button});
        }

        # Display the window.

        $instance->{window}->show_all();
        $instance->{window}->present();

        # Register the window for management and set up the help callbacks.

        $wm->manage($instance,
                    $window_type,
                    $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => "arbitrary_revision_1_advanced_find_button",
              help_ref => __("mtnb-lachc-compare-arbitrary-revisions-"
                             . "buttons")},
             {widget   => "arbitrary_revision_1_advanced_find_button",
              help_ref => __("mtnb-lachc-compare-arbitrary-revisions-"
                             . "buttons")},
             {widget   => "arbitrary_compare_button",
              help_ref => __("mtnb-lachc-compare-arbitrary-revisions-"
                             . "buttons")},
             {widget   => undef,
              help_ref => __("mtnb-lachc-the-compare-arbitrary-revisions-"
                             . "window")});

    }
    else
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        $instance->{arbitrary_compare_button}->set_sensitive(FALSE);
        set_label_value($instance->{arbitrary_revision_id_1_value_label}, "");
        set_label_value($instance->{arbitrary_revision_id_2_value_label}, "");
        $instance->{window}->show_all();
        $instance->{window}->present();
    }

    return $instance;

}
#
##############################################################################
#
#   Routine      - get_revision_comparison_window
#
#   Description  - Creates or prepares an existing revision comparison window
#                  for use.
#
#   Data         - $mtn         : The Monotone::AutomateStdio object that is
#                                 to be used to do the comparison.
#                  Return Value : A reference to the newly created or unused
#                                 change log instance record.
#
##############################################################################



sub get_revision_comparison_window($)
{

    my $mtn = $_[0];

    my $instance;
    my $window_type = "revision_comparison_window";
    my $wm = WindowManager->instance();

    # Create a new revision comparison window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

        my ($glade,
            $renderer);

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($window_type);
        foreach my $widget ("appbar",
                            "file_comparison_combobox",
                            "external_diffs_button",
                            "stop_button",
                            "comparison_textview",
                            "comparison_scrolledwindow",
                            "revision_change_log_1_button",
                            "revision_change_log_1_button_label",
                            "revision_change_log_2_button_label")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $window_type);

        # Setup the file history callbacks.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 hide_find_text_and_goto_line
                     ($instance->{comparison_textview});
                 $widget->hide();
                 $instance->{file_comparison_combobox}->get_model()->clear();
                 $instance->{comparison_buffer}->set_text("");
                 $instance->{diff_output} = [];
                 $instance->{mtn} = undef;
                 return TRUE;
             },
             $instance);
        if ($mtn->supports(MTN_CONTENT_DIFF_EXTRA_OPTIONS))
        {
            $instance->{kill_mtn_subprocess} = 0;
            $instance->{stop_button}->signal_connect
                ("clicked",
                 sub {
                     my ($widget, $instance) = @_;
                     $instance->{stop} = 1;
                     kill("TERM", $instance->{mtn}->get_pid())
                         if ($instance->{kill_mtn_subprocess});
                 },
                 $instance);
        }
        else
        {
            $instance->{stop_button}->signal_connect
                ("clicked", sub { $_[1]->{stop} = 1; }, $instance);
        }

        # Setup the file combobox.

        $instance->{file_comparison_combobox}->
            set_model(Gtk2::ListStore->new("Glib::String",
                                           "Glib::Int",
                                           "Glib::String",
                                           "Glib::String"));
        $renderer = Gtk2::CellRendererText->new();
        $instance->{file_comparison_combobox}->pack_start($renderer, TRUE);
        $instance->{file_comparison_combobox}->add_attribute($renderer,
                                                             "text" => 0);
        $instance->{file_comparison_combobox}->get_model()->set
            ($instance->{file_comparison_combobox}->get_model()->append(),
             CLS_FILE_NAME_COLUMN, " ",
             CLS_LINE_NR_COLUMN, 0,
             CLS_FILE_ID_1_COLUMN, "",
             CLS_FILE_ID_2_COLUMN, "");

        # Setup the revision comparison viewer.

        $instance->{comparison_buffer} =
            $instance->{comparison_textview}->get_buffer();
        create_format_tags($instance->{comparison_buffer});
        $instance->{comparison_textview}->modify_font($mono_font);

        # Setup the revision log button labels.

        if ($user_preferences->{coloured_diffs})
        {
            $instance->{revision_change_log_1_button_label}->
                set_markup("<span foreground='"
                           . $user_preferences->{colours}->{cmp_revision_1}->
                               {fg}
                           . "'>"
                           . __("Revision Change Log")
                           . "</span>");
            $instance->{revision_change_log_2_button_label}->
                set_markup("<span foreground='"
                           . $user_preferences->{colours}->{cmp_revision_2}->
                               {fg}
                           . "'>"
                           . __("Revision Change Log")
                           . "</span>");
        }
        else
        {
            $instance->{revision_change_log_1_button_label}->
                set_text(__("- Revision Change Log"));
            $instance->{revision_change_log_2_button_label}->
                set_text(__("+ Revision Change Log"));
        }

        # Display the window (it needs to be realised before it is registered).

        $instance->{window}->show_all();
        $instance->{window}->present();

        # Register the window for management and set up the help callbacks.

        $wm->manage($instance,
                    $window_type,
                    $instance->{window},
                    $instance->{stop_button});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => "file_comparison_hbox",
              help_ref => __("mtnb-lachc-differences-buttons")},
             {widget   => "comparison_hbuttonbox",
              help_ref => __("mtnb-lachc-differences-buttons")},
             {widget   => undef,
              help_ref => __("mtnb-lachc-the-differences-window")});

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        set_window_size($instance->{window}, $window_type);
        $instance->{external_diffs_button}->set_sensitive(FALSE);
        $instance->{stop_button}->set_sensitive(FALSE);
        $instance->{file_comparison_combobox}->get_model()->clear();
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->clear_stack();

    }

    local $instance->{in_cb} = 1;

    $instance->{stop} = 0;

    # Empty out the contents.

    $instance->{diff_output} = [];
    $instance->{comparison_buffer}->set_text("");

    return $instance;

}
#
##############################################################################
#
#   Routine      - external_diffs
#
#   Description  - Launch the external differences helper program, loading in
#                  the contents of the specified files.
#
#   Data         - $parent        : The parent window widget for any dialogs
#                                   that may appear.
#                  $mtn           : The Monotone::AutomateStdio object that is
#                                   to be used to get the files' details.
#                  $old_file_name : The file name of the older version of the
#                                   file.
#                  $old_file_id   : Monotone's file id for the older version
#                                   of the file.
#                  $new_file_name : The file name of the newer version of the
#                                   file.
#                  $new_file_id   : Either Monotone's file id for the newer
#                                   version of the file or undef if the file
#                                   relates to one in a workspace.
#
##############################################################################



sub external_diffs($$$$$$)
{

    my ($parent,
        $mtn,
        $old_file_name,
        $old_file_id,
        $new_file_name,
        $new_file_id) = @_;

    my ($cmd,
        $data,
        $new_fh,
        $new_file,
        $old_fh,
        $old_file);

    # Just check that we do actually have an external helper application to
    # call.

    if (! defined($user_preferences->{diffs_application})
        || $user_preferences->{diffs_application} =~ m/^\s*$/)
    {
        my $dialog = Gtk2::MessageDialog->new
            ($parent,
             ["modal"],
             "warning",
             "close",
             __("Cannot call the external helper application\n"
                . "to do the comparison as one has not been\n"
                . "specified in the user's preferences."));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }
    return unless (program_valid((split(/[[:blank:]]/,
                                        $user_preferences->{diffs_application})
                                  )[0],
                                 $parent));

    # Generate temporary disk file names.

    if (! defined($old_file = generate_tmp_path(__("OLDER_")
                                                . basename($old_file_name)))
        || ! defined($new_file =
                     generate_tmp_path(__("NEWER_")
                                       . basename($new_file_name))))
    {
        my $dialog = Gtk2::MessageDialog->new
            ($parent,
             ["modal"],
             "warning",
             "close",
             __x("Cannot generate temporary file name:\n{error_message}.",
                 error_message => $!));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }

    # Attempt to save the contents to the files.

    if (! defined($old_fh = IO::File->new($old_file, "w"))
        || ! defined($new_fh = IO::File->new($new_file, "w")))
    {
        my $dialog = Gtk2::MessageDialog->new
            ($parent,
             ["modal"],
             "warning",
             "close",
             __x("Cannot create temporary file:\n{error_message}.",
                 error_message => $!));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }
    binmode($old_fh);
    binmode($new_fh);
    $mtn->get_file(\$data, $old_file_id);
    $old_fh->print($data);
    if (defined($new_file_id))
    {
        $mtn->get_file(\$data, $new_file_id);
        $new_fh->print($data);
    }
    else
    {

        my ($ws_dir,
            $ws_file_name);

        # Ok we are doing a workspace comparison.

        # Get the workspace base directory and calculate the full path to the
        # file.

        if (! defined($ws_dir = $mtn->get_ws_path()))
        {
            my $dialog = Gtk2::MessageDialog->new
                ($parent,
                 ["modal"],
                 "warning",
                 "close",
                 __("The workspace location cannot be determined.\n"
                    . "The external file comparison has been aborted."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }
        $ws_file_name = File::Spec->catfile($ws_dir, $new_file_name);

        # Copy the content of the file into the open file handle.

        if (! File::Copy::syscopy($ws_file_name, $new_fh))
        {
            my $dialog = Gtk2::MessageDialog->new
                ($parent,
                 ["modal"],
                 "warning",
                 "close",
                 __x("Cannot copy `{ws_file_name}' ({error_message}).\n"
                        . "The external file comparison has been aborted.",
                     ws_file_name => $ws_file_name,
                     error_message => $!));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }

    }

    $old_fh->close();
    $new_fh->close();
    $old_fh = $new_fh = undef;

    # Use the specified differences application, replacing `{file1}' and
    # `{file2}' with the real file names.

    $cmd = $user_preferences->{diffs_application};
    $cmd =~ s/\{file1\}/$old_file/g;
    $cmd =~ s/\{file2\}/$new_file/g;

    # Launch it.

    system($cmd . " &");

}
#
##############################################################################
#
#   Routine      - mtn_diff
#
#   Description  - Compare two the specified two revisions, optionally
#                  restricting the comparison to the specified file.
#
#   Data         - $list          : A reference to the list that is to contain
#                                   the output from the diff command.
#                  $abort         : A reference to an abort flag that when
#                                   true will cause the mtn diff process to
#                                   stop.
#                  $mtn           : The Monotone::AutomateStdio object that is
#                                   to be used to do the comparison.
#                  $revision_id_1 : The first revision id that is to be
#                                   compared.
#                  $revision_id_2 : Either the second revision id that is to be
#                                   compared or undef if the current workspace
#                                   is to be used.
#                  $file_name     : Either the name of the file that the
#                                   revision comparison should be restricted
#                                   to or undef for a full revision
#                                   comparison.
#                  Return Value   : True if the comparison worked, otherwise
#                                   false if something went wrong.
#
##############################################################################



sub mtn_diff($$$$$;$)
{

    my ($list,
        $abort,
        $mtn,
        $revision_id_1,
        $revision_id_2,
        $file_name) = @_;

    my ($buffer,
        @cmd,
        $cwd,
        $diff_wd,
        $error_msg,
        $ok);

    # Run mtn diff in the root directory so as to avoid any workspace
    # conflicts.

    @$list = ();
    push(@cmd, "mtn");
    push(@cmd, "--db=" . $mtn->get_db_name())
        if (defined($mtn->get_db_name()));
    push(@cmd, "diff");
    push(@cmd, "-r");
    push(@cmd, "i:" . $revision_id_1);
    if (defined($revision_id_2))
    {
        push(@cmd, "-r");
        push(@cmd, "i:" . $revision_id_2);
        $diff_wd = File::Spec->rootdir();
    }
    else
    {
        $diff_wd = $mtn->get_ws_path();
    }
    push(@cmd, $file_name) if (defined($file_name));
    $cwd = getcwd();
    eval
    {
        die(__("chdir failed: ") . $!) unless (chdir($diff_wd));
        $ok = run_command(\$buffer,
                          undef,
                          undef,
                          undef,
                          $abort,
                          \$error_msg,
                          @cmd);
        cleanup_mtn_error_message(\$error_msg) unless ($ok);
    };
    $error_msg = $@ if ($@);
    chdir($cwd);
    if (! $$abort)
    {
        if (! $ok)
        {
            my $dialog = Gtk2::MessageDialog->new_with_markup
                (undef,
                 ["modal"],
                 "warning",
                 "close",
                 __x("Problem running mtn diff, got:\n"
                         . "<b><i>{error_message}</i></b>",
                     error_message => Glib::Markup::escape_text($error_msg)));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }

        # Break up the input into a list of lines.

        @$list = split(/\n/, $buffer);

    }
    else
    {
        @$list = ();
    }

    return $ok;

}

1;
