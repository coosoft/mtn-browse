##############################################################################
#
#   File Name    - Annotate.pm
#
#   Description  - The annotate module for the mtn-browse application. This
#                  module contains all the routines for implementing the
#                  annotation window.
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

# ***** GLOBAL DATA DECLARATIONS *****

# Constant for the nujmber of lines to examine when trying to sync up a line
# position between different versions of a file.

use constant LINE_SYNC_SIZE => 1000;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_annotation($$$;$$);

# Private routines.

sub annotate_previous_version($$$);
sub annotation_textview_populate_popup_cb($$$);
sub annotation_textview_popup_menu_item_cb($$);
sub compare_file_with_previous($$);
sub compare_revision_with_parent($$);
sub get_annotation_window();
sub get_files_previous_version_details($$$$$);
sub get_significant_lines_around_iter($$$);
sub mtn_annotate($$$$);
sub sync_line_position($$$);
#
##############################################################################
#
#   Routine      - display_annotation
#
#   Description  - Display the annotated listing for the specified file.
#
#   Data         - $mtn         : The Monotone::AutomateStdio object that is
#                                 to be used to display the annotated file.
#                  $revision_id : The revision id in which the desired version
#                                 of the file resides.
#                  $file_name   : The name of the file that is to be
#                                 annotated.
#                  $line_nr     : The number of the line that should be
#                                 scrolled to once the file has been
#                                 annotated (starting from 0). This is
#                                 otional.
#                  $sync_data   : A reference to a record containing the
#                                 current line, previous lines and subsequent
#                                 lines. This is used to attempt to
#                                 synchronise a position between different
#                                 versions of the file. This is optional.
#
##############################################################################



sub display_annotation($$$;$$)
{

    my ($mtn, $revision_id, $file_name, $line_nr, $sync_data) = @_;

    my ($goto_iter,
        $i,
        $instance,
        $iter,
        $len,
        @lines,
        $max_len,
        $padding,
        @prefix,
        $prefix_tag,
        $template,
        $text_tag);
    my $wm = WindowManager->instance();

    $instance = get_annotation_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{file_name} = $file_name;
    $instance->{revision_id} = $revision_id;
    $instance->{window}->set_title(__x("Annotated Listing Of {file}",
                                       file => $instance->{file_name}));
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $wm->update_gui();

    # Get Monotone to do the annotation.

    $instance->{appbar}->set_status(__("Annotating file"));
    $wm->update_gui();
    {
        local $pulse_widget = $instance->{appbar}->get_progress();
        mtn_annotate(\@lines,
                     $mtn->get_db_name(),
                     $revision_id,
                     $instance->{file_name});
    }

    # Find the longest line for future padding and also split each line into
    # their prefix and text parts. Please note that the use of unpack un-utf8s
    # the returned strings.

    $max_len = 0;
    if (scalar(@lines) > 0)
    {
        $instance->{prefix_length} = length(($lines[0] =~ m/^([^:]+):.*$/)[0]);
    }
    else
    {
        $instance->{prefix_length} = 0;
    }
    $template = sprintf("a%da2a*", $instance->{prefix_length});
    for ($i = 0; $i < scalar(@lines); ++ $i)
    {
        ($prefix[$i], $lines[$i]) = (unpack($template, $lines[$i]))[0, 2];
        eval
        {
            $lines[$i] = decode($file_encoding, $lines[$i], Encode::FB_CROAK);
        };
        $lines[$i] =~ s/\s+$//;
        $lines[$i] = expand($lines[$i]);
        $max_len = $len if (($len = length($lines[$i])) > $max_len);
    }

    # Display the result, highlighting according to the annotate output.

    $instance->{appbar}->set_status
        (__("Formatting and displaying annotated file"));
    $wm->update_gui();
    $padding = " " x $max_len;
    $prefix_tag = $text_tag = "";
    for ($i = 0; $i < scalar(@lines); ++ $i)
    {

        # Change the colours if there is a new prefix.

        if ($prefix[$i] !~ m/^\s+$/)
        {
            if ($prefix_tag ne "annotate-prefix-1")
            {
                $prefix_tag = "annotate-prefix-1";
                $text_tag = "annotate-text-1";
            }
            else
            {
                $prefix_tag = "annotate-prefix-2";
                $text_tag = "annotate-text-2";
            }
        }

        # Print out the prefix.

        $instance->{annotation_buffer}->insert_with_tags_by_name
            ($instance->{annotation_buffer}->get_end_iter(),
             $prefix[$i] . " ",
             $prefix_tag);

        # Print out the text.

        $instance->{annotation_buffer}->insert_with_tags_by_name
            ($instance->{annotation_buffer}->get_end_iter(),
             substr($lines[$i] . $padding, 0, $max_len) . "\n",
             $text_tag);

        if (($i % 100) == 0)
        {
            $instance->{appbar}->set_progress_percentage
                (($i + 1) / scalar(@lines));
            $wm->update_gui();
        }

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Delete the trailing newline.

    $iter = $instance->{annotation_buffer}->get_end_iter();
    $instance->{annotation_buffer}->delete
        ($iter, $instance->{annotation_buffer}->get_end_iter())
        if ($iter->backward_char());

    # If the caller cares about file position then attempt to go to the line
    # indicated by the arguments, synchronising if possible.

    if (defined($line_nr) && $line_nr >= 0)
    {

        # Get an iter for the current line.

        if (defined($goto_iter = $instance->{annotation_buffer}->
                    get_iter_at_line($line_nr)))
        {
            $goto_iter->backward_line() unless $goto_iter->starts_line();
        }

        # Now attempt to synchronise line position between the two versions of
        # the file if we have the data.

        if (defined($sync_data))
        {
            $goto_iter = sync_line_position($instance, $line_nr, $sync_data);
        }

    }

    # Either go to the chosen line if we have one or the top if we don't.

    if (defined($goto_iter))
    {
        $instance->{annotation_textview}->
            scroll_to_iter($goto_iter, 0, TRUE, 0, 0.5);
    }
    else
    {
        $instance->{annotation_buffer}->
            place_cursor($instance->{annotation_buffer}->get_start_iter());
        $instance->{annotation_scrolledwindow}->get_vadjustment()->
            set_value(0);
        $instance->{annotation_scrolledwindow}->get_hadjustment()->
            set_value(0);
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
#   Routine      - annotation_textview_populate_popup_cb
#
#   Description  - Callback routine called when the user right clicks on any
#                  textview window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $menu     : The Gtk2::Menu widget that is to be updated.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub annotation_textview_populate_popup_cb($$$)
{

    my ($widget, $menu, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($iter,
        $menu_item,
        $revision_part,
        $separator,
        $x,
        $y);

    # Extract the revision id relating to the block of text directly under the
    # mouse cursor.

    ($x, $y) = ($widget->window()->get_pointer())[1 .. 2];
    ($x, $y) = $widget->window_to_buffer_coords("widget", $x, $y);
    if (defined($iter = ($widget->get_line_at_y($y))[0]))
    {

        my ($prefix,
            $no_more);
        my $end_iter = $iter->copy();
        my $start_iter = $iter->copy();
        my $text_buffer = $widget->get_buffer();

        $end_iter->forward_to_line_end() unless ($end_iter->ends_line());
        $prefix = substr($text_buffer->get_text($start_iter, $end_iter, TRUE),
                         0,
                         $instance->{prefix_length});
        while ($prefix !~ m/^ *[0-9a-f]+\.+.*$/)
        {
            if (! $start_iter->backward_line())
            {
                $no_more = 1;
                last;
            }
            $end_iter->backward_line();
            $end_iter->forward_to_line_end() unless ($end_iter->ends_line());
            $prefix = substr($text_buffer->get_text($start_iter,
                                                    $end_iter,
                                                    TRUE),
                             0,
                             $instance->{prefix_length});
        }
        ($revision_part) = ($prefix =~ m/^ *([0-9a-f]+)\.+.*$/)
            unless ($no_more);

    }

    # Add a number of display, browse and comparison options to the right-click
    # menu that acts on the revision responsible for the text directly under
    # the mouse cursor.

    $separator = Gtk2::SeparatorMenuItem->new();
    $separator->show();
    $menu->append($separator);

    $menu_item = Gtk2::MenuItem->new(__("Display Change _Log"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      display_change_log($instance->{mtn},
                                                         $revision_id,
                                                         "",
                                                         undef);
                                  },
              progress_message => __("Displaying change log"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $menu_item = Gtk2::MenuItem->new(__("Display File _History"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      my $old_file_name;
                                      $instance->{mtn}->get_corresponding_path
                                          (\$old_file_name,
                                           $instance->{revision_id},
                                           $instance->{file_name},
                                           $revision_id);
                                      display_file_change_history
                                          ($instance->{mtn},
                                           $revision_id,
                                           $old_file_name);
                                  },
              progress_message => __("Displaying file history"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $menu_item = Gtk2::MenuItem->new(__("Display _Revision History"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      display_revision_change_history
                                          ($instance->{mtn},
                                           undef,
                                           $revision_id);
                                  },
              progress_message => __("Displaying revision history"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $separator = Gtk2::SeparatorMenuItem->new();
    $separator->show();
    $menu->append($separator);

    $menu_item = Gtk2::MenuItem->new(__("_Browse File"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      my (@certs,
                                          $dir,
                                          $file,
                                          $old_file_name);
                                      my $branch = "";
                                      $instance->{mtn}->certs(\@certs,
                                                              $revision_id);
                                      foreach my $cert (@certs)
                                      {
                                          if ($cert->{name} eq "branch"
                                              && ($branch eq ""
                                                  || $cert->{value}
                                                      lt $branch))
                                          {
                                              $branch = $cert->{value};
                                          }
                                      }
                                      $instance->{mtn}->get_corresponding_path
                                          (\$old_file_name,
                                           $instance->{revision_id},
                                           $instance->{file_name},
                                           $revision_id);
                                      $dir = dirname($old_file_name);
                                      $dir = "" if ($dir eq ".");
                                      $file = basename($old_file_name);
                                      get_browser_window($instance->{mtn},
                                                         $branch,
                                                         $revision_id,
                                                         $dir,
                                                         $file);
                                  },
              progress_message => __("Displaying revision in a new browser"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $separator = Gtk2::SeparatorMenuItem->new();
    $separator->show();
    $menu->append($separator);

    $menu_item =
        Gtk2::MenuItem->new(__("Compare File With Previous _Version"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      compare_file_with_previous($instance,
                                                                 $revision_id);
                                  },
              progress_message => __("Doing file comparison"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $menu_item =
        Gtk2::MenuItem->new(__("Compare Revision _With Parent"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      compare_revision_with_parent
                                          ($instance, $revision_id);
                                  },
              progress_message => __("Doing revision comparison"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $separator = Gtk2::SeparatorMenuItem->new();
    $separator->show();
    $menu->append($separator);

    $menu_item =
        Gtk2::MenuItem->new(__("A_nnotate File At Previous Version"));
    if (! defined($revision_part))
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             \&annotation_textview_popup_menu_item_cb,
             {instance         => $instance,
              cb               => sub {
                                      my ($instance, $revision_id, $iter) = @_;
                                      annotate_previous_version
                                          ($instance, $revision_id, $iter);
                                  },
              progress_message => __("Doing file annotation"),
              revision_part    => $revision_part,
              iter             => $iter});
    }
    $menu_item->show();
    $menu->append($menu_item);

}
#
##############################################################################
#
#   Routine      - annotation_textview_popup_menu_item_cb
#
#   Description  - Callback routine called when the user selects an item on
#                  the annotation textview's popup menu.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $menu    : The Gtk2::Menu widget that is to be updated.
#                  $details : A reference to an anonymous hash containing the
#                             window instance, callback routine, progress
#                             message, the start of the revision id extracted
#                             from the annotated listing and the
#                             Gtk2::TextIter for the start of the text line
#                             under the mouse when the user right clicked.
#
##############################################################################



sub annotation_textview_popup_menu_item_cb($$)
{

    my ($widget, $details) = @_;

    return if ($details->{instance}->{in_cb});
    local $details->{instance}->{in_cb} = 1;

    my @revision_ids;
    my $wm = WindowManager->instance();

    $wm->make_busy($details->{instance}, 1);
    $details->{instance}->{appbar}->
        push($details->{instance}->{appbar}->get_status()->get_text());
    $details->{instance}->{appbar}->set_status($details->{progress_message});
    $wm->update_gui();

    $details->{instance}->{mtn}->
        select(\@revision_ids, "i:" . $details->{revision_part});
    if (scalar(@revision_ids) == 1)
    {
        $details->{cb}($details->{instance},
                       $revision_ids[0],
                       $details->{iter});
    }
    else
    {
        my $dialog = Gtk2::MessageDialog->new
            ($details->{instance}->{window},
             ["modal"],
             "warning",
             "close",
             __("Cannot access a unique revision id."));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

    $details->{instance}->{appbar}->pop();
    $wm->make_busy($details->{instance}, 0);

}
#
##############################################################################
#
#   Routine      - compare_file_with_previous
#
#   Description  - Compare the file at the specified version with the previous
#                  version of the file.
#
#   Data         - $instance    : The window instance that is associated with
#                                 the annotation window.
#                  $revision_id : The revision id on which this file changed.
#
##############################################################################



sub compare_file_with_previous($$)
{

    my ($instance, $revision_id) = @_;

    my ($ancestor_id,
        $file_name,
        $old_file_name);

    if (get_files_previous_version_details($instance,
                                           $revision_id,
                                           \$ancestor_id,
                                           \$file_name,
                                           \$old_file_name))
    {

        # Ok now make sure the file name hasn't changed. If it has then use the
        # external differences tool.

        if ($old_file_name ne $file_name)
        {
            display_renamed_file_comparison($instance->{window},
                                            $instance->{mtn},
                                            $ancestor_id,
                                            $old_file_name,
                                            $revision_id,
                                            $file_name);
        }
        else
        {
            display_revision_comparison($instance->{mtn},
                                        $ancestor_id,
                                        $revision_id,
                                        $file_name);
        }

    }

}
#
##############################################################################
#
#   Routine      - annotate_previous_version
#
#   Description  - Annotate the previous version of the file.
#
#   Data         - $instance    : The window instance that is associated with
#                                 the annotation window.
#                  $revision_id : The revision id on which this file changed.
#                  $iter        : The Gtk2::TextIter for the start of the text
#                                 line under the mouse when the user right
#                                 clicked.
#
##############################################################################



sub annotate_previous_version($$$)
{

    my ($instance, $revision_id, $iter) = @_;

    my ($ancestor_id,
        $file_name,
        $old_file_name);

    if (get_files_previous_version_details($instance,
                                           $revision_id,
                                           \$ancestor_id,
                                           \$file_name,
                                           \$old_file_name))
    {

        my ($line_nr,
            $sync_details);

        # We have an iter representing where we are in the listing. This could
        # be used to jump to a line in the previous version of the file but the
        # contents has changed. Therefore build up a list of text either side
        # of the line so that we can later scan the previous version of the
        # file looking for a matching line and hence get back in sync.

        if (defined($iter))
        {
            my (@backwards,
                $details,
                @forwards);

            $line_nr = $iter->get_line();
            $details = get_significant_lines_around_iter($instance,
                                                         $iter,
                                                         LINE_SYNC_SIZE);

            # Strip off the iter objects.

            @backwards = map($_->{text}, @{$details->{backwards}});
            @forwards = map($_->{text}, @{$details->{forwards}});
            $sync_details = {current   => $details->{current}->{text},
                             backwards => \@backwards,
                             forwards  => \@forwards};
        }

        display_annotation($instance->{mtn},
                           $ancestor_id,
                           $old_file_name,
                           $line_nr,
                           $sync_details);

    }

}
#
##############################################################################
#
#   Routine      - get_files_previous_version_details
#
#   Description  - Tries to get details about the previous version of the
#                  file, dealing with assorted edge cases.
#
#   Data         - $instance      : The window instance that is associated
#                                   with the annotation window.
#                  $revision_id   : The current revision id on which this file
#                                   changed.
#                  $ancestor_id   : A reference to a variable that is to
#                                   contain the ancestor revision id where the
#                                   file previously last changed.
#                  $file_name     : A reference to a variable that is to
#                                   contain the name of the file in the
#                                   current revision. This will be different
#                                   if the file was renamed.
#                  $old_file_name : A reference to a variable that is to
#                                   contain the name of the file in the
#                                   ancestor revision. This will be different
#                                   if the file was renamed.
#                  Return Value   : Either true if everything was ok, no edge
#                                   cases etc, otherwise false if there was an
#                                   edge case or something went wrong (either
#                                   way it was dealt with).
#
##############################################################################



sub get_files_previous_version_details($$$$$)
{

    my ($instance, $revision_id, $ancestor_id, $file_name, $old_file_name)
        = @_;

    my (@chg_ancestors,
        @parents);

    # Remember that a warning is generated when one goes back beyond a file's
    # addition revision, so temporarily disable the warning handler.

    {

        local $suppress_mtn_warnings = 1;

        # First get the name of the file at the specified revision (it might
        # have been moved or renamed).

        $instance->{mtn}->get_corresponding_path($file_name,
                                                 $instance->{revision_id},
                                                 $instance->{file_name},
                                                 $revision_id);

        # Get the revision's parent and then find out when the file last
        # changed.

        $instance->{mtn}->parents(\@parents, $revision_id);
        if (scalar(@parents) > 1)
        {
            my $dialog = Gtk2::MessageDialog->new
                ($instance->{window},
                 ["modal"],
                 "info",
                 "close",
                 __("The selected revision has more than one parent.\n"
                    . "I will display the file's history so you can select\n"
                    . "the specific parent revision."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            display_file_change_history($instance->{mtn},
                                        $revision_id,
                                        $$file_name);
            return;
        }
        elsif (scalar(@parents) == 0)
        {
            my $dialog = Gtk2::MessageDialog->new
                ($instance->{window},
                 ["modal"],
                 "info",
                 "close",
                 __("The selected revision has no parents."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }
        $instance->{mtn}->get_content_changed(\@chg_ancestors,
                                              $parents[0],
                                              $$file_name);
        if (scalar(@chg_ancestors) > 1)
        {
            my $dialog = Gtk2::MessageDialog->new
                ($instance->{window},
                 ["modal"],
                 "info",
                 "close",
                 __("The current version of the file probably\n"
                    . "resulted from a merge as it is directly\n"
                    . "descended from multiple versions.\n"
                    . "I will display the file's history so you\n"
                    . "can select the specific parent revision."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            display_file_change_history($instance->{mtn},
                                        $revision_id,
                                        $$file_name);
            return;
        }
        elsif (scalar(@chg_ancestors) == 0)
        {
            my $dialog = Gtk2::MessageDialog->new
                ($instance->{window},
                 ["modal"],
                 "info",
                 "close",
                 __("The selected file version has no ancestors."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }

    }

    # Ok now get the old name of the file.

    $instance->{mtn}->get_corresponding_path($old_file_name,
                                             $revision_id,
                                             $$file_name,
                                             $chg_ancestors[0]);

    # Return remaining information.

    $$ancestor_id = $chg_ancestors[0];

    return 1;

}
#
##############################################################################
#
#   Routine      - compare_revision_with_parent
#
#   Description  - Compare the revision at the specified version with its
#                  parent.
#
#   Data         - $instance    : The window instance that is associated with
#                                 the annotation window.
#                  $revision_id : The revision id that is to be compared
#                                 against its parent.
#
##############################################################################



sub compare_revision_with_parent($$)
{

    my ($instance, $revision_id) = @_;

    my @parents;

    # First get the revision's parent(s).

    $instance->{mtn}->parents(\@parents, $revision_id);
    if (scalar(@parents) > 1)
    {
        my $dialog = Gtk2::MessageDialog->new
            ($instance->{window},
             ["modal"],
             "info",
             "close",
             __("The selected revision has more than one parent.\n"
                . "I will display the revision's history so you can select\n"
                . "the specific parent revision."));
        busy_dialog_run($dialog);
        $dialog->destroy();
        display_revision_change_history($instance->{mtn}, undef, $revision_id);
        return;
    }
    elsif (scalar(@parents) == 0)
    {
        my $dialog = Gtk2::MessageDialog->new
            ($instance->{window},
             ["modal"],
             "info",
             "close",
             __("The selected revision has no parents."));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }

    # Ok now compare the revisions and display the results.

    display_revision_comparison($instance->{mtn}, $parents[0], $revision_id);

}
#
##############################################################################
#
#   Routine      - get_annotation_window
#
#   Description  - Creates or prepares an existing annotation window for use.
#
#   Data         - Return Value : A reference to the newly created or unused
#                                 annotation instance record.
#
##############################################################################



sub get_annotation_window()
{

    my $instance;
    my $window_type = "annotation_window";
    my $wm = WindowManager->instance();

    # Create a new annotation window if an unused one wasn't found, otherwise
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
        foreach my $widget ("appbar",
                            "annotation_textview",
                            "annotation_scrolledwindow")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $window_type);

        # Setup the annotation window deletion handler.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 hide_find_text($instance->{annotation_textview});
                 $widget->hide();
                 $instance->{annotation_buffer}->set_text("");
                 $instance->{mtn} = undef;
                 return TRUE;
             },
             $instance);

        # Setup the revision annotation viewer.

        $instance->{annotation_buffer} =
            $instance->{annotation_textview}->get_buffer();
        create_format_tags($instance->{annotation_buffer});
        $instance->{annotation_textview}->modify_font($mono_font);

        # Display the window (it needs to be realised before it is registered).

        $instance->{window}->show_all();
        $instance->{window}->present();

        # Register the window for management and set up the help callbacks.

        $wm->manage($instance, $window_type, $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => undef,
              help_ref => __("mtnb-lachc-the-annotated-listing-window")});

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        set_window_size($instance->{window}, $window_type);
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->clear_stack();

    }

    local $instance->{in_cb} = 1;

    # Empty out the contents.

    $instance->{annotation_buffer}->set_text("");

    return $instance;

}
#
##############################################################################
#
#   Routine      - sync_line_position
#
#   Description  - Attempt to synchronise the current line position between
#                  different versions of the file.
#
#                  Just to clarify subsequent comments:
#                  The previous version of the file is the one where the user
#                  selected the annotate file at previous version menu option
#                  (i.e. the younger version of the file) and the current
#                  version is the one currently being annotated.
#
#   Data         - $instance    : The window instance that is associated with
#                                 the annotation window.
#                  $line_nr     : The number of the line that should be
#                                 scrolled to once the file has been
#                                 annotated (starting from 0). This is
#                                 otional.
#                  $sync_data   : A reference to a record containing the
#                                 current line, previous lines and subsequent
#                                 lines. This is used to attempt to
#                                 synchronise a position between different
#                                 versions of the file. This is optional.
#                  Return Value : Either a Gtk2::TextIter marking the position
#                                 of the line in the buffer on success or
#                                 undef on failure.
#
##############################################################################



sub sync_line_position($$$)
{

    my ($instance, $line_nr, $sync_data) = @_;

    my ($details,
        $done,
        $goto_iter,
        %line_hash,
        @matches,
        $nr_backward_lines,
        $nr_forward_lines);

    # Get an iter for the current line.

    return unless (defined($goto_iter = $instance->{annotation_buffer}->
                           get_iter_at_line($line_nr)));
    $goto_iter->backward_line() unless $goto_iter->starts_line();

    # Get surrounding line details.

    $details = get_significant_lines_around_iter($instance,
                                                 $goto_iter,
                                                 LINE_SYNC_SIZE);

    # Cheat a little and add the details of the current line to the start of
    # the backwards list.

    unshift(@{$details->{backwards}}, $details->{current});

    # Build up a hash containing information about the lines of text around the
    # current line in the current version of the file. The hash key is the SHA1
    # hash of the text and the value is a list of records containing the list
    # that the line came from, it's index in that list and the size of the list
    # (cached for efficiency reasons).

    $nr_backward_lines = scalar(@{$details->{backwards}});
    $nr_forward_lines = scalar(@{$details->{forwards}});
    for (my $i = 0, $done = undef; ! $done; ++ $i)
    {
        my @items;
        $done = 1;
        if ($i < $nr_backward_lines)
        {
            push(@items,
                 {list  => $details->{backwards},
                  size  => $nr_backward_lines,
                  key   => sha1($details->{backwards}->[$i]->{text})});
        }
        if ($i < $nr_forward_lines)
        {
            push(@items,
                 {list  => $details->{forwards},
                  size  => $nr_forward_lines,
                  key   => sha1($details->{forwards}->[$i]->{text})});
        }
        foreach my $item (@items)
        {
            if (exists($line_hash{$item->{key}}))
            {
                push(@{$line_hash{$item->{key}}}, {list  => $item->{list},
                                                   size  => $item->{size},
                                                   index => $i});
            }
            else
            {
                $line_hash{$item->{key}} = [{list  => $item->{list},
                                             size  => $item->{size},
                                             index => $i}];
            }
            $done = undef;
        }
    }

    # Now go through the sync data (which came from the previous version of the
    # file) looking for matching lines.

    $nr_backward_lines = scalar(@{$sync_data->{backwards}});
    $nr_forward_lines = scalar(@{$sync_data->{forwards}});
    for (my $i = 0, $done = undef; ! $done; ++ $i)
    {

        $done = 1;

        # Process the next line from the backwards list and then the forwards
        # one.

        foreach my $direction ({size => $nr_backward_lines,
                                list => $sync_data->{backwards}},
                               {size => $nr_forward_lines,
                                list => $sync_data->{forwards}})
        {

            # Only do something if we haven't come to the end of the list.

            if ($i < $direction->{size})
            {

                my $key = sha1($direction->{list}->[$i]);

                $done = undef;

                # See if we have a matching line in the current version of the
                # file.

                if (exists($line_hash{$key}))
                {

                    # Yes we have so check each instance of that line of text.

                    foreach my $entry (@{$line_hash{$key}})
                    {

                        my $index = $entry->{index};
                        my $list = $entry->{list};
                        my $match = 1;
                        my $size = $entry->{size};

                        # Ok so now make sure we have a match with the
                        # surrounding lines also (two in both directions).

                        foreach my $j (-2, -1, 1, 2)
                        {
                            my $curr = (($index + $j) < 0
                                        || ($index + $j) >= $size)
                                ? "" : $$list[$index + $j]->{text};
                            my $prev = (($i + $j) < 0
                                        || ($i + $j) >= $direction->{size})
                                ? "" : $direction->{list}->[$i + $j];
                            if ($prev ne $curr)
                            {
                                $match = undef;
                                last;
                            }
                        }
                        if ($match)
                        {

                            # Ok we have a match so save the details. The
                            # offset field basically contains the distance from
                            # the original line in the previous version of the
                            # file to a line block of lines that match in the
                            # current version of the file.

                            push(@matches, {iter   => $$list[$index]->{iter},
                                            offset => $i});
                            last;

                        }

                    }

                }

            }

        }

    }

    # If we have some matches then sort them on their offsets and return t he
    # first one. In English basically pick the matching block of text which is
    # as close to the original line in the previous version of the file as we
    # can get.

    if (scalar(@matches) > 0)
    {
        @matches = sort({ $a->{offset} <=> $b->{offset} } @matches);
        return $matches[0]->{iter};
    }

    return;

}
#
##############################################################################
#
#   Routine      - get_significant_lines_around_iter
#
#   Description  - Return the significant lines of text around the specified
#                  Gtk2::TextIter (assumed to be at the start of a line).
#                  Significant lines are deemed to be non-whitespace lines
#                  with at least four non-whitespace characters on them
#                  (bascially it is not much use matching lines just
#                  containing braces etc).
#
#   Data         - $instance    : The window instance that is associated with
#                                 the annotation window.
#                  $iter        : The Gtk2::TextIter for the start of the text
#                                 line under the mouse when the user right
#                                 clicked.
#                  $nr_lines    : The number of lines to go back and forward
#                                 by from the specified iter.
#                  Return Value : A reference to a record containing the text
#                                 in the buffer around the specified iter on a
#                                 line by line basis along with associated
#                                 iters.
#
##############################################################################



sub get_significant_lines_around_iter($$$)
{

    my ($instance, $iter, $nr_lines) = @_;

    my (@backwards,
        $current,
        $end_iter,
        @forwards,
        $prefix_length,
        $start_iter);
    my $text_buffer = $instance->{annotation_textview}->get_buffer();

    # Remember that the prefix length doesn't include the extra space we put in
    # to separate the prefix from the text.

    $prefix_length = $instance->{prefix_length} + 1;

    # Ok so start storing non-whitespace lines back from but also including the
    # current line. Also strip out the annotation prefix and any trailing
    # whitespace padding that was added during the pretty printing stage.

    # Go backwards.

    $start_iter = $iter->copy();
    $end_iter = $iter->copy();
    $end_iter->forward_to_line_end() unless ($end_iter->ends_line());
    for (my $i = 0; $i <= $nr_lines;)
    {
        my $line = substr($text_buffer->get_text($start_iter,
                                                 $end_iter,
                                                 TRUE),
                          $prefix_length);
        if ($line =~ m/^.*\S{4}.*$/)
        {
            $line =~ s/\s+$//;
            push(@backwards, {iter => $start_iter->copy(), text => $line});
            ++ $i;
        }
        last unless ($start_iter->backward_line());
        $end_iter->backward_line();
        $end_iter->forward_to_line_end() unless ($end_iter->ends_line());
    }
    $current = shift(@backwards);

    # Go forwards.

    $start_iter = $iter->copy();
    $end_iter = $iter->copy();
    $end_iter->forward_to_line_end() unless ($end_iter->ends_line());
    for (my $i = 0; $i <= $nr_lines;)
    {

        # Skip the iter line.

        if ($i > 0)
        {
            my $line = substr($text_buffer->get_text($start_iter,
                                                     $end_iter,
                                                     TRUE),
                              $prefix_length);
            if ($line =~ m/^.*\S{4}.*$/)
            {
                $line =~ s/\s+$//;
                push(@forwards, {iter => $start_iter->copy(), text => $line});
                ++ $i;
            }
        }
        else
        {
            ++ $i;
        }

        last unless ($start_iter->forward_line());
        $end_iter->forward_to_line_end();

    }

    return {current   => $current,
            backwards => \@backwards,
            forwards  => \@forwards};

}
#
##############################################################################
#
#   Routine      - mtn_annotate
#
#   Description  - Annotate the specified file on the specified revision.
#
#   Data         - $list        : A reference to the list that is to contain
#                                 the output from the annotate command.
#                  $mtn_db      : The Monotone database that is to be used or
#                                 undef if the database associated with the
#                                 current workspace is to be used.
#                  $revision_id : The revision id on which the desired version
#                                 of the file resides.
#                  $file_name   : The name of file that is to be annotated.
#                  Return Value : True if the comparison worked, otherwise
#                                 false if something went wrong.
#
##############################################################################



sub mtn_annotate($$$$)
{

    my ($list, $mtn_db, $revision_id, $file_name) = @_;

    my ($buffer,
        @cmd,
        $cwd,
        $error_msg,
        $ok);

    # Run mtn annotate in the root directory so as to avoid any workspace
    # conflicts.

    @$list = ();
    push(@cmd, "mtn");
    push(@cmd, "--db=" . $mtn_db) if (defined($mtn_db));
    push(@cmd, "annotate");
    push(@cmd, "-r");
    push(@cmd, "i:" . $revision_id);
    push(@cmd, $file_name);
    $cwd = getcwd();
    eval
    {
        die("chdir failed: " . $!) unless (chdir(File::Spec->rootdir()));
        $ok = run_command(\$buffer,
                          undef,
                          undef,
                          undef,
                          undef,
                          \$error_msg,
                          @cmd);
        cleanup_mtn_error_message(\$error_msg) unless ($ok);
    };
    $error_msg = $@ if ($@);
    chdir($cwd);
    if (! $ok)
    {
        my $dialog = Gtk2::MessageDialog->new_with_markup
            (undef,
             ["modal"],
             "warning",
             "close",
             __x("Problem running mtn annotate, got:\n"
                     . "<b><i>{error_message}</i></b>",
                 error_message => Glib::Markup::escape_text($error_msg)));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }

    # Break up the input into a list of lines.

    @$list = split(/\n/, $buffer);

    return $ok;

}

1;
