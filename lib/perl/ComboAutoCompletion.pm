##############################################################################
#
#   File Name    - ComboAutoCompletion.pm
#
#   Description  - The custom combo entry auto-completion utilities module for
#                  the mtn-browse application. This module contains assorted
#                  routines that implement auto-completion for all branch,
#                  revision and directory combo entries. Standard comboboxentry
#                  widgets were not used due to performance issues with large
#                  completion lists. At least with these custom combo entries,
#                  the only performance penalty is when the user is actually
#                  displaying the completion list and not all of the time.
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

# The types of windows that are going to be managed by this module.

my $completions_window_type = "completions_window";
my $tooltip_window_type = "tooltip_window";

# A hash of event types that are to be examined when determining when to
# dismiss a completions window. Please note that keyboard events are not in
# this list as they get grabbed by any Gnome2::App widget, so we rely on focus
# notification on the entry widget itself.

my %user_activity_events = ("2button-press"  => undef,
                            "3button-press"  => undef,
                            "button-press"   => undef,
                            "button-release" => undef,
                            "delete"         => undef,
                            "scroll"         => undef);

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub activate_auto_completion($$);
sub tagged_checkbutton_toggled_cb($$);

# Private routines.

sub auto_completion_entry_key_release_event_cb($$$);
sub calculate_window_coordinates($$);
sub completions_list_togglebutton_toggled_cb($$);
sub completions_treeselection_changed_cb($$);
sub completions_window_done_event_handler($$);
sub get_completions_window($$$$$);
sub get_tooltip_window($$$$);
sub hide_completions_window();
sub hide_tooltip_window();
sub update_completions_list_window($$$$$);
#
##############################################################################
#
#   Routine      - activate_auto_completion
#
#   Description  - Sets up the specified combo entry widget for
#                  auto-completion.
#
#   Data         - $entry    : The entry widget that is to be set up for
#                              auto-completion.
#                  $instance : The window instance that is associated with
#                              this widget. It is expected to have a window,
#                              appbar, update_handler and custom combo entry
#                              details.
#
##############################################################################



sub activate_auto_completion($$)
{

    my ($entry, $instance) = @_;

    my ($change_state,
        $combo_details,
        $move_to_end,
        $name,
        $togglebutton);

    # Sort out the precise details depending upon which custom combo entry
    # widget has been passed in.

    if ($entry == $instance->{branch_entry})
    {
        $change_state = BRANCH_CHANGED;
        $combo_details = $instance->{branch_combo_details};
        $move_to_end = 1;
        $name = __("branch");
        $togglebutton = $instance->{branch_list_togglebutton};
    }
    elsif ($entry == $instance->{revision_entry})
    {
        $change_state = REVISION_CHANGED;
        $combo_details = $instance->{revision_combo_details};
        $name = __("revision");
        $togglebutton = $instance->{revision_list_togglebutton};
    }
    elsif ($entry == $instance->{directory_entry})
    {
        $change_state = DIRECTORY_CHANGED;
        $combo_details = $instance->{directory_combo_details};
        $move_to_end = 1;
        $name = __("directory");
        $togglebutton = $instance->{directory_list_togglebutton};
    }
    else
    {
        return;
    }

    # Set up all the required callbacks.

    $entry->signal_connect("key_release_event",
                           \&auto_completion_entry_key_release_event_cb,
                           {instance      => $instance,
                            change_state  => $change_state,
                            combo_details => $combo_details,
                            name          => $name,
                            togglebutton  => $togglebutton});
    $togglebutton->signal_connect("toggled",
                                  \&completions_list_togglebutton_toggled_cb,
                                  {instance      => $instance,
                                   change_state  => $change_state,
                                   combo_details => $combo_details,
                                   entry         => $entry,
                                   move_to_end   => $move_to_end,
                                   name          => $name});
    $entry->signal_connect("focus_out_event",
                           sub {
                               hide_completions_window();
                               hide_tooltip_window();
                               return FALSE;
                           });

}
#
##############################################################################
#
#   Routine      - tagged_checkbutton_toggled_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the tagged check button.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub tagged_checkbutton_toggled_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{appbar}->clear_stack();
    &{$instance->{update_handler}}($instance, BRANCH_CHANGED);

}
#
##############################################################################
#
#   Routine      - completions_list_togglebutton_toggled_cb
#
#   Description  - Callback routine called when the user toggles the show
#                  completions list button.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $details : A reference to an anonymous hash containing the
#                             window instance, change state, custom combo
#                             entry details (including the widget), cursor
#                             handling hints and the name for that entry.
#
##############################################################################



sub completions_list_togglebutton_toggled_cb($$)
{

    my ($widget, $details) = @_;

    my $instance = $details->{instance};

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    if ($widget->get_active())
    {

        my ($completions,
            @item_list,
            $len,
            $x,
            $y);
        my $combo_details = $details->{combo_details};
        my $entry = $details->{entry};
        my $wm = WindowManager->instance();

        # Work out where to place the completions window.

        ($x, $y) = calculate_window_coordinates($instance->{window}, $entry);

        # Make sure the entry has the focus and then display the completions
        # window.

        if (! $entry->has_focus())
        {
            $entry->grab_focus();
            if ($details->{move_to_end})
            {
                $entry->set_position(-1);
            }
            else
            {
                $entry->set_position(0);
            }
        }
        $completions =
            get_completions_window($instance, $entry, $widget, $x, $y);

        # We can't go busy until now as the completions window installs a
        # custom main event handler.

        $wm->make_busy($instance, 1);
        $wm->update_gui();

        # Stash any data needed by the treeview selection changed callback.

        $completions->{details} = $details;

        # Now update the completions window.

        $len = length($combo_details->{filter});
        foreach my $item (@{$combo_details->{list}})
        {
            my $item_len = length($item);
            push(@item_list, $item)
                if ($len <= $item_len
                    && $combo_details->{filter} eq substr($item, 0, $len));
        }
        update_completions_list_window($instance->{appbar},
                                       $details->{name},
                                       \@item_list,
                                       refaddr($combo_details),
                                       $combo_details->{update});
        $combo_details->{update} = 0;

        $wm->make_busy($instance, 0);

    }
    else
    {
        hide_completions_window();
    }

}
#
##############################################################################
#
#   Routine      - auto_completion_entry_key_release_event_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  a branch, revision or directory entry by entering a
#                  character (key release event).
#
#   Data         - $widget      : The widget object that received the signal.
#                  $event       : A Gtk2::Gdk::Event object describing the
#                                 event that has occurred.
#                  $details     : A reference to an anonymous hash containing
#                                 the window instance, change state, custom
#                                 combo entry details, the name for that entry
#                                 and the associated togglebutton.
#                  Return Value : TRUE if the event has been handled and needs
#                                 no further handling, otherwise false if the
#                                 event should carry on through the remaining
#                                 event handling.
#
##############################################################################



sub auto_completion_entry_key_release_event_cb($$$)
{

    my ($widget, $event, $details) = @_;

    my $instance = $details->{instance};

    return FALSE if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $combo_details = $details->{combo_details};
    my $old_value = $combo_details->{value};
    my $value = $widget->get_text();

    # The user has typed something in then validate it and auto-complete it if
    # necessary.

    if ($value ne $old_value)
    {

        my ($auto_completion_triggered,
            $busy,
            $completion,
            $len,
            $success);
        my $completion_list_displayed = $details->{togglebutton}->get_active();
        my $complete = 0;
        my $old_complete = $combo_details->{complete};
        my $wm = WindowManager->instance();

        # Don't auto-complete if the user is simply deleting from the extreme
        # right (otherwise we'll just put back what they have deleted!).

        $len = length($value);
        if ($len >= length($old_value)
            || $value ne substr($old_value, 0, $len))
        {

            # Initialise a new auto-completion object with a new list of terms
            # if it hasn't been done so already.

            $combo_details->{completion} =
                Completion->new($combo_details->{list})
                if (! defined($combo_details->{completion}));

            # Try auto-completing with what we have got, if that fails then try
            # stripping off any trailing white space before trying again (this
            # means the user can use the spacebar to trigger auto-completion in
            # a similar fashion to bash's use of <Tab>).

            if (! ($success = $combo_details->{completion}->
                       get_completion($value, \$completion, \$complete)))
            {
                $value =~ s/\s+$//;
                $success = $combo_details->{completion}->
                       get_completion($value, \$completion, \$complete);
            }
            if ($success)
            {
                $instance->{appbar}->clear_stack();
                hide_tooltip_window();
                $auto_completion_triggered = 1;
            }
            else
            {

                my $message;

                # Tell the user what is wrong via the status bar.

                $message = __x("Invalid {name} name `{value}'",
                               name  => $details->{name},
                               value => $value);
                $instance->{appbar}->set_status($message);

                # Also via a tooltip as well if so desired (need to position it
                # to be just below the entry widget).

                if ($user_preferences->{completion_tooltips})
                {
                    my ($x, $y) = calculate_window_coordinates
                        ($instance->{window}, $widget);
                    $x -= 10;
                    get_tooltip_window($instance->{window}, $message, $x, $y);
                }

            }

            $value = $completion;
            $len = length($value);
            $widget->set_text($value);
            $widget->set_position(-1);

        }
        else
        {
            $instance->{appbar}->clear_stack();
            hide_tooltip_window();
        }
        $combo_details->{filter} = $value;
        $combo_details->{update} = 1;
        $combo_details->{value} = $value;
        $combo_details->{complete} = $complete;

        # Has the value actually changed or the user triggered an
        # auto-completion update by pressing the space bar? Also remember that
        # what the user has entered may have been discarded due to not being
        # valid.

        if ($value ne $old_value
            || ($completion_list_displayed && $auto_completion_triggered))
        {

            my @item_list;

            # Yes so scan through the list of matches looking for a complete
            # match. This is needed when the user is simply deleting characters
            # from the right and we can't use the autocompletion object to
            # determine completion (as it may do this by adding unique text
            # onto the end of the user's input). Also update any visible
            # completions window.

            $wm->make_busy($instance, 1);
            $busy = 1;
            $wm->update_gui();

            foreach my $item (@{$combo_details->{list}})
            {
                my $item_len = length($item);
                if ($len <= $item_len && $value eq substr($item, 0, $len))
                {
                    push(@item_list, $item)
                        if ($completion_list_displayed);
                    if ($len == $item_len)
                    {
                        $combo_details->{complete} = 1;
                        last unless ($completion_list_displayed);
                    }
                }
            }
            if ($completion_list_displayed)
            {
                update_completions_list_window($instance->{appbar},
                                               $details->{name},
                                               \@item_list,
                                               refaddr($combo_details),
                                               $combo_details->{update});
                $combo_details->{update} = 0;
            }

        }

        # Update the window state on a significant change.

        if ($combo_details->{complete} != $old_complete
            || ($combo_details->{complete}
                && $combo_details->{value} ne $old_value))
        {
            if (! $busy)
            {
                $wm->make_busy($instance, 1);
                $busy = 1;
            }
            $wm->update_gui();
            &{$instance->{update_handler}}($instance,
                                           $details->{change_state});
        }

        $wm->make_busy($instance, 0) if ($busy);

    }

    return FALSE;

}
#
##############################################################################
#
#   Routine      - completions_treeselection_changed_cb
#
#   Description  - Callback routine called when the user selects an entry in
#                  the completions list treeview in the completions window.
#
#   Data         - $widget      : The widget object that received the signal.
#                  $completions : The completions instance that is associated
#                                 with this widget.
#
##############################################################################



sub completions_treeselection_changed_cb($$)
{

    my ($widget, $completions) = @_;

    return if ($completions->{in_cb});
    local $completions->{in_cb} = 1;

    # Only do something if an entry is selected.

    if ($widget->count_selected_rows() > 0)
    {

        my ($iter,
            $model,
            $timeout_source_id,
            $timer_active);
        my $change_state = $completions->{details}->{change_state};
        my $combo_details = $completions->{details}->{combo_details};
        my $entry = $completions->{details}->{entry};
        my $instance = $completions->{details}->{instance};
        my $move_to_end = $completions->{details}->{move_to_end};
        my $old_value = $combo_details->{value};

        return if ($instance->{in_cb});
        local $instance->{in_cb} = 1;

        # Get the selected entry. Please note that we specifically don't update
        # the combo_details' filter field here as selections from a completions
        # list do not update that list (this is like the old ComboBoxEntry
        # behaviour and is more useful to the user than if we did update the
        # list).

        ($model, $iter) = $widget->get_selected();
        $combo_details->{value} = $model->get($iter, 0);

        # Update the window.

        $combo_details->{complete} = 1;
        $instance->{appbar}->clear_stack();
        hide_tooltip_window();
        $entry->set_text($combo_details->{value});
        if ($move_to_end)
        {
            $entry->set_position(-1);
        }
        else
        {
            $entry->set_position(0);
        }

        # Dismiss the completions list window in half a second or so.

        $timeout_source_id =
            Glib::Timeout->add(500,
                               sub {
                                   hide_completions_window();
                                   $timer_active = undef;
                                   return FALSE;
                               });
        $timer_active = 1;

        # Update the parent window if necessary.

        &{$instance->{update_handler}}($instance, $change_state)
            unless ($old_value eq $combo_details->{value});

        # We are done so make sure that the completions list window is hidden.

        Glib::Source->remove($timeout_source_id) if ($timer_active);
        hide_completions_window();

    }

}
#
##############################################################################
#
#   Routine      - completions_window_done_event_handler
#
#   Description  - Event handler for determining when the completions window
#                  should be dismissed.
#
#   Data         - $event    : The Gtk2::Gdk::Event object representing the
#                              current event.
#                  $instance : The completions window instance that is
#                              associated with this event handler.
#
##############################################################################



sub completions_window_done_event_handler($$)
{

    my ($event, $instance) = @_;

    # Actually process the event.

    Gtk2->main_do_event($event);

    # Is it an event that we are interested in?

    if (exists($user_activity_events{$event->type()}))
    {

        my ($found,
            $widget);

        # Yes it is so see if the destination widget was anything to do with
        # the associated custom combo entry widgets.

        $widget = Gtk2->get_event_widget($event);
        while (defined($widget))
        {
            if (exists($instance->{event_widgets}->{$widget}))
            {
                $found = 1;
                last;
            }
            $widget = $widget->get_parent();
        }

        # If the event is for an unrelated part of the window then dismiss the
        # assocaited completions window.

        hide_completions_window() unless ($found);

    }

}
#
##############################################################################
#
#   Routine      - update_completions_list_window
#
#   Description  - Update the visible completions window with the specified
#                  list of values.
#
#   Data         - $appbar     : The appbar widget in the parent window that
#                                needs to be updated with any progress
#                                information.
#                  $name       : The name of the custom combo entry associated
#                                with the completions window.
#                  $list       : A reference to the list of values that are to
#                                be loaded into the completions window.
#                  $context_id : The unique id associated with the custom
#                                combo entry details.
#                  $update     : True if the completion list should be updated
#                                regardless of the internal state of the
#                                completions list window, otherwise false if
#                                the update should only be done if the
#                                completions list window thinks that it is
#                                necessary.
#
##############################################################################



sub update_completions_list_window($$$$$)
{

    my ($appbar, $name, $list, $context_id, $update) = @_;

    my $instance;
    my $wm = WindowManager->instance();

    # Only do something if there is a mapped completions window.

    if (defined($instance = $wm->cond_find
                ($completions_window_type,
                 sub {
                     my ($instance, $type) = @_;
                     return 1 if ($instance->{window}->mapped());
                 })))
    {

        return if ($instance->{in_cb});
        local $instance->{in_cb} = 1;

        # Only update the completion list if we have to.

        if ($update
            || $instance->{context_id} != $context_id
            || $instance->{completions_liststore}->iter_n_children() == 0)
        {

            my ($counter,
                $update_interval);

            $appbar->set_progress_percentage(0);
            $appbar->push(__x("Populating {name} list", name => $name));
            $wm->update_gui();
            $counter = 1;
            $update_interval = calculate_update_interval($list, 4);
            $instance->{completions_liststore}->clear();
            $instance->{completions_treeview}->hide();
            foreach my $item (@$list)
            {
                $instance->{completions_liststore}->
                    set($instance->{completions_liststore}->append(),
                        0, $item);
                if (($counter % $update_interval) == 0)
                {
                    $appbar->set_progress_percentage
                        ($counter / scalar(@$list));
                    $wm->update_gui();

                    # Abort this whole update process if the user has hidden
                    # the completions window by changing the focus.

                    if (! $instance->{window}->mapped())
                    {
                        $instance->{completions_liststore}->clear();
                        $list = [];
                        last;
                    }
                }
                ++ $counter;
            }
            $instance->{context_id} = $context_id;
            $appbar->set_progress_percentage(1);
            $wm->update_gui();

        }

        # Adjust the size of the completions window. Do this by using the size
        # of a cell in the treeview to calculate the treeview's total size. If
        # this total size exceeds what is desirable then resize the top level
        # window and activate/deactivate any scrollbars as necessary.

        if (scalar(@$list) > 0)
        {

            my ($h_policy,
                $height,
                $v_policy,
                $width,
                $x_size,
                $y_size);
            my ($default_width, $default_height) =
                $instance->{window}->get_default_size();
            my $tv_column = $instance->{completions_treeview}->get_column(0);

            ($width, $height) = ($tv_column->cell_get_size())[2, 3];
            $width += 2;
            $height = ($height * scalar(@$list)) + 2;

            $v_policy =
                ($instance->{completions_scrolledwindow}->get_policy())[1];
            $y_size = ($instance->{window}->get_size_request())[1];
            if ($width > $instance->{max_width})
            {
                $instance->{completions_scrolledwindow}->
                    set_policy("automatic", $v_policy);
                $instance->{window}->
                    set_size_request($instance->{max_width}, $y_size);
            }
            else
            {
                $instance->{completions_scrolledwindow}->
                    set_policy("never", $v_policy);
                $instance->{window}->resize
                    ($default_width,
                     ($y_size > 0) ? $y_size : $default_height);
                $instance->{window}->set_size_request(-1, $y_size);
            }

            $h_policy =
                ($instance->{completions_scrolledwindow}->get_policy())[0];
            $x_size = ($instance->{window}->get_size_request())[0];
            if ($height > $instance->{max_height})
            {
                $instance->{completions_scrolledwindow}->
                    set_policy($h_policy, "automatic");
                $instance->{window}->
                    set_size_request($x_size, $instance->{max_height});
            }
            else
            {
                $instance->{completions_scrolledwindow}->
                    set_policy($h_policy, "never");
                $instance->{window}->resize
                    (($x_size > 0) ? $x_size : $default_width,
                     $default_height);
                $instance->{window}->set_size_request($x_size, -1);
            }

        }

        $instance->{completions_treeview}->show();
        $instance->{completions_treeview}->scroll_to_point(0, 0)
            if ($instance->{completions_treeview}->realized());
        $instance->{completions_treeview}->get_selection()->unselect_all();

        $appbar->set_progress_percentage(0);
        $appbar->pop();
        $wm->update_gui();

    }

}
#
##############################################################################
#
#   Routine      - get_completions_window
#
#   Description  - Creates or prepares an existing entry completions window
#                  for use.
#
#   Data         - $parent_instance : The window instance that is making use
#                                     of the completions window.
#                  $entry           : The entry widget associated with the
#                                     completions window.
#                  $togglebutton    : The togglebutton widget associated with
#                                     the completions window.
#                  $x               : The x coordinate for where the
#                                     completions window is to be placed.
#                  $y               : The y coordinate for where the
#                                     completions window is to be placed.
#                  Return Value     : A reference to the newly created or
#                                     unused completions instance record.
#
##############################################################################



sub get_completions_window($$$$$)
{

    my ($parent_instance, $entry, $togglebutton, $x, $y) = @_;

    my ($glade,
        $height,
        $instance,
        $root_x,
        $root_y,
        $width,
        $appbar_y);
    my $wm = WindowManager->instance();

    # Create a new completions window if an existing one wasn't found,
    # otherwise reuse an existing one (used or unused).

    if (! defined($instance = $wm->cond_find($completions_window_type,
                                             sub { return 1; })))
    {

        my ($default_background_colour,
            $renderer,
            $tv_column);

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $completions_window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($completions_window_type);
        foreach my $widget ("completions_scrolledwindow",
                            "completions_treeview")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Setup the completions list.

        $instance->{completions_liststore} =
            Gtk2::ListStore->new("Glib::String");
        $instance->{completions_treeview}->
            set_model($instance->{completions_liststore});

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_sizing("autosize");
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => 0);
        $instance->{completions_treeview}->append_column($tv_column);

        $instance->{completions_treeview}->set_search_column(0);
        $instance->{completions_treeview}->
            set_search_equal_func(\&treeview_column_searcher);

        $instance->{completions_treeview}->get_selection()->
            signal_connect("changed",
                           \&completions_treeselection_changed_cb,
                           $instance);

        $instance->{context_id} = undef;

    }
    else
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        $instance->{window}->hide();
        ($width, $height) = $instance->{window}->get_default_size();
        $instance->{window}->resize($width, $height);
        $instance->{window}->set_size_request($width, $height);
        $instance->{completions_scrolledwindow}->set_policy("never", "never");
        $instance->{togglebutton}->set_active(FALSE)
            if (defined($instance->{togglebutton})
                && $togglebutton != $instance->{togglebutton});
    }

    local $instance->{in_cb} = 1;

    # Calculate the maximum allowable size for this window (do not let it
    # wander outside of the parent window nor obscure the progress bar).

    $appbar_y = ($parent_instance->{appbar}->translate_coordinates
                 ($parent_instance->{window}, 0, 0))[1];
    ($root_x, $root_y) = $parent_instance->{window}->window()->get_origin();
    $width = ($parent_instance->{window}->get_size())[0];
    $instance->{max_width} = $root_x + $width - $x;
    $instance->{max_height} = $root_y + $appbar_y - $y;

    # Position it, reparent window and display it.

    $instance->{window}->move($x, $y);
    $instance->{window}->set_transient_for($parent_instance->{window});
    $instance->{window}->show_all();
    $instance->{completions_treeview}->hide();
    $instance->{window}->present();

    # Stash any widgets associated with this completions window that we will
    # need later on and build up a set containing related widgets (used by the
    # dismissal event handler).

    $instance->{togglebutton} = $togglebutton;
    $instance->{event_widgets} = {};
    $instance->{event_widgets}->{$entry} = undef;
    $instance->{event_widgets}->{$togglebutton} = undef;
    $instance->{event_widgets}->{$instance->{window}} = undef;

    # Register an event handler that will dismiss the completions window as
    # soon as the user does something outside of it or the associated entry and
    # togglebutton.

    WindowManager->instance()->event_handler
        (\&completions_window_done_event_handler, $instance);

    # If necessary, register the window for management.

    $wm->manage($instance, $completions_window_type, $instance->{window})
        if (defined($glade));

    return $instance;

}
#
##############################################################################
#
#   Routine      - get_tooltip_window
#
#   Description  - Creates or prepares an existing tooltip window for use.
#
#   Data         - $parent       : The parent window widget for the tooltip
#                                  window.
#                  $message      : The tooltip that is to be displayed.
#                  $x            : The x coordinate for where the tooltip
#                                  window is to be placed.
#                  $y            : The y coordinate for where the tooltip
#                                  window is to be placed.
#                  Return Value  : A reference to the newly created or unused
#                                  tooltip instance record.
#
##############################################################################



sub get_tooltip_window($$$$)
{

    my ($parent, $message, $x, $y) = @_;

    my ($glade,
        $instance);
    my $wm = WindowManager->instance();

    # Create a new tooltip window if an existing one wasn't found, otherwise
    # reuse an existing one (used or unused).

    if (! defined($instance = $wm->cond_find($tooltip_window_type,
                                             sub { return 1; })))
    {

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $tooltip_window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($tooltip_window_type);
        foreach my $widget ("eventbox", "message_label")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Keep track of timers.

        $instance->{timer_active} = undef;

        # Setup the colours used for the tooltip window.

        $instance->{window}->modify_bg("normal",
                                       Gtk2::Gdk::Color->parse("Black"));
        $instance->{eventbox}->modify_bg("normal",
                                         Gtk2::Gdk::Color->parse("Pink"));

    }
    else
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        $instance->{window}->hide();
        Glib::Source->remove($instance->{timeout_source_id})
            if ($instance->{timer_active});
    }

    local $instance->{in_cb} = 1;

    # Update the tooltip message text and setup a timeout handler to dismiss
    # the window after three seconds.

    $instance->{message_label}->set_text($message);
    $instance->{timeout_source_id} =
        Glib::Timeout->add(3000,
                           sub {
                               my $instance = $_[0];
                               $instance->{window}->hide();
                               $instance->{timer_active} = undef;
                               return FALSE;
                           },
                           $instance);
    $instance->{timer_active} = 1;

    # Position it, reparent window and display it.

    $instance->{window}->move($x, $y);
    $instance->{window}->set_transient_for($parent);
    $instance->{window}->show_all();
    $instance->{window}->present();

    # If necessary, register the window for management.

    $wm->manage($instance, $tooltip_window_type, $instance->{window})
        if (defined($glade));

    return $instance;

}
#
##############################################################################
#
#   Routine      - calculate_window_coordinates
#
#   Description  - Calculate the coordinates for a popup window relative to
#                  the specified widget that is in the specified top level
#                  window.
#
#   Data         - $window      : The top level window widget that contains
#                                 the specified widget.
#   Data         - $widget      : The widget relative to which the coordinates
#                                 should be calculated.
#                  Return Value : A list containing the calculated x and y
#                                 coordinates.
#
##############################################################################



sub calculate_window_coordinates($$)
{

    my ($window, $widget) = @_;

    my ($height,
        $root_x,
        $root_y,
        $x,
        $y);
    ($x, $y) = $widget->translate_coordinates($window, 0, 0);
    $height = ($widget->window()->get_geometry())[3];
    ($root_x, $root_y) = $window->window()->get_origin();
    $x += $root_x;
    $y += $height + $root_y + 5;

    return ($x, $y);

}
#
##############################################################################
#
#   Routine      - hide_completions_window
#
#   Description  - Hides the completions window if it is visible.
#
#   Data         - None.
#
##############################################################################



sub hide_completions_window()
{

    my $instance;

    # Look for a mapped completions window, if found then hide it and empty its
    # contents.

    if (defined($instance = WindowManager->instance()->cond_find
                ($completions_window_type,
                 sub {
                     my ($instance, $type) = @_;
                     return $instance->{window}->mapped();
                 })))
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        WindowManager->instance()->event_handler();
        $instance->{window}->hide();
        $instance->{details} = undef;
        if (defined($instance->{togglebutton}))
        {
            $instance->{togglebutton}->set_active(FALSE);
            $instance->{togglebutton} = undef;
        }
    }

}
#
##############################################################################
#
#   Routine      - hide_tooltip_window
#
#   Description  - Hides the tooltip window if it is visible.
#
#   Data         - None.
#
##############################################################################



sub hide_tooltip_window()
{

    my $instance;

    # Look for a mapped tooltip window, if found then hide it and cancel its
    # hide timeout handler.

    if (defined($instance = WindowManager->instance()->cond_find
                ($tooltip_window_type,
                 sub {
                     my ($instance, $type) = @_;
                     return $instance->{window}->mapped();
                 })))
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        $instance->{window}->hide();
        $instance->{timer_active} = undef;
        Glib::Source->remove($instance->{timeout_source_id});
    }

}

1;
