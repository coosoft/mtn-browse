##############################################################################
#
#   File Name    - FindTextAndGoToLine.pm
#
#   Description  - The find text and go to line module for the mtn-browse
#                  application. This module contains all the routines for
#                  implementing find text and go to line windows.
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

my $find_text_window_type = "find_text_window";
my $goto_line_window_type = "goto_line_window";

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub enable_find_text_and_goto_line($$);
sub find_text($$);
sub find_text_and_goto_line_textview_key_press_event_cb($$$);
sub find_text_and_goto_line_textview_populate_popup_cb($$$);
sub goto_line($$);
sub hide_find_text_and_goto_line($);
sub reset_find_text($);

# Private routines.

sub find_comboboxentry_changed_cb($$);
sub find_current_window($$);
sub find_text_button_clicked_cb($$);
sub get_find_text_window($$);
sub get_goto_line_window($$);
sub goto_line_button_clicked_cb($$);
#
##############################################################################
#
#   Routine      - find_text
#
#   Description  - Display the find text window associated with the specified
#                  textview widget, creating one if necessary, and allow the
#                  user to search through the related text buffer.
#
#   Data         - $parent    : The parent window widget for the find text
#                               window.
#                  $text_view : The textview widget that is to be searched.
#
##############################################################################



sub find_text($$)
{

    my ($parent, $text_view) = @_;

    # Only go looking for a spare find text window, creating one if necessary,
    # if there isn't one already mapped for the specified textview widget.

    get_find_text_window($parent, $text_view)
        unless (defined(find_current_window($text_view,
                                            $find_text_window_type)));

    delete($text_view->{find_text_and_goto_line_disabled});

}
#
##############################################################################
#
#   Routine      - goto_line
#
#   Description  - Display the go to line window associated with the specified
#                  textview widget, creating one if necessary, and allow the
#                  user jump to a specific line in the related text buffer.
#
#   Data         - $parent    : The parent window widget for the go to line
#                               window.
#                  $text_view : The textview widget where the user wants to
#                               jump to a specific line.
#
##############################################################################



sub goto_line($$)
{

    my ($parent, $text_view) = @_;

    # Only go looking for a spare go to line window, creating one if necessary,
    # if there isn't one already mapped for the specified textview widget.

    get_goto_line_window($parent, $text_view)
        unless (defined(find_current_window($text_view,
                                            $goto_line_window_type)));

    delete($text_view->{find_text_and_goto_line_disabled});

}
#
##############################################################################
#
#   Routine      - reset_find_text
#
#   Description  - Resets the search context for the find text window
#                  associated with the specified textview widget.
#
#   Data         - $text_view : The textview widget to which the find text
#                               window is associated.
#
##############################################################################



sub reset_find_text($)
{

    my $text_view = $_[0];

    my $instance;

    # Simply reset the search context for the found find text window.

    $instance->{match_offset_start} = $instance->{match_offset_end} = -1
        if (defined($instance = find_current_window($text_view,
                                                    $find_text_window_type)));

}
#
##############################################################################
#
#   Routine      - enable_find_text_and_goto_line
#
#   Description  - Enables or disables the find text and go to line windows
#                  associated with the specified textview widget.
#
#   Data         - $text_view : The textview widget to which the find text
#                               window is associated.
#                  $enable    : True if the window is to be enabled,
#                               otherwise false if it is to be disabled.
#
##############################################################################



sub enable_find_text_and_goto_line($$)
{

    my ($text_view, $enable) = @_;

    my $instance;

    # Simply enable/disable the found find text and go to line windows.

    if (defined($instance = find_current_window($text_view,
                                                $find_text_window_type)))
    {
        if ($enable)
        {
            $instance->{main_vbox}->set_sensitive(TRUE);
            $instance->{find_text_button}->set_sensitive
                ((length($instance->{find_comboboxentry}->child()->get_text())
                  > 0) ?
                 TRUE : FALSE);
        }
        else
        {
            $instance->{main_vbox}->set_sensitive(FALSE);
            $instance->{find_text_button}->set_sensitive(FALSE);
        }
    }

    if (defined($instance = find_current_window($text_view,
                                                $goto_line_window_type)))
    {
        if ($enable)
        {
            $instance->{main_hbox}->set_sensitive(TRUE);
            $instance->{goto_line_button}->set_sensitive(TRUE);
        }
        else
        {
            $instance->{main_hbox}->set_sensitive(FALSE);
            $instance->{goto_line_button}->set_sensitive(FALSE);
        }
    }

    # Amend the textview object to reflect its file text enabled/disabled
    # state.

    if ($enable)
    {
        delete($text_view->{find_text_and_goto_line_disabled});
    }
    else
    {
        $text_view->{find_text_and_goto_line_disabled} = 1;
    }

}
#
##############################################################################
#
#   Routine      - hide_find_text_and_goto_line
#
#   Description  - Hides the find text and go to line windows associated with
#                  the specified textview widget.
#
#   Data         - $text_view : The textview widget to which the find text
#                               window is associated.
#
##############################################################################



sub hide_find_text_and_goto_line($)
{

    my $text_view = $_[0];

    my $instance;

    # Simply hide the found find text and go to line windows.

    foreach my $window_type ($find_text_window_type, $goto_line_window_type)
    {
        $instance->{window}->hide()
            if (defined($instance = find_current_window($text_view,
                                                        $window_type)));
    }

}
#
##############################################################################
#
#   Routine      - find_text_and_goto_line_textview_populate_popup_cb
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



sub find_text_and_goto_line_textview_populate_popup_cb($$$)
{

    my ($widget, $menu, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($menu_item,
        $separator);

    # Add a `Find' and `Go To Line' options to the right-click menu that
    # displays those dialogs accordingly.

    $separator = Gtk2::SeparatorMenuItem->new();
    $separator->show();
    $menu->append($separator);

    $menu_item = Gtk2::MenuItem->new(__("_Find"));
    if ($widget->{find_text_and_goto_line_disabled})
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             sub {
                 my ($widget, $details) = @_;
                 return if ($details->{instance}->{in_cb});
                 local $details->{instance}->{in_cb} = 1;
                 find_text($details->{instance}->{window},
                           $details->{textview_widget});
             },
             {instance        => $instance,
              textview_widget => $widget});
    }
    $menu_item->show();
    $menu->append($menu_item);

    $menu_item = Gtk2::MenuItem->new(__("_Go To Line"));
    if ($widget->{find_text_and_goto_line_disabled})
    {
        $menu_item->set_sensitive(FALSE);
    }
    else
    {
        $menu_item->signal_connect
            ("activate",
             sub {
                 my ($widget, $details) = @_;
                 return if ($details->{instance}->{in_cb});
                 local $details->{instance}->{in_cb} = 1;
                 goto_line($details->{instance}->{window},
                           $details->{textview_widget});
             },
             {instance        => $instance,
              textview_widget => $widget});
    }
    $menu_item->show();
    $menu->append($menu_item);

}
#
##############################################################################
#
#   Routine      - find_text_and_goto_line_textview_key_press_event_cb
#
#   Description  - Callback routine called when the user presses a key inside
#                  a textview window.
#
#   Data         - $widget      : The widget object that received the signal.
#                  $event       : A Gtk2::Gdk::Event object describing the
#                                 event that has occurred.
#                  $instance    : The window instance that is associated with
#                                 this widget.
#                  Return Value : TRUE if the event has been handled and needs
#                                 no further handling, otherwise false if the
#                                 event should carry on through the remaining
#                                 event handling.
#
##############################################################################



sub find_text_and_goto_line_textview_key_press_event_cb($$$)
{

    my ($widget, $event, $instance) = @_;

    return FALSE
        if ($instance->{in_cb} || $widget->{find_text_and_goto_line_disabled});
    local $instance->{in_cb} = 1;

    my ($consumed_modifiers,
        $keymap,
        $keyval,
        $state);

    # Ignore the state of the caps-lock key.

    $state = $event->state() - "lock-mask";

    # Work out what the key is having taken into account any modifier keys
    # (except caps-lock).

    $keymap =
        Gtk2::Gdk::Keymap->get_for_display(Gtk2::Gdk::Display->get_default());
    ($keyval, $consumed_modifiers) =
        ($keymap->translate_keyboard_state
         ($event->hardware_keycode(), $state, $event->group()))[0, 3];

    # We are only interested in Ctrl-f and Ctrl-g.

    if (defined($keyval) && ($state - $consumed_modifiers) eq "control-mask")
    {
        if ($keyval == $Gtk2::Gdk::Keysyms{f})
        {
            find_text($instance->{window}, $widget);
            return TRUE;
        }
        elsif ($keyval == $Gtk2::Gdk::Keysyms{g})
        {
            goto_line($instance->{window}, $widget);
            return TRUE;
        }
    }

    return FALSE;

}
#
##############################################################################
#
#   Routine      - find_text_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the find
#                  button in the find text window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub find_text_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($case_sensitive,
        $done,
        $end_iter,
        $expr,
        $forward,
        $found,
        $line,
        $match_len,
        $rect,
        $search_term,
        $start_iter,
        $use_regexp);

    # Get the search parameters.

    $search_term = $instance->{find_comboboxentry}->child()->get_text();
    $case_sensitive = $instance->{case_sensitive_checkbutton}->get_active();
    $forward = ! $instance->{search_backwards_checkbutton}->get_active();
    $use_regexp = $instance->{regular_expression_checkbutton}->get_active();

    # Precompile the regular expression based upon the search term. When the
    # user himself is using regular expressions then check for errors.

    if ($use_regexp)
    {
        eval
        {
            if ($case_sensitive)
            {
                $expr = qr/$search_term/;
            }
            else
            {
                $expr = qr/$search_term/i;
            }
        };
        if ($@)
        {
            my $dialog = Gtk2::MessageDialog->new
                ($instance->{window},
                 ["modal"],
                 "warning",
                 "close",
                 __x("`{pattern}' is an invalid\ncontent search pattern.",
                     pattern => $search_term));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }
    }
    else
    {
        if ($case_sensitive)
        {
            $expr = qr/\Q$search_term\E/;
        }
        else
        {
            $expr = qr/\Q$search_term\E/i;
        }
    }

    # Store the search term in the history.

    handle_comboxentry_history($instance->{find_comboboxentry},
                               "find_text",
                               $search_term);

    # Work out where to start searching from.

    $rect = $instance->{text_view}->get_visible_rect();
    $done = 0;
    if ($search_term eq $instance->{old_search_term}
        && $instance->{old_y} == $rect->y()
        && $instance->{match_offset_start} >= 0)
    {

        # Resume searching from where we left off. Adjust the iters so as to
        # encompass the remaining text on the line, or if there isn't any then
        # move them to the next/previous line.

        if ($forward)
        {
            $start_iter = $instance->{text_buffer}->
                get_iter_at_offset($instance->{match_offset_end});
            $end_iter = $start_iter->copy();
            if ($start_iter->ends_line())
            {
                $done = ! $start_iter->forward_line();
                $end_iter->forward_to_line_end();
            }
            else
            {
                $end_iter->forward_to_line_end();
            }
        }
        else
        {
            $start_iter = $instance->{text_buffer}->
                get_iter_at_offset($instance->{match_offset_start});
            $end_iter = $start_iter->copy();
            if ($start_iter->starts_line())
            {
                $done = ! $start_iter->backward_line();
                $end_iter->backward_line();
                $end_iter->forward_to_line_end()
                    unless ($end_iter->ends_line());
            }
            else
            {
                $start_iter->backward_line();
            }
        }

    }
    else
    {

        # Start searching from the visible part of the file.

        my $y;

        if ($forward)
        {
            $y = $rect->y();
        }
        else
        {
            $y = $rect->y() + $rect->height();
        }
        $start_iter = ($instance->{text_view}->get_line_at_y($y))[0];
        $end_iter = $start_iter->copy();
        $end_iter->forward_to_line_end() unless ($end_iter->ends_line());

    }

    # Search for the text.

    $found = 0;
    while (! $done)
    {
        my $pos;
        $line =
            $instance->{text_buffer}->get_text($start_iter, $end_iter, TRUE);
        if ($forward)
        {
            if ($found = scalar($line =~ m/$expr/g))
            {
                $pos = pos($line);
                $match_len = length($&);
            }
        }
        else
        {
            while ($line =~ m/$expr/g)
            {
                $pos = pos($line);
                $match_len = length($&);
                $found = 1;
            }
        }
        if ($found)
        {
            $instance->{match_offset_start} =
                $start_iter->get_offset() + $pos - $match_len;
            $instance->{match_offset_end} = $start_iter->get_offset() + $pos;
            $done = 1;
        }
        else
        {
            if ($forward)
            {
                $done = ! $start_iter->forward_line();
                $end_iter->forward_to_line_end();
            }
            else
            {
                $done = ! $start_iter->backward_line();
                $end_iter->backward_line();
                $end_iter->forward_to_line_end()
                    unless ($end_iter->ends_line());
            }
        }
    }

    # Either select the found text or tell the user that nothing could be
    # found.

    if ($found)
    {
        my $start_line_iter;
        $start_iter = $instance->{text_buffer}->
            get_iter_at_offset($instance->{match_offset_start});
        $start_line_iter = $start_iter->copy();
        $start_line_iter->backward_line()
            unless $start_line_iter->starts_line();
        $end_iter = $instance->{text_buffer}->
            get_iter_at_offset($instance->{match_offset_end});
        $instance->{text_buffer}->select_range($start_iter, $end_iter);
        $instance->{text_view}->scroll_to_iter
            ($start_line_iter, 0.05, FALSE, 0, 0);
        $instance->{text_view}->scroll_to_iter($end_iter, 0.05, FALSE, 0, 0);
    }
    else
    {
        my $dialog;
        $dialog = Gtk2::MessageDialog->new
            ($instance->{window},
             ["modal"],
             "info",
             "close",
             __x("Could not find\n`{search_term}'.",
                 search_term => $search_term));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

    $rect = $instance->{text_view}->get_visible_rect();
    $instance->{old_y} = $rect->y();
    $instance->{old_search_term} = $search_term;

}
#
##############################################################################
#
#   Routine      - find_comboboxentry_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the find text comboboxentry by selecting an entry from its
#                  pulldown list or entering text directly.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub find_comboboxentry_changed_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{find_text_button}->set_sensitive
        ((length($instance->{find_comboboxentry}->child()->get_text()) > 0) ?
         TRUE : FALSE);

}
#
##############################################################################
#
#   Routine      - goto_line_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the go to
#                  button in the go to line window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub goto_line_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($iter,
        $line_nr);

    # Get the line number. Please note that the update method needs to be
    # called on the spinbutton so as to make sure that it's internal state is
    # completely up to date (the user might have entered a value directly into
    # the entry field). Updates are usually done when it looses the focus,
    # which is fine for the go to line dialog window as it stands but it's
    # probably best not to rely on the presence of any focus stealing buttons.

    $instance->{goto_line_spinbutton}->update();
    $line_nr = $instance->{goto_line_spinbutton}->get_value_as_int();

    # Gtk2::TextBuffer lines start at 0.

    -- $line_nr;

    # Get the iter at the desired line and then scroll to it.

    if (defined($iter = $instance->{text_buffer}->get_iter_at_line($line_nr)))
    {
        $instance->{text_view}->scroll_to_iter($iter, 0, TRUE, 0, 0.5);
    }

}
#
##############################################################################
#
#   Routine      - find_current_window
#
#   Description  - Look for the specified type of window (either a find text
#                  or go to line window) that is mapped and belongs to the
#                  specified textview widget.
#
#   Data         - $text_view   : The textview widget that is to be searched.
#                  $window_type : The type of window that is to be found.
#                  Return Value : Either a reference to the window's instance
#                                 record on success of undef on failure.
#
##############################################################################



sub find_current_window($$)
{

    my ($text_view, $window_type) = @_;

    return WindowManager->instance()->cond_find
        ($window_type,
         sub {
             my ($instance, $type) = @_;
             return 1
                 if ($instance->{window}->mapped()
                     && $instance->{text_view} == $text_view);
             return;
         });

}
#
##############################################################################
#
#   Routine      - get_find_text_window
#
#   Description  - Creates or prepares an existing find text window for use.
#
#   Data         - $parent      : The parent window widget for the find text
#                                 window.
#                  $text_view   : The textview widget that is to be searched.
#                  Return Value : A reference to the newly created or unused
#                                 find text instance record.
#
##############################################################################



sub get_find_text_window($$)
{

    my ($parent, $text_view) = @_;

    my ($glade,
        $instance);
    my $wm = WindowManager->instance();

    # Create a new find text window if an unused one wasn't found, otherwise
    # reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($find_text_window_type)))
    {
        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $find_text_window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($find_text_window_type);
        foreach my $widget ("main_vbox",
                            "find_comboboxentry",
                            "case_sensitive_checkbutton",
                            "search_backwards_checkbutton",
                            "regular_expression_checkbutton",
                            "find_text_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Setup the find text window deletion handlers.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $instance->{window}->hide();
                 return TRUE;
             },
             $instance);
        $glade->get_widget("close_button")->signal_connect
            ("clicked",
             sub {
                 my ($widget, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $instance->{window}->hide();
             },
             $instance);

        # Setup the comboboxentry changed signal handler.

        $instance->{find_comboboxentry}->child()->
            signal_connect("changed",
                           \&find_comboboxentry_changed_cb,
                           $instance);

        # Setup the combobox.

        $instance->{find_comboboxentry}->
            set_model(Gtk2::ListStore->new("Glib::String"));
        $instance->{find_comboboxentry}->set_text_column(0);
    }
    else
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        $instance->{main_vbox}->set_sensitive(TRUE);
    }

    local $instance->{in_cb} = 1;

    # Reset the search context.

    $instance->{match_offset_start} = $instance->{match_offset_end} = -1;
    $instance->{old_y} = 0;
    $instance->{old_search_term} = "";

    # Load in the comboboxentry history.

    handle_comboxentry_history($instance->{find_comboboxentry}, "find_text");

    # Make sure the find button is only enabled when there is something entered
    # into the comboboxentry widget.

    $instance->{find_text_button}->set_sensitive
        ((length($instance->{find_comboboxentry}->child()->get_text()) > 0) ?
         TRUE : FALSE);

    # Store the associated textview and textbuffer.

    $instance->{text_view} = $text_view;
    $instance->{text_buffer} = $instance->{text_view}->get_buffer();

    # Reparent window and display it.

    $instance->{window}->set_transient_for($parent);
    $instance->{window}->show_all();
    $instance->{window}->present();

    # Make sure that the find comboboxentry has the focus.

    $instance->{find_comboboxentry}->child()->grab_focus();
    $instance->{find_comboboxentry}->child()->set_position(-1);

    # If necessary, register the window for management and set up the help
    # callbacks.

    if (defined($glade))
    {
        $wm->manage($instance, $find_text_window_type, $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => undef,
              help_ref => __("mtnb-gsc-browser-buttons")});
    }

    return $instance;

}
#
##############################################################################
#
#   Routine      - get_goto_line_window
#
#   Description  - Creates or prepares an existing go to line window for use.
#
#   Data         - $parent      : The parent window widget for the go to line
#                                 window.
#                  $text_view   : The textview widget in which we are to go to
#                                 a line.
#                  Return Value : A reference to the newly created or unused
#                                 go to line instance record.
#
##############################################################################



sub get_goto_line_window($$)
{

    my ($parent, $text_view) = @_;

    my ($glade,
        $instance);
    my $wm = WindowManager->instance();

    # Create a new find text window if an unused one wasn't found, otherwise
    # reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($goto_line_window_type)))
    {
        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $goto_line_window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($goto_line_window_type);
        foreach my $widget ("main_hbox",
                            "goto_line_spinbutton",
                            "goto_line_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Setup the find text window deletion handlers.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $instance->{window}->hide();
                 return TRUE;
             },
             $instance);
        $glade->get_widget("close_button")->signal_connect
            ("clicked",
             sub {
                 my ($widget, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $instance->{window}->hide();
             },
             $instance);
    }
    else
    {
        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;
        $instance->{main_hbox}->set_sensitive(TRUE);
        $instance->{goto_line_button}->set_sensitive(TRUE);
    }

    local $instance->{in_cb} = 1;

    # Store the associated textview and textbuffer.

    $instance->{text_view} = $text_view;
    $instance->{text_buffer} = $instance->{text_view}->get_buffer();

    # Reparent window and display it.

    $instance->{window}->set_transient_for($parent);
    $instance->{window}->show_all();
    $instance->{window}->present();

    # Make sure that the go to line spinbutton has the focus.

    $instance->{goto_line_spinbutton}->grab_focus();
    $instance->{goto_line_spinbutton}->set_position(-1);

    # If necessary, register the window for management and set up the help
    # callbacks.

    if (defined($glade))
    {
        $wm->manage($instance, $goto_line_window_type, $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => undef,
              help_ref => __("mtnb-gsc-browser-buttons")});
    }

    return $instance;

}

1;
