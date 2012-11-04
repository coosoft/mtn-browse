##############################################################################
#
#   File Name    - HistoryGraph.pm
#
#   Description  - The history graph module for the mtn-browse application.
#                  This module contains all the routines for implementing the
#                  history graph and change history graph windows.
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

# Constants for the border, scale and size of items in a graph.

use constant CANVAS_BORDER    => 5;
use constant DPI              => 72;
use constant FONT_SIZE        => 10;
use constant HEIGHT           => 28;
use constant LINE_WIDTH       => 2;
use constant SELECTION_BORDER => 5;
use constant TEXT_BORDER      => 7;
use constant WIDTH            => 72;

# Constant for the number of characters displayed for a hexadecimal id.

use constant HEX_ID_LENGTH => 8;

# Constants representing the graph node flags that can be set.

use constant CIRCULAR_NODE  => 0x01;
use constant NO_PARENTS     => 0x02;
use constant SELECTED_NODE  => 0x04;
use constant SUSPENDED_NODE => 0x08;

# Constants representing certain colours.

use constant FONT_COLOUR                => "Black";
use constant NOT_SELECTED_BORDER_COLOUR => "Gray";
use constant SELECTED_BORDER_COLOUR     => "Black";
use constant SELECTION_COLOUR           => "Tomato";
use constant SUSPENDED_BORDER_COLOUR    => "DeepSkyBlue";

# Constants for the columns within the branches liststore widget.

use constant BLS_COLUMN_TYPES    => ("Glib::Boolean",
                                     "Glib::String");
use constant BLS_SELECTED_COLUMN => 0;
use constant BLS_BRANCH_COLUMN   => 1;

# The type of window that is going to be managed by this module.

my $window_type = "history_graph_window";

# Default values for certain graphing parameters that are not specified by the
# caller but are changeable via the change history graph window.

my $colour_by_author = 0;
my $draw_left_to_right = 0;
my $show_all_propagate_nodes = 0;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_history_graph($;$$$$);

# Private routines.

sub branch_filter_button_clicked_cb($$);
sub browse_revision_button_clicked_cb($$);
sub canvas_item_event_cb($$$);
sub change_history_graph_button_clicked_cb($$);
sub change_history_graph_parameters($$);
sub compile_tag_weighting_patterns($);
sub default_zoom_button_clicked_cb($$);
sub dot_input_handler_cb($$);
sub draw_graph($);
sub generate_ancestry_graph($);
sub generate_history_graph($);
sub get_change_history_graph_window($);
sub get_history_graph_window();
sub get_node_colour($$);
sub get_node_tag($$);
sub go_to_selected_revision_button_clicked_cb($$);
sub graph_advanced_find_button_clicked_cb($$);
sub graph_reconnect_helper($$);
sub graph_refresh_button_clicked_cb($$);
sub graph_revision_change_history_button_clicked_cb($$);
sub graph_revision_change_log_button_clicked_cb($$);
sub hsv_to_rgb($$$$$$);
sub layout_graph($);
sub load_branch_liststore($;$);
sub populate_revision_details($$);
sub reset_history_graph_instance($);
sub reset_history_graph_window($);
sub scale_canvas($);
sub scroll_to_node($$);
sub select_node($$);
sub show_key_textview($$);
sub show_key_togglebutton_toggled_cb($$);
sub tag_weightings_button_clicked_cb($$);
sub tick_untick_branches_button_clicked_cb($$);
sub update_key_textview($);
sub zoom_in_button_clicked_cb($$);
sub zoom_out_button_clicked_cb($$);
#
##############################################################################
#
#   Routine      - display_history_graph
#
#   Description  - Display a history graph for the specified branches within
#                  the specified date range.
#
#   Data         - $mtn         : The Monotone::AutomateStdio object that is
#                                 to be used to generate the history graph.
#                  $branches    : A reference to a list of branches to
#                                 generate a graph from. This parameter can be
#                                 undef or an empty list if all branches are
#                                 to be selected.
#                  $from_date   : The earliest date from which revisions will
#                                 be selected for the history graph. This
#                                 parameter can be undef or an empty string if
#                                 no such age restriction is required.
#                  $to_date     : The latest date from which revisions will be
#                                 selected for the history graph. This
#                                 parameter can be undef or an empty string if
#                                 no such age restriction is required.
#                  $revision_id : The id of the revision that is to be
#                                 selected and shown when the history graph is
#                                 drawn. This parameter can be undef if no
#                                 specific revision is to be selected.
#
##############################################################################



sub display_history_graph($;$$$$)
{

    my ($mtn,
        $branches,
        $from_date,
        $to_date,
        $revision_id)
        = @_;

    my $instance;

    $instance = get_history_graph_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{graph_data}->{parameters}->{branches} =
        defined($branches) ? $branches : [];
    $instance->{graph_data}->{parameters}->{from_date} =
        defined($from_date) ? $from_date : "";
    $instance->{graph_data}->{parameters}->{to_date} =
        defined($to_date) ? $to_date : "";
    $instance->{graph_data}->{parameters}->{draw_left_to_right} =
        $draw_left_to_right;
    $instance->{graph_data}->{parameters}->{show_all_propagate_nodes} =
        $show_all_propagate_nodes;
    $instance->{graph_data}->{parameters}->{colour_by_author} =
        $colour_by_author;
    $instance->{graph_data}->{parameters}->{revision_id} = $revision_id;
    $instance->{graph_data}->{parameters}->{new} = 1;
    $instance->{window}->show_all();
    $instance->{window}->present();

    # If the caller hasn't provided any selectors then present the user with
    # the change graph parameters window as well as an empty graph window,
    # otherwise simply display the revision graph.

    if (scalar(@{$instance->{graph_data}->{parameters}->{branches}}) == 0
        && $instance->{graph_data}->{parameters}->{from_date} eq ""
        && $instance->{graph_data}->{parameters}->{to_date} eq "")
    {
        WindowManager->instance()->update_gui();
        if (change_history_graph_parameters
            ($instance, $instance->{graph_data}->{parameters}))
        {

            # Save the graph rendering settings so that we can recall them
            # later.

            $draw_left_to_right =
                $instance->{graph_data}->{parameters}->{draw_left_to_right};
            $show_all_propagate_nodes = $instance->{graph_data}->{parameters}->
                {show_all_propagate_nodes};
            $colour_by_author =
                $instance->{graph_data}->{parameters}->{colour_by_author};

            generate_history_graph($instance);

        }
    }
    else
    {
        generate_history_graph($instance);
    }

}
#
##############################################################################
#
#   Routine      - change_history_graph_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the change
#                  history graph button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub change_history_graph_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $old_colour_by_author =
        $instance->{graph_data}->{parameters}->{colour_by_author};

    if (change_history_graph_parameters($instance,
                                        $instance->{graph_data}->{parameters}))
    {

        # Save the graph rendering settings so that we can recall them later.

        $draw_left_to_right =
            $instance->{graph_data}->{parameters}->{draw_left_to_right};
        $show_all_propagate_nodes =
            $instance->{graph_data}->{parameters}->{show_all_propagate_nodes};
        $colour_by_author =
            $instance->{graph_data}->{parameters}->{colour_by_author};

        # Reset the colour database cache as we are changing what we colour
        # nodes by. This is primarily done so as to stop the key textview from
        # getting cluttered with old colour codes.

        $instance->{colour_db} = {}
            if ($instance->{graph_data}->{parameters}->{colour_by_author}
                != $old_colour_by_author);

        # Now graph it.

        $instance->{graph_data}->{parameters}->{revision_id} =
            $instance->{selected_revision_id};
        generate_history_graph($instance);

    }

}
#
##############################################################################
#
#   Routine      - tag_weightings_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the change
#                  history graph button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub tag_weightings_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    if (manage_tag_weightings($instance->{window},
                              $user_preferences->{tag_weightings}))
    {

        save_preferences($user_preferences, $instance->{window});

        # Regenerate all of the compiled tag weightings tables.

        WindowManager->instance()->cond_find
            ($window_type,
             sub {
                 my ($instance, $type) = @_;
                 compile_tag_weighting_patterns
                     ($instance->{compiled_tag_weightings});
                 return;
             });

    }

}
#
##############################################################################
#
#   Routine      - graph_refresh_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the refresh
#                  history graph button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub graph_refresh_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    generate_history_graph($instance);

}
#
##############################################################################
#
#   Routine      - zoom_in_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the zoom in
#                  button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub zoom_in_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = $instance->{scale} * sqrt(2);
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - zoom_out_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the zoom
#                  out button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub zoom_out_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = $instance->{scale} / sqrt(2);
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - default_zoom_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the default
#                  zoom button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub default_zoom_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = 1;
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - show_key_togglebutton_toggled_cb
#
#   Description  - Callback routine called when the user toggles the show
#                  colour key button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub show_key_togglebutton_toggled_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    show_key_textview($instance, $widget->get_active());

}
#
##############################################################################
#
#   Routine      - go_to_selected_revision_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  go to selected revision button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub go_to_selected_revision_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    scroll_to_node($instance, $instance->{selected_revision_id});

}
#
##############################################################################
#
#   Routine      - graph_advanced_find_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  advanced find button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub graph_advanced_find_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my (@dummy,
        $revision_id);

    # Let the user choose the revision (we aren't interested in the branch
    # name(s)). Only allow the user to select graphed revisions.

    if (advanced_find
        ($instance,
         \$revision_id,
         \@dummy,
         sub {
             my ($parent, $revision_id, $instance) = @_;
             if (! exists($instance->{graph_data}->{child_graph}->
                          {$revision_id}))
             {
                 my $dialog = Gtk2::MessageDialog->new
                     ($parent,
                      ["modal"],
                      "info",
                      "close",
                      __x("Revision `{revision_id}'\n"
                              . "cannot be found within the current history "
                              . "graph.\nPlease select another revision.",
                          revision_id => $revision_id));
                 busy_dialog_run($dialog);
                 $dialog->destroy();
                 return;
             }
             return 1;
         },
         $instance))
    {

        # The user has selected a graphed revision so select it and make sure
        # it is visible.

        scroll_to_node($instance, $revision_id);
        select_node($instance, $revision_id);

    }

}
#
##############################################################################
#
#   Routine      - graph_revision_change_history_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  revision change history button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub graph_revision_change_history_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    display_revision_change_history
        ($instance->{mtn},
         get_node_tag($instance, $instance->{selected_revision_id}),
         $instance->{selected_revision_id});

}
#
##############################################################################
#
#   Routine      - graph_revision_change_log_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  revision change log button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub graph_revision_change_log_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    display_change_log
        ($instance->{mtn},
         $instance->{selected_revision_id},
         "",
         get_node_tag($instance, $instance->{selected_revision_id}));

}
#
##############################################################################
#
#   Routine      - browse_revision_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  browse revision button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub browse_revision_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $branches = $instance->{graph_data}->{child_graph}->
        {$instance->{selected_revision_id}}->{branches};

    get_browser_window($instance->{mtn},
                       (scalar(@$branches) > 0) ? $$branches[0] : "",
                       $instance->{selected_revision_id});

}
#
##############################################################################
#
#   Routine      - canvas_item_event_cb
#
#   Description  - Callback routine called when an event is delivered to a
#                  canvas item widget or its parent scrolled window in the
#                  history graph window.
#
#   Data         - $widget      : The widget object that received the signal.
#                  $event       : A Gtk2::Gdk::Event object describing the
#                                 event that has occurred.
#                  $details     : A reference to an anonymous hash containing
#                                 the window instance and either the revision
#                                 id that is associated with the canvas widget
#                                 or undef if the event occurred in the parent
#                                 scrolled window.
#                  Return Value : TRUE if the event has been handled and needs
#                                 no further handling, otherwise FALSE if the
#                                 event should carry on through the remaining
#                                 event handling.
#
##############################################################################



sub canvas_item_event_cb($$$)
{

    my ($widget, $event, $details) = @_;

    my $instance = $details->{instance};
    my $revision_id = $details->{revision_id};

    return FALSE if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $type = $event->type();

    $revision_id = defined($revision_id)
        ? $revision_id : $instance->{selected_revision_id};

    if ($type eq "button-press")
    {

        my $button = $event->button();

        if ($button == 1)
        {
            if (defined($revision_id))
            {
                select_node($instance, $revision_id);
                return TRUE;
            }
        }
        elsif ($button == 3)
        {

            my ($menu,
                $menu_item);

            # Create a popup menu with the options in it.

            $menu = Gtk2::Menu->new();

            $menu_item =
                Gtk2::MenuItem->new(__("_Copy Revision Id To Clipboard"));
            $menu->append($menu_item);
            if (defined($revision_id))
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         my $clipboard = Gtk2::Clipboard->
                             get(Gtk2::Gdk->SELECTION_PRIMARY());
                         $clipboard->set_text($revision_id);
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            $menu_item =
                Gtk2::MenuItem->new(__("_Go To Selected Revision"));
            $menu->append($menu_item);
            if (defined($instance->{selected_revision_id}))
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         scroll_to_node($instance,
                                        $instance->{selected_revision_id});
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            $menu_item = Gtk2::SeparatorMenuItem->new();
            $menu_item->show();
            $menu->append($menu_item);

            $menu_item = Gtk2::MenuItem->new(__("Display Change _Log"));
            $menu->append($menu_item);
            if (defined($revision_id))
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         display_change_log($instance->{mtn},
                                            $revision_id,
                                            "",
                                            get_node_tag($instance,
                                                         $revision_id));
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            $menu_item = Gtk2::MenuItem->new(__("Display _Revision History"));
            $menu->append($menu_item);
            if (defined($revision_id))
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         display_revision_change_history($instance->{mtn},
                                                         get_node_tag
                                                             ($instance,
                                                              $revision_id),
                                                         $revision_id);
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            $menu_item = Gtk2::SeparatorMenuItem->new();
            $menu_item->show();
            $menu->append($menu_item);

            $menu_item = Gtk2::MenuItem->new(__("_Browse Revision"));
            $menu->append($menu_item);
            if (defined($revision_id))
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         my $branches = $instance->{graph_data}->
                             {child_graph}->{$revision_id}->{branches};
                         get_browser_window
                             ($instance->{mtn},
                              (scalar(@$branches) > 0) ? $$branches[0] : "",
                              $revision_id);
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            $menu_item = Gtk2::SeparatorMenuItem->new();
            $menu_item->show();
            $menu->append($menu_item);

            $menu_item = Gtk2::MenuItem->new
                (__("Compare Revision _With Selected"));
            $menu->append($menu_item);
            if (defined($instance->{selected_revision_id})
                && defined($revision_id)
                && $instance->{selected_revision_id} ne $revision_id)
            {
                $menu_item->signal_connect
                    ("activate",
                     sub {
                         my ($widget, $instance) = @_;
                         my @revision_ids;
                         $instance->{mtn}->toposort
                             (\@revision_ids,
                              $instance->{selected_revision_id},
                              $revision_id);
                         display_revision_comparison($instance->{mtn},
                                                     $revision_ids[0],
                                                     $revision_ids[1],
                                                     undef);
                     },
                     $instance);
            }
            else
            {
                $menu_item->set_sensitive(FALSE);
            }
            $menu_item->show();

            # Display the popup menu.

            $menu->popup(undef, undef, undef, undef, $button, $event->time());

            return TRUE;

        }

    }
    elsif (defined($revision_id)
           && $type eq "2button-press" && $event->button() == 1)
    {

        my $node = $instance->{graph_data}->{child_graph}->{$revision_id};

        # Display a new graph for the node under the mouse if it is in the
        # history graph but not selected (i.e. its on an unselected branch).

        if (! ($node->{flags} & SELECTED_NODE))
        {
            if (scalar(@{$node->{branches}}) > 0)
            {
                display_history_graph
                    ($instance->{mtn},
                     [$node->{branches}->[0]],
                     $instance->{graph_data}->{parameters}->{from_date},
                     $instance->{graph_data}->{parameters}->{to_date},
                     $revision_id);
            }
            else
            {
                my $dialog = Gtk2::MessageDialog->new
                    ($instance->{window},
                     ["modal"],
                     "info",
                     "close",
                     __x("Revision `{revision_id}'\n"
                         . "is not on a branch and so a separate history\n"
                         . "graph showing that branch cannot be generated.",
                         revision_id => $revision_id));
                busy_dialog_run($dialog);
                $dialog->destroy();
            }
        }

        return TRUE;

    }

    return FALSE;

}
#
##############################################################################
#
#   Routine      - generate_history_graph
#
#   Description  - Display a history graph according to the current parameters
#                  in the history graph window.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub generate_history_graph($)
{

    my $instance = $_[0];

    my ($counter,
        @revision_ids);
    my $wm = WindowManager->instance();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $wm->update_gui();

    reset_history_graph_window($instance);
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();

    # Get the list of file change revisions. Remember to include the current
    # revision in the history.

    generate_ancestry_graph($instance);

    # Populate all of the graphed nodes with essential revision information.

    if (! $instance->{stop})
    {
        my (%branch_set,
            @branches);
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->set_status(__("Getting revision information"));
        $wm->update_gui();
        $instance->{mtn}->branches(\@branches);
        foreach my $branch (@branches)
        {
            $branch_set{$branch} = undef;
        }
        @branches = ();
        $counter = 1;
        @revision_ids = keys(%{$instance->{graph_data}->{child_graph}});
        foreach my $revision_id (@revision_ids)
        {
            my $node = $instance->{graph_data}->{child_graph}->{$revision_id};
            populate_revision_details($instance, $revision_id);
            foreach my $branch (@{$node->{branches}})
            {
                $node->{flags} |= SUSPENDED_NODE
                    unless (exists($branch_set{$branch}));
            }
            if (($counter % 100) == 0)
            {
                $instance->{appbar}->set_progress_percentage
                    ($counter / scalar(@revision_ids));
                $wm->update_gui();
            }
            ++ $counter;

            # Stop if the user wants to.

            last if ($instance->{stop});
        }
        $instance->{appbar}->set_progress_percentage(1);
        $wm->update_gui();
    }

    # Get dot to lay out the history graph.

    if (! $instance->{stop})
    {
        local $pulse_widget = $instance->{appbar}->get_progress();
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->
            set_status(__x("Laying out graph with {program}",
                           program => GRAPHVIZ_LAYOUT_PROGRAM));
        $wm->update_gui();
        layout_graph($instance);
    }

    # Now draw it in our canvas.

    draw_graph($instance) unless ($instance->{stop});

    # Update the key textview.

    update_key_textview($instance) unless ($instance->{stop});

    # Clean up the graph and any associated data if the user stopped the
    # drawing process but without loosing any settings (the graph will be a
    # mess anyway), otherwise scroll to a suitable revision.

    if ($instance->{stop})
    {
        my $parameters = $instance->{graph_data}->{parameters};
        $instance->{graph_data}->{parameters} = undef;
        reset_history_graph_instance($instance);
        $instance->{graph_data}->{parameters} = $parameters;
    }
    else
    {
        $instance->{graph_advanced_find_button}->set_sensitive(TRUE);
        if (defined($instance->{graph_data}->{parameters}->{revision_id})
            && exists($instance->{graph_data}->{child_graph}->
                      {$instance->{graph_data}->{parameters}->{revision_id}}))
        {
            scroll_to_node($instance,
                           $instance->{graph_data}->{parameters}->
                               {revision_id});
            select_node($instance,
                        $instance->{graph_data}->{parameters}->{revision_id});
        }
        elsif (scalar(@{$instance->{graph_data}->{head_revisions}}) > 0)
        {
            scroll_to_node($instance,
                           $instance->{graph_data}->{head_revisions}->[0]);
        }
    }

    $instance->{stop_button}->set_sensitive(FALSE);
    $instance->{stop} = 0;
    $instance->{appbar}->set_progress_percentage(0);
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - generate_ancestry_graph
#
#   Description  - Generate the ancestry graph database from the specified
#                  selection criteria.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub generate_ancestry_graph($)
{

    my $instance = $_[0];

    my (%branches_set,
        $branch_selector,
        $counter,
        %date_set,
        $date_range_selector,
        $date_selector,
        @graph,
        %graph_db,
        @head_revisions,
        %nr_of_parents,
        $selected_set,
        $update_interval);
    my $parameters = $instance->{graph_data}->{parameters};
    my $wm = WindowManager->instance();

    $instance->{appbar}->set_status(__("Building ancestry graph"));
    $wm->update_gui();

    $instance->{graph_data}->{child_graph} = {};
    $instance->{graph_data}->{head_revisions} = [];

    # First build up revision hit lists based upon the selection criteria. This
    # will be then used when scanning the graph to weed out unwanted revisions.

    if ($parameters->{from_date} ne "")
    {
        $date_range_selector = "l:" . $parameters->{from_date};
        $date_selector = 1;
    }
    else
    {
        $date_range_selector = "";
    }
    if ($parameters->{to_date} ne "")
    {
        if ($date_range_selector eq "")
        {
            $date_range_selector = "e:" . $parameters->{to_date};
        }
        else
        {
            $date_range_selector .= "/e:" . $parameters->{to_date};
        }
        $date_selector = 1;
    }

    # Remember that this is a user generated query and so may contain invalid
    # data. So protect ourselves.

    CachingAutomateStdio->register_error_handler
        (MTN_SEVERITY_ALL,
         sub {
             my ($severity, $message) = @_;
             my $dialog;
             $dialog = Gtk2::MessageDialog->new_with_markup
                 ($instance->{window},
                  ["modal"],
                  "warning",
                  "close",
                  __x("There is a problem with an internal query, Monotone "
                          . "gave:\n<b><i>{error_message}</i></b>",
                      error_message =>
                          Glib::Markup::escape_text($message)));
             busy_dialog_run($dialog);
             $dialog->destroy();
             die("Bad query");
         });
    eval
    {

        my $nr_selections;

        # Do the branches hit list (using the or operator and one selection
        # operation if we can). Please note that this rather convoluted code is
        # due to a small bug in Monotone 1.0 where
        # `(b:branch1|b:branch2)/l:2008-01-01' and `(b:branch1|b:branch2)' are
        # invalid but `b:branch1|b:branch2' and
        # `l:2008-01-01/(b:branch1|b:branch2)' are not.

        $instance->{appbar}->set_progress_percentage(0);
        $wm->update_gui();
        $nr_selections =
            (($instance->{mtn}->supports(MTN_SELECTOR_OR_OPERATOR))
                 ? 1 : scalar(@{$parameters->{branches}}))
            + (($date_range_selector ne "") ? 1 : 0);
        $update_interval = calculate_update_interval($nr_selections, 40);
        if (scalar(@{$parameters->{branches}}) > 0)
        {

            my (@escaped_branches,
                @revision_ids);
            my $date_range = ($date_range_selector ne "")
                ? ($date_range_selector . "/") : "";

            foreach my $branch (@{$parameters->{branches}})
            {
                my $escaped_value = $branch;
                $escaped_value =~ s/$select_escape_re/\\$1/g;
                push(@escaped_branches, $escaped_value);
            }

            $branch_selector = 1;
            if ($instance->{mtn}->supports(MTN_SELECTOR_OR_OPERATOR))
            {
                my $selector;
                $selector = "b:" . join("|b:", @escaped_branches);
                if ($date_range ne "")
                {
                    $selector = $date_range . "(" . $selector . ")";
                }
                $instance->{mtn}->select(\@revision_ids, $selector);
                foreach my $revision_id (@revision_ids)
                {
                    $branches_set{$revision_id} = undef;
                }
                $instance->{appbar}->set_progress_percentage
                    (1 / $nr_selections);
            }
            else
            {
                $counter = 1;
                foreach my $branch (@escaped_branches)
                {
                    $instance->{mtn}->select(\@revision_ids,
                                             $date_range . "b:" . $branch);
                    foreach my $revision_id (@revision_ids)
                    {
                        $branches_set{$revision_id} = undef;
                    }

                    if (($counter % $update_interval) == 0)
                    {
                        $instance->{appbar}->set_progress_percentage
                            ($counter / $nr_selections);
                        $wm->update_gui();
                    }
                    ++ $counter;

                    # Stop if the user wants to.

                    last if ($instance->{stop});
                }
            }

        }
        $wm->update_gui();

        # Now do the date hit list.

        if (! $instance->{stop} && $date_range_selector ne "")
        {
            my @revision_ids;
            $instance->{mtn}->select(\@revision_ids, $date_range_selector);
            foreach my $revision_id (@revision_ids)
            {
                $date_set{$revision_id} = undef;
            }
        }
        $instance->{appbar}->set_progress_percentage(1);
        $wm->update_gui();

        %branches_set = %date_set = () if ($instance->{stop});

    };
    $instance->{stop} = 1 if ($@);
    CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
                                                 \&mtn_error_handler);

    # Set the selected_set variable to point to which ever selector set should
    # be used to determine whether a revision exactly matches the caller's
    # search criteria. If there are no selectors then this variable will be
    # left undefined. If we have both a branch and date range selector then
    # although the branch selector is date limited, we still need the date
    # range selector for propagation revisions (which should be included if
    # they are in the date range but not necessarily in the selected set of
    # branches).

    if ($branch_selector)
    {
        $selected_set = \%branches_set;
    }
    elsif ($date_selector)
    {
        $selected_set = \%date_set;
    }

    # Get the revision graph from Monotone.

    $instance->{mtn}->graph(\@graph) unless ($instance->{stop});

    $wm->update_gui();

    # Build up a revision child graph indexed on revision id with only the
    # revisions that we are interested in within.

    $instance->{appbar}->set_progress_percentage(0);
    $wm->update_gui();
    $update_interval = calculate_update_interval(\@graph);
    $counter = 1;
    foreach my $entry (@graph)
    {

        my ($current_selected,
            $selected);

        # A revision is selected if:
        #     1) There are no selectors, i.e. everything is selected.
        #     2) Not all propagation nodes are to be shown and the revision is
        #        within the specified date range and on one of the selected
        #        branches.
        #     3) All propagation nodes are to be shown and the revision is
        #        within the specified date range and it or one of its parents
        #        are on one of the selected branches.
        #     4) There is no branch selector and the revision is within the
        #        specified date range.

        if (! defined($selected_set)
            || exists($selected_set->{$entry->{revision_id}}))
        {
            $current_selected = $selected = 1;
        }
        elsif ($parameters->{show_all_propagate_nodes} && $branch_selector
               && (! $date_selector
                   || exists($date_set{$entry->{revision_id}})))
        {
            foreach my $parent_id (@{$entry->{parent_ids}})
            {
                if (exists($selected_set->{$parent_id}))
                {
                    $selected = 1;
                    last;
                }
            }
        }

        # If the revision has been selected then process it.

        if ($selected)
        {

            # File the revision. Remember it may already exist as it might be a
            # parent of a revision that has already been processed.

            if (! exists($graph_db{$entry->{revision_id}}))
            {
                $graph_db{$entry->{revision_id}} = {children => [],
                                                    flags    => 0};
            }

            # If the current revision is selected itself then all parents
            # should be included if they are in the date range (need to show
            # propagation revisions), otherwise only those parents that are
            # within the date range and on the right branch(es) should be
            # included.

            foreach my $parent_id (@{$entry->{parent_ids}})
            {
                if (($current_selected
                     && (! $date_selector || exists($date_set{$parent_id})))
                    || (! $current_selected
                        && (! defined($selected_set)
                            || exists($selected_set->{$parent_id}))))
                {
                    if (exists($graph_db{$parent_id}))
                    {
                        push(@{$graph_db{$parent_id}->{children}},
                             $entry->{revision_id});
                    }
                    else
                    {
                        $graph_db{$parent_id} =
                            {children => [$entry->{revision_id}],
                             flags    => 0};
                    }
                }
            }

            # If this node was actually selected then mark it as such.

            $graph_db{$entry->{revision_id}}->{flags} |= SELECTED_NODE
                if ($current_selected);

        }

        if (($counter % $update_interval) == 0)
        {
            $instance->{appbar}->set_progress_percentage
                ($counter / scalar(@graph));
            $wm->update_gui();
        }
        ++ $counter;

        # Stop if the user wants to.

        last if ($instance->{stop});

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Now find out how many head revisions we have.

    if (! $instance->{stop})
    {
        foreach my $revision_id (keys(%graph_db))
        {
            if (scalar(@{$graph_db{$revision_id}->{children}}) == 0)
            {
                push(@head_revisions, $revision_id);
            }
        }
    }

    # If we have more than one head revision and we have restricted the graph
    # to a number of branches then we better make sure that as many head and
    # tail revisions are joined up (may happen due to intermediate revisions
    # being excluded because of branch selectors). For this we need a parent
    # database but limited by date if possible.

    if (! $instance->{stop} && $branch_selector && scalar(@head_revisions) > 1)
    {
        my ($context,
            %parent_db);
        if ($date_selector)
        {
            foreach my $entry (@graph)
            {
                if (exists($date_set{$entry->{revision_id}}))
                {
                    my @parent_ids;
                    foreach my $parent_id (@{$entry->{parent_ids}})
                    {
                        push(@parent_ids, $parent_id)
                            if (exists($date_set{$parent_id}));
                    }
                    $parent_db{$entry->{revision_id}} = \@parent_ids;
                }
            }
        }
        else
        {
            foreach my $entry (@graph)
            {
                $parent_db{$entry->{revision_id}} = $entry->{parent_ids};
            }
        }
        $context = {selected_set      => $selected_set,
                    graph_db          => \%graph_db,
                    outside_selection => undef,
                    parent_db         => \%parent_db,
                    parents           => [],
                    processed_set     => {},
                    aggressive_search => 0};
        $instance->{appbar}->set_progress_percentage(0);
        $wm->update_gui();
        $update_interval = calculate_update_interval(\@head_revisions);
        $counter = 1;
        foreach my $head_id (@head_revisions)
        {

            # Try and join up gaps starting from this head revision.

            $context->{outside_selection} = undef;
            $context->{parents} = [];
            graph_reconnect_helper($context, $head_id);

            if (($counter % $update_interval) == 0)
            {
                $instance->{appbar}->set_progress_percentage
                    ($counter / scalar(@head_revisions));
                $wm->update_gui();
            }
            ++ $counter;

            # Stop if the user wants to.

            last if ($instance->{stop});

        }
        $instance->{appbar}->set_progress_percentage(1);
        $wm->update_gui();
    }

    if (! $instance->{stop})
    {

        # Create a hash indexed by revision id that gives the number of parents
        # for that revision.

        foreach my $revision_id (keys(%graph_db))
        {
            foreach my $child_id (@{$graph_db{$revision_id}->{children}})
            {
                if (exists($nr_of_parents{$child_id}))
                {
                    ++ $nr_of_parents{$child_id};
                }
                else
                {
                    $nr_of_parents{$child_id} = 1;
                }
            }
        }

        # Now use this hash to mark up nodes that either have no parents or
        # multiple ones (i.e. what will be rendered as circular merge nodes).

        foreach my $revision_id (keys(%graph_db))
        {
            my $nr_parents = 0;
            if (exists($nr_of_parents{$revision_id}))
            {
                $nr_parents = $nr_of_parents{$revision_id};
            }
            if ($nr_parents == 0)
            {
                $graph_db{$revision_id}->{flags} |= NO_PARENTS;
            }
            elsif ($nr_parents > 1)
            {
                $graph_db{$revision_id}->{flags} |= CIRCULAR_NODE;
            }
        }
        %nr_of_parents = ();

        # Update the list of head revisions.

        @head_revisions = ();
        foreach my $revision_id (keys(%graph_db))
        {
            if ((! defined($selected_set)
                 || exists($selected_set->{$revision_id}))
                && scalar(@{$graph_db{$revision_id}->{children}}) == 0)
            {
                push(@head_revisions, $revision_id);
            }
        }

        # Store the graph database and the list of head revisions.

        $instance->{graph_data}->{child_graph} = \%graph_db;
        $instance->{graph_data}->{head_revisions} = \@head_revisions;

    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - graph_reconnect_helper
#
#   Description  - Recursive routine for searching a revision graph connecting
#                  ancestors to children where there is a break due to
#                  intermediate revisions not being selected (due to a branch
#                  selector).
#
#   Data         - $context     : The scanning context for this routine.
#                  $revision_id : The revision id from where the search is to
#                                 commence.
#
##############################################################################



sub graph_reconnect_helper($$)
{

    my ($context, $revision_id) = @_;

    # Please note that by definition the selected_set field must contain a
    # valid set as this recursive routine would never have been called
    # otherwise.

    # Were we outside our selected set of revisions?

    if ($context->{outside_selection})
    {

        # Yes we were so if we have just stumbled across a node that is to be
        # graphed then make a note of the current revision in our parents list
        # and then stop searching any further down this path. Otherwise carry
        # on looking.

        if (exists($context->{graph_db}->{$revision_id}))
        {
            push(@{$context->{parents}}, $revision_id);
            return;
        }
        else
        {
            foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
            {
                graph_reconnect_helper($context, $parent_id)
                    unless (exists($context->{processed_set}->{$parent_id}));
            }
        }

    }
    else
    {

        # No we weren't off selection.

        # If the current node is selected then decide whether we need to go
        # into aggressive search mode and then process each parent.

        if (exists($context->{selected_set}->{$revision_id}))
        {

            my $aggressive_search;

            # Determine whether we need to go into aggressive search mode. This
            # is where we have no selected parents so we have tp do our best to
            # find a graphed ancestor.

            $aggressive_search = 1;
            foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
            {
                if (exists($context->{selected_set}->{$parent_id}))
                {
                    $aggressive_search = 0;
                    last;
                }
            }
            local $context->{aggressive_search} = $aggressive_search
                unless ($context->{aggressive_search} == $aggressive_search);

            # Now process any unprocessed graphed parents.

            foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
            {
                graph_reconnect_helper($context, $parent_id)
                    if (! exists($context->{processed_set}->{$parent_id})
                        && exists($context->{graph_db}->{$parent_id}));
            }

        }

        # Otherwise if the current node is a propagate node (i.e. in the graph
        # database but not selected, e.g. an off-branch parent of an on-branch
        # node) then see what we can join together. Remember that the parents
        # of propagate nodes weren't even looked at until now.

        elsif (exists($context->{graph_db}->{$revision_id}))
        {

            my (@graphed_parents,
                @selected_parents);

            # First scan the immediate parents for any that are graphed. If we
            # find selected parents then join those up, if not then try joining
            # up any graphed parents and if that fails and we are in aggressive
            # search mode (i.e. desparate to find any graphed ancestor) then
            # start up each parent path searching for any ancestor before
            # finally giving up.

            foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
            {
                if (exists($context->{selected_set}->{$parent_id}))
                {
                    push(@selected_parents, $parent_id);
                }
                elsif (exists($context->{graph_db}->{$parent_id}))
                {
                    push(@graphed_parents, $parent_id);
                }
            }

            # Selected parents take precedence.

            if (scalar(@selected_parents) > 0)
            {
                push(@{$context->{parents}}, @selected_parents);
            }

            # Then graphed ones.

            elsif (scalar(@graphed_parents) > 0)
            {
                push(@{$context->{parents}}, @graphed_parents);
            }

            # Nothing found so far, do we need to do an aggressive search?

            elsif ($context->{aggressive_search})
            {

                # Ok getting desperate, start a recursive search.

                # Stash the current hit list away and use a blank one as we are
                # going outside our selected set of revisions (we don't want to
                # prevent subsequent walks outside our selected set on
                # different nodes from finding a possible route to a graphed
                # parent).

                local $context->{processed_set} = {};

                # Search up each parent node, we are going off selecion.

                local $context->{outside_selection} = 1;
                foreach my $parent_id
                    (@{$context->{parent_db}->{$revision_id}})
                {
                    graph_reconnect_helper($context, $parent_id)
                        unless (exists($context->{processed_set}->
                                       {$parent_id}));
                }

            }

            # For any parents that we have found file this revision as a child
            # of those parents in the graph database. By definition all found
            # parent nodes already exist in the graph database so no need to
            # worry about creating new nodes. Also avoid adding duplicate child
            # revision entries.

            foreach my $parent_id (@{$context->{parents}})
            {
                my $found;
                foreach my $child_id
                    (@{$context->{graph_db}->{$parent_id}->{children}})
                {
                    if ($child_id eq $revision_id)
                    {
                        $found = 1;
                        last;
                    }
                }
                push(@{$context->{graph_db}->{$parent_id}->{children}},
                     $revision_id)
                    unless ($found);
            }
            $context->{parents} = [];

        }

    }

    # Mark this node as having been processed.

    $context->{processed_set}->{$revision_id} = undef;

}
#
##############################################################################
#
#   Routine      - layout_graph
#
#   Description  - Given a child graph database, get dot to lay out the
#                  geometric shapes that we want to render and then read back
#                  the result.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub layout_graph($)
{

    my $instance = $_[0];

    my (@arrows,
        $buffer,
        @circles,
        $prev_lines,
        @rectangles);
    my $nre = '[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?';

    # The above $nre variable is used to match numbers (integers, integers with
    # exponents and floats). It keeps the subsequent REs readable and sane.

    $instance->{graph_data}->{arrows} = [];
    $instance->{graph_data}->{circles} = [];
    $instance->{graph_data}->{rectangles} = [];
    $instance->{graph_data}->{max_x} = 0;
    $instance->{graph_data}->{max_y} = 0;

    # Run the dot subprocess.

    return unless (run_command(\$buffer,
                               1,
                               \&dot_input_handler_cb,
                               $instance,
                               \$instance->{stop},
                               GRAPHVIZ_LAYOUT_PROGRAM, "-q", "-s" . DPI,
                                   "-Txdot"));

    # Parse the dot output, line by line.

    $prev_lines = "";
    foreach my $line (split(/\n/, $buffer))
    {

        my $revision_id;

        # Deal with statements that span multiple lines.

        if ($line =~ m/^(.*)\\$/)
        {
            $prev_lines .= $1;
            next;
        }
        elsif ($prev_lines ne "")
        {
            $line = $prev_lines . $line;
            $prev_lines = "";
        }

        # Parse statements.

        # Look out for revision ids, always take the first one.

        if ($line =~ m/[ \t"]([0-9a-f]{40})[ \t"]/)
        {
            $revision_id = $1;
        }

        # Lines with arrow heads.

        if ($line =~ m/B $nre(( $nre $nre)+) *\".* P 3(( $nre){6}) *\"/)
        {
            my (@arrow_points,
                @line_points);
            @line_points = split(/ /, $1);
            shift(@line_points);
            @arrow_points = split(/ /, $3);
            shift(@arrow_points);
            if ($line =~ m/-> "??([0-9a-f]{40})[ \t"]/)
            {
                my $to_revision_id = $1;
                push(@arrows, {from_revision_id => $revision_id,
                               to_revision_id   => $to_revision_id,
                               line             => \@line_points,
                               arrow            => \@arrow_points});
            }
        }

        # Boxes.

        elsif ($line =~ m/p 4(( $nre){8}) *\"/)
        {
            my ($br_x,
                $br_y,
                @coords,
                $tl_x,
                $tl_y);
            @coords = split(/ /, $1);
            shift(@coords);
            $tl_x = $instance->{graph_data}->{max_x} + 100;
            $br_x = 0;
            foreach my $i (0, 2, 4, 6)
            {
                $tl_x = ($tl_x < $coords[$i]) ? $tl_x : $coords[$i];
                $br_x = ($br_x > $coords[$i]) ? $br_x : $coords[$i];
            }
            $tl_y = $instance->{graph_data}->{max_y} + 100;
            $br_y = 0;
            foreach my $i (1, 3, 5, 7)
            {
                $tl_y = ($tl_y < $coords[$i]) ? $tl_y : $coords[$i];
                $br_y = ($br_y > $coords[$i]) ? $br_y : $coords[$i];
            }
            push(@rectangles, {revision_id => $revision_id,
                               tl_x        => $tl_x,
                               tl_y        => $tl_y,
                               br_x        => $br_x,
                               br_y        => $br_y});
        }

        # Circles.

        elsif ($line =~ m/e(( $nre){4}) *\"/)
        {
            my @list;
            @list = split(/ /, $1);
            shift(@list);
            push(@circles, {revision_id => $revision_id,
                            x           => $list[0],
                            y           => $list[1],
                            width       => $list[2],
                            height      => $list[3]});
        }

        # Bounding box (i.e. the total size of the graph).

        elsif ($line =~ m/bb=\"($nre),($nre),($nre),($nre)\"/)
        {
            $instance->{graph_data}->{max_x} = max($1, $3);
            $instance->{graph_data}->{max_y} = max($2, $4);
        }

    }

    $instance->{graph_data}->{arrows} = \@arrows;
    $instance->{graph_data}->{circles} = \@circles;
    $instance->{graph_data}->{rectangles} = \@rectangles;

}
#
##############################################################################
#
#   Routine      - dot_input_handler_cb
#
#   Description  - Given a child graph database, generate the dot instructions
#                  writing them out to the specified file handle.
#
#   Data         - $fh_in    : The STDIN file handle for the dot subprocess.
#                  $instance : The history graph window instance.
#
##############################################################################



sub dot_input_handler_cb($$)
{

    my ($fh_in, $instance) = @_;

    my ($child_db,
        $hex_id_height,
        $hex_id_width,
        @revision_ids,
        $text_item);

    # Temporarily switch our locale to the standard default C one. We don't
    # want floats to be written with commas in them for instance.

    setlocale(LC_ALL, "C");

    # Create a canvas text item and then use it to get the pixel size of a hex
    # id when displayed on the screen. This text item is also used later on for
    # any tags that need to be displayed.

    $text_item = Gnome2::Canvas::Item->new
        ($instance->{graph_canvas}->root(),
         "Gnome2::Canvas::Text",
         font_desc  => $instance->{fontdescription},
         text       => "A" x HEX_ID_LENGTH);
    $hex_id_width = $text_item->get("text-width");
    $hex_id_height = $text_item->get("text-height");
    $hex_id_height = max(HEIGHT, $hex_id_height + (TEXT_BORDER * 2));
    $hex_id_width = max(WIDTH, $hex_id_width + (TEXT_BORDER * 2));

    # Generate a list of topographically sorted revision ids for each revision
    # in the graph database.

    $child_db = $instance->{graph_data}->{child_graph};
    @revision_ids = keys(%$child_db);

    # Now write out the dot graph to the dot subprocess. We don't need to worry
    # about reading and writing at the same time as dot needs the complete
    # graph before it can do its magic.

    # Pre-amble.

    $fh_in->print("digraph \"mtn-browse\"\n"
                  . "{\n");
    if ($instance->{graph_data}->{parameters}->{draw_left_to_right})
    {
        $fh_in->print("  graph [rankdir=LR];\n");
    }
    else
    {
        $fh_in->print("  graph [rankdir=BT];\n");
    }
    $fh_in->print("  graph [ranksep=\"0.25\"];\n"
                  . "  node [label=\"\"];\n");

    # Rectangular non-merge nodes, possibly changing the width for tagged nodes
    # if we need more space. We give more padding for tagged nodes as these can
    # be long strings and the text scaling is not as fine grained as the rest
    # of the canvas widgets).

    $fh_in->printf("  node [shape=box, width = %f, height = %f];\n",
                   $hex_id_width / DPI,
                   $hex_id_height / DPI);
    foreach my $revision_id (@revision_ids)
    {
        if (! ($child_db->{$revision_id}->{flags} & CIRCULAR_NODE))
        {
            my $tag;
            my $width = WIDTH;
            $fh_in->print("  \"" . $revision_id . "\"");
            if (defined($tag = get_node_tag($instance, $revision_id)))
            {
                $text_item->set(text => $tag);
                $width = max(WIDTH,
                             $text_item->get("text-width")
                                 + (TEXT_BORDER * 6));
            }
            if ($width != WIDTH)
            {
                $fh_in->printf(" [width = %f];\n", $width / DPI);
            }
            else
            {
                $fh_in->print(";\n");
            }
        }
    }

    # Circular merge nodes.

    $fh_in->printf("  node [shape=circle, width = %f, height = %f];\n",
                   $hex_id_height / DPI,
                   $hex_id_height / DPI);
    foreach my $revision_id (@revision_ids)
    {
        if ($child_db->{$revision_id}->{flags} & CIRCULAR_NODE)
        {
            $fh_in->print("  \"" . $revision_id . "\";\n");
        }
    }

    # Head nodes. These need to be grouped together.

    $fh_in->print("  subgraph heads\n"
                  . "  {\n"
                  . "    rank = sink;\n");
    foreach my $revision_id (@{$instance->{graph_data}->{head_revisions}})
    {
        $fh_in->print("    \"" . $revision_id . "\";\n");
    }
    $fh_in->print("  }\n");

    # Lines. Use the weight attribute on lines that go between a selected node
    # and an unselected one, with the unselected one having no
    # parents/children. The higher the weighting the shorter and straighter the
    # lines are. This has the effect of making sure that edge propagation nodes
    # are closely clustered around their selected relations.

    foreach my $revision_id (@revision_ids)
    {
        my $selected = $child_db->{$revision_id}->{flags} & SELECTED_NODE;
        my $no_parents = $child_db->{$revision_id}->{flags} & NO_PARENTS;
        foreach my $child_id (@{$child_db->{$revision_id}->{children}})
        {
            my $child_selected =
                $child_db->{$child_id}->{flags} & SELECTED_NODE;
            $fh_in->print("  \"" . $revision_id . "\" -> \"" . $child_id
                          . "\"");
            if ((! $selected && $child_selected && $no_parents)
                || ($selected && ! $child_selected
                    && scalar(@{$child_db->{$child_id}->{children}}) == 0))
            {
                $fh_in->print(" [weight = 4];\n");
            }
            else
            {
                $fh_in->print(";\n");
            }
        }
    }

    # Close off the graph and close dot's input so that it knows to start
    # processing the data.

    $fh_in->print("}\n");
    $fh_in->close();

    # Destroy the canvas text item that was used in the font size calculations.

    $text_item->destroy();

    # Reset our locale back to the local one again.

    setlocale(LC_ALL, "");

}
#
##############################################################################
#
#   Routine      - draw_graph
#
#   Description  - Given a child graph database and the geometric objects laid
#                  out by dot, draw the history graph in the canvas widget.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub draw_graph($)
{

    my $instance = $_[0];

    my ($counter,
        $total,
        $update_interval);
    my $child_db = $instance->{graph_data}->{child_graph};
    my $wm = WindowManager->instance();

    $instance->{graph_canvas}->set_scroll_region
        (0,
         0,
         $instance->{graph_data}->{max_x} + (CANVAS_BORDER * 2),
         $instance->{graph_data}->{max_y} + (CANVAS_BORDER * 2));
    $instance->{graph}->{group} =
        Gnome2::Canvas::Item->new($instance->{graph_canvas}->root(),
                                  "Gnome2::Canvas::Group",
                                  x => CANVAS_BORDER,
                                  y => CANVAS_BORDER);

    $instance->{graph}->{node_text_items} = [];
    $instance->{graph}->{selection_box} = Gnome2::Canvas::Item->new
        ($instance->{graph}->{group},
         "Gnome2::Canvas::Rect",
         x1            => 0,
         y1            => 0,
         x2            => 1,
         y2            => 1,
         fill_color    => "Red",
         outline_color => "Red",
         width_pixels  => LINE_WIDTH);
    $instance->{graph}->{selection_box}->hide();

    $total = scalar(@{$instance->{graph_data}->{rectangles}})
        + scalar(@{$instance->{graph_data}->{circles}})
        + scalar(@{$instance->{graph_data}->{arrows}});
    $update_interval = calculate_update_interval($total);

    # Cancel any selections that are currently active as we have just blanked
    # the canvas.

    foreach my $item ($instance->{graph_advanced_find_button},
                      @{$instance->{revision_sensitive_group}})
    {
        $item->set_sensitive(FALSE);
    }

    # Draw the rectangular nodes with text inside them.

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status(__("Drawing graph"));
    $wm->update_gui();
    $counter = 1;
    foreach my $rectangle (@{$instance->{graph_data}->{rectangles}})
    {

        my ($tag,
            $text,
            $widget);
        my $node = $child_db->{$rectangle->{revision_id}};
        my $node_group = Gnome2::Canvas::Item->new($instance->{graph}->{group},
                                                   "Gnome2::Canvas::Group",
                                                   x => 0,
                                                   y => 0);
        my $outline_colour = SELECTED_BORDER_COLOUR;

        # Link the child database node to the relevant geometric canvas
        # information.

        $node->{canvas_item_details} = $rectangle;

        # Decide on the colour depending on whether the node is suspended and
        # then if it is not selected.

        if ($node->{flags} & SUSPENDED_NODE)
        {
            $outline_colour = SUSPENDED_BORDER_COLOUR;
        }
        elsif (! ($node->{flags} & SELECTED_NODE))
        {
            $outline_colour = NOT_SELECTED_BORDER_COLOUR;
        }

        # Draw the rectangle.

        $widget = Gnome2::Canvas::Item->new
            ($node_group,
             "Gnome2::Canvas::Rect",
             x1             => $rectangle->{tl_x},
             y1             => $rectangle->{tl_y},
             x2             => $rectangle->{br_x},
             y2             => $rectangle->{br_y},
             fill_color_gdk => get_node_colour($instance, $node),
             outline_color  => $outline_colour,
             width_pixels   => LINE_WIDTH);

        # Now the text, use a revision's tag and failing that use the first
        # eight characters of its hex id.

        if (defined($tag = get_node_tag($instance, $rectangle->{revision_id})))
        {
            $text = $tag;
        }
        else
        {
            $text = substr($rectangle->{revision_id}, 0, HEX_ID_LENGTH);
        }
        $widget = Gnome2::Canvas::Item->new
            ($node_group,
             "Gnome2::Canvas::Text",
             x          => $rectangle->{tl_x}
                           + floor(($rectangle->{br_x} - $rectangle->{tl_x}
                                    + 1) / 2),
             y          => $rectangle->{tl_y}
                           + floor(($rectangle->{br_y} - $rectangle->{tl_y}
                                    + 1) / 2),
             font_desc  => $instance->{fontdescription},
             text       => $text,
             fill_color => FONT_COLOUR);
        $widget->raise_to_top();
        $widget->show();
        push(@{$instance->{graph}->{node_text_items}}, $widget);

        $node_group->signal_connect
            ("event",
             \&canvas_item_event_cb,
             {instance    => $instance,
              revision_id => $rectangle->{revision_id}});

        if (($counter % $update_interval) == 0)
        {
            $instance->{appbar}->set_progress_percentage($counter / $total);
            $wm->update_gui();
        }
        ++ $counter;

        # Stop if the user wants to.

        last if ($instance->{stop});

    }

    # Draw the circular nodes.

    foreach my $circle (@{$instance->{graph_data}->{circles}})
    {

        my $widget;
        my $node = $child_db->{$circle->{revision_id}};
        my $outline_colour = SELECTED_BORDER_COLOUR;

        # Link the child database node to the relevant geometric canvas
        # information.

        $node->{canvas_item_details} = $circle;

        # Decide on the colour depending on whether the node is suspended and
        # then if it is not selected.

        if ($node->{flags} & SUSPENDED_NODE)
        {
            $outline_colour = SUSPENDED_BORDER_COLOUR;
        }
        elsif (! ($node->{flags} & SELECTED_NODE))
        {
            $outline_colour = NOT_SELECTED_BORDER_COLOUR;
        }

        # Draw the circle.

        $widget = Gnome2::Canvas::Item->new
            ($instance->{graph}->{group},
             "Gnome2::Canvas::Ellipse",
             x1             => $circle->{x} - $circle->{width},
             y1             => $circle->{y} - $circle->{height},
             x2             => $circle->{x} + $circle->{width},
             y2             => $circle->{y} + $circle->{height},
             fill_color_gdk => get_node_colour($instance, $node),
             outline_color  => $outline_colour,
             width_pixels   => LINE_WIDTH);
        $widget->raise_to_top();
        $widget->show();

        $widget->signal_connect("event",
                                \&canvas_item_event_cb,
                                {instance    => $instance,
                                 revision_id => $circle->{revision_id}});

        if (($counter % $update_interval) == 0)
        {
            $instance->{appbar}->set_progress_percentage($counter / $total);
            $wm->update_gui();
        }
        ++ $counter;

        # Stop if the user wants to.

        last if ($instance->{stop});
    }

    # Draw the lines.

    foreach my $arrow (@{$instance->{graph_data}->{arrows}})
    {

        my ($bpath,
            $colour,
            $i,
            $pathdef);
        my $head = $arrow->{arrow};
        my $line = $arrow->{line};

        if (! ($child_db->{$arrow->{from_revision_id}}->{flags}
               & SELECTED_NODE)
            || ! ($child_db->{$arrow->{to_revision_id}}->{flags}
                  & SELECTED_NODE))
        {
            $colour = NOT_SELECTED_BORDER_COLOUR;
        }
        else
        {
            $colour = SELECTED_BORDER_COLOUR;
        }
        $pathdef = Gnome2::Canvas::PathDef->new();
        $pathdef->moveto($$line[0], $$line[1]);
        $i = 2;
        for ($i = 2; $i < scalar(@$line); $i += 6)
        {
            $pathdef->curveto($$line[$i],
                              $$line[$i + 1],
                              $$line[$i + 2],
                              $$line[$i + 3],
                              $$line[$i + 4],
                              $$line[$i + 5]);
        }
        $pathdef->lineto($$head[2], $$head[3]);
        $pathdef->moveto($$head[0], $$head[1]);
        $pathdef->lineto($$head[2], $$head[3]);
        $pathdef->lineto($$head[4], $$head[5]);
        $pathdef->closepath();
        $bpath = Gnome2::Canvas::Item->new($instance->{graph}->{group},
                                           "Gnome2::Canvas::Bpath",
                                           fill_color    => $colour,
                                           outline_color => $colour,
                                           width_pixels  => LINE_WIDTH);
        $bpath->set_path_def($pathdef);
        $bpath->lower_to_bottom();
        $bpath->show();

        if (($counter % $update_interval) == 0)
        {
            $instance->{appbar}->set_progress_percentage($counter / $total);
            $wm->update_gui();
        }
        ++ $counter;

        # Stop if the user wants to.

        last if ($instance->{stop});

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Work around a bug where the scroll bar arrows don't work with canvases,
    # basically we need to set the step increment to something so take an
    # eighth of the page size.

    foreach my $adjustment
        ($instance->{graph_scrolledwindow}->get_hadjustment(),
         $instance->{graph_scrolledwindow}->get_vadjustment())
    {
        $adjustment->step_increment($adjustment->page_increment() / 8);
    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - select_node
#
#   Description  - Select the specified node in the history graph.
#
#   Data         - $instance    : The history graph window instance.
#                  $revision_id : The id of the revision that is to be
#                                 selected.
#
##############################################################################



sub select_node($$)
{

    my ($instance, $revision_id) = @_;

    my ($branches,
        @certs,
        $change_log,
        $date,
        $item,
        $node);

    # Look up the information node for the revision id.

    return unless (exists($instance->{graph_data}->{child_graph}->
                          {$revision_id}));
    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    return unless (exists($node->{canvas_item_details}));
    $item = $node->{canvas_item_details};

    # Move and resize the selection rectangle depending upon whether we are
    # dealing with a rectangular or circular node.

    if ($node->{flags} & CIRCULAR_NODE)
    {
        $instance->{graph}->{selection_box}->set
            (x1 => $item->{x} - $item->{width} - SELECTION_BORDER,
             y1 => $item->{y} - $item->{height} - SELECTION_BORDER,
             x2 => $item->{x} + $item->{width} + SELECTION_BORDER,
             y2 => $item->{y} + $item->{height} + SELECTION_BORDER);
    }
    else
    {
        $instance->{graph}->{selection_box}->set
            (x1 => $item->{tl_x} - SELECTION_BORDER,
             y1 => $item->{tl_y} - SELECTION_BORDER,
             x2 => $item->{br_x} + SELECTION_BORDER,
             y2 => $item->{br_y} + SELECTION_BORDER);
    }
    $instance->{graph}->{selection_box}->lower_to_bottom();
    $instance->{graph}->{selection_box}->show();

    # Display the details about the revision in the details section at the
    # bottom of the history graph window. We need to get the changelog but the
    # rest is cached from before.

    $instance->{mtn}->certs(\@certs, $revision_id);
    foreach my $cert (@certs)
    {
        if ($cert->{name} eq "changelog")
        {
            $change_log = $cert->{value};
            $change_log =~ s/\s+$//s;
            last;
        }
    }
    $branches = join("\n", @{$node->{branches}});
    $date = mtn_time_string_to_locale_time_string($node->{date});
    set_label_value($instance->{author_value_label}, $node->{author});
    set_label_value($instance->{date_value_label}, $date);
    set_label_value($instance->{branch_value_label}, $branches);
    set_label_value($instance->{change_log_value_label}, $change_log);

    # Enable any buttons that should be enabled once a revision has been
    # selected.

    foreach my $item (@{$instance->{revision_sensitive_group}})
    {
        $item->set_sensitive(TRUE);
    }

    # Make a note of what revision has been selected.

    $instance->{selected_revision_id} = $revision_id;

}
#
##############################################################################
#
#   Routine      - scroll_to_node
#
#   Description  - Scroll the canvas to the specified node in the history
#                  graph.
#
#   Data         - $instance    : The history graph window instance.
#                  $revision_id : The id of the revision that is to be
#                                 selected.
#
##############################################################################



sub scroll_to_node($$)
{

    my ($instance, $revision_id) = @_;

    my ($height,
        $item,
        $node,
        $width,
        $x,
        $y);

    # Look up the information node for the revision id.

    return unless (exists($instance->{graph_data}->{child_graph}->
                          {$revision_id}));
    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    return unless (exists($node->{canvas_item_details}));
    $item = $node->{canvas_item_details};

    # Find out the centre of the node on the canvas.

    if ($node->{flags} & CIRCULAR_NODE)
    {
        $x = $item->{x};
        $y = $item->{y};
    }
    else
    {
        $x = $item->{tl_x} + floor(($item->{br_x} - $item->{tl_x} + 1) / 2);
        $y = $item->{tl_y} + floor(($item->{br_y} - $item->{tl_y} + 1) / 2);
    }

    # Get the current dimensions of the canvas.

    ($width, $height) =
        ($instance->{graph_canvas}->window()->get_geometry())[2, 3];

    # Convert from world coordinates to canvas pixels (takes into account any
    # scaling factors currently in effect). Don't forget to also add in the
    # canvas borders.

    $x += CANVAS_BORDER;
    $y += CANVAS_BORDER;
    ($x, $y) = $instance->{graph_canvas}->w2c($x, $y);

    # The scroll_to() method moves the (x,y) spot to the top left hand corner
    # so adjust it so that it is in the centre instead.

    $x = max($x - floor($width / 2), 0);
    $y = max($y - floor($height / 2), 0);
    $instance->{graph_canvas}->scroll_to($x, $y);

}
#
##############################################################################
#
#   Routine      - scale_canvas
#
#   Description  - Adjust the scale of the canvas widgets.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub scale_canvas($)
{

    my $instance = $_[0];

    my $wm = WindowManager->instance();

    # Hide the canvas during the zoom operation, it can be a bit messy.

    $wm->make_busy($instance, 1);
    $instance->{graph_canvas}->hide();
    $wm->update_gui();

    # Adjust the canvas zoom factor, also resize the fonts on all the text
    # items (hiding them when the text gets too small to be of any use).

    $instance->{graph_canvas}->set_pixels_per_unit($instance->{scale});
    if ((FONT_SIZE * $instance->{scale}) < 3)
    {
        foreach my $text_item (@{$instance->{graph}->{node_text_items}})
        {
            $text_item->hide();
        }
    }
    else
    {
        $instance->{fontdescription}->set_size
            (floor(FONT_SIZE * $instance->{scale}) * PANGO_SCALE);
        foreach my $text_item (@{$instance->{graph}->{node_text_items}})
        {
            $text_item->set(font_desc => $instance->{fontdescription});
            $text_item->show();
        }
    }

    # If we have something to redraw then do so, resized text looks
    # blocky/pixelated but a redraw sorts this out.

    if (defined($instance->{graph}->{group}))
    {
        $instance->{graph_canvas}->request_redraw
            (0,
             0,
             $instance->{graph_data}->{max_x} + (CANVAS_BORDER * 2),
             $instance->{graph_data}->{max_y} + (CANVAS_BORDER * 2));
    }

    # Make sure the canvas is up to date and then show it again.

    $instance->{graph_canvas}->update_now();
    $instance->{graph_canvas}->show();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - get_node_colour
#
#   Description  - Given the specified node in the graph database, either find
#                  a colour that has been used for that `type' of node before
#                  or generate a new colour from scratch.
#
#   Data         - $instance    : The history graph window instance.
#                  $node        : The node in the graph database that is to
#                                 have a colour returned for it.
#                  Return Value : The colour as a Gtk2::Gdk::Color object.
#
##############################################################################



sub get_node_colour($$)
{

    my ($instance, $node) = @_;

    my $colour;
    my $hash_values = [""];

    # Decide what to base the colour on depending upon the user's preferences
    # (either branch or author).

    if ($instance->{graph_data}->{parameters}->{colour_by_author})
    {
        $hash_values = [$node->{author}] if (defined($node->{author}));
    }
    else
    {
        $hash_values = $node->{branches} if (scalar(@{$node->{branches}}) > 0);
    }

    # First look for an existing colour for any of the branches.

    foreach my $value (@$hash_values)
    {
        if (exists($instance->{colour_db}->{$value}))
        {
            $colour = $instance->{colour_db}->{$value};
            last;
        }
    }

    # Do we need a new colour?

    if (! defined($colour))
    {

        # Yes we do.

        # If the value used to generate the hash hash is "" then choose white
        # otherwise generate a colour based on the MD5 hash of that value.

        if ($$hash_values[0] eq "")
        {
            $colour = Gtk2::Gdk::Color->new(65535, 65535, 65535);
        }
        else
        {

            my ($blue,
                $green,
                $hue,
                $red,
                $saturation,
                $value);

            # Generate a new colour by hashing the differentiating value and
            # then using the first few bytes of that hash as HSV values (idea
            # taken from monotone-viz).

            ($hue, $saturation, $value) = unpack("CCC", md5($$hash_values[0]));

            # Now scale values. Hue 0 to 359, saturation and value 0 to 1. In
            # addition scale saturation to only go from 35% to 50% and value to
            # only go from 70% to 100%. Then convert from HSV to RGB.

            $hue = ($hue / 255) * 359;
            $saturation = (($saturation / 255) * 0.15) + 0.35;
            $value = (($value / 255) * 0.30) + 0.70;
            hsv_to_rgb($hue, $saturation, $value, \$red, \$green, \$blue);

            # Scale RGB values and create a new colour object.

            $colour =
                Gtk2::Gdk::Color->new(floor(($red * 65535) + 0.5) & 0xffff,
                                      floor(($green * 65535) + 0.5) & 0xffff,
                                      floor(($blue * 65535) + 0.5) & 0xffff);

        }

        # Store colour under its hash value for possible reuse.

        $instance->{colour_db}->{$$hash_values[0]} = $colour;

    }

    return $colour;

}
#
##############################################################################
#
#   Routine      - hsv_to_rgb
#
#   Description  - Convert hue, saturation and value into RGB values. This
#                  algorithm was taken from:
#                      http://www.cs.rit.edu/~ncs/color/t_convert.html
#
#   Data         - $hue        : Hue value.
#                  $saturation : Saturation value.
#                  $value      : Brightness value.
#                  $red        : A reference to a variable that is to contain
#                                the red component.
#                  $green      : A reference to a variable that is to contain
#                                the green component.
#                  $blue       : A reference to a variable that is to contain
#                                the blue component.
#
##############################################################################



sub hsv_to_rgb($$$$$$)
{

    my ($hue, $saturation, $value, $red, $green, $blue) = @_;

    # Deal with achromatic grey.

    if ($saturation == 0)
    {
        $$red = $$green = $$blue = $value;
    }

    # Now non-grey colours.

    else
    {

        my ($f,
            $i,
            $p,
            $q,
            $t);

        # Hue is 0 to 360, scale it down into 0 to 5 and put the factorial part
        # of that division into $f.

        $hue /= 60;
        $i = floor($hue);
        $f = $hue - $i;
        $p = $value * (1 - $saturation);
        $q = $value * (1 - ($saturation * $f));
        $t = $value * (1 - ($saturation * (1 - $f)));

        if ($i == 0)
        {
            $$red = $value;
            $$green = $t;
            $$blue = $p;
        }
        elsif ($i == 1)
        {
            $$red = $q;
            $$green = $value;
            $$blue = $p;
        }
        elsif ($i == 2)
        {
            $$red = $p;
            $$green = $value;
            $$blue = $t;
        }
        elsif ($i == 3)
        {
            $$red = $p;
            $$green = $q;
            $$blue = $value;
        }
        elsif ($i == 4)
        {
            $$red = $t;
            $$green = $p;
            $$blue = $value;
        }
        else
        {
            $$red = $value;
            $$green = $p;
            $$blue = $q;
        }

    }

}
#
##############################################################################
#
#   Routine      - populate_revision_details
#
#   Description  - Given the specified revision id, populate the relevant node
#                  in the graph database with the revision's details such as
#                  its author, its date and lists of its branches and tags.
#
#   Data         - $instance    : The history graph window instance.
#                  $revision_id : The id of the revision that is to have its
#                                 details populated in the graph database.
#
##############################################################################



sub populate_revision_details($$)
{

    my ($instance, $revision_id) = @_;

    my ($author,
        @branches,
        @certs,
        $date,
        $node,
        @tags);

    # Get the revision's list of branches, tags and date.

    $instance->{mtn}->certs(\@certs, $revision_id);
    foreach my $cert (@certs)
    {
        if ($cert->{name} eq "author")
        {
            $author = $cert->{value} unless (defined($author));
        }
        elsif ($cert->{name} eq "branch")
        {
            push(@branches, $cert->{value});
        }
        elsif ($cert->{name} eq "date")
        {
            $date = $cert->{value} unless (defined($date));
        }
        elsif ($cert->{name} eq "tag")
        {
            push(@tags, $cert->{value});
        }
    }
    @branches = sort(@branches);
    @tags = sort(@tags);

    # Now store this data in the relevant node in the graph database. Also make
    # sure that the node is no longer circular if it has a tag name that needs
    # to be displayed.

    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    $node->{author} = $author;
    $node->{branches} = \@branches;
    $node->{date} = $date;
    $node->{tags} = \@tags;
    $node->{flags} &= ~CIRCULAR_NODE if (scalar(@tags) > 0);

}
#
##############################################################################
#
#   Routine      - show_key_textview
#
#   Description  - Shows or hides the key textview window.
#
#   Data         - $instance : The history graph window instance.
#                  $show     : True if the key textview window is to be shown,
#                              otherwise false if it is to be hidden.
#
##############################################################################



sub show_key_textview($$)
{

    my ($instance, $show) = @_;

    my $current_child = ($instance->{graph_container_hbox}->get_children())[0];

    if ($show && $current_child == $instance->{graph_scrolledwindow})
    {

        # We need to show the key textview and update it.

        $instance->{graph_container_hbox}->
            remove($instance->{graph_scrolledwindow});
        $instance->{graph_hpaned}->add1($instance->{graph_scrolledwindow});
        $instance->{graph_hpaned}->child1_resize(TRUE);
        $instance->{graph_hpaned}->child1_shrink(FALSE);
        $instance->{graph_container_hbox}->
            pack_start($instance->{graph_hpaned}, TRUE, TRUE, 0);
        $instance->{graph_container_hbox}->show_all();

        update_key_textview($instance);

    }
    elsif (! $show && $current_child == $instance->{graph_hpaned})
    {

        # We need to hide the key textview.

        $instance->{graph_container_hbox}->remove($instance->{graph_hpaned});
        $instance->{graph_hpaned}->remove($instance->{graph_scrolledwindow});
        $instance->{graph_hpaned}->hide();
        $instance->{graph_container_hbox}->
            pack_start($instance->{graph_scrolledwindow}, TRUE, TRUE, 0);
        $instance->{graph_container_hbox}->show_all();
        $instance->{key_buffer}->set_text("");

    }

}
#
##############################################################################
#
#   Routine      - update_key_textview
#
#   Description  - Update the key textview with the current contents of the
#                  colour database (the cache used to remember colours when
#                  drawing a graph).
#
#   Data         - $instance    : The history graph window instance.
#
##############################################################################



sub update_key_textview($)
{

    my $instance = $_[0];

    # Don't bother if we aren't displaying the key textview.

    return unless (($instance->{graph_container_hbox}->get_children())[0]
                   == $instance->{graph_hpaned});

    my (@names,
        $padding,
        $tag_table,
        @tags);
    my $max_len = 0;

    # Empty out the key textview along with any tags.

    $instance->{key_buffer}->set_text("");
    $tag_table = $instance->{key_buffer}->get_tag_table();
    $tag_table->foreach(sub { push(@tags, $_[0]); });
    foreach my $tag (@tags)
    {
        $tag_table->remove($tag);
    }

    @names = sort(keys(%{$instance->{colour_db}}));

    # Find out the longest name so that we can pad the rest.

    foreach my $name (@names)
    {
        my $len;
        $max_len = $len if (($len = length($name)) > $max_len);
    }
    $padding = " " x $max_len;

    # Now populate the key textview with the colour coded keys.

    foreach my $name (@names)
    {
        my $tag;
        $tag = $instance->{key_buffer}->create_tag
            ($tag,
             "background" => colour_to_string($instance->{colour_db}->
                                              {$name}));
        $instance->{key_buffer}->insert_with_tags
            ($instance->{key_buffer}->get_end_iter(),
             substr($name . $padding, 0, $max_len) . "\n",
             $tag);
    }

    # Make sure we are at the top.

    $instance->{key_buffer}->
        place_cursor($instance->{key_buffer}->get_start_iter());
    if ($instance->{key_scrolledwindow}->realized())
    {
        $instance->{key_scrolledwindow}->get_vadjustment()->set_value(0);
        $instance->{key_scrolledwindow}->get_hadjustment()->set_value(0);
    }

}
#
##############################################################################
#
#   Routine      - get_node_tag
#
#   Description  - Return a tag associated with the specified node in the
#                  history graph.
#
#   Data         - $instance    : The history graph window instance.
#                  $revision_id : The id of the revision that is to have one
#                                 of its tags returned.
#                  Return Value : Either one of the tags associated with the
#                                 specified node or undef if the node has no
#                                 tags.
#
##############################################################################



sub get_node_tag($$)
{

    my ($instance, $revision_id) = @_;

    my $node = $instance->{graph_data}->{child_graph}->{$revision_id};

    # Are there any tags?

    if (scalar(@{$node->{tags}}) > 0)
    {

        # Yes there are so either return the highest weighted tag if we have a
        # weightings list or just the first one if we don't.

        if (scalar(@{$instance->{compiled_tag_weightings}}) > 0)
        {

            my @results;

            # Ok so now find the tag with the highest weighting.

            foreach my $tag (@{$node->{tags}})
            {
                my $weighting = 0;
                foreach my $entry (@{$instance->{compiled_tag_weightings}})
                {
                    if ($tag =~ m/$entry->{compiled_re}/)
                    {
                        $weighting = $entry->{weighting};
                        last;
                    }
                }
                push(@results, {tag_name => $tag, weighting => $weighting});
            }

            # Now sort the results, highest weighting first and then sorted on
            # tag name.

            @results = sort({ $b->{weighting} <=> $a->{weighting}
                              || $a->{tag_name} cmp $b->{tag_name} }
                            @results);
            return $results[0]->{tag_name};

        }
        else
        {
            return $node->{tags}->[0];
        }

    }

    return;

}
#
##############################################################################
#
#   Routine      - compile_tag_weighting_patterns
#
#   Description  - Compiles up a list of regular expresion records
#                  reppresenting the user's tag weightings list.
#
#   Data         - $list : A reference to the list that is to populated with
#                          the compiled regular expression weightings data.
#
##############################################################################



sub compile_tag_weighting_patterns($)
{

    my $list = $_[0];

    @$list = ();
    foreach my $entry (@{$user_preferences->{tag_weightings}})
    {
        push(@$list, {weighting   => $entry->{weighting},
                      compiled_re => qr/$entry->{pattern}/});
    }

}
#
##############################################################################
#
#   Routine      - get_history_graph_window
#
#   Description  - Creates or prepares an existing history graph window for
#                  use.
#
#   Data         - Return Value : A reference to the newly created or unused
#                                 history instance record.
#
##############################################################################



sub get_history_graph_window()
{

    my $instance;
    my $wm = WindowManager->instance();

    # Create a new history graph window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

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
                            "graph_container_hbox",
                            "graph_hpaned",
                            "graph_scrolledwindow",
                            "key_scrolledwindow",
                            "key_textview",
                            "graph_button_vbox",
                            "show_key_togglebutton",
                            "graph_advanced_find_button",
                            "stop_button",
                            "author_value_label",
                            "date_value_label",
                            "branch_value_label",
                            "change_log_value_label")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $window_type);

        # Create the graph canvas widget. We can't do this in Glade as
        # something does not honour the anti-aliased setting.

        $instance->{graph_canvas} = Gnome2::Canvas->new_aa();
        $instance->{graph_scrolledwindow}->add($instance->{graph_canvas});
        $instance->{graph_canvas}->show_all();

        # Setup the history graph callbacks.

        $instance->{window}->signal_connect
            ("delete_event",
             sub {
                 my ($widget, $event, $instance) = @_;
                 return TRUE if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $widget->hide();
                 reset_history_graph_instance($instance);
                 $instance->{mtn} = undef;
                 return TRUE;
             },
             $instance);
        $instance->{stop_button}->signal_connect
            ("clicked", sub { $_[1]->{stop} = 1; }, $instance);
        $instance->{graph_scrolledwindow}->signal_connect
            ("button_press_event",
             \&canvas_item_event_cb,
             {instance    => $instance,
              revision_id => undef});

        # Gnome2::Canvas is a bit buggy and can get upset if any of its widgets
        # are referenced by Perl when it gets destroyed (or so it seems).
        # Therefore register a cleanup handler that will make sure all canvas
        # widgets are unreferenced before the application exits.

        $instance->{cleanup_handler} =
            sub {
                my $instance = $_[0];
                $instance->{graph} = undef;
                $instance->{graph_canvas} = undef;
            };

        # Create the font description for displaying text on the graph. I am
        # using a fixed width or monospaced font as I think on balance it makes
        # it easier to read hex ids.

        $instance->{fontdescription} = Gtk2::Pango::FontDescription->
            from_string($user_preferences->{fixed_font});

        # Setup button sensitivity groups.

        $instance->{revision_sensitivity_group} = [];
        foreach my $item ("go_to_selected_revision",
                          "graph_revision_change_history",
                          "graph_revision_change_log",
                          "browse_revision")
        {
            push(@{$instance->{revision_sensitive_group}},
                 $glade->get_widget($item . "_button"));
        }

        # Setup the key viewer.

        $instance->{key_buffer} = $instance->{key_textview}->get_buffer();
        $instance->{key_textview}->modify_font($mono_font);

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
             {widget   => "graph_scrolledwindow",
              help_ref => __("mtnb-uhg-colouring-used-in-graphs")},
             {widget   => "graph_button_vbox",
              help_ref => __("mtnb-uhg-graph-buttons")},
             {widget   => undef,
              help_ref => __("mtnb-uhg-the-history-graph-window")});

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        set_window_size($instance->{window}, $window_type);
        $instance->{graph_hpaned}->set_position(600);
        $instance->{show_key_togglebutton}->set_active(FALSE);
        $instance->{stop_button}->set_sensitive(FALSE);
        $instance->{graph_canvas}->set_pixels_per_unit(1);
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->clear_stack();

    }

    local $instance->{in_cb} = 1;

    $instance->{fontdescription}->set_size(FONT_SIZE * PANGO_SCALE);
    $instance->{scale} = 1;
    $instance->{stop} = 0;
    $instance->{selected_revision_id} = undef;
    $instance->{compiled_tag_weightings} =[];
    compile_tag_weighting_patterns($instance->{compiled_tag_weightings});
    reset_history_graph_instance($instance);
    show_key_textview($instance, undef);

    return $instance;

}
#
##############################################################################
#
#   Routine      - reset_history_graph_instance
#
#   Description  - Resets the specified instance to a known empty state.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub reset_history_graph_instance($)
{

    my $instance = $_[0];

    reset_history_graph_window($instance);
    $instance->{colour_db} = {};
    $instance->{graph_data} =
        {parameters     =>
             {new                      => 0,
              branches                 => [],
              date_state               => undef,
              from_date                => "",
              to_date                  => "",
              draw_left_to_right       => $draw_left_to_right,
              show_all_propagate_nodes => $show_all_propagate_nodes,
              colour_by_author         => $colour_by_author,
              revision_id              => undef},
         child_graph    => {},
         head_revisions => [],
         arrows         => [],
         circles        => [],
         rectangles     => [],
         max_x          => 0,
         max_y          => 0};

}
#
##############################################################################
#
#   Routine      - reset_history_graph_window
#
#   Description  - Destroys all of the history graph canvas item widgets and
#                  then resets the history graph window to its known empty
#                  state.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub reset_history_graph_window($)
{

    my $instance = $_[0];

    my $group = $instance->{graph}->{group};

    $instance->{graph} = {group           => undef,
                          node_text_items => [],
                          selection_box   => undef};
    $group->destroy() if defined($group);
    $instance->{graph_canvas}->set_scroll_region(0, 0, 0, 0);
    $instance->{key_buffer}->set_text("");
    foreach my $item ($instance->{graph_advanced_find_button},
                      @{$instance->{revision_sensitive_group}})
    {
        $item->set_sensitive(FALSE);
    }
    set_label_value($instance->{author_value_label}, "");
    set_label_value($instance->{date_value_label}, "");
    set_label_value($instance->{branch_value_label}, "");
    set_label_value($instance->{change_log_value_label}, "");

}
#
##############################################################################
#
#   Routine      - change_history_graph_parameters
#
#   Description  - Allows the user to change the history graphing parameters
#                  via a change history graph dialog window.
#
#   Data         - $parent_instance : The history graph window instance that
#                                     requested the new history graph
#                                     parameters.
#                  $parameters      : The parameters record containing the
#                                     current history graphing parameters that
#                                     are to be updated by the user.
#                  Return Value     : True if the user has submitted new
#                                     parameters otherwise false.
#
##############################################################################



sub change_history_graph_parameters($$)
{

    my ($parent_instance, $parameters) = @_;

    my ($from_date,
        $instance,
        $ret_val,
        $to_date);
    my $wm = WindowManager->instance();

    $instance = get_change_history_graph_window($parent_instance);

    # Update the window's internal state.

    {

        local $instance->{in_cb} = 1;

        # Load the branch liststore with branches that are either selected or
        # match the current branch filter.

        $instance->{branch_list} = [];
        $instance->{mtn}->branches($instance->{branch_list});
        load_branch_liststore($instance, $parameters->{branches});

        # Reset the date/time widgets to sensible values. If this is the first
        # time the user has changed the parameters for this history graph then
        # set the date/time widgets to the values given in the parameters,
        # otherwise simply restore the values previously saved for this history
        # graph. Doing otherwise will annoy the user as their date/time ranges
        # would keep getting reset.

        if ($parameters->{new})
        {

            # New graph so use the date/time ranges given in the parameters if
            # we have any, otherwise completely reset the date.time widgets.

            if ($parameters->{from_date} ne "")
            {
                my $to_date;
                $to_date = ($parameters->{to_date} ne "")
                    ? mtn_time_string_to_time($parameters->{to_date}) : time();
                $to_date = floor(($to_date + 59) / 60) * 60;
                reset_date_state($instance);
                set_date_range($instance,
                               $parameters->{from_date},
                               strftime(MTN_TIME_STRING, gmtime($to_date)))
                    if ($parameters->{from_date} ne "" && defined($to_date));
                $parameters->{date_state} = get_date_state($instance);
            }
            else
            {
                reset_date_state($instance);
            }

            $parameters->{new} = 0;

        }
        else
        {
            if (defined($parameters->{date_state}))
            {
                set_date_state($instance, $parameters->{date_state});
            }
            else
            {
                reset_date_state($instance);
            }
        }

        # Load the remaining settings into the GUI.

        $instance->{draw_graph_left_to_right_checkbutton}->
            set_active($parameters->{draw_left_to_right} ? TRUE : FALSE);
        $instance->{show_all_propagate_revisions_checkbutton}->
            set_active($parameters->{show_all_propagate_nodes} ? TRUE : FALSE);
        if ($parameters->{colour_by_author})
        {
            $instance->{colour_by_author_radiobutton}->set_active(TRUE);
        }
        else
        {
            $instance->{colour_by_branch_radiobutton}->set_active(TRUE);
        }

    }

    # Handle all events until the dialog is dismissed with valid values.

    $wm->make_busy($instance, 1, 1);
    while (! $instance->{done})
    {
        while (! $instance->{done})
        {
            Gtk2->main_iteration();
        }
        if ($instance->{changed})
        {
            local $instance->{in_cb} = 1;
            $instance->{done} = $instance->{changed} =
                get_date_range($instance, \$from_date, \$to_date);
        }
    }
    $wm->make_busy($instance, 0);
    local $instance->{in_cb} = 1;
    $instance->{window}->hide();

    # Deal with the result.

    if ($instance->{changed})
    {

        my ($branch_list,
            @certs_list);

        # Get the selected branches.

        $parameters->{branches} = [];
        $instance->{branches_liststore}->foreach
            (sub {
                 my ($widget, $path, $iter) = @_;
                 my ($selected, $branch) =
                     $instance->{branches_liststore}->get($iter);
                 push(@{$parameters->{branches}}, $branch) if ($selected);
                 return FALSE;
             });

        # Get any date range, making sure that the dates are either valid or an
        # empty string.

        $parameters->{from_date} = defined($from_date) ? $from_date : "";
        $parameters->{to_date} = defined($to_date) ? $to_date : "";

        # Get the settings.

        $parameters->{draw_left_to_right} =
            $instance->{draw_graph_left_to_right_checkbutton}->get_active()
            ? 1 : 0;
        $parameters->{show_all_propagate_nodes} =
            $instance->{show_all_propagate_revisions_checkbutton}->get_active()
            ? 1 : 0;
        $parameters->{colour_by_author} =
            $instance->{colour_by_author_radiobutton}->get_active() ? 1 : 0;

        # Leave the revision id parameter alone.

        $parameters->{date_state} = get_date_state($instance);
        $ret_val = 1;

    }

    $instance->{mtn} = undef;
    $instance->{branch_list} = [];
    $instance->{branches_liststore}->clear();

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - branch_filter_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the branch
#                  filter button in the change history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub branch_filter_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($case_sensitive,
        $expr,
        $search_term,
        $use_regexp);

    # Get the search parameters.

    $search_term =
        $instance->{branch_filter_comboboxentry}->child()->get_text();
    $case_sensitive = $instance->{case_sensitive_checkbutton}->get_active();
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
                 __x("`{pattern}' is an invalid\nbranch search pattern.",
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
    $instance->{branch_filter_search_term} = $search_term;
    $instance->{branch_filter_re} = $expr;

    # Store the search term in the history.

    handle_comboxentry_history($instance->{branch_filter_comboboxentry},
                               "change_history_graph_branch_filters",
                               $search_term);

    # Reload the branch liststore with branches that are either selected or
    # match the current branch filter.

    load_branch_liststore($instance);

}
#
##############################################################################
#
#   Routine      - tick_untick_branches_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on either the
#                  tick or untick branches buttons in the change history graph
#                  window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub tick_untick_branches_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my ($sort_column,
        $sort_order);
    my $set = ($instance->{tick_branches_button} == $widget) ? TRUE : FALSE;

    # If the sort column is the selected one then temporarily make it the
    # branch name column otherwise we get constant resorting and it will muck
    # up the selection.

    ($sort_column, $sort_order) =
        $instance->{branches_liststore}->get_sort_column_id();
    if (defined($sort_column) && $sort_column == BLS_SELECTED_COLUMN)
    {
        ($sort_column, $sort_order) =
            $instance->{branches_liststore}->get_sort_column_id();
        $instance->{branches_liststore}->set_sort_column_id(BLS_BRANCH_COLUMN,
                                                            "ascending");
    }
    else
    {
        $sort_column = $sort_order = undef;
    }
    foreach my $path ($instance->{branches_treeview}->get_selection()->
                      get_selected_rows())
    {
        my $iter = $instance->{branches_liststore}->get_iter($path);
        $instance->{branches_liststore}->set($iter, BLS_SELECTED_COLUMN, $set);
    }
    $instance->{branches_liststore}->set_sort_column_id($sort_column,
                                                        $sort_order)
        if (defined($sort_column));

}
#
##############################################################################
#
#   Routine      - load_branch_liststore
#
#   Description  - Load the branch liststore with branches that are either
#                  selected or match the current branch filter.
#
#   Data         - $instance          : The history graph window instance.
#                  $selected_branches : A reference to a list of currently
#                                       selected branches. This is optional.
#
##############################################################################



sub load_branch_liststore($;$)
{

    my ($instance, $selected_branches) = @_;

    my (%selected_set);

    # Generate a selected branch set either from the list we were given or from
    # the liststore.

    if (defined($selected_branches))
    {
        foreach my $branch (@$selected_branches)
        {
            $selected_set{$branch} = 1;
        }
    }
    else
    {
        $instance->{branches_liststore}->foreach
            (sub {
                 my ($widget, $path, $iter) = @_;
                 my ($selected, $branch) =
                     $instance->{branches_liststore}->get($iter);
                 $selected_set{$branch} = 1 if ($selected);
                 return FALSE;
             });
    }

    # Load the liststore with selected unsuspended branches and those that
    # match the current branch filter pattern.

    $instance->{branches_liststore}->clear();
    foreach my $branch (@{$instance->{branch_list}})
    {
        if (exists($selected_set{$branch}))
        {
            $instance->{branches_liststore}->
                set($instance->{branches_liststore}->append(),
                    BLS_SELECTED_COLUMN, TRUE,
                    BLS_BRANCH_COLUMN, $branch);
            $selected_set{$branch} = undef;
        }
        elsif ($branch =~ m/$instance->{branch_filter_re}/)
        {
            $instance->{branches_liststore}->
                set($instance->{branches_liststore}->append(),
                    BLS_SELECTED_COLUMN, FALSE,
                    BLS_BRANCH_COLUMN, $branch);
        }
    }

    # Now deal with any suspended branches that were given in
    # $selected_branches that have not been put into the liststore as selected
    # branches. This can happen when the user double clicks on a propagate
    # revision that is on a suspended branch, that suspended branch is graphed
    # and then the user left clicks on the change history graph button within
    # that new window. In this situation the most logical thing to do is to add
    # that branch to the list of selected ones, otherwise there will be no
    # branches selected and the user is stuck if he wants to change the
    # graphing parameters. The code above has reset the entries to false in the
    # %selected_set hash if the branch has been dealt with.

    foreach my $suspended_branch (sort(grep($selected_set{$_},
                                            keys(%selected_set))))
    {
        $instance->{branches_liststore}->
            set($instance->{branches_liststore}->append(),
                BLS_SELECTED_COLUMN, TRUE,
                BLS_BRANCH_COLUMN, $suspended_branch);
    }

    $instance->{branches_treeview}->scroll_to_point(0, 0)
        if ($instance->{branches_treeview}->realized());

}
#
##############################################################################
#
#   Routine      - get_change_history_graph_window
#
#   Description  - Creates or prepares an existing change history graph dialog
#                  window for use.
#
#   Data         - $parent_instance : The history graph window instance that
#                                     requested the change history graph
#                                     window.
#                  Return Value     : A reference to the newly created or
#                                     unused change history graph instance
#                                     record.
#
##############################################################################



sub get_change_history_graph_window($)
{

    my $parent_instance = $_[0];

    my $instance;
    my $change_window_type = "change_history_graph_window";
    my $wm = WindowManager->instance();

    # Create a new change history graph window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($change_window_type)))
    {

        my ($glade,
            $image,
            $renderer,
            $tv_column);

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $change_window_type,
                                     APPLICATION_NAME);

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($change_window_type);
        foreach my $widget ("branch_filter_comboboxentry",
                            "tick_branches_button",
                            "case_sensitive_checkbutton",
                            "regular_expression_checkbutton",
                            "branches_treeview",
                            "date_range_checkbutton",
                            "between_range_radiobutton",
                            "older_date_dateedit",
                            "and_label",
                            "younger_date_dateedit",
                            "during_range_radiobutton",
                            "time_spinbutton",
                            "time_units_combobox",
                            "draw_graph_left_to_right_checkbutton",
                            "show_all_propagate_revisions_checkbutton",
                            "colour_by_branch_radiobutton",
                            "colour_by_author_radiobutton")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $change_window_type);

        # Setup the change history graph callbacks.

        $instance->{window}->signal_connect
            ("delete_event",
             sub { $_[2]->{done} = 1 unless ($_[2]->{in_cb}); return TRUE; },
             $instance);
        $glade->get_widget("cancel_button")->signal_connect
            ("clicked",
             sub { $_[1]->{done} = 1 unless ($_[1]->{in_cb}); },
             $instance);
        $glade->get_widget("ok_button")->signal_connect
            ("clicked",
             sub { $_[1]->{done} = $_[1]->{changed} = 1
                       unless ($_[1]->{in_cb}); },
             $instance);

        # Setup the combobox.

        $instance->{branch_filter_comboboxentry}->
            set_model(Gtk2::ListStore->new("Glib::String"));
        $instance->{branch_filter_comboboxentry}->set_text_column(0);

        # Setup the branches list browser.

        $instance->{branches_liststore} =
            Gtk2::ListStore->new(BLS_COLUMN_TYPES);
        $instance->{branches_treeview}->
            set_model($instance->{branches_liststore});
        $instance->{branches_treeview}->get_selection()->set_mode("multiple");

        $tv_column = Gtk2::TreeViewColumn->new();
        $image = Gtk2::Image->new_from_stock("mtnb-tick", "menu");
        $image->show_all();
        $tv_column->set_widget($image);
        $tv_column->set_resizable(FALSE);
        $tv_column->set_sort_column_id(BLS_SELECTED_COLUMN);
        $renderer = Gtk2::CellRendererToggle->new();
        $renderer->set(activatable => TRUE);
        $renderer->signal_connect
            ("toggled",
             sub {
                 my ($widget, $path, $instance) = @_;
                 return if ($instance->{in_cb});
                 local $instance->{in_cb} = 1;
                 $instance->{branches_liststore}->
                     set($instance->{branches_liststore}->
                             get_iter_from_string($path),
                         BLS_SELECTED_COLUMN, ! $widget->get_active());
             },
             $instance);
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "active" => BLS_SELECTED_COLUMN);
        $instance->{branches_treeview}->append_column($tv_column);

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Branch"));
        $tv_column->set_resizable(FALSE);
        $tv_column->set_sizing("autosize");
        $tv_column->set_sort_column_id(BLS_BRANCH_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => BLS_BRANCH_COLUMN);
        $instance->{branches_treeview}->append_column($tv_column);

        $instance->{branches_treeview}->set_search_column(BLS_BRANCH_COLUMN);
        $instance->{branches_treeview}->
            set_search_equal_func(\&treeview_column_searcher);

        # Setup the date range widgets.

        setup_date_range_widgets($instance);

        # Setup the default branch filter.

        $instance->{branch_filter_search_term} = "";
        $instance->{branch_filter_re} = qr/\Q\E/;

        # Reparent the change history graph window to the specified window.

        $instance->{window}->set_transient_for($parent_instance->{window});

        # Display the window.

        $instance->{window}->show_all();
        $instance->{window}->present();

        # Register the window for management and set up the help callbacks.

        $wm->manage($instance, $change_window_type, $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => undef,
              help_ref => __("mtnb-uhg-the-change-history-graph-dialog-"
                             . "window")});

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Reset the change history graph dialog's state.

        set_window_size($instance->{window}, $change_window_type);
        $instance->{window}->set_transient_for($parent_instance->{window});
        $instance->{branches_liststore}->clear();
        $instance->{branches_liststore} =
            Gtk2::ListStore->new(BLS_COLUMN_TYPES);
        $instance->{branches_treeview}->
            set_model($instance->{branches_liststore});
        $instance->{branches_treeview}->set_search_column(BLS_BRANCH_COLUMN);
        $instance->{window}->show_all();
        $instance->{window}->present();

    }

    local $instance->{in_cb} = 1;

    $instance->{changed} = 0;
    $instance->{done} = 0;
    $instance->{stop} = 0;
    $instance->{mtn} = $parent_instance->{mtn};

    # Load in the comboboxentry history.

    handle_comboxentry_history($instance->{branch_filter_comboboxentry},
                               "change_history_graph_branch_filters");

    # Make sure that the branch pattern has the successfully executed filter
    # pattern in it (makes it consistent with the associated RE) and that it
    # has the focus instead of the cancel button.

    $instance->{branch_filter_comboboxentry}->child()->
        set_text($instance->{branch_filter_search_term});
    $instance->{branch_filter_comboboxentry}->child()->grab_focus();
    $instance->{branch_filter_comboboxentry}->child()->set_position(-1);

    return $instance;

}

1;
