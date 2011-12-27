##############################################################################
#
#   File Name    - ManageTagWeightings.pm
#
#   Description  - The manage tag weightings module for the mtn-browse
#                  application. This module contains all the routines for
#                  implementing the manage tag weightings dialog window.
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

# Constants for the columns within the tag weightings liststore widget.

use constant TLS_COLUMN_TYPES     => ("Glib::Int",
                                      "Glib::String");
use constant TLS_WEIGHTING_COLUMN => 0;
use constant TLS_PATTERN_COLUMN   => 1;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub manage_tag_weightings($$);

# Private routines.

sub add_tag_weighting_button_clicked_cb($$);
sub get_manage_tag_weightings_window($$);
sub load_tags_treeview($);
sub remove_tag_weighting_button_clicked_cb($$);
sub tag_pattern_entry_changed_cb($$);
sub tags_treeselection_changed_cb($$);
#
##############################################################################
#
#   Routine      - manage_tag_weightings
#
#   Description  - Displays the manage tag weightings dialog window and then
#                  lets the user change the tag weightings list.
#
#   Data         - $parent      : The parent window widget for the manage tag
#                                 weightings window.
#                  $weightings  : The list of tag weightings that is to be
#                                 edited.
#                  Return Value : True if the tag weightimgs list was
#                                 modified, otherwise false if no changes were
#                                 made.
#
##############################################################################



sub manage_tag_weightings($$)
{

    my ($parent, $weightings) = @_;

    my ($changed,
        $instance,
        $response);

    $instance = get_manage_tag_weightings_window($parent, $weightings);
    $response = busy_dialog_run($instance);
    $instance->{window}->hide();
    if ($response eq "ok")
    {
        $changed = 1;
        @$weightings = @{$instance->{tag_weightings}};
    }
    $instance->{tags_liststore}->clear();
    $instance->{tag_weightings} = [];

    return $changed;

}
#
##############################################################################
#
#   Routine      - tags_treeselection_changed_cb
#
#   Description  - Callback routine called when the user selects an entry in
#                  the tags treeview in the manage tag weightings window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub tags_treeselection_changed_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    # Store the details of the newly selected tag weighting if one was
    # selected, also enabling the remove button if appropriate.

    if ($widget->count_selected_rows() > 0)
    {
        my ($iter,
            $model);
        ($model, $iter) = $widget->get_selected();
        $instance->{selected_tag} =
            {weighting => $model->get($iter, TLS_WEIGHTING_COLUMN),
             pattern   => $model->get($iter, TLS_PATTERN_COLUMN)};
        $instance->{remove_tag_weighting_button}->set_sensitive(TRUE);
    }
    else
    {
        $instance->{selected_tag} = undef;
        $instance->{remove_tag_weighting_button}->set_sensitive(FALSE);
    }

}
#
##############################################################################
#
#   Routine      - tag_pattern_entry_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the tag pattern entry field in the manage tag weightings
#                  window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub tag_pattern_entry_changed_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{add_tag_weighting_button}->set_sensitive
        ((length($instance->{tag_pattern_entry}->get_text()) > 0) ?
         TRUE : FALSE);

}
#
##############################################################################
#
#   Routine      - add_tag_weighting_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the add
#                  tag weighting button in the manage tag weightings window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub add_tag_weighting_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $pattern;

    # Check entry to see if it is a valid regular expression.

    $pattern = $instance->{tag_pattern_entry}->get_text();
    eval
    {
        my $expr = qr/$pattern/;
    };
    if ($@)
    {
        my $dialog = Gtk2::MessageDialog->new
            ($instance->{window},
             ["modal"],
             "warning",
             "close",
             __x("`{pattern}' is an invalid\ntag name pattern.",
                 pattern => $pattern));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }

    # Now check for duplicate entries.

    if (grep(/^\Q$pattern\E$/,
             map($_->{pattern}, @{$instance->{tag_weightings}}))
        > 0)
    {
        my $dialog = Gtk2::MessageDialog->new
            ($instance->{window},
             ["modal"],
             "warning",
             "close",
             __x("`{pattern}' is already entered\n"
                     . "into your tag weightings list.",
                 pattern => $pattern));
        busy_dialog_run($dialog);
        $dialog->destroy();
        return;
    }

    # Ok so add it to the tag weightings list and reload the tags treeview.
    # Please note that the update method needs to be called on the spinbutton
    # so as to make sure that it's internal state is completely up to date (the
    # user might have entered a value directly into the entry field). Updates
    # are usually done when it looses the focus, however the parent window may
    # not make use of any focus stealing buttons.

    $instance->{weighting_spinbutton}->update();
    push(@{$instance->{tag_weightings}},
         {weighting => $instance->{weighting_spinbutton}->get_value_as_int(),
          pattern   => $pattern});
    @{$instance->{tag_weightings}} = sort({ $a->{pattern} cmp $b->{pattern} }
                                          @{$instance->{tag_weightings}});
    load_tags_treeview($instance);

}
#
##############################################################################
#
#   Routine      - remove_tag_weighting_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the remove
#                  tag weighting button in the manage tag weightings window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub remove_tag_weighting_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $i;

    # Simply remove the selected tag weighting entry from the list.

    if (defined($instance->{selected_tag}))
    {

        # Locate the pattern and remove it from the list.

        for ($i = 0; $i < scalar(@{$instance->{tag_weightings}}); ++ $i)
        {
            last if ($instance->{tag_weightings}->[$i]->{pattern}
                     eq $instance->{selected_tag}->{pattern});
        }
        splice(@{$instance->{tag_weightings}}, $i, 1);

        # Reload the tags treeview.

        load_tags_treeview($instance);
        $instance->{remove_tag_weighting_button}->set_sensitive(FALSE);

    }

}
#
##############################################################################
#
#   Routine      - get_manage_tag_weightings_window
#
#   Description  - Creates or prepares an existing manage tags weightings
#                  dialog window for use.
#
#   Data         - $parent      : The parent window widget for the manage tag
#                                 weightings window.
#                  $weightings  : The list of tag weightings that is to be
#                                 edited.
#                  Return Value : A reference to the newly created or unused
#                                 manage tag weightings instance record.
#
##############################################################################



sub get_manage_tag_weightings_window($$)
{

    my ($parent, $weightings) = @_;

    my ($glade,
        $instance);
    my $window_type = "manage_tag_weightings_window";
    my $wm = WindowManager->instance();

    # Create a new manage tag weightings window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

        my ($image,
            $renderer,
            $tv_column);

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
        foreach my $widget ("tags_treeview",
                            "tag_pattern_entry",
                            "weighting_spinbutton",
                            "add_tag_weighting_button",
                            "remove_tag_weighting_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        # Setup the tag weightings list.

        $instance->{tags_liststore} = Gtk2::ListStore->new(TLS_COLUMN_TYPES);
        $instance->{tags_treeview}->set_model($instance->{tags_liststore});

        $tv_column = Gtk2::TreeViewColumn->new();
        $image = Gtk2::Image->new_from_stock("mtnb-weighting", "menu");
        $image->show_all();
        $tv_column->set_widget($image);
        $tv_column->set_resizable(FALSE);
        $tv_column->set_sort_column_id(TLS_WEIGHTING_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => TLS_WEIGHTING_COLUMN);
        $instance->{tags_treeview}->append_column($tv_column);

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Tag Name Pattern"));
        $tv_column->set_resizable(FALSE);
        $tv_column->set_sizing("grow-only");
        $tv_column->set_sort_column_id(TLS_PATTERN_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => TLS_PATTERN_COLUMN);
        $instance->{tags_treeview}->append_column($tv_column);

        $instance->{tags_treeview}->set_search_column(TLS_PATTERN_COLUMN);
        $instance->{tags_treeview}->
            set_search_equal_func(\&treeview_column_searcher);

        $instance->{tags_treeview}->get_selection()->
            signal_connect("changed",
                           \&tags_treeselection_changed_cb,
                           $instance);

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Reset the manage tag weightings dialog's state.

        $instance->{tags_liststore}->clear();
        $instance->{tags_liststore} = Gtk2::ListStore->new(TLS_COLUMN_TYPES);
        $instance->{tags_treeview}->set_model($instance->{tags_liststore});
        $instance->{tags_treeview}->set_search_column(TLS_PATTERN_COLUMN);

    }

    local $instance->{in_cb} = 1;

    $instance->{selected_tag} = undef;
    $instance->{tag_weightings} = undef;

    # Disable the add and remove buttons and make sure the tag pattern entry
    # field is empty.

    $instance->{tag_pattern_entry}->set_text("");
    $instance->{add_tag_weighting_button}->set_sensitive(FALSE);
    $instance->{remove_tag_weighting_button}->set_sensitive(FALSE);

    # Reparent window and display it.

    $instance->{window}->set_transient_for($parent);
    $instance->{window}->show_all();
    $instance->{window}->present();

    # Load in the tag weightings.

    @{$instance->{tag_weightings}} = @$weightings;
    load_tags_treeview($instance);

    # Make sure that the server entry field has the focus.

    $instance->{tag_pattern_entry}->grab_focus();
    $instance->{tag_pattern_entry}->set_position(-1);

    # If necessary, register the window for management and set up the help
    # callbacks.

    if (defined($glade))
    {
        $wm->manage($instance, $window_type, $instance->{window});
        register_help_callbacks
            ($instance,
             $glade,
             {widget   => undef,
              help_ref => __("TDB-mtnb-upc-the-manage-server-bookmarks-dialog-"
                             . "window")});
    }

    return $instance;

}
#
##############################################################################
#
#   Routine      - load_tags_treeview
#
#   Description  - Load up the tags treeview with the current tag weightings.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub load_tags_treeview($)
{

    my $instance = $_[0];

    # Load up the tags treeview.

    $instance->{tags_liststore}->clear();
    foreach my $entry (@{$instance->{tag_weightings}})
    {
        $instance->{tags_liststore}->
            set($instance->{tags_liststore}->append(),
                TLS_WEIGHTING_COLUMN, $entry->{weighting},
                TLS_PATTERN_COLUMN, $entry->{pattern});
    }
    $instance->{tags_treeview}->scroll_to_point(0, 0)
        if ($instance->{tags_treeview}->realized());

}

1;
