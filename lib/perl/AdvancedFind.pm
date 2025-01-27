##############################################################################
#
#   File Name    - AdvancedFind.pm
#
#   Description  - The advanced find module for the mtn-browse application.
#                  This module contains all the routines for implementing the
#                  advanced find dialog window.
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

# Constants for the columns within the advanced find liststore widget.

use constant AFLS_COLUMN_TYPES       => ("Glib::String",
                                         "Glib::String",
                                         "Glib::String",
                                         "Glib::String");
use constant AFLS_REVISION_ID_COLUMN => 0;
use constant AFLS_BRANCH_COLUMN      => 1;
use constant AFLS_DATE_COLUMN        => 2;
use constant AFLS_AUTHOR_COLUMN      => 3;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub advanced_find($$$;$$);

# Private routines.

sub execute_button_clicked_cb($$);
sub get_advanced_find_window($);
sub populate_button_clicked_cb($$);
sub revisions_treeselection_changed_cb($$);
sub revisions_treeview_row_activated_cb($$$$);
sub simple_query_radiobutton_toggled_cb($$);
sub term_combobox_changed_cb($$);
sub update_advanced_find_state($$);
#
##############################################################################
#
#   Routine      - advanced_find
#
#   Description  - Displays the advanced find dialog window and then gets the
#                  user to select the revision they want.
#
#   Data         - $parent_instance : The browser instance that started the
#                                     advanced find.
#                  $revision_id     : A reference to a variable that is to
#                                     contain the selected revision id.
#                  $branches        : A reference to a list that is to contain
#                                     the list of branches that the selected
#                                     revision is on.
#                  $check_cb        : A reference to a call-back routine that
#                                     is called to check any selected result
#                                     before dismissing the advanced find
#                                     dialog window. This call-back routine is
#                                     passed the parent window for any dialogs
#                                     that are to be displayed, the revision
#                                     id that has been selected and what ever
#                                     was passed for $client_data. The return
#                                     value from this call-back routine is a
#                                     boolean that is true if the revision id
#                                     is ok, otherwise false if the user
#                                     should choose another revision. This
#                                     parameter is optional.
#                  $client_data     : A value that is passed to the call back
#                                     routine specified in $check_cb. This
#                                     parameter is optional.
#                  Return Value     : True if a revision has been selected,
#                                     otherwise false.
#
##############################################################################



sub advanced_find($$$;$$)
{

    my ($parent_instance, $revision_id, $branches, $check_cb, $client_data) =
        @_;

    my ($advanced_find,
        $ret_val);
    my $wm = WindowManager->instance();

    $advanced_find = get_advanced_find_window($parent_instance);

    # Update the window's internal state.

    {

        local $advanced_find->{in_cb} = 1;

        # Update it with any preset values if they exist.

        if (exists($parent_instance->{branch_combo_details}))
        {
            $advanced_find->{branch_combo_details}->{preset} = 1;
            $advanced_find->{branch_combo_details}->{complete} =
                $parent_instance->{branch_combo_details}->{complete};
            $advanced_find->{branch_combo_details}->{value} =
                $parent_instance->{branch_combo_details}->{value};

            $advanced_find->{revision_combo_details}->{preset} = 1;
            $advanced_find->{revision_combo_details}->{complete} =
                $parent_instance->{revision_combo_details}->{complete};
            $advanced_find->{revision_combo_details}->{value} =
                $parent_instance->{revision_combo_details}->{value};

            $advanced_find->{tagged_checkbutton}->
                set_active($parent_instance->{tagged_checkbutton}->
                           get_active());
        }
        else
        {
            $advanced_find->{branch_combo_details}->{preset} = 0;
            $advanced_find->{revision_combo_details}->{preset} = 0;
            $advanced_find->{tagged_checkbutton}->set_active(FALSE);
        }

        &{$advanced_find->{update_handler}}($advanced_find, NEW_FIND);

    }

    # Handle all events until the dialog is dismissed.

    $wm->make_busy($advanced_find, 1, 1);
    while (! $advanced_find->{done})
    {
        while (! $advanced_find->{done})
        {
            Gtk2->main_iteration();
        }
        if ($advanced_find->{selected} && defined($check_cb))
        {
            local $advanced_find->{in_cb} = 1;
            $advanced_find->{done} = $advanced_find->{selected} =
                &$check_cb($advanced_find->{window},
                           $advanced_find->{revisions_treeview_details}->
                               {value},
                           $client_data);
        }
    }
    $wm->make_busy($advanced_find, 0);
    local $advanced_find->{in_cb} = 1;
    hide_find_text_and_goto_line($advanced_find->{details_textview});
    $advanced_find->{window}->hide();

    # Deal with the result.

    @$branches = ();
    $$revision_id = "";
    if ($advanced_find->{selected})
    {
        my ($branch_list,
            @certs_list,
            $found);

        $$revision_id = $advanced_find->{revisions_treeview_details}->{value};

        # Build up a list of branches that the selected revision is on, putting
        # the branch named in the branch combo box at the head if it is still
        # applicable.

        $advanced_find->{mtn}->certs(\@certs_list, $$revision_id);
        $found = 0;
        foreach my $cert (@certs_list)
        {
            if ($cert->{name} eq "branch")
            {
                if ($cert->{value}
                    ne $advanced_find->{branch_combo_details}->{value})
                {
                    push(@$branches, $cert->{value});
                }
                else
                {
                    $found = 1;
                }
            }
        }
        unshift(@$branches, $advanced_find->{branch_combo_details}->{value})
            if ($found);

        $ret_val = 1;
    }

    $advanced_find->{mtn} = undef;
    $advanced_find->{branch_combo_details}->{preset} = 0;
    $advanced_find->{revision_combo_details}->{preset} = 0;
    &{$advanced_find->{update_handler}}($advanced_find, ALL_CHANGED);

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - simple_query_radiobutton_toggled_cb
#
#   Description  - Callback routine called when the user changes the advanced
#                  find mode radio button.
#
#   Data         - $widget        : The widget object that received the
#                                   signal.
#                  $advanced_find : The advanced find dialog window instance
#                                   that is associated with this widget.
#
##############################################################################



sub simple_query_radiobutton_toggled_cb($$)
{

    my ($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    # Simply enable the relevant find widgets depending upon whether simple or
    # advanced mode is selected.

    if ($advanced_find->{simple_query_radiobutton}->get_active())
    {
        $advanced_find->{simple_frame}->set_sensitive(TRUE);
        $advanced_find->{advanced_frame}->set_sensitive(FALSE);
    }
    else
    {
        $advanced_find->{simple_frame}->set_sensitive(FALSE);
        $advanced_find->{advanced_frame}->set_sensitive(TRUE);
    }

}
#
##############################################################################
#
#   Routine      - execute_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the execute
#                  query button in the advanced find window.
#
#   Data         - $widget        : The widget object that received the
#                                   signal.
#                  $advanced_find : The advanced find dialog window instance
#                                   that is associated with this widget.
#
##############################################################################



sub execute_button_clicked_cb($$)
{

    my ($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    # Simply let the update handler deal with it.

    &{$advanced_find->{update_handler}}($advanced_find, REVISION_CHANGED);

}
#
##############################################################################
#
#   Routine      - populate_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the
#                  populate selector button in the advanved find window.
#
#   Data         - $widget        : The widget object that received the
#                                   signal.
#                  $advanced_find : The advanced find dialog window instance
#                                   that is associated with this widget.
#
##############################################################################



sub populate_button_clicked_cb($$)
{

    my ($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my ($arg,
        $dont_escape,
        $pos,
        $selector,
        $time_val,
        $to_insert);

    # Simply get the currently selected selector and then insert it into the
    # user's query string.

    $selector = $advanced_find->{term_combobox}->get_model()->get
        ($advanced_find->{term_combobox}->get_active_iter(), 0);
    $arg = $advanced_find->{argument_entry}->get_text();
    $time_val = strftime(MTN_TIME_STRING,
                         gmtime($advanced_find->{date_dateedit}->get_time()));
    $to_insert = "";
    if ($selector eq __("Author"))
    {
        $to_insert = "a:" . (($arg eq "") ? __("<Author>") : $arg);
    }
    elsif ($selector eq __("Branch"))
    {
        $to_insert = "b:" . (($arg eq "") ? __("<Branch>") : $arg);
    }
    elsif ($selector eq __("Cert"))
    {
        $to_insert = "c:" . (($arg eq "") ? __("<Cert Expression>") : $arg);
    }
    elsif ($selector eq __("Date (=)"))
    {
        $to_insert = "d:" . $time_val;
    }
    elsif ($selector eq __("Date (<=)"))
    {
        $to_insert = "e:" . $time_val;
    }
    elsif ($selector eq __("Date (>)"))
    {
        $to_insert = "l:" . $time_val;
    }
    elsif ($selector eq __("Head Revision"))
    {
        $to_insert = "h:" . (($arg eq "") ? __("<Branch>") : $arg);
    }
    elsif ($selector eq __("Identifier"))
    {
        $to_insert = "i:" . (($arg eq "") ? __("<Revision Id>") : $arg);
    }
    elsif ($selector eq __("Key"))
    {
        $to_insert = "k:" . (($arg eq "") ? __("<Signing Key>") : $arg);
    }
    elsif ($selector eq __("Logical And"))
    {
        $to_insert = "/";
        $dont_escape = 1;
    }
    elsif ($selector eq __("Logical Or"))
    {
        $to_insert = "|";
        $dont_escape = 1;
    }
    elsif ($selector eq __("Message"))
    {
        $to_insert = "m:" . (($arg eq "") ? __("<Message Text>") : $arg);
    }
    elsif ($selector eq __("Parent"))
    {
        $to_insert = "p:" . (($arg eq "") ? __("<Revision Id>") : $arg);
    }
    elsif ($selector eq __("Tag"))
    {
        $to_insert = "t:" . (($arg eq "") ? __("<Tag Name>") : $arg);
    }
    elsif ($selector eq __("ancestors()"))
    {
        $to_insert =
            "ancestors(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("children()"))
    {
        $to_insert =
            "children(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("descendants()"))
    {
        $to_insert =
            "descendants(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("difference()"))
    {
        $to_insert = "difference("
            . (($arg eq "") ? __("<Selector 1>;<Selector 2>") : $arg) . ")";
    }
    elsif ($selector eq __("lca()"))
    {
        $to_insert = "lca("
            . (($arg eq "") ? __("<Selector 1>;<Selector 2>") : $arg) . ")";
    }
    elsif ($selector eq __("max()"))
    {
        $to_insert = "max(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("min()"))
    {
        $to_insert = "min(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("not()"))
    {
        $to_insert = "not(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("parents()"))
    {
        $to_insert =
            "parents(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    elsif ($selector eq __("pick()"))
    {
        $to_insert = "pick(" . (($arg eq "") ? __("<Selector>") : $arg) . ")";
    }
    $to_insert =~ s/$select_escape_re/\\$1/g unless ($dont_escape);

    $pos =
        $advanced_find->{search_term_comboboxentry}->child()->get_position();
    $advanced_find->{search_term_comboboxentry}->child()->insert_text
        ($to_insert, $pos);
    $advanced_find->{search_term_comboboxentry}->child()->set_position
        ($pos + length($to_insert));

}
#
##############################################################################
#
#   Routine      - term_combobox_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the term combobox by selecting an entry from its pulldown
#                  list in the advanced find window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub term_combobox_changed_cb($$)
{

    my ($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my ($arg,
        $date,
        $logical,
        $pos,
        $selector,
        $time_val,
        $to_insert);

    # Simply get the currently selected term and then enable/disable the text
    # entry and date entry widgets accordingly.

    $selector = $advanced_find->{term_combobox}->get_model()->get
        ($advanced_find->{term_combobox}->get_active_iter(), 0);
    $date = __("Date");
    $logical = __("Logical");
    if ($selector =~ m/^$date .*$/)
    {
        $advanced_find->{argument_entry}->set_sensitive(FALSE);
        $advanced_find->{date_dateedit}->set_sensitive(TRUE);
    }
    elsif ($selector =~ m/^$logical .*$/)
    {
        $advanced_find->{argument_entry}->set_sensitive(FALSE);
        $advanced_find->{date_dateedit}->set_sensitive(FALSE);
    }
    else
    {
        $advanced_find->{argument_entry}->set_sensitive(TRUE);
        $advanced_find->{date_dateedit}->set_sensitive(FALSE);
    }

}
#
##############################################################################
#
#   Routine      - revisions_treeselection_changed_cb
#
#   Description  - Callback routine called when the user selects an entry in
#                  the revisions treeview in the advanced find window.
#
#   Data         - $widget        : The widget object that received the
#                                   signal.
#                  $advanced_find : The advanced find dialog window instance
#                                   that is associated with this widget.
#
##############################################################################



sub revisions_treeselection_changed_cb($$)
{

    my ($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    # Get the selected revision id and update the window accordingly.

    if ($widget->count_selected_rows() > 0)
    {
        my ($iter,
            $model,
            $revision_id);
        ($model, $iter) = $widget->get_selected();
        $revision_id = $model->get($iter, AFLS_REVISION_ID_COLUMN);
        if ($revision_id
            ne $advanced_find->{revisions_treeview_details}->{value})
        {
            $advanced_find->{revisions_treeview_details}->{value} =
                $revision_id;
            &{$advanced_find->{update_handler}}($advanced_find,
                                                SELECTED_REVISION_CHANGED);
        }
    }
    else
    {
        if ($advanced_find->{revisions_treeview_details}->{value} ne "")
        {
            $advanced_find->{revisions_treeview_details}->{value} = "";
            &{$advanced_find->{update_handler}}($advanced_find,
                                                SELECTED_REVISION_CHANGED);
        }
    }

}
#
##############################################################################
#
#   Routine      - revisions_treeview_row_activated_cb
#
#   Description  - Callback routine called when the user double clicks on an
#                  entry in the revisions treeview in the advanced find
#                  window.
#
#   Data         - $widget           : The widget object that received the
#                                      signal.
#                  $tree_path        : A Gtk2::TreePath object for the
#                                      selected item.
#                  $tree_view_column : A Gtk2::TreeViewColumn object for the
#                                      selected item.
#                  $advanced_find    : The advanced find dialog window
#                                      instance that is associated with this
#                                      widget.
#
##############################################################################



sub revisions_treeview_row_activated_cb($$$$)
{

    my ($widget, $tree_path, $tree_view_column, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my $revision_id;

    # Get the selected revision id.

    $widget->get_selection()->selected_foreach
        (sub {
             my ($model, $path, $iter) = @_;
             $revision_id = $model->get($iter, AFLS_REVISION_ID_COLUMN); });

    if (defined($revision_id))
    {
        $advanced_find->{revisions_treeview_details}->{value} = $revision_id;
        $advanced_find->{selected} = 1;
        $advanced_find->{done} = 1;
    }

}
#
##############################################################################
#
#   Routine      - get_advanced_find_window
#
#   Description  - Creates or prepares an existing advanced find dialog window
#                  for use.
#
#   Data         - $parent_instance : The browser instance that started the
#                                     advanced find.
#                  Return Value     : A reference to the newly created or
#                                     unused advanced find instance record.
#
##############################################################################



sub get_advanced_find_window($)
{

    my $parent_instance = $_[0];

    my $instance;
    my $window_type = "advanced_find_window";
    my $wm = WindowManager->instance();

    # Create a new advanced find dialog window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

        my (@delete_list,
            $glade,
            $index,
            $renderer,
            $tv_column);

        $instance = {};
        $glade = Gtk2::GladeXML->new($glade_file,
                                     $window_type,
                                     APPLICATION_NAME);
        $instance->{mtn} = $parent_instance->{mtn};

        # Flag to stop recursive calling of callbacks.

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Connect Glade registered signal handlers.

        glade_signal_autoconnect($glade, $instance);

        # Link in the update handler for the advanced find window.

        $instance->{update_handler} = \&update_advanced_find_state;

        # Get the widgets that we are interested in.

        $instance->{window} = $glade->get_widget($window_type);
        foreach my $widget ("appbar",
                            "simple_query_radiobutton",
                            "simple_frame",
                            "advanced_frame",
                            "branch_entry",
                            "branch_list_togglebutton",
                            "revision_entry",
                            "revision_list_togglebutton",
                            "tagged_checkbutton",
                            "search_term_comboboxentry",
                            "stop_button",
                            "term_combobox",
                            "argument_entry",
                            "date_dateedit",
                            "revisions_hpaned",
                            "revisions_treeview",
                            "details_textview",
                            "details_scrolledwindow",
                            "ok_button")
        {
            $instance->{$widget} = $glade->get_widget($widget);
        }

        set_window_size($instance->{window}, $window_type);

        # Setup the advanced find callbacks.

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
             sub { $_[1]->{done} = $_[1]->{selected} = 1
                       unless ($_[1]->{in_cb}); },
             $instance);
        $instance->{stop_button}->signal_connect
            ("clicked", sub { $_[1]->{stop} = 1; }, $instance);

        # Setup the combobox.

        $instance->{search_term_comboboxentry}->
            set_model(Gtk2::ListStore->new("Glib::String"));
        $instance->{search_term_comboboxentry}->set_text_column(0);
        handle_comboxentry_history($instance->{search_term_comboboxentry},
                                   "advanced_find");

        # Remove any unsupported selectors from the term combobox.

        $index = 0;
        $instance->{term_combobox}->get_model()->foreach
            (sub {
                 my ($model, $path, $iter) = @_;
                 my $value = $model->get($iter, 0);
                 push(@delete_list, $index)
                     if (($value eq __("Key")
                          && ! $instance->{mtn}->supports(MTN_K_SELECTOR))
                         || ($value eq __("Logical Or")
                             && ! $instance->{mtn}->supports
                                 (MTN_SELECTOR_OR_OPERATOR))
                         || ($value eq __("Message")
                             && ! $instance->{mtn}->supports(MTN_M_SELECTOR))
                         || ($value eq __("Parent")
                             && ! $instance->{mtn}->supports(MTN_P_SELECTOR))
                         || ($value =~ m/^[a-z0-9_]+\(\)$/
                             && ! $instance->{mtn}->supports
                                 (MTN_SELECTOR_FUNCTIONS))
                         || ($value eq __("min()")
                             && ! $instance->{mtn}->supports
                                 (MTN_SELECTOR_MIN_FUNCTION))
                         || ($value eq __("not()")
                             && ! $instance->{mtn}->supports
                                 (MTN_SELECTOR_NOT_FUNCTION)));
                 ++ $index;
                 return FALSE;
             });
        foreach my $row (reverse(@delete_list))
        {
            $instance->{term_combobox}->remove_text($row);
        }
        $instance->{term_combobox}->set_active(0);

        # Setup the revisions list browser.

        $instance->{revisions_liststore} =
            Gtk2::ListStore->new(AFLS_COLUMN_TYPES);
        $instance->{revisions_treeview}->
            set_model($instance->{revisions_liststore});

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Revision Id"));
        $tv_column->set_resizable(TRUE);
        $tv_column->set_sizing("fixed");
        $tv_column->set_fixed_width(100);
        $tv_column->set_sort_column_id(AFLS_REVISION_ID_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer,
                                   "text" => AFLS_REVISION_ID_COLUMN);
        $instance->{revisions_treeview}->append_column($tv_column);

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Branch"));
        $tv_column->set_resizable(TRUE);
        $tv_column->set_sizing("grow-only");
        $tv_column->set_sort_column_id(AFLS_BRANCH_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => AFLS_BRANCH_COLUMN);
        $instance->{revisions_treeview}->append_column($tv_column);

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Date"));
        $tv_column->set_resizable(TRUE);
        $tv_column->set_sizing("grow-only");
        $tv_column->set_sort_column_id(AFLS_DATE_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => AFLS_DATE_COLUMN);
        $instance->{revisions_treeview}->append_column($tv_column);

        $tv_column = Gtk2::TreeViewColumn->new();
        $tv_column->set_title(__("Author"));
        $tv_column->set_resizable(TRUE);
        $tv_column->set_sizing("autosize");
        $tv_column->set_sort_column_id(AFLS_AUTHOR_COLUMN);
        $renderer = Gtk2::CellRendererText->new();
        $tv_column->pack_start($renderer, TRUE);
        $tv_column->set_attributes($renderer, "text" => AFLS_AUTHOR_COLUMN);
        $instance->{revisions_treeview}->append_column($tv_column);

        treeview_setup_search_column_selection
            ($instance->{revisions_treeview}, 0 .. 3);
        $instance->{revisions_treeview}->
            set_search_column(AFLS_REVISION_ID_COLUMN);
        $instance->{revisions_treeview}->
            set_search_equal_func(\&treeview_column_searcher);

        $instance->{revisions_treeview}->get_selection()->signal_connect
            ("changed", \&revisions_treeselection_changed_cb, $instance);

        # Setup the revision details viewer.

        $instance->{details_buffer} =
            $instance->{details_textview}->get_buffer();
        create_format_tags($instance->{details_textview}->get_buffer());
        $instance->{details_textview}->modify_font($mono_font);

        # Reparent the advanced find window to the specified browser.

        $instance->{window}->set_transient_for($parent_instance->{window});

        # Display the window.

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
             {widget   => "simple_frame",
              help_ref => __("mtnb-mcqc-simple-queries")},
             {widget   => "advanced_frame",
              help_ref => __("mtnb-mcqc-helper-tools")},
             {widget   => undef,
              help_ref => __("mtnb-mcqc-the-advanced-find-dialog-window")});

        # Update the advanced find dialog's state.

        $instance->{branch_combo_details}->{preset} = 0;
        $instance->{revision_combo_details}->{preset} = 0;
        &{$instance->{update_handler}}($instance, NEW_FIND);

        # Now the advanced find instance is completely initialised, set up the
        # branch and revision entry widgets for auto-completion.

        activate_auto_completion($instance->{branch_entry}, $instance);
        activate_auto_completion($instance->{revision_entry}, $instance);

    }
    else
    {

        $instance->{in_cb} = 0;
        local $instance->{in_cb} = 1;

        # Reset the advanced find dialog's state.

        $instance->{mtn} = $parent_instance->{mtn};
        set_window_size($instance->{window}, $window_type);
        $instance->{revisions_hpaned}->set_position(300);
        $instance->{window}->set_transient_for($parent_instance->{window});
        $instance->{stop_button}->set_sensitive(FALSE);
        $instance->{branch_combo_details}->{preset} = 0;
        $instance->{revision_combo_details}->{preset} = 0;
        $instance->{appbar}->set_progress_percentage(0);
        $instance->{appbar}->clear_stack();
        $instance->{revisions_liststore}->clear();
        $instance->{revisions_liststore} =
            Gtk2::ListStore->new(AFLS_COLUMN_TYPES);
        $instance->{revisions_treeview}->
            set_model($instance->{revisions_liststore});
        $instance->{revisions_treeview}->
            set_search_column(AFLS_REVISION_ID_COLUMN);
        &{$instance->{update_handler}}($instance, NEW_FIND);
        $instance->{window}->show_all();
        $instance->{window}->present();

    }

    local $instance->{in_cb} = 1;

    $instance->{done} = 0;
    $instance->{selected} = 0;
    $instance->{stop} = 0;

    # Make sure that the branch entry has the focus and not the cancel button.

    $instance->{branch_entry}->grab_focus();
    $instance->{branch_entry}->set_position(-1);

    return $instance;

}
#
##############################################################################
#
#   Routine      - update_advanced_find_state
#
#   Description  - Update the display of the specified advanced find dialog
#                  window instance according to the specified state.
#
#   Data         - $advanced_find : The advanced find dialog window instance
#                                   that is to have its state updated.
#                  $changed       : What the user has changed.
#
##############################################################################



sub update_advanced_find_state($$)
{

    my ($advanced_find, $changed) = @_;

    my $matches;
    my $wm = WindowManager->instance();

    $wm->make_busy($advanced_find, 1);
    $advanced_find->{appbar}->
        push($advanced_find->{appbar}->get_status()->get_text());
    $wm->update_gui();

    # The list of available branches has changed.

    if ($changed & BRANCH)
    {

        my @branch_list;

        # Reset the query mode.

        $advanced_find->{simple_query_radiobutton}->set_active(TRUE);
        $advanced_find->{simple_frame}->set_sensitive(TRUE);
        $advanced_find->{advanced_frame}->set_sensitive(FALSE);

        # Reset the branch selection.

        $advanced_find->{branch_combo_details}->{completion} = undef;
        if ($advanced_find->{branch_combo_details}->{preset})
        {
            $advanced_find->{branch_combo_details}->{preset} = 0;
        }
        else
        {
            $advanced_find->{branch_combo_details}->{complete} = 0;
            $advanced_find->{branch_combo_details}->{value} = "";
        }

        # Get the new list of branches.

        $advanced_find->{appbar}->set_status(__("Fetching branch list"));
        $wm->update_gui();
        {
            local $pulse_widget = $advanced_find->{appbar}->get_progress();
            $advanced_find->{mtn}->branches(\@branch_list)
                if (defined($advanced_find->{mtn}));
            $advanced_find->{branch_combo_details}->{list} = \@branch_list;
        }

        # Update the branch entry.

        $advanced_find->{branch_combo_details}->{filter} =
            $advanced_find->{branch_combo_details}->{value};
        $advanced_find->{branch_combo_details}->{update} = 1;
        $advanced_find->{branch_entry}->
            set_text($advanced_find->{branch_combo_details}->{value});
        $advanced_find->{branch_entry}->set_position(-1);
        $advanced_find->{appbar}->set_progress_percentage(0);
        $advanced_find->{appbar}->set_status("");
        $wm->update_gui();

    }

    # The list of available revisions has changed.

    if ($changed & REVISION)
    {

        my @revision_list;

        # Reset the revision selection.

        $advanced_find->{revision_combo_details}->{completion} = undef;
        if ($advanced_find->{revision_combo_details}->{preset})
        {
            $advanced_find->{revision_combo_details}->{preset} = 0;
        }
        else
        {
            $advanced_find->{revision_combo_details}->{complete} = 0;
            $advanced_find->{revision_combo_details}->{value} = "";
        }

        # Get the new list of revisions.

        if ($advanced_find->{branch_combo_details}->{complete})
        {
            $advanced_find->{appbar}->set_status(__("Fetching revision list"));
            $wm->update_gui();
            get_branch_revisions($advanced_find->{mtn},
                                 $advanced_find->{branch_combo_details}->
                                     {value},
                                 $advanced_find->{tagged_checkbutton}->
                                     get_active(),
                                 $advanced_find->{appbar},
                                 \@revision_list);
        }
        $advanced_find->{revision_combo_details}->{list} = \@revision_list;

        # Update the revision entry.

        $advanced_find->{revision_combo_details}->{filter} =
            $advanced_find->{revision_combo_details}->{value};
        $advanced_find->{revision_combo_details}->{update} = 1;
        $advanced_find->{revision_entry}->
            set_text($advanced_find->{revision_combo_details}->{value});
        $advanced_find->{appbar}->set_progress_percentage(0);
        $advanced_find->{appbar}->set_status("");
        $wm->update_gui();

    }

    # The list of displayed revisions has changed.

    if ($changed & REVISION_LIST)
    {

        my ($i,
            @revision_ids);

        # Reset the revisions tree view.

        $advanced_find->{revisions_liststore}->clear();
        $advanced_find->{revisions_treeview_details}->{value} = "";

        # Get the list of matching revisions.

        $advanced_find->{appbar}->set_status(__("Finding revisions"));
        $wm->update_gui();
        if ($advanced_find->{simple_query_radiobutton}->get_active())
        {
            if ($advanced_find->{revision_combo_details}->{complete})
            {
                get_revision_ids($advanced_find, \@revision_ids);
                $matches = scalar(@revision_ids);
            }
        }
        else
        {

            my ($exception,
                $query);

            $query = $advanced_find->{search_term_comboboxentry}->child()->
                get_text();

            # Remember the user can type in any old rubbish with advanced
            # queries! So protect ourselves.

            CachingAutomateStdio->register_error_handler
                (MTN_SEVERITY_ALL,
                 sub {
                     my ($severity, $message) = @_;
                     my $dialog;
                     $dialog = Gtk2::MessageDialog->new_with_markup
                         ($advanced_find->{window},
                          ["modal"],
                          "warning",
                          "close",
                          __x("There is a problem with your query, Monotone "
                                  . "gave:\n<b><i>{error_message}</i></b>",
                              error_message =>
                                  Glib::Markup::escape_text($message)));
                     busy_dialog_run($dialog);
                     $dialog->destroy();
                     die("Bad query");
                 });
            eval
            {
                local $pulse_widget = $advanced_find->{appbar}->get_progress();
                $advanced_find->{mtn}->select(\@revision_ids, $query);
                $matches = scalar(@revision_ids);
            };
            $exception = $@;
            CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
                                                         \&mtn_error_handler);

            # If the query was valid then store it in the history.

            if (! $exception)
            {
                if (scalar(@revision_ids) == 0)
                {
                    my $dialog;
                    $dialog = Gtk2::MessageDialog->new
                        ($advanced_find->{window},
                         ["modal"],
                         "info",
                         "close",
                         __("No revisions matched your query."));
                    $advanced_find->{appbar}->set_progress_percentage(0);
                    busy_dialog_run($dialog);
                    $dialog->destroy();
                }
                handle_comboxentry_history
                    ($advanced_find->{search_term_comboboxentry},
                     "advanced_find",
                     $query);
            }

        }
        $advanced_find->{mtn}->toposort(\@revision_ids, @revision_ids)
            if (scalar(@revision_ids) > 0);
        @revision_ids = reverse(@revision_ids);

        # Update the revisions tree view.

        $advanced_find->{appbar}->
            set_status(__("Populating revision details"));
        $advanced_find->{stop_button}->set_sensitive(TRUE);
        $i = 0;
        while ($i < scalar(@revision_ids) && ! $advanced_find->{stop})
        {

            my ($branch,
                %branch_hits,
                $nr_revisions,
                @revision_group);

            # Build up a list of revision ids that share the same branch.

            for ($nr_revisions = 1;
                 $i < scalar(@revision_ids);
                 ++ $i, ++ $nr_revisions)
            {

                my ($author,
                    @certs,
                    $date,
                    $found,
                    $key,
                    %unique,
                    $value);

                $advanced_find->{mtn}->certs(\@certs, $revision_ids[$i]);

                # Total up unique occurrences of branch names for this
                # revision and stash away any author and date information that
                # we may find.

                $found = 0;
                $author = $date = "";
                foreach my $cert (@certs)
                {
                    if ($cert->{name} eq "author")
                    {
                        $author = $cert->{value};
                    }
                    elsif ($cert->{name} eq "branch")
                    {
                        if (! $unique{$cert->{value}} ++)
                        {
                            ++ $branch_hits{$cert->{value}};
                            $found = 1;
                        }
                    }
                    elsif ($cert->{name} eq "date")
                    {
                        $date = mtn_time_string_to_locale_time_string
                            ($cert->{value});
                    }
                }
                ++ $branch_hits{""} if (! $found);

                # Exit this loop if a new sequence of revisions with a
                # different common branch has started.

                $found = 0;
                while (($key, $value) = each(%branch_hits))
                {
                    if ($value == $nr_revisions)
                    {
                        $branch = $key;
                        $found = 1;
                        last;
                    }
                }
                last if (! $found);

                # Ok so add this revision onto the end of the group.

                push(@revision_group,
                     {revision_id => $revision_ids[$i],
                      author      => $author,
                      date        => $date});

                if ((($i + 1) % 100) == 0)
                {
                    $advanced_find->{appbar}->set_progress_percentage
                        (($i + 1) / scalar(@revision_ids));
                    $wm->update_gui();
                }
                if ($advanced_find->{stop})
                {
                    $matches = $i + 1;
                    last;
                }

            }

            # Add the current revision group to the revisions tree view.

            foreach my $item (@revision_group)
            {
                $advanced_find->{revisions_liststore}->
                    set($advanced_find->{revisions_liststore}->append(),
                        AFLS_REVISION_ID_COLUMN, $item->{revision_id},
                        AFLS_BRANCH_COLUMN, $branch,
                        AFLS_DATE_COLUMN, $item->{date},
                        AFLS_AUTHOR_COLUMN, $item->{author});
            }

        }

        $advanced_find->{appbar}->set_progress_percentage(1);
        $wm->update_gui();
        $advanced_find->{stop_button}->set_sensitive(FALSE);
        $advanced_find->{stop} = 0;
        $advanced_find->{revisions_treeview}->scroll_to_point(0, 0)
            if ($advanced_find->{revisions_treeview}->realized());
        $advanced_find->{appbar}->set_progress_percentage(0);
        $advanced_find->{appbar}->set_status("");
        $wm->update_gui();

    }

    # The selected revision has changed.

    if ($changed & REVISION_DETAILS)
    {

        if ($advanced_find->{revisions_treeview_details}->{value} ne "")
        {
            my (@certs_list,
                @revision_details);

            $advanced_find->{details_buffer}->set_text("");
            $advanced_find->{mtn}->certs
                (\@certs_list,
                 $advanced_find->{revisions_treeview_details}->{value});
            $advanced_find->{mtn}->get_revision
                (\@revision_details,
                 $advanced_find->{revisions_treeview_details}->{value});
            generate_revision_report
                ($advanced_find->{details_buffer},
                 $advanced_find->{revisions_treeview_details}->{value},
                 \@certs_list,
                 "",
                 \@revision_details);

            # Scroll back up to the top left.

            $advanced_find->{details_buffer}->
                place_cursor($advanced_find->{details_buffer}->
                             get_start_iter());
            if ($advanced_find->{details_scrolledwindow}->realized())
            {
                $advanced_find->{details_scrolledwindow}->
                    get_vadjustment()->set_value(0);
                $advanced_find->{details_scrolledwindow}->
                    get_hadjustment()->set_value(0);
            }

            $advanced_find->{ok_button}->set_sensitive(TRUE);
            reset_find_text($advanced_find->{details_textview});
            enable_find_text_and_goto_line($advanced_find->{details_textview},
                                           1);
        }
        else
        {
            $advanced_find->{ok_button}->set_sensitive(FALSE);
            $advanced_find->{details_buffer}->set_text("");
            enable_find_text_and_goto_line($advanced_find->{details_textview},
                                           0);
        }

    }

    $advanced_find->{appbar}->pop();
    if (defined($matches))
    {
        if ($matches > 0)
        {
            $advanced_find->{appbar}->
                set_status(__nx("Found 1 revision",
                                "Found {revisions_found} revisions",
                                $matches,
                                revisions_found => $matches));
        }
        else
        {
            $advanced_find->{appbar}->set_status(__("Nothing found"));
        }
    }
    $wm->make_busy($advanced_find, 0);

}

1;
