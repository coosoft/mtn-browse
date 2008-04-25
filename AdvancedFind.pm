##############################################################################
#
#   File Name    - AdvancedFind.pm
#
#   Description  - The advanced find module for the mtn-browse application.
#                  This module contains all the routines for implementing the
#                  advanced find dialog.
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

# ***** GLOBAL DATA DECLARATIONS *****

# The type of window that is going to be managed by this module.

my $window_type = "advanced_find_window";

# ***** FUNCTIONAL PROTOTYPES FOR THIS FILE *****

# Public routines.

sub advanced_find($$$);

# Private routines.

sub create_advanced_find_window();
sub execute_button_clicked_cb($$);
sub populate_button_clicked_cb($$);
sub revisions_treeview_cursor_changed_cb($$);
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
#   Data         - $browser     : The browser instance that started the
#                                 advanced find.
#                  $revision_id : A reference to a variable that is to contain
#                                 the selected revision id.
#                  $branches    : A reference to a list that is to contain the
#                                 list of branches that the selected revision
#                                 is on.
#                  Return Value : True if a revision has been selected,
#                                 otherwise false.
#
##############################################################################



sub advanced_find($$$)
{

    my($browser, $revision_id, $branches) = @_;

    my($advanced_find,
       $new,
       $ret_val);
    my $wm = WindowManager->instance();

    # Create a new advanced find window if an unused one wasn't found,
    # otherwise reuse the existing unused one.

    if (defined($advanced_find = $wm->find_unused($window_type)))
    {
	$new = 0;
    }
    else
    {
	$advanced_find = create_advanced_find_window();
	$new = 1;
    }

    $advanced_find->{mtn} = $browser->{mtn};

    # Update the window's internal state.

    {

	my($height,
	   $width);

	local $advanced_find->{in_cb} = 1;

	$advanced_find->{selected} = 0;

	# Reset the window contents, then show it.

	$advanced_find->{window}->set_transient_for($browser->{window});
	$advanced_find->{branch_combo_details}->{preset} = 0;
	$advanced_find->{revision_combo_details}->{preset} = 0;
	$advanced_find->{appbar}->set_progress_percentage(0);
	$advanced_find->{appbar}->clear_stack();
	&{$advanced_find->{update_handler}}($advanced_find, NEW_FIND);
	($width, $height) = $advanced_find->{window}->get_default_size();
	$advanced_find->{window}->resize($width, $height);
	$advanced_find->{window}->show_all();

	# If necessary, register the window for management and setup the list
	# of additional windows that can be made busy for this application
	# window.

	if ($new)
	{
	    $wm->manage($advanced_find, $window_type);
	    $wm->add_busy_widgets($advanced_find,
				  $advanced_find->{details_textview}->
				      get_window("text"));
	}

	# Now actually update it with any preset values.

	$advanced_find->{branch_combo_details}->{preset} = 1;
	$advanced_find->{branch_combo_details}->{complete} =
	    $browser->{branch_combo_details}->{complete};
	$advanced_find->{branch_combo_details}->{value} =
	    $browser->{branch_combo_details}->{value};

	$advanced_find->{revision_combo_details}->{preset} = 1;
	$advanced_find->{revision_combo_details}->{complete} =
	    $browser->{revision_combo_details}->{complete};
	$advanced_find->{revision_combo_details}->{value} =
	    $browser->{revision_combo_details}->{value};

	$advanced_find->{tagged_tick}->
	    set_active($browser->{tagged_tick}->get_active());

	&{$advanced_find->{update_handler}}($advanced_find, NEW_FIND);

    }

    # Handle all events until the dialog is dismissed.

    $advanced_find->{done} = 0;
    while (! $advanced_find->{done})
    {
	Gtk2->main_iteration();
    }
    $advanced_find->{window}->hide();

    # Deal with the result.

    @$branches = ();
    $$revision_id = "";
    if ($advanced_find->{selected})
    {
	my($branch_list,
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
	push(@$branches, "") if (scalar(@$branches) == 0);

	$ret_val = 1;
    }

    $advanced_find->{mtn} = undef;

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - create_advanced_find_window
#
#   Description  - Creates the advanced find dialog window.
#
#   Data         - Return Value : A reference to the newly created advanced
#                                 find instance record.
#
##############################################################################



sub create_advanced_find_window()
{

    my(@branch_list,
       $instance,
       $renderer,
       $tv_column);

    $instance = {};
    $instance->{glade} = Gtk2::GladeXML->new($glade_file, $window_type);

    # Flag to stop recursive calling of callbacks.

    $instance->{in_cb} = 0;

    # Connect Glade registered signal handlers.

    glade_signal_autoconnect($instance->{glade}, $instance);

    # Link in the update handler for the advanced find window.

    $instance->{update_handler} = \&update_advanced_find_state;

    # Get the widgets that we are interested in.

    $instance->{window} = $instance->{glade}->get_widget($window_type);
    $instance->{appbar} = $instance->{glade}->get_widget("appbar");
    $instance->{simple_query_radiobutton} =
	$instance->{glade}->get_widget("simple_query_radiobutton");
    $instance->{simple_frame} = $instance->{glade}->get_widget("simple_frame");
    $instance->{advanced_frame} =
	$instance->{glade}->get_widget("advanced_frame");
    $instance->{branch_combo} =
	$instance->{glade}->get_widget("branch_comboboxentry");
    $instance->{revision_combo} =
	$instance->{glade}->get_widget("revision_comboboxentry");
    $instance->{tagged_tick} =
	$instance->{glade}->get_widget("tagged_checkbutton");
    $instance->{search_term_combo} =
	$instance->{glade}->get_widget("search_term_comboboxentry");
    $instance->{term_combo} = $instance->{glade}->get_widget("term_combobox");
    $instance->{argument_entry} =
	$instance->{glade}->get_widget("argument_entry");
    $instance->{date_dateedit} =
	$instance->{glade}->get_widget("date_dateedit");
    $instance->{revisions_treeview} =
	$instance->{glade}->get_widget("revisions_treeview");
    $instance->{details_textview} =
	$instance->{glade}->get_widget("details_textview");
    $instance->{details_scrolledwindow} =
	$instance->{glade}->get_widget("details_scrolledwindow");
    $instance->{selected_branch_label} =
	$instance->{glade}->get_widget("selected_branch_value_label");
    $instance->{selected_revision_label} =
	$instance->{glade}->get_widget("selected_revision_value_label");
    $instance->{ok_button} = $instance->{glade}->get_widget("ok_button");

    # Setup the advanced find window deletion handlers.

    $instance->{window}->signal_connect
	("delete_event",
	 sub { $_[2]->{done} = 1 unless ($_[2]->{in_cb}); return TRUE; },
         $instance);
    $instance->{glade}->get_widget("cancel_button")->signal_connect
	("clicked", sub { $_[1]->{done} = 1; }, $instance);
    $instance->{glade}->get_widget("ok_button")->signal_connect
	("clicked", sub { $_[1]->{done} = $_[1]->{selected} = 1; }, $instance);

    # Setup the comboboxentry key release signal handlers.

    $instance->{branch_combo}->child()->
	signal_connect("key_release_event",
		       \&combo_key_release_event_cb,
		       $instance);
    $instance->{revision_combo}->child()->
	signal_connect("key_release_event",
		       \&combo_key_release_event_cb,
		       $instance);

    # Setup the comboboxes.

    $instance->{branch_combo}->set_model(Gtk2::ListStore->new("Glib::String"));
    $instance->{branch_combo}->set_text_column(0);
    $instance->{branch_combo}->set_wrap_width(2);
    $instance->{revision_combo}->
	set_model(Gtk2::ListStore->new("Glib::String"));
    $instance->{revision_combo}->set_text_column(0);
    $instance->{revision_combo}->set_wrap_width(2);
    $instance->{search_term_combo}->
	set_model(Gtk2::ListStore->new("Glib::String"));
    $instance->{search_term_combo}->set_text_column(0);
    $instance->{query_history} = [];
    $instance->{term_combo}->set_active(0);

    # Setup the revisions list browser.

    $instance->{revisions_liststore} = Gtk2::ListStore->new("Glib::String");
    $instance->{revisions_treeview}->
	set_model($instance->{revisions_liststore});
    $tv_column = Gtk2::TreeViewColumn->new();
    $tv_column->set_title("Matching Revision Ids");
    $tv_column->set_sort_column_id(0);
    $renderer = Gtk2::CellRendererText->new();
    $tv_column->pack_start($renderer, FALSE);
    $tv_column->set_attributes($renderer, "text" => 0);
    $instance->{revisions_treeview}->append_column($tv_column);
    $instance->{revisions_treeview}->set_search_column(0);

    # Setup the revision details viewer.

    $instance->{details_buffer} = $instance->{details_textview}->get_buffer();
    create_format_tags($instance->{details_textview}->get_buffer());
    $instance->{details_textview}->modify_font($mono_font);

    return $instance;

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

    my($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my($len,
       $value);

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

    my($widget, $advanced_find) = @_;

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

    my($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my($arg,
       $pos,
       $selector,
       $time_val,
       $to_insert);

    # Simply get the currently selected selector and then insert it into the
    # user's query string.

    $selector = $advanced_find->{term_combo}->get_model()->get
	($advanced_find->{term_combo}->get_active_iter(), 0);
    $arg = $advanced_find->{argument_entry}->get_text();
    $time_val = strftime("%Y-%m-%dT%H:%M:%S",
			 localtime($advanced_find->{date_dateedit}->
				   get_time()));
    $to_insert = "";
    if ($selector eq "Author")
    {
	$to_insert = "a:" . (($arg eq "") ? "<Author>" : $arg);
    }
    elsif ($selector eq "Branch")
    {
	$to_insert = "b:" . (($arg eq "") ? "<Branch>" : $arg);
    }
    elsif ($selector eq "Cert")
    {
	$to_insert = "c:" . (($arg eq "") ? "<Cert Expression>" : $arg);
    }
    elsif ($selector eq "Date (=)")
    {
	$to_insert = "d:" . $time_val;
    }
    elsif ($selector eq "Date (<=)")
    {
	$to_insert = "e:" . $time_val;
    }
    elsif ($selector eq "Date (>)")
    {
	$to_insert = "l:" . $time_val;
    }
    elsif ($selector eq "Head Revision")
    {
	$to_insert = "h:";
    }
    elsif ($selector eq "Identifier")
    {
	$to_insert = "i:" . (($arg eq "") ? "<Revision Id>" : $arg);
    }
    elsif ($selector eq "Parent")
    {
	$to_insert = "p:" . (($arg eq "") ? "<Revision Id>" : $arg);
    }
    elsif ($selector eq "Separator")
    {
	$to_insert = "/";
    }
    elsif ($selector eq "Tag")
    {
	$to_insert = "t:" . (($arg eq "") ? "<Tag Name>" : $arg);
    }

    $pos = $advanced_find->{search_term_combo}->child()->get_position();
    $advanced_find->{search_term_combo}->child()->insert_text
	($to_insert, $pos);
    $advanced_find->{search_term_combo}->child()->set_position
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

    my($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my($arg,
       $pos,
       $selector,
       $time_val,
       $to_insert);

    # Simply get the currently selected term and then enable/disable the text
    # entry and date entry widgets accordingly.

    $selector = $advanced_find->{term_combo}->get_model()->get
	($advanced_find->{term_combo}->get_active_iter(), 0);

    if ($selector =~ m/^Date .*$/o)
    {
	$advanced_find->{argument_entry}->set_sensitive(FALSE);
	$advanced_find->{date_dateedit}->set_sensitive(TRUE);
    }
    elsif ($selector eq "Head" || $selector eq "Separator")
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
#   Routine      - revisions_treeview_cursor_changed_cb
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



sub revisions_treeview_cursor_changed_cb($$)
{

    my($widget, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my $revision_id;

    # Get the selected revision id.

    $widget->get_selection()->selected_foreach
	(sub {
	     my($model, $path, $iter) = @_;
	     $revision_id = $model->get($iter, 0); });

    if (defined($revision_id)
	&& $revision_id
	    ne $advanced_find->{revisions_treeview_details}->{value})
    {
	$advanced_find->{revisions_treeview_details}->{value} = $revision_id;
	$advanced_find->{appbar}->clear_stack();
	&{$advanced_find->{update_handler}}($advanced_find,
					    SELECTED_REVISION_CHANGED);
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

    my($widget, $tree_path, $tree_view_column, $advanced_find) = @_;

    return if ($advanced_find->{in_cb});
    local $advanced_find->{in_cb} = 1;

    my $revision_id;

    # Get the selected revision id.

    $widget->get_selection()->selected_foreach
	(sub {
	     my($model, $path, $iter) = @_;
	     $revision_id = $model->get($iter, 0); });

    if (defined($revision_id))
    {
	$advanced_find->{revisions_treeview_details}->{value} = $revision_id;
	$advanced_find->{appbar}->clear_stack();
	$advanced_find->{selected} = 1;
	$advanced_find->{done} = 1;
    }

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

    my($advanced_find, $changed) = @_;

    my $made_busy = 0;
    my $wm = WindowManager->instance();

    if ($advanced_find->{window}->realized())
    {
	$made_busy = 1;
	$wm->make_busy($advanced_find, 1);
    }
    $advanced_find->{appbar}->push("");
    gtk2_update();

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

	$advanced_find->{appbar}->set_status("Fetching branch list");
	gtk2_update();
	$advanced_find->{mtn}->branches(\@branch_list)
	    if (defined($advanced_find->{mtn}));
	$advanced_find->{branch_combo_details}->{list} = \@branch_list;

	# Update the branch list combobox.

	$advanced_find->{appbar}->set_status("Populating branch list");
	gtk2_update();
	my $counter = 1;
	$advanced_find->{branch_combo}->get_model()->clear();
	foreach my $branch (@branch_list)
	{
	    $advanced_find->{branch_combo}->append_text($branch);
	    $advanced_find->{appbar}->set_progress_percentage
		($counter ++ / scalar(@branch_list));
	    gtk2_update();
	}
	$advanced_find->{branch_combo}->child()->
	    set_text($advanced_find->{branch_combo_details}->{value});
	$advanced_find->{appbar}->set_progress_percentage(0);
	$advanced_find->{appbar}->set_status("");
	gtk2_update();

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
	    $advanced_find->{appbar}->set_status("Fetching revision list");
	    gtk2_update();

	    # Get either a list of tags or revision ids depending upon what the
	    # user has chosen.

	    if ($advanced_find->{tagged_tick}->get_active())
	    {
		my(%dup_list,
		   @list);
		$advanced_find->{mtn}->
		    tags(\@list,
			 $advanced_find->{branch_combo_details}->{value});
		$advanced_find->{appbar}->set_progress_percentage(0.5);
		gtk2_update();
		foreach my $item (@list)
		{
		    if (! exists($dup_list{$item->{tag}}))
		    {
			push(@revision_list, $item->{tag});
			$dup_list{$item->{tag}} = 1;
		    }
		}
	    }
	    else
	    {
		$advanced_find->{mtn}->
		    select(\@revision_list,
			   "b:" . $advanced_find->{branch_combo_details}->
			       {value});
		$advanced_find->{appbar}->set_progress_percentage(0.33);
		gtk2_update();
		$advanced_find->{mtn}->toposort(\@revision_list,
						@revision_list);
		$advanced_find->{appbar}->set_progress_percentage(0.66);
		gtk2_update();
		splice(@revision_list, 0, scalar(@revision_list) - 100);
		@revision_list = reverse(@revision_list);
	    }
	    $advanced_find->{appbar}->set_progress_percentage(1);
	    gtk2_update();
	}
	$advanced_find->{revision_combo_details}->{list} = \@revision_list;

	# Update the revision list combobox.

	$advanced_find->{appbar}->set_progress_percentage(0);
	$advanced_find->{appbar}->set_status("Populating revision list");
	gtk2_update();
	my $counter = 1;
	$advanced_find->{revision_combo}->get_model()->clear();
	foreach my $revision (@revision_list)
	{
	    $advanced_find->{revision_combo}->append_text($revision);
	    $advanced_find->{appbar}->set_progress_percentage
		($counter ++ / scalar(@revision_list));
	    gtk2_update();
	}
	$advanced_find->{revision_combo}->child()->
	    set_text($advanced_find->{revision_combo_details}->{value});
	$advanced_find->{appbar}->set_progress_percentage(0);
	$advanced_find->{appbar}->set_status("");
	gtk2_update();

    }

    # The list of displayed revisions has changed.

    if ($changed & REVISION_LIST)
    {

	my($counter,
	   @revision_ids);

	# Reset the revisions tree view.

	$advanced_find->{revisions_liststore}->clear();
	$advanced_find->{revisions_treeview_details}->{value} = "";

	# Get the list of matching revisions.

	$advanced_find->{appbar}->set_status("Finding revisions");
	gtk2_update();
	if ($advanced_find->{simple_query_radiobutton}->get_active())
	{
	    if ($advanced_find->{revision_combo_details}->{complete})
	    {
		get_revision_ids($advanced_find, \@revision_ids);
	    }
	}
	else
	{
	    my($err,
	       $query);
	    $query = $advanced_find->{search_term_combo}->child()->get_text();

	    # Remember the user can type in any old rubbish with advanced
	    # queries! So protect ourselves.

	    Monotone::AutomateStdio->register_error_handler
		("both",
		 sub {
		     my($severity, $message) = @_;
		     my $dialog;
		     $dialog = Gtk2::MessageDialog->new_with_markup
			 ($advanced_find->{window},
			  ["modal"],
			  "warning",
			  "close",
			  sprintf("Problem with your query, Monotone "
				      . "gave:\n<b><i>%s</i></b>",
				  Glib::Markup::escape_text($message)));
		     $dialog->run();
		     $dialog->destroy();
		     die("Bad query"); });
	    eval
	    {
		$advanced_find->{mtn}->select(\@revision_ids, $query);
	    };
	    $err = $@;
	    Monotone::AutomateStdio->register_error_handler
		("both", \&mtn_error_handler);

	    # If the query was valid the store it in the history.

	    if ($err eq "")
	    {
		my $found;
		if (scalar(@revision_ids) == 0)
		{
		    my $dialog;
		    $dialog = Gtk2::MessageDialog->new
			($advanced_find->{window},
			 ["modal"],
			 "info",
			 "close",
			 "No revisions matched your query");
		     $dialog->run();
		     $dialog->destroy();
		}
		$found = 0;
		foreach my $entry (@{$advanced_find->{query_history}})
		{
		    if ($entry eq $query)
		    {
			$found = 1;
			last;
		    }
		}
		if (! $found)
		{
		    if (unshift(@{$advanced_find->{query_history}}, $query)
			> 20)
		    {
			pop(@{$advanced_find->{query_history}});
		    }
		    $advanced_find->{search_term_combo}->get_model()->clear();
		    foreach my $entry (@{$advanced_find->{query_history}})
		    {
			$advanced_find->{search_term_combo}->
			    append_text($entry);
		    }
		}
	    }

	}
	$advanced_find->{mtn}->toposort(\@revision_ids, @revision_ids);
	@revision_ids = reverse(@revision_ids);

	# Update the revisions tree view.

	$advanced_find->{appbar}->set_status("Populating revision details");
	$counter = 1;
	foreach my $item (@revision_ids)
	{
	    $advanced_find->{revisions_liststore}->
		set($advanced_find->{revisions_liststore}->append(),
		    0, $item);
	    $advanced_find->{appbar}->set_progress_percentage
		($counter ++ / scalar(@revision_ids));
	    gtk2_update();
	}
	$advanced_find->{revisions_treeview}->scroll_to_point(0, 0)
	    if ($advanced_find->{revisions_treeview}->realized());

	$advanced_find->{appbar}->set_progress_percentage(0);
	$advanced_find->{appbar}->set_status("");
	gtk2_update();

    }

    # The selected revision has changed.

    if ($changed & REVISION_DETAILS)
    {

	if ($advanced_find->{revisions_treeview_details}->{value} ne "")
	{
	    if ($advanced_find->{selected_revision_label}->get_text()
		ne $advanced_find->{revisions_treeview_details}->{value})
	    {
		my($branch,
		   @certs_list,
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

		# Update the selected branch and revision labels.

		$branch = "";
		foreach my $cert (@certs_list)
		{
		    if ($cert->{name} eq "branch")
		    {
			$branch = $cert->{value};
			last;
		    }
		}
		set_label_value($advanced_find->{selected_branch_label},
				$branch);
		set_label_value($advanced_find->{selected_revision_label},
				$advanced_find->{revisions_treeview_details}->
				    {value});

		$advanced_find->{ok_button}->set_sensitive(TRUE);
	    }
	}
	else
	{
	    $advanced_find->{ok_button}->set_sensitive(FALSE);
	    $advanced_find->{details_buffer}->set_text("");
	    set_label_value($advanced_find->{selected_branch_label}, "");
	    set_label_value($advanced_find->{selected_revision_label}, "");
	}

    }

    $advanced_find->{appbar}->pop();
    $wm->make_busy($advanced_find, 0) if ($made_busy);

}

1;
