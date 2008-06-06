##############################################################################
#
#   File Name    - Preferences.pm
#
#   Description  - The preferences module for the mtn-browse application. This
#                  module contains all the routines for implementing the
#                  preferences dialog.
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

# Constants for the columns within the mime types liststore widget.

use constant MTLS_NAME_COLUMN     => 0;
use constant MTLS_PATTERNS_COLUMN => 1;
use constant MTLS_HELPER_COLUMN   => 2;
use constant MTLS_ENTRY_COLUMN    => 3;

# Constant for the name of the user's preferences file.

use constant PREFERENCES_FILE_NAME => ".mtn-browserc";

# Constant for the preferences file's format version.

use constant PREFERENCES_FORMAT_VERSION => 1;

# Text viewable application mime types.

my @text_viewable_app_mime_types =
    qw(postscript
       rtf
       x-awk
       x-cgi
       x-csh
       x-glade
       x-java
       x-javascript
       x-jbuilder-project
       x-perl
       x-php
       x-python
       x-shellscript
       x-troff-man
       x-troff
       xhtml+xml);

# A preferences sub-record mapping table, used for loading in and saving the
# colour preferences.

my @colour_mapping_table =
    ({widget => "annotation_prefix_1_foreground_colorbutton",
      record => "annotate_prefix_1"},
     {widget => "annotation_prefix_1_background_colorbutton",
      record => "annotate_prefix_1"},
     {widget => "annotation_text_1_foreground_colorbutton",
      record => "annotate_text_1"},
     {widget => "annotation_text_1_background_colorbutton",
      record => "annotate_text_1"},
     {widget => "annotation_prefix_2_foreground_colorbutton",
      record => "annotate_prefix_2"},
     {widget => "annotation_prefix_2_background_colorbutton",
      record => "annotate_prefix_2"},
     {widget => "annotation_text_2_foreground_colorbutton",
      record => "annotate_text_2"},
     {widget => "annotation_text_2_background_colorbutton",
      record => "annotate_text_2"},
     {widget => "revision_1_foreground_colorbutton",
      record => "cmp_revision_1"},
     {widget => "revision_1_background_colorbutton",
      record => "cmp_revision_1"},
     {widget => "revision_1_highlight_colorbutton",
      record => "cmp_revision_1"},
     {widget => "revision_2_foreground_colorbutton",
      record => "cmp_revision_2"},
     {widget => "revision_2_background_colorbutton",
      record => "cmp_revision_2"},
     {widget => "revision_2_highlight_colorbutton",
      record => "cmp_revision_2"});

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub build_mime_match_table($);
sub load_preferences();
sub preferences($);
sub save_preferences($);

# Private routines.

sub add_file_name_pattern_button_clicked_cb($$);
sub add_mime_type_button_clicked_cb($$);
sub database_browse_button_clicked_cb($$);
sub file_name_pattern_entry_changed_cb($$);
sub file_name_patterns_treeview_cursor_changed_cb($$);
sub file_glob_to_regexp($);
sub get_preferences_window($$);
sub home_dir();
sub initialise_mime_info_table();
sub load_file_name_patterns_treeview($);
sub load_mime_types_page($);
sub load_preferences_into_gui($);
sub mime_type_entry_changed_cb($$);
sub mime_types_treeview_cursor_changed_cb($$);
sub remove_file_name_pattern_button_clicked_cb($$);
sub remove_mime_type_button_clicked_cb($$);
sub save_current_mime_types_settings($);
sub save_preferences_from_gui($);
#
##############################################################################
#
#   Routine      - preferences
#
#   Description  - Displays the preferences dialog window and then lets the
#                  user change the application preferences.
#
#   Data         - $browser     : The browser instance that called up the
#                                 preferences dialog window.
#                  Return Value : True if the preferences were changed,
#                                 otherwise false if they were left unaltered.
#
##############################################################################



sub preferences($)
{

    my $browser = $_[0];

    my($instance,
       $preferences);
    my $wm = WindowManager->instance();

    # Load in the user's preferences.

    eval
    {
	$preferences = load_preferences();
    };
    if ($@ ne "")
    {
	chomp($@);
	my $dialog = Gtk2::MessageDialog->new
	    ($browser->{window},
	     ["modal"],
	     "warning",
	     "close",
	     __("The preferences dialog cannot be displayed:\n") . $@);
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Get the preferences dialog window.

    $instance = get_preferences_window($browser->{window}, $preferences);

    # Handle all events until the dialog is dismissed.

    $wm->make_busy($instance, 1, 1);
    $instance->{done} = 0;
    while (! $instance->{done})
    {
	Gtk2->main_iteration();
    }
    $wm->make_busy($instance, 0);
    $instance->{window}->hide();

    # Deal with the result.

    if ($instance->{preferences_changed})
    {
	save_preferences_from_gui($instance);
	eval
	{
	    save_preferences($preferences);
	};
	if ($@ ne "")
	{
	    chomp($@);
	    my $dialog = Gtk2::MessageDialog->new
		($browser->{window},
		 ["modal"],
		 "warning",
		 "close",
		 __("Your preferences could not be saved:\n") . $@);
	    $dialog->run();
	    $dialog->destroy();
	    return;
	}
    }

    # Cleanup.

    $instance->{mime_types_liststore}->clear();
    $instance->{file_name_patterns_liststore}->clear();
    $instance->{preferences} = undef;

    return $instance->{preferences_changed};

}
#
##############################################################################
#
#   Routine      - load_preferences
#
#   Description  - Loads in the user's preferences or initialises them from
#                  scratch if no preferences can be found.
#
#   Data         - Return Value : A reference to the newly created preferences
#                                 record.
#
##############################################################################



sub load_preferences()
{

    my($file_name,
       $mime_table,
       %preferences,
       $prefs_file);

    # Determine the name of the user's preferences file.

    $file_name = File::Spec->catfile(home_dir(), PREFERENCES_FILE_NAME);

    # Either load in the preferences or initialise them from scratch depending
    # upon whether the preferences file exists or not.

    if (-f $file_name)
    {
	defined($prefs_file = IO::File->new($file_name, "r"))
	    or die(__x("open failed: {error_message}\n", error_message => $!));
	eval(join("", $prefs_file->getlines()));
	die(__x("Invalid user preferences file: {error_message}\n",
		error_message => $@))
	    if ($@ ne "");
	$prefs_file->close();
	die(__x("Preferences file, `{file_name}',\n", file_name => $file_name)
	    . __("is at the wrong version, please remove it.\n"))
	    if (! exists($preferences{version})
		|| $preferences{version} != PREFERENCES_FORMAT_VERSION);
    }
    else
    {
	defined($mime_table = initialise_mime_info_table())
	    or die(__("Cannot load system MIME types.\n"));
	%preferences =
	    (version        => PREFERENCES_FORMAT_VERSION,
	     default_mtn_db => "",
	     workspace      => {takes_precedence => 0,
				auto_select      => 0},
	     query          => {tagged => {limit => 0,
					   sort_cronologically => 1},
				id     => {limit => 0,
					   sort_cronologically => 1}},
	     fixed_font     => "monospace 10",
	     colours        => {annotate_prefix_1 => {fg => "AliceBlue",
						      bg => "CadetBlue"},
				annotate_text_1   => {fg => "MidnightBlue",
						      bg => "PaleTurquoise"},
				annotate_prefix_2 => {fg => "AliceBlue",
						      bg => "SteelBlue"},
				annotate_text_2   => {fg => "MidnightBlue",
						      bg => "SkyBlue"},
				cmp_revision_1    => {fg => "DarkRed",
						      bg => "MistyRose1",
						      hl => "IndianRed1"},
				cmp_revision_2    => {fg => "DarkGreen",
						      bg => "DarkSeaGreen1",
						      hl => "SpringGreen1"}},
	     mime_table     => $mime_table);
    }

    return \%preferences;

}
#
##############################################################################
#
#   Routine      - save_preferences
#
#   Description  - Saves the specified preferences record to the user's
#                  preferences file.
#
#   Data         - $preferences : A reference to the preferences record that
#                                 is to be saved.
#
##############################################################################



sub save_preferences($)
{

    my $preferences = $_[0];

    my($file_name,
       $prefs_file);

    # Determine the name of the user's preferences file.

    $file_name = File::Spec->catfile(home_dir(), PREFERENCES_FILE_NAME);

    # Write out the preferences record to disk.

    defined($prefs_file = IO::File->new($file_name, "w"))
	or die(__x("open failed: {error_message}\n", error_message => $!));
    $prefs_file->print("#\n");
    $prefs_file->
	print(__("# DO NOT EDIT! This is an automatically generated file.\n"));
    $prefs_file->print(__("# Changes to this file may be lost or cause ")
		       . __("mtn-browse to malfunction.\n"));
    $prefs_file->print("#\n");
    $prefs_file->print(Data::Dumper->Dump([$preferences], ["*preferences"]));
    $prefs_file->close();

}
#
##############################################################################
#
#   Routine      - build_mime_match_table
#
#   Description  - Build a regular expression pattern matching table for
#                  working out the MIME type of a file from its file name.
#
#   Data         - $mime_info_table : A reference to the MIME information
#                                     table on which the pattern matching
#                                     table is to be based.
#                  Return Value     : A reference to the newly created pattern
#                                     matching table.
#
##############################################################################



sub build_mime_match_table($)
{

    my $mime_info_table = $_[0];

    my($re_str,
       @table);

    foreach my $entry (@$mime_info_table)
    {
	foreach my $file_glob (@{$entry->{file_name_patterns}})
	{
	    $re_str = file_glob_to_regexp($file_glob);
	    push(@table,
		 {re      => qr/$re_str/,
		  details => $entry});
	}
    }

    return \@table;

}
#
##############################################################################
#
#   Routine      - database_browse_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the browse
#                  button in the preferences window.
#
#   Data         - $instance : The window instance that is associated with
#                              this widget.
#                  $browser : The browser instance that is associated with
#                             this widget.
#
##############################################################################



sub database_browse_button_clicked_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $file_name;

    $instance->{database_entry}->set_text($file_name)
	if (open_database($instance->{window}, undef, \$file_name));

}
#
##############################################################################
#
#   Routine      - mime_types_treeview_cursor_changed_cb
#
#   Description  - Callback routine called when the user selects an entry in
#                  the MIME types treeview in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub mime_types_treeview_cursor_changed_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my($entry,
       $entry_path);

    # Get the MIME table entry details for the item that was selected.

    $widget->get_selection()->selected_foreach
	(sub {
	     my($model, $path, $iter) = @_;
	     $entry = $model->get($iter, MTLS_ENTRY_COLUMN);
	     $entry_path = $path->to_string();
	 });

    # If something has been selected and it is different from before then deal
    # with it.

    if (defined($entry)
	&& (! defined($instance->{selected_mime_types_entry})
	    || $entry != $instance->{selected_mime_types_entry}))
    {

	# If an entry was selected before then save any changes and update the
	# liststore.

	if (defined($instance->{selected_mime_types_entry}))
	{
	    save_current_mime_types_settings($instance);
	    $instance->{mime_types_liststore}->
		set($instance->{mime_types_liststore}->
		        get_iter_from_string($instance->
					     {selected_mime_types_path}),
		    MTLS_PATTERNS_COLUMN,
		        join(" ",
			     @{$instance->{selected_mime_types_entry}->
			       {file_name_patterns}}),
		    MTLS_HELPER_COLUMN,
		        $instance->{selected_mime_types_entry}->
		            {helper_application});
	}

	# Load in the newly selected entry.

	$instance->{selected_mime_types_entry} = $entry;
	$instance->{selected_mime_types_path} = $entry_path;
	$instance->{remove_mime_type_button}->set_sensitive(TRUE);
	foreach my $widget (@{$instance->{mime_type_sensitivity_list}})
	{
	    $widget->set_sensitive(TRUE);
	}
	load_file_name_patterns_treeview($instance);
	$instance->{file_name_pattern_entry}->set_text("");
	$instance->{add_file_name_pattern_button}->set_sensitive(FALSE);
	$instance->{remove_file_name_pattern_button}->set_sensitive(FALSE);
	$instance->{display_internally_checkbutton}->
	    set_active($entry->{display_internally} ? TRUE : FALSE);
	$instance->{syntax_highlight_checkbutton}->
	    set_active($entry->{syntax_highlight} ? TRUE : FALSE);
	$instance->{helper_application_entry}->
	    set_text($entry->{helper_application});

    }

}
#
##############################################################################
#
#   Routine      - mime_type_entry_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the MIME type entry in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub mime_type_entry_changed_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{add_mime_type_button}->set_sensitive
	((length($instance->{mime_type_entry}->get_text()) > 0) ?
	 TRUE : FALSE);

}
#
##############################################################################
#
#   Routine      - add_mime_type_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the add
#                  MIME type button in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub add_mime_type_button_clicked_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my($match,
       $mime_type);

    # Check entry to see if it is valid.

    $mime_type = $instance->{mime_type_entry}->get_text();
    if ($mime_type !~ m/^[^\/]+\/[^\/]+$/o)
    {
	my $dialog = Gtk2::MessageDialog->new
	    ($instance->{window},
	     ["modal"],
	     "warning",
	     "close",
	     __x("`{mime_type}' does not\nlook like a valid MIME type.",
		 mime_type => $mime_type));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Now check for duplicate entries.

    foreach my $entry (@{$instance->{preferences}->{mime_table}})
    {
	if ($mime_type eq $entry->{name})
	{
	    $match = $entry->{name};
	    last;
	}
    }
    if (defined($match))
    {
	my $dialog = Gtk2::MessageDialog->new
	    ($instance->{window},
	     ["modal"],
	     "warning",
	     "close",
	     __x("`{mime_type}' is already listed.",
		 mime_type => $mime_type));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Ok so add it to the MIME types list and reload the MIME types treeview.

    push(@{$instance->{preferences}->{mime_table}},
	 {name               => $mime_type,
	  file_name_patterns => [],
	  display_internally => 0,
	  syntax_highlight   => 0,
	  helper_application => ""});
    @{$instance->{preferences}->{mime_table}} =
	sort({ $a->{name} cmp $b->{name} }
	     @{$instance->{preferences}->{mime_table}});
    load_mime_types_page($instance);

}
#
##############################################################################
#
#   Routine      - remove_mime_type_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the remove
#                  MIME type button in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub remove_mime_type_button_clicked_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $i;

    # Simply remove the selected MIME type from the list.

    if (defined($instance->{selected_mime_types_entry}))
    {

	# Locate the file name pattern and remove it from the list.

	for ($i = 0;
	     $i < scalar(@{$instance->{preferences}->{mime_table}});
	     ++ $i)
	{
	    last if ($instance->{selected_mime_types_entry}->{name}
		     eq $instance->{preferences}->{mime_table}->[$i]->{name});
	}
	splice(@{$instance->{preferences}->{mime_table}}, $i, 1);

	# Reload the MIME types list.

	load_mime_types_page($instance);

    }

}
#
##############################################################################
#
#   Routine      - file_name_patterns_treeview_cursor_changed_cb
#
#   Description  - Callback routine called when the user selects an entry in
#                  the file name patterns treeview in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub file_name_patterns_treeview_cursor_changed_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    # Store the details of the newly selected file name pattern.

    $widget->get_selection()->selected_foreach
	(sub {
	     my($model, $path, $iter) = @_;
	     $instance->{selected_file_name_pattern} = $model->get($iter, 0);
	 });

    # Enable the remove file name patterns button if something was selected.

    $instance->{remove_file_name_pattern_button}->set_sensitive(TRUE)
	 if (defined($instance->{selected_file_name_pattern}));

}
#
##############################################################################
#
#   Routine      - file_name_pattern_entry_changed_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the file name pattern entry in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub file_name_pattern_entry_changed_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{add_file_name_pattern_button}->set_sensitive
	((length($instance->{file_name_pattern_entry}->get_text()) > 0) ?
	 TRUE : FALSE);

}
#
##############################################################################
#
#   Routine      - add_file_name_pattern_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the add
#                  file name pattern button in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub add_file_name_pattern_button_clicked_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my($match,
       $pattern,
       $re_text);

    # Check entry to see if it is valid.

    $pattern = $instance->{file_name_pattern_entry}->get_text();
    $re_text = file_glob_to_regexp($pattern);
    eval
    {
	qr/$re_text/;
    };
    if ($@ ne "")
    {
	my $dialog = Gtk2::MessageDialog->new
	    ($instance->{window},
	     ["modal"],
	     "warning",
	     "close",
	     __x("`{pattern}' is an invalid\nfile name pattern.",
		 pattern=> $pattern));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Now check for duplicate entries.

    foreach my $entry (@{$instance->{preferences}->{mime_table}})
    {
	if (grep(/^\Q$pattern\E$/, @{$entry->{file_name_patterns}}) > 0)
	{
	    $match = $entry->{name};
	    last;
	}
    }
    if (defined($match))
    {
	my $dialog = Gtk2::MessageDialog->new
	    ($instance->{window},
	     ["modal"],
	     "warning",
	     "close",
	     __x("`{pattern}' is already used in MIME type\n`{mime_type}'.",
		 pattern   => $pattern,
		 mime_type => $match));
	$dialog->run();
	$dialog->destroy();
	return;
    }

    # Ok so add it to the file name patterns list and reload the file name
    # patterns treeview.

    push(@{$instance->{selected_mime_types_entry}->{file_name_patterns}},
	 $pattern);
    @{$instance->{selected_mime_types_entry}->{file_name_patterns}} =
	sort(@{$instance->{selected_mime_types_entry}->{file_name_patterns}});
    load_file_name_patterns_treeview($instance);

}
#
##############################################################################
#
#   Routine      - remove_file_name_pattern_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the add
#                  file name pattern button in the preferences window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub remove_file_name_pattern_button_clicked_cb($$)
{

    my($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $i;

    # Simply remove the selected file name pattern from the list.

    if (defined($instance->{selected_file_name_pattern}))
    {

	# Locate the file name pattern and remove it from the list.

	for ($i = 0;
	     $i < scalar(@{$instance->{selected_mime_types_entry}->
			   {file_name_patterns}});
	     ++ $i)
	{
	    last if ($instance->{selected_mime_types_entry}->
		         {file_name_patterns}->[$i]
		     eq $instance->{selected_file_name_pattern});
	}
	splice(@{$instance->{selected_mime_types_entry}->{file_name_patterns}},
	       $i,
	       1);

	# Reload the file name patterns list.

	load_file_name_patterns_treeview($instance);
	$instance->{remove_file_name_pattern_button}->set_sensitive(FALSE);

    }

}
#
##############################################################################
#
#   Routine      - get_preferences_window
#
#   Description  - Creates or prepares an existing preferences dialog window
#                  for use.
#
#   Data         - $parent      : The parent window for the preferences dialog
#                                 window.
#                  $preferences : A reference to the preferences record that
#                                 is to be updated by the preferences dialog
#                                 window.
#                  Return Value : A reference to the newly created or unused
#                                 preferences instance record.
#
##############################################################################



sub get_preferences_window($$)
{

    my($parent, $preferences) = @_;

    my $instance;
    my $window_type = "preferences_window";
    my $wm = WindowManager->instance();

    # Create a new preferences dialog window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {

	my($renderer,
	   $tv_column);

	$instance = {};
	$instance->{glade} = Gtk2::GladeXML->new($glade_file, $window_type);

	# Flag to stop recursive calling of callbacks.

	$instance->{in_cb} = 0;

	# Connect Glade registered signal handlers.

	glade_signal_autoconnect($instance->{glade}, $instance);

	# Get the widgets that we are interested in.

	$instance->{window} = $instance->{glade}->get_widget($window_type);
	foreach my $widget ("ok_button",
			    "cancel_button",

			    # General pane widgets.

			    "database_entry",
			    "database_browse_button",
			    "precedence_checkbutton",
			    "auto_select_checkbutton",
			    "tagged_lists_limit_spinbutton",
			    "tagged_lists_sort_cronologically_radiobutton",
			    "tagged_lists_sort_by_name_radiobutton",
			    "id_lists_limit_spinbutton",
			    "id_lists_sort_cronologically_radiobutton",
			    "id_lists_sort_by_id_radiobutton",

			    # Appearance pane widgets.

			    "fonts_fontbutton",
			    "annotation_prefix_1_foreground_colorbutton",
			    "annotation_prefix_1_background_colorbutton",
			    "annotation_text_1_foreground_colorbutton",
			    "annotation_text_1_background_colorbutton",
			    "annotation_prefix_2_foreground_colorbutton",
			    "annotation_prefix_2_background_colorbutton",
			    "annotation_text_2_foreground_colorbutton",
			    "annotation_text_2_background_colorbutton",
			    "revision_1_foreground_colorbutton",
			    "revision_1_background_colorbutton",
			    "revision_1_highlight_colorbutton",
			    "revision_2_foreground_colorbutton",
			    "revision_2_background_colorbutton",
			    "revision_2_highlight_colorbutton",

			    # MIME Types pane widgets.

			    "mime_types_hpaned",
			    "mime_types_treeview",
			    "mime_type_entry",
			    "add_mime_type_button",
			    "remove_mime_type_button",
			    "file_name_patterns_treeview",
			    "file_name_pattern_entry",
			    "add_file_name_pattern_button",
			    "remove_file_name_pattern_button",
			    "display_internally_checkbutton",
			    "syntax_highlight_checkbutton",
			    "helper_application_entry")
	{
	    $instance->{$widget} = $instance->{glade}->get_widget($widget);
	}
	$instance->{mime_type_sensitivity_list} =
	    [$instance->{glade}->get_widget("file_name_patterns_label"),
	     $instance->{glade}->get_widget("file_name_pattern_entry"),
	     $instance->{glade}->get_widget("file_actions_frame")];

	# Setup the advanced find window deletion handlers.

	$instance->{window}->signal_connect
	    ("delete_event",
	     sub { $_[2]->{done} = 1 unless ($_[2]->{in_cb}); return TRUE; },
	     $instance);
	$instance->{glade}->get_widget("cancel_button")->signal_connect
	    ("clicked",
	     sub { $_[1]->{done} = 1 unless ($_[1]->{in_cb}); },
	     $instance);
	$instance->{glade}->get_widget("ok_button")->signal_connect
	    ("clicked",
	     sub { $_[1]->{done} = $_[1]->{preferences_changed} = 1
		       unless ($_[1]->{in_cb}); },
	     $instance);

	# Setup the mime types list.

	$instance->{mime_types_liststore} =
	    Gtk2::ListStore->new("Glib::String",
				 "Glib::String",
				 "Glib::String",
				 "Glib::Scalar");
	$instance->{mime_types_treeview}->
	    set_model($instance->{mime_types_liststore});

	$tv_column = Gtk2::TreeViewColumn->new();
	$tv_column->set_title(__("Mime Type"));
	$tv_column->set_resizable(TRUE);
	$tv_column->set_sizing("grow-only");
	$tv_column->set_sort_column_id(MTLS_NAME_COLUMN);
	$renderer = Gtk2::CellRendererText->new();
	$tv_column->pack_start($renderer, FALSE);
	$tv_column->set_attributes($renderer, "text" => MTLS_NAME_COLUMN);
	$instance->{mime_types_treeview}->append_column($tv_column);

	$tv_column = Gtk2::TreeViewColumn->new();
	$tv_column->set_title(__("File Name Patterns"));
	$tv_column->set_resizable(TRUE);
	$tv_column->set_sizing("grow-only");
	$tv_column->set_sort_column_id(MTLS_PATTERNS_COLUMN);
	$renderer = Gtk2::CellRendererText->new();
	$tv_column->pack_start($renderer, FALSE);
	$tv_column->set_attributes($renderer, "text" => MTLS_PATTERNS_COLUMN);
	$instance->{mime_types_treeview}->append_column($tv_column);

	$tv_column = Gtk2::TreeViewColumn->new();
	$tv_column->set_title(__("Helper Application"));
	$tv_column->set_resizable(TRUE);
	$tv_column->set_sizing("grow-only");
	$tv_column->set_sort_column_id(MTLS_HELPER_COLUMN);
	$renderer = Gtk2::CellRendererText->new();
	$tv_column->pack_start($renderer, FALSE);
	$tv_column->set_attributes($renderer, "text" => MTLS_HELPER_COLUMN);
	$instance->{mime_types_treeview}->append_column($tv_column);

	$instance->{mime_types_treeview}->set_search_column(MTLS_NAME_COLUMN);

	# Setup the file name patterns list.

	$instance->{file_name_patterns_liststore} =
	    Gtk2::ListStore->new("Glib::String");
	$instance->{file_name_patterns_treeview}->
	    set_model($instance->{file_name_patterns_liststore});

	$tv_column = Gtk2::TreeViewColumn->new();
	$tv_column->set_sizing("grow-only");
	$renderer = Gtk2::CellRendererText->new();
	$tv_column->pack_start($renderer, FALSE);
	$tv_column->set_attributes($renderer, "text" => 0);
	$instance->{file_name_patterns_treeview}->append_column($tv_column);

	$instance->{file_name_patterns_treeview}->set_search_column(0);

	# Update the advanced find dialog's state.

	$instance->{window}->set_transient_for($parent);

	# Load in the preferences.

	$instance->{preferences} = $preferences;
	load_preferences_into_gui($instance);

	local $instance->{in_cb} = 1;
	$instance->{window}->show_all();

	# Register the window for management.

	$wm->manage($instance, $window_type, $instance->{window});

    }
    else
    {

	my($height,
	   $width);

	$instance->{in_cb} = 0;
	local $instance->{in_cb} = 1;

	# Reset the preferences dialog's state.

	($width, $height) = $instance->{window}->get_default_size();
	$instance->{window}->resize($width, $height);
	$instance->{mime_types_hpaned}->set_position(700);
	$instance->{window}->set_transient_for($parent);
	$instance->{preferences} = $preferences;
	load_preferences_into_gui($instance);
	$instance->{window}->show_all();

    }

    $instance->{done} = 0;
    $instance->{preferences_changed} = undef;
    $instance->{selected_file_name_pattern} = undef;
    $instance->{selected_mime_types_entry} = undef;
    $instance->{selected_mime_types_path} = undef;

    return $instance;

}
#
##############################################################################
#
#   Routine      - load_preferences_into_gui
#
#   Description  - Loads the user's preferences into the preferences dialog
#                  window.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub load_preferences_into_gui($)
{

    my $instance = $_[0];

    # Do the general page.

    $instance->{database_entry}->
	set_text($instance->{preferences}->{default_mtn_db});
    $instance->{precedence_checkbutton}->
	set_active($instance->{preferences}->{workspace}->{takes_precedence} ?
		   TRUE : FALSE);
    $instance->{auto_select_checkbutton}->
	set_active($instance->{preferences}->{workspace}->{auto_select} ?
		   TRUE : FALSE);
    $instance->{tagged_lists_limit_spinbutton}->
	set_value($instance->{preferences}->{query}->{tagged}->{limit});
    if ($instance->{preferences}->{query}->{tagged}->{sort_cronologically})
    {
	$instance->{tagged_lists_sort_cronologically_radiobutton}->
	    set_active(TRUE);
    }
    else
    {
	$instance->{tagged_lists_sort_by_name_radiobutton}->set_active(TRUE);
    }
    $instance->{id_lists_limit_spinbutton}->
	set_value($instance->{preferences}->{query}->{id}->{limit});
    if ($instance->{preferences}->{query}->{id}->{sort_cronologically})
    {
	$instance->{id_lists_sort_cronologically_radiobutton}->
	    set_active(TRUE);
    }
    else
    {
	$instance->{id_lists_sort_by_id_radiobutton}->set_active(TRUE);
    }

    # Do the appearance page.

    $instance->{fonts_fontbutton}->
	set_font_name($instance->{preferences}->{fixed_font});
    for my $item (@colour_mapping_table)
    {
	my $field;
	if ($item->{widget} =~ m/foreground/o)
	{
	    $field = "fg";
	}
	elsif ($item->{widget} =~ m/background/o)
	{
	    $field = "bg";
	}
	else
	{
	    $field = "hl";
	}
	$instance->{$item->{widget}}->
	    set_color(Gtk2::Gdk::Color->parse($instance->{preferences}->
					      {colours}->{$item->{record}}->
					      {$field}));
    }

    # Do the MIME types page.

    load_mime_types_page($instance);

}
#
##############################################################################
#
#   Routine      - load_mime_types_page
#
#   Description  - Loads the user's preferences into the MIME types page on
#                  the preferences dialog window.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub load_mime_types_page($)
{

    my $instance = $_[0];

    $instance->{mime_types_liststore}->clear();
    foreach my $entry (@{$instance->{preferences}->{mime_table}})
    {
	$instance->{mime_types_liststore}->
	    set($instance->{mime_types_liststore}->append(),
		MTLS_NAME_COLUMN, $entry->{name},
		MTLS_PATTERNS_COLUMN, join(" ",
					   @{$entry->{file_name_patterns}}),
		MTLS_HELPER_COLUMN, $entry->{helper_application},
		MTLS_ENTRY_COLUMN, $entry);
    }
    $instance->{mime_types_treeview}->scroll_to_point(0, 0)
	if ($instance->{mime_types_treeview}->realized());
    $instance->{mime_type_entry}->set_text("");
    $instance->{add_mime_type_button}->set_sensitive(FALSE);
    $instance->{remove_mime_type_button}->set_sensitive(FALSE);
    $instance->{file_name_patterns_liststore}->clear();
    $instance->{file_name_pattern_entry}->set_text("");
    foreach my $widget (@{$instance->{mime_type_sensitivity_list}})
    {
	$widget->set_sensitive(FALSE);
    }
    $instance->{add_file_name_pattern_button}->set_sensitive(FALSE);
    $instance->{remove_file_name_pattern_button}->set_sensitive(FALSE);
    $instance->{display_internally_checkbutton}->set_active(FALSE);
    $instance->{syntax_highlight_checkbutton}->set_active(FALSE);
    $instance->{helper_application_entry}->set_text("");
    $instance->{selected_mime_types_entry} = undef;
    $instance->{selected_mime_types_path} = undef;

}
#
##############################################################################
#
#   Routine      - save_preferences_from_gui
#
#   Description  - Saves the user's preferences from the preferences dialog
#                  window.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub save_preferences_from_gui($)
{

    my $instance = $_[0];

    # Do the general pane.

    $instance->{preferences}->{default_mtn_db} =
	$instance->{database_entry}->get_text();
    $instance->{preferences}->{workspace}->{takes_precedence} =
	$instance->{precedence_checkbutton}->get_active() ? 1 : 0;
    $instance->{preferences}->{workspace}->{auto_select} =
	$instance->{auto_select_checkbutton}->get_active() ? 1 : 0;
    $instance->{preferences}->{query}->{tagged}->{limit} =
	$instance->{tagged_lists_limit_spinbutton}->get_value_as_int();
    $instance->{preferences}->{query}->{tagged}->{sort_cronologically} =
	$instance->{tagged_lists_sort_cronologically_radiobutton}->get_active()
	? 1 : 0;
    $instance->{preferences}->{query}->{id}->{limit} =
	$instance->{id_lists_limit_spinbutton}->get_value_as_int();
    $instance->{preferences}->{query}->{id}->{sort_cronologically} =
	$instance->{id_lists_sort_cronologically_radiobutton}->get_active()
	? 1 : 0;

    # Do the appearance pane.

    $instance->{preferences}->{fixed_font} =
	$instance->{fonts_fontbutton}->get_font_name();
    for my $item (@colour_mapping_table)
    {
	my $field;
	if ($item->{widget} =~ m/foreground/o)
	{
	    $field = "fg";
	}
	elsif ($item->{widget} =~ m/background/o)
	{
	    $field = "bg";
	}
	else
	{
	    $field = "hl";
	}
	$instance->{preferences}->{colours}->{$item->{record}}->{$field} =
	    colour_to_string($instance->{$item->{widget}}->get_color());
    }

    # Do the MIME types pane (most of it has possibly been saved already).

    save_current_mime_types_settings($instance);

    return;

}
#
##############################################################################
#
#   Routine      - initialise_mime_info_table
#
#   Description  - Creates a brand new MIME information table based upon the
#                  system's Mime database.
#
#   Data         - Return Value : A reference to the newly created MIME
#                                 information table on success, otherwise
#                                 undef on failure.
#
##############################################################################



sub initialise_mime_info_table()
{

    my($display_internally,
       $pattern,
       $globs_file,
       $line,
       %lookup,
       $part,
       $syntax_highlight,
       @table,
       $type);

    # Open the MIME globs file and then scan through reading in all the
    # entries.

    return if (! defined($globs_file = IO::File->new(MIME_GLOB_FILE, "r")));

    while (defined($line = $globs_file->getline()))
    {
	chomp($line);

	# Only process recognisable MIME type entries.

	if ($line !~ m/^(\s)|(\#.*)$/o && $line =~ m/^[^:]*:.*$/o)
	{

	    # Break lines into their MIME type and file name patterns fields.

	    ($type, $pattern) = $line =~ m/^([^:]*):(.*)$/o;

	    # File the data, creating a node if necessary

	    if (exists($lookup{$type}))
	    {
		push(@{$lookup{$type}->{file_name_patterns}}, $pattern);
	    }
	    else
	    {
		$display_internally = $syntax_highlight = 0;
		if ($type =~ m/^application\/.+$/o)
		{
		    ($part) = ($type =~ m/^application\/(.+)$/o);
		    $display_internally = $syntax_highlight = 1
			if (grep(/\Q$part\E/, @text_viewable_app_mime_types)
			    > 0);
		}
		elsif ($type =~ m/^image\/.+$/o)
		{
		    $display_internally = 1;
		}
		elsif ($type =~ m/^text\/.+$/o)
		{
		    $display_internally = $syntax_highlight = 1;
		}
		$lookup{$type} = {name               => $type,
			          file_name_patterns => [$pattern],
			          display_internally => $display_internally,
			          syntax_highlight   => $syntax_highlight,
			          helper_application => ""};
		push(@table, $lookup{$type});
	    }

	}
    }
    $globs_file->close();

    # Sort the results and convert file name globs into res.

    @table = sort({ $a->{name} cmp $b->{name} } @table);
    foreach my $entry (@table)
    {
	@{$entry->{file_name_patterns}} =
	    sort(@{$entry->{file_name_patterns}});
    }

    return \@table;

}
#
##############################################################################
#
#   Routine      - load_file_name_patterns_treeview
#
#   Description  - Load up the file name patterns treeview with the currently
#                  selected name patterns.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub load_file_name_patterns_treeview($)
{

    my $instance = $_[0];

    # Load up the file name patterns list.

    $instance->{file_name_patterns_liststore}->clear();
    foreach my $pattern (@{$instance->{selected_mime_types_entry}->
			   {file_name_patterns}})
    {
	$instance->{file_name_patterns_liststore}->
	    set($instance->{file_name_patterns_liststore}->append(),
		0,
		$pattern);
    }
    $instance->{file_name_patterns_treeview}->scroll_to_point(0, 0)
	if ($instance->{file_name_patterns_treeview}->realized());

}
#
##############################################################################
#
#   Routine      - save_current_mime_types_settings
#
#   Description  - Save the settings for the currently selected MIME typ entry
#                  back to the preferences record.
#
#   Data         - $instance : The associated window instance.
#
##############################################################################



sub save_current_mime_types_settings($)
{

    my $instance = $_[0];

    $instance->{selected_mime_types_entry}->{display_internally} =
	$instance->{display_internally_checkbutton}->get_active() ? 1 : 0;
    $instance->{selected_mime_types_entry}->{syntax_highlight} =
	$instance->{syntax_highlight_checkbutton}->get_active() ? 1 : 0;
    $instance->{selected_mime_types_entry}->{helper_application} =
	$instance->{helper_application_entry}->get_text();

}
#
##############################################################################
#
#   Routine      - file_glob_to_regexp
#
#   Description  - Converts the specified string containing a file name style
#                  glob into a regular expression.
#
#   Data         - $file_glob   : The file name wildcard that is to be
#                                 converted.
#                  Return Value : The resultant regular expression string.
#
##############################################################################



sub file_glob_to_regexp($)
{

    my $file_glob = $_[0];

    my($escaping,
       $first,
       $re_text);

    $escaping = 0;
    $first = 1;
    foreach my $char (split(//, $file_glob))
    {
	if ($first)
	{
	    $re_text .= "(?=[^\\.])" unless $char eq ".";
	    $first = 0;
	}
	if (".+^\$\@%()|" =~ m/\Q$char\E/)
	{
	    $re_text .= "\\" . $char;
	}
	elsif ($char eq "*")
	{
	    $re_text .= $escaping ? "\\*" : ".*";
	}
	elsif ($char eq "?")
	{
	    $re_text .= $escaping ? "\\?" : ".";
	}
	elsif ($char eq "\\")
	{
	    if ($escaping)
	    {
		$re_text .= "\\\\";
		$escaping = 0;
	    }
	    else
	    {
		$escaping = 1;
	    }
	}
	else
	{
	    $re_text .= "\\" if ($escaping && $char eq "[");
	    $re_text .= $char;
	    $escaping = 0;
	}
    }

    $re_text .= "\$";

    return $re_text;

}
#
##############################################################################
#
#   Routine      - home_dir
#
#   Description  - Returns the user's home directory.
#
#   Data         - Return Value : A string containing the users home
#                                 directory.
#
##############################################################################



sub home_dir()
{

    return $ENV{HOME} if (exists($ENV{HOME}));
    return $ENV{USERPROFILE} if (exists($ENV{USERPROFILE}));
    return "";

}

1;
