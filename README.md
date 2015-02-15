# simplenote2.el

simplenote2.el is a new version of [simplenote.el](https://github.com/dotemacs/simplenote.el) which assists
the interaction with [Simplenote](http://simplenoteapp.com/).
The major improvement points from the original version are

* Uses Simplenote API ver.2 to interact with server which can provide the support of tags, automatic merge of notes, and some other features.
* Accesses to server asynchronously so as not to block UI as far as possible.

## Requirements

This package requires Emacs 23 or later. Tested on Emacs 23.3 and Emacs 24.3 on Linux.

## Installation

This package requires [request-deferred](https://github.com/tkf/emacs-request) which is also available from MELPA. Install it in advance.

Then, just download `simplenote2.el` and put it on a directory anywhere Emacs can find.
*(Installation from MELPA by package.el will be coming soon...)*

## Configuration

Add the following lines to your `.emacs`.

```.emacs
(require 'simplenote2)
(setq simplenote2-email "email@example.com")
(setq simplenote2-password "yourpassword")
(simplenote2-setup)
```

where the email and password are the ones that you use to login to the
Simplenote application. It is also possible to set
`simplenote2-password` to `nil`. In this case you will be asked for the
password when it is required. `simplenote2.el` keeps a local copy of
your notes in a directory defined in the variable
`simplenote2-directory` whose default value is `~/.simplenote2`.
You can change the value of `simplenote2-directory` before calling
`simplenote2-setup`. The latter checks for the existence of
`simplenote2-directory` and some necessary sub-directories and creates
these if they do not exist.

## Usage

There are two basic usages for `simplenote2.el`. It can be used as a
browser for your notes (with local caching), or you can synchronize
individual notes to the Simplenote application.

### Simplenote browser

Give the command `M-x simplenote2-browse`. If this the first time you
will see an almost empty buffer. Click on the `[Sync with server]` button
to download your notes from the Simplenote server. If you have not set
the variables `simplenote2-email` or `simplenote2-password` you will be
asked interactively for their values.

The rest of the interface should be more or less clear. You have the
option to create new notes, and edit or delete existing notes. Syncing
is not automatic but has to be invoked manually either by clicking the
`[Sync with sever]` button at the top of the Simplenote browser or by
`M-x simplenote2-sync-notes`.

### Sync individual notes

The following commands can be used for syncing individual notes.

`M-x simplenote2-create-note-from-buffer`

This command will create a new note with the contents of the file currently visiting. When the note is successfully created on the server, the file is moved under `simplenote2-directory`.

*Note: The behavior of this command has been changed from the simplenote.el. The function that identifies files by adding buffer local variable to the file is no longer supported.*

`M-x simplenote2-push-buffer`

This command pushes the modificaitions locally added to the note currently visiting, and retreives the modifications on the server. If the file visiting is in "new notes" directory, this command acts as `M-x simplenote2-create-note-from-buffer`.

`M-x simplenote2-pull-buffer`

This command just pulls the modifications on the server added to the note currently visiting. When the note is locally modified, you will be asked if you push them to the server. If you answer yes, this command acts as `M-x simplenote2-push-buffer`, otherwise the modification will be discarded.

## New features of simplenote2

### Tags support

Tags attached to each note are displayed on the notes list of the browser. You can filter the notes list with specified tags by `M-x simplenote2-filter-notes-by-tag`. This command interactively asks you the name of tags. You can input multiple tag names until you just input [Enter]. After this command, the only notes which have the either of tags you specified will be displayed. `C-u M-x simplenote2-filter-notes-by-tag` unsets the filter.

You can also specify the default filter by setting the variable `simplenote2-filter-note-tag-list` in your `.emacs` like below.

```.emacs
(setq simplenote2-filter-note-tag-list '("tag1" "tag2" "tag3"))
```

*Attaching tags on simplenote2 will be supported soon.*

### Pinned to top

The notes which have the attribute "Pinned to top" are displayed on the top of the notes list by default.

### Markdown formatted

*Will be coming soon...*
