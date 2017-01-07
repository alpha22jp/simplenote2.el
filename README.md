# simplenote2.el [![MELPA](http://melpa.org/packages/simplenote2-badge.svg)](http://melpa.org/#/simplenote2) [![MELPA Stable](http://stable.melpa.org/packages/simplenote2-badge.svg)](http://stable.melpa.org/#/simplenote2)

(Here is [Japanese version](./README_ja.md))

simplenote2.el is a new version of [simplenote.el](https://github.com/dotemacs/simplenote.el) which assists
the interaction with [Simplenote](http://simplenoteapp.com/).
The major improvement points from the original version are

* Use of Simplenote API ver.2 to interact with server which can provide the support of tags, automatic merge of notes, and some other features.
* Asynchronous and concurrent access to server as far as possible which brings faster sync of notes and non-blocking UI.

**Since version 3.0.0, a new type of browser screen based on tabulated list has been introduced. See "Simplenote List" section below.**

## Requirements

This package requires Emacs 24 or later. Tested on Emacs 24.3 on Linux.

## Installation

### Via MELPA

<kbd>M-x package-install [RET] simplenote2 [RET]</kbd>

### Manually

This package depends on [request-deferred](https://github.com/tkf/emacs-request). Install it in advance. Then, just download `simplenote2.el` and put it on a directory anywhere Emacs can find.

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

### Simplenote list

This is another browser screen based on tabulated list mode invoked by
`M-x simplenote2-list`. It's simpler and cleaner than Simplenote browser.

Available shortcuts in this mode are the followings.

* `g`: sync with the server (same as `M-x simplenote2-sync-notes`)
* `a`: create a new note
* `Enter`: open note on the current line
* `d`: mark note on the current line for deletion
* `u`: unmark note on the current line for deletion
* `t`: set tags for filtering (same as `M-x simplenote2-filter-note-by-tag`)
* `^`: toggle tags filtering condition between "AND" and "OR"
* `/`: set regexp for filtering

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

Tags attached to each note are displayed on the notes list of the browser. You can filter the notes list with specified tags by `M-x simplenote2-filter-notes-by-tag`. This command interactively asks you the name of tags. You can input multiple tag names until you just input [Enter]. Tag name completion by [Tab] is available. After inputing tags, the only notes which have the either of tags you specified will be displayed. `C-u M-x simplenote2-filter-notes-by-tag` unsets the filter.

You can also specify the default filter by setting the variable `simplenote2-filter-note-tag-list` in your `.emacs` like below.

```.emacs
(setq simplenote2-filter-note-tag-list '("tag1" "tag2" "tag3"))
```

You can add a tag by `M-x simplenote2-add-tag` and delete a tag by `M-x simplenote2-delete-tag` to the note currently visiting. Tag name completion is available in these commands too.

### Pinned to top

The notes which have the attribute "Pinned to top" are displayed on the top of the notes list by default.

You can set this attribute by `M-x simplenote2-set-pinned` to the note currently visiting. `C-u M-x simplenote2-set-pinned` unset the attribute.

### Markdown formatted

When you edit the notes which have the attribute "Markdown formatted", the file is opened with the major mode specified by the customize variable `simplenote2-markdown-notes-mode` whose default value is `text-mode`. If you want to use `markdown-mode` for example, install `markdown-mode.el` and set this variable to `markdown-mode`.

You can set this attribute by `M-x simplenote2-set-markdown` to the note currently visiting. `C-u M-x simplenote2-set-markdown` unset the attribute.

### Add tags or attributes to new notes

You can set tags or attributes to new notes by default using customize variable `simplenote2-create-note-hook` as below.

```.emacs
(add-hook 'simplenote2-create-note-hook
	  (lambda ()
	    (simplenote2-set-markdown)
	    (simplenote2-add-tag "tag1")))
```

### Minor mode for simplenote note buffer

When you open simplenote note from Simplenote browser, minor mode `simplenote2-note-mode` is applied to the buffer. This minor mode doesn't do anything by default, but it can be used for key bindings or something like below.

```.emacs
(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
            (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
            (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))
```

## History

version 3.0.0 (2016-11-27)

* New: Introduce a new type of browser screen based on tabulated list.
* Fix: #18: Can not retrieve all notes when there are many notes to sync.

version 2.2.2 (2015-04-05)

* New: Minor mode `simplenote2-note-mode` is now applied to simplenote note buffer.
* Fix: Notes including some sort of characters are not updated correctly due to the wrong way of URI encoding for JSON data.

version 2.2.1 (2015-03-17)

* Fix: Empty header line causes crash

version 2.2.0 (2015-03-04)

* New: support adding tags and other attributes to new notes before they are synced to the server.
* New: add customize variable `simplenote2-create-note-hook`.
* Modify: improve buffer handling when new notes are synced to the server.
* Fix: `M-x simplenote2-set-markdown` doesn't change the major mode immediately.

version 2.1.1 (2015-02-25)

* Fix: some characters can't be synced due to lack of URI encode.

version 2.1.0 (2015-02-22)

* New: support editing tags and other attributes (pinned, markdown).
* New: customize variable `simplenote2-markdown-notes-mode`.
* Modify: improve some browser screen appearance and UI.
* Fix: fix some minor bugs.

version 2.0.0 (2015-02-16)

* Initial release as simplenote2.el
