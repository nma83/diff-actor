# diff-actor.el
Act out diff's between files in Emacs.

## Summary

Given 2 files as input, diff-actor generates the diff between them and acts out the changes as live edits in an Emacs buffer.
The first file is used as the starting point and edits are made on it to lead to the second file.
Has wrapper functions to pick the 2 files from revision history of various VCSs like git and p4.

## Installation:

Install diff-actor.el in load-path.
Basic file diff acting:
  (diff-actor-file file-name-1 file-name-2)

This function uses the Emacs variable diff-command for diff-ing.

VCS (vcs-name) wrappers:
  (diff-actor-vcs-name-vers file-name &optional ver-1 ver-2)

Perforce and Git are supported right now, with functions:
  (diff-actor-p4-vers ...)
  (diff-actor-git-vers ...)

This function uses the VCS specific diff command for diff-ing
ver1 and ver2 of file-name. Default values:
  ver1 - first version
  ver2 - top of tree (HEAD)

## Demo

TODO embed aciinema video.
