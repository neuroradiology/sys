;;; <LMFS>FS-DCLS.LSP;11 25-Mar-81 19:51:48, Edit by BSG -*-Mode:LISP;Package:lmfs-*-
;;; <LMFS>FS-DCLS.LSP 16-Mar-81 12:57:47, Edit by BSG
;;;
;;;

;;; Copyright (c) Symbolics, Inc., 1981

;;: Macros to make various things compile without including bufmacs...
(defmacro return-values rest #M `(return (values . ,rest)) #Q `(return . ,rest))
(defmacro send x `(#M => #Q funcall . ,x))
(defmacro send-self x `(#M => #M self #Q funcall-self . ,x))

;;; Declarations for active data structures and disk-resident data structures
;;; for Lisp Machine file system

#M (eval-when (compile eval)				;when in maclisp
    (setsyntax '/: 'splicing '*ct-nilret)
    (defun *ct-nilret n nil)
    (or (status feature defstorage)
	(load 'defstorage)))				;get fasl or lsp/lisp

;;; Must define included structures first


(defstorage (disk-address)
  (*		      fixnum))

(defstorage (date)
  (*		      fixnum))

(defstorage (tapeinfo)
  (date		      date)
  (tapename	      char 8.))

(defstorage (block-check-words)
  (file-id	      fixnum)
  (word-0s-rel-addr   fixnum-bytes 3)			;get in low order
  (ignore	      bit 7)				;a paddance
							;for larger word-0s..
  (headerp	      flag))

(defstorage (link-transparencies)
  (attributes being flag
    read-thru
    write-thru
    create-thru
    rename-thru
    delete-thru
    lpad6 lpad5 lpad4 lpad3 lpad2 lpad1)
    (ignore mod word))

;;; Structure for a directory


(defstorage (dir-header)
  (version	      fixnum-bytes 2)			;structure version
  (size		      fixnum-bytes 2)
  (name		      char-with-length 30.)

  (numbers being (fixnum-bytes 2)

     number-of-entries					;for scanning
     free-entry-list					;for easier alloc
     entries-index-offset				;to find first one
     direntry-size					;for scanning
     entries-per-block					;==

     default-generation-retention-count
     uid-path-offset
     uid-path-length
     hierarchy-depth)

  (default-volid      fixnum)				;for creation

  (auto-expunge-p     flag)
  (auto-expunge-interval   date)
  (auto-expunge-last-time  date)

  (default-link-transparencies link-transparencies))

;;; Structure def for directory entry

(defstorage (directory-entry :abbrev dire)
  (file-name          char-with-length 30.)		;File name, to 30 chars
  (file-type	      char-with-length 14.)		;Extension
  (file-version	      fixnum-bytes 3.)			;Generation
  (bytesize	      fixnum-bytes 1)
  (author	      char-with-length 14.)		;Creator of file
  (file-name-true-length fixnum-bytes 2)		;for quick check.
  (number-of-records  fixnum)				;basically for listing

  (dates being date
    date-time-created					;file created
    date-time-deleted					;for expungement
    date-time-reconstructed				;for salvagement
    date-time-modified					;for update-mode
    date-time-used)

  (unique-ID	      fixnum)				;for matching header
  (byte-length	      fixnum)				;for listing
  (generation-retention-count
		      fixnum-bytes 1)
  
  (logical-volume-index fixnum-bytes 3)                 ;log vol index where found.

  (record-0-address   disk-address)			;how to find the file

  (archive-a	      tapeinfo)
  (archive-b	      tapeinfo)

  (ignore  mod word)

  (switches being flag
    safety-switch					;Don't delete without complaint
    deleted						;to be expunged
    directory						;is a directory
    link						;is a link
    migrated						;header only retained
    read-only						;read-only file
    incrementally-backed-up
    completely-backed-up
    properties-were-put				        ;plist bears investigation
    ignore2 
    
    binary-p					        ;was written in binary mode

    user-damaged					;user-settable damage
    damaged-header-missing
    damaged-header-malformed
    damaged-records-missing
    damaged-records-incongruous
    damaged-records
    ))							;mods word all by its lonesome

(defstorage (fh-element)
  (name			char 4)
  (location		fixnum-bytes 2)			;in words in log. headr
  (length		fixnum-bytes 2))			;in words

(defstorage (file-header)
  (version		fixnum)
  (logical-size		fixnum-bytes 2)
  (bootload-generation  fixnum)				;for validating
  (version-in-bootload  fixnum)				;used in above

  (number-of-elements	fixnum-bytes 2)			;for later expansion
  (ignore mod word)

  (parts being fh-element				;and/or reformatting
    fh-header						;this header itself
    info						;header-resident info
    header-fm						;file map of header
    dire						;directory-entry-ifno
    property-list
    file-map
    pad-area						;many natures of obsolescence
    pad2-area))						;are supported

(defstorage (property-pair)				;2 words each
  (indicator		fixnum-bytes 2)			;offset into file to a vstring
  (property		fixnum-bytes 2))

(defstorage (plist-block)				;where plists and plist strings go
  (version		fixnum)

  (total-length		fixnum-bytes 2)
  (lowest-high-word-allocated fixnum-bytes 2)

  (lowest-free-word	      fixnum-bytes 2)
  (perceived-words-deallocated fixnum-bytes 2)			;gc trigger

  (next-fragment	       fixnum-bytes 2)
  (number-of-properties fixnum-bytes 2)

  (props		array (*) property-pair))

(defstorage (vstring)
  (data			char-with-length 100.))		;gets 2 bytes length info

(defstorage (file-map)
  (allocated-length     fixnum)
  (valid-length         fixnum-bytes 2)			;how many in this fragment
  (link                 fixnum-bytes 2)			;next fragment address or 0
  (element		array (*) fixnum))

(defstorage (uid-array)
  (element		array (*) fixnum))

;;;
;;;   Info only in file header
;;;

(defstorage (fh-info)
  (version		fixnum)
  (parent-dir-uid	fixnum)
  (parent-dir-part-id	fixnum)
  (parent-dir-address	fixnum)
  (dir-entry-index-in-parent fixnum-bytes 2)
  (duplicate-uid	fixnum)
  (inc-dump		tapeinfo)
  (comp-dump		tapeinfo))

(defstorage (partition-label)
  (version		fixnum)				;of structure
  (name			char-with-length 30.)
  (label-size		fixnum)				;in words
  (partition-id		fixnum)
  (addresses being disk-address
    primary-root					;for base partition
    free-store-info
    bad-track-info
    volume-table
  aspare1 aspare2 aspare3 aspare4)
  (update-times being date
    label-accepted					;put in use
    shut-down
    free-map-reconstructed
    structure-salvaged
    scavenged						;searching reclaimer
    tspare1 tspare2 tspare3 tspare4)
  
  (uid-generator	fixnum)
  (monotone-generator	fixnum)
  (root-list		fh-element)
  )

(defstorage (freemap-header)
  (version		fixnum)
  (number-of-blocks	fixnum)				;first numb is one
  (records-represented-in-block fixnum)
  (n-bits-per-word	fixnum)
  (first-bit-record-no  fixnum)				;allows for label, etc.
  )


(defstorage (freemap-block)
  (blocknum		fixnum)				;gotta check
  (recno-of-bit-0	fixnum)
  (number-of-words	fixnum)
  (checksum		fixnum)
  (number-of-free-recs	fixnum)
  (highest-recno+1	fixnum)
  (array		array (*) fixnum))

(defstorage (link-target)
  (version		fixnum)
  (transparent		link-transparencies)
  (pathname		char-with-length 400.))



;;;----------------------------------------------------------------------

(defstruct (file-desc :named (:conc-name fd-))
  uid					;for search

  partition-desc			;describing partition
  parent				;filedesc for par
  entry-index				;in parent, for easy finding.
  r0addr				;for dir inheriting.
  file-name				;most useful for directories
  file-type
  file-version				;for non-directories
  link-p

  sons

  buffer-list
  current-length			;in words
  addressible-length			;needed for growance
  byte-length				;in bytes, for ext. interface

  byte-size
  logical-header-length
  header-address			;if not in buffer...
  header-buffer				;if around...
  file-map-addenda			;interesting things grown against file
  date-time-created			;chaos prot wants/needs
  dir-entries-index-offset		;for dirs only
  dir-entry-size			;for dirs only
  grc-info				;for close time
  damages				;list of observed damaged flags

  openings
  )

(defstruct (file-buffer :named (:conc-name fb-))

  file-desc
  file-opening				;flush this?
  record-length

  address				;record addr rel to part of file
  					; c/b nil

  lowest-data-addr			; tell you exactly what's in here
					;COULD BE GROSSLY NEGATIVE...
  highest-data-addr+1
  lowest-header-addr
  highest-header-addr+1


  good-p				;if this nil, DONT WRITE THIS BUFFER
  modified
  creating				;used to synchronize creation


 (reference-count 0)			;how many pointers to this buffer
					;other than freelists
  array					;32-bitter usable to address this fb
  )

(defstruct (volume :named (:conc-name vol-))
  volume-id
  volume-name
  demountable-p				;con
  partition-ids				;numbers of all
  accepted-partitions			;partt- objects
    )

;;;Per-partition info mainly derived from partition label.

(defstruct (partition :named (:conc-name partt-))
  partition-id
  partition-name
  volume				;vol- structure
  device-id				;physicalness

  ;; Physical sizes

  block-size-words
  record-size-blocks
  record-size-words			;product of above two

  ;; Base/Limits of partition on device

  address-base-in-blocks		;comme ca
  disk-addr-limit			;how many RECORDS

  dataw-per-block			;data words per block
  dataw-per-record

  volume-indirectory-size		;so know when to allocate bigger one
  volume-indirectory

  partition-indirectory-size
  partition-indirectory

  active-directory-list
  buffer-list
  root-list

  uid-generator
  monotone-generator-a
  monotone-generator-b

  free-record-info			;free record structure
    )


(defstruct (freemap :named (:conc-name freemap-))
  partition						;allow fewer pointers
  file
  file-nblocks
  file-n-bits-per-word
  file-bit-zero-recno
  records-represented-in-block
  trouble-encountered					;cs error etc.
  array							;mebbe do with ar-ldrs


  pool-size						;size of array
  cur-put						;next to put
  cur-take						;next to take
  cur-howmany						;how many in there

  ;; magic parameters

  when-empty-get-this-many
  when-this-many-dump
  dump-that-many
  )
