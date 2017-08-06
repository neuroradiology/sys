;;; -*- Mode: Text; Package: System-Internals -*-

;;; This file contains documentation for microcoded functions

(DEFPROP %SPREAD "Takes a list and pushes its elements on the stack" :DOCUMENTATION)

(DEFPROP %LOGLDB "Fixnums//only form of LDB
No complaint about loading//clobbering the sign bit" :DOCUMENTATION)

(DEFPROP %LOGDPB "Fixnums//only form of DPB
No complaint about loading//clobbering the sign bit" :DOCUMENTATION)

(DEFPROP LSH "Fixnum-only logical shift" :DOCUMENTATION)

(DEFPROP ROT "24-bit logical rotate for fixnums" :DOCUMENTATION)

(DEFPROP %OLD-MAKE-LIST "Construct a cdr-coded list of NILs of the specified length" :DOCUMENTATION)

(DEFPROP %MAKE-LIST "Construct a cdr-coded list of objects of the specified length" :DOCUMENTATION)

(DEFPROP BIND "Bind any location to a specified value.
Adds the binding to the current stack-frame.  Only works in compiled code.
This allows you to bind cells other than value cells and to do conditional
binding." :DOCUMENTATION)

(DEFPROP GET-LIST-POINTER-INTO-ARRAY "Makes an ART-Q-LIST array into a list.
Gives you the list which resides inside the array." :DOCUMENTATION)

(DEFPROP ARRAY-PUSH "Add an element to an array.
The fill pointer (leader element 0) is the index of the next element to
be added.  Gives an error if the array is full, you can use ARRAY-PUSH-EXTEND
instead if you want the array to grow automatically." :DOCUMENTATION)

(DEFPROP + "Synonymous with PLUS" :DOCUMENTATION)

(DEFPROP - "Synonymous with DIFFERENCE
Except, with one argument synonymous with MINUS" :DOCUMENTATION)

(DEFPROP * "Synonymous with TIMES" :DOCUMENTATION)

(DEFPROP // "Synonymous with QUOTIENT" :DOCUMENTATION)

(DEFPROP \ "Synonymous with REMAINDER" :DOCUMENTATION)

(DEFPROP \\ "Synonymous with GCD" :DOCUMENTATION)

(DEFPROP ^ "Synonymous with EXPT" :DOCUMENTATION)

(DEFPROP %STORE-CONDITIONAL "The basic locking primitive.
If the contents of the cell addressed by POINTER equals OLD,
the uninterruptibly store NEW into it." :DOCUMENTATION)

(DEFPROP %DATA-TYPE "Internal data-type code of its argument" :DOCUMENTATION)

(DEFPROP %POINTER "Address or pointer-field of its argument" :DOCUMENTATION)

(DEFPROP %MAKE-POINTER "Given data-type and address, fake up an object" :DOCUMENTATION)

(DEFPROP %MAKE-POINTER-OFFSET
	 "Given data-type and address as pointer+offset, fake up an object"
	 :DOCUMENTATION)

(DEFPROP %POINTER-DIFFERENCE "Return the number of words difference between two pointers.
They had better be locatives into the same object for this operation to be meaningful."
	 :DOCUMENTATION)

(DEFPROP %FIND-STRUCTURE-HEADER "Given a locative return the object containing it.
Finds the overall structure containing the cell addressed by the locative pointer."
	 :DOCUMENTATION)

(DEFPROP %FIND-STRUCTURE-LEADER "Given a locative return the object containing it.
This is like %FIND-STRUCTURE-HEADER except that it always returns the base of the
structure; thus for an array with a leader it gives a locative to the base instead
of giving the array." :DOCUMENTATION)

(DEFPROP %STRUCTURE-TOTAL-SIZE "Returns the number of words in an object." :DOCUMENTATION)

(DEFPROP %STRUCTURE-BOXED-SIZE "Returns the number of normal Lisp pointers in an object."
	 :DOCUMENTATION)

(DEFPROP STACK-GROUP-RETURN
	"Resumes the current stack group's resumer, transmitting the value X.
The resumer's own resumer is not affected."
	:DOCUMENTATION)

(DEFPROP STACK-GROUP-RESUME
	"Resumes SG, transmitting the value X.  SG's resumer is not affected."
	:DOCUMENTATION)

(DEFPROP BIGNUM-TO-ARRAY
	 "Converts a bignum into an array.
The first argument is a bignum, the second is a fixnum.  The bignum is
expressed in the base of the fixnum and stuffed into an appropriate art-q
array.  The sign of the bignum is ignored."
	 :DOCUMENTATION)

(DEFPROP ARRAY-TO-BIGNUM
	"Converts an array into a bignum.
The first argument is an art-q array, the second is a fixnum and the
third is the sign bit (0 or 1).  The art-q array is interpreted as a bignum
expressed in the base of the fixnum and converted into that bignum.  It is
given the third argument as its sign.  This is the inverse of BIGNUM-TO-ARRAY."
	 :DOCUMENTATION)