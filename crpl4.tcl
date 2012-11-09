# crpl4.tcl
#
# Routines for minimizing typing in tcl
#
# Author: strick
#
# Date: December, 1999

#
# crpl functions
#


proc crpl4. {} { return 2003-02-14 }
namespace eval Crpl {}
package provide Crpl 4.0

if {![info exists ::Crpl::common]} {

set ::Crpl::common {}

############################################
#
#  metaCrpl -- defines patterns for unknown to dispatch

proc ::Crpl::metaCrpl {pat cmd} {
	global ::Crpl::metaCrpl
	lappend ::Crpl::metaCrpl $pat $cmd
}

# first the prefix characters
::Crpl::metaCrpl {^\^}	::Crpl::Imp_unk_hat
::Crpl::metaCrpl {^[`]}	::Crpl::Imp_unk_bquot
::Crpl::metaCrpl {^[']}	::Crpl::Imp_unk_squot
::Crpl::metaCrpl {^[~]}	::Crpl::Imp_unk_tilde
::Crpl::metaCrpl {^[!]}	::Crpl::Imp_unk_bang
::Crpl::metaCrpl {^[@]}	::Crpl::Imp_unk_assert

# then in this order: comma, apostrophe, equals.
::Crpl::metaCrpl {^.*[,]}		::Crpl::Imp_unk_comma
::Crpl::metaCrpl {^.*[']}		::Crpl::Imp_unk_apos 
::Crpl::metaCrpl {^[:/+|^=\w]*[=][^=]*$}	::Crpl::Imp_unk_equals 

# finally beginning-and-closing characters
::Crpl::metaCrpl {^[(].*[)]$}	::Crpl::Imp_unk_parens 


#############################################

proc defcommon args {
	global ::Crpl::common
	set ::Crpl::common $args
}

# The empty command ( i.e. proc {} ) is fundamental to Crpl.
# It joins its args with spaces.
# (It is like the builtin "join" command,
#  except it takes multiple arguments,
#  rather than a single list argument.
#  Notice it just calls join!)
#  (If for some reason it cannot be defined as "proc {}",
#  it can be intercepted by the unknown command.)

proc {} args {join $args}

# Crpl::human cleans logging and error messages.
# It should prevent control characters and 
# really long strings.
# Users can redefine it.

proc ::Crpl::human s {
	regsub -all {[^ -~\u00a1-\u00ff]} $s {#} s
	set s [string range $s 0 999]
	return $s
}

# Crpl::log goes to stderr ; user can redefine

proc ::Crpl::log s {
	puts stderr [human "## $s"]
}

######################################################
######################################################
#
#   defmacro

# Use "defmacro" a lot like "proc", 
#    except return, break, and continue affect caller.

proc defmacro {name params body} {

	if {![info complete $body]} { error "INCOMPLETE body in defmacro $name" }

	append xbody " set _STATUS_ \[catch { $body } _RESULT_ ] "

#a	append xbody "
#a		global ::Crpl::macro_count
#a		if {\[catch { incr ::Crpl::macro_count([list $name]) } ]} {
#a			set ::Crpl::macro_count([list $name])  1
#a			set ::errorInfo {}
#a		}
#a	"

	append xbody {
		switch $_STATUS_ {
		  0	{return $_RESULT_ }
		  1	{return -code error -errorinfo $::errorInfo -errorcode $::errorCode $_RESULT_ }
		  2	{return -code return $_RESULT_}
		  3	{return -code break}
		  4	{return -code continue}
		default	{return -code $_STATUS_}
		}
	}

	uplevel 1 [list proc $name $params $xbody]
}

#proc Crpl::IncrArray {var sub} {
#	upvar 1 $var v
#	if [info exists v($sub)] {
#		incr v($sub)
#	} else {
#		set v($sub) 1
#	}
#}

proc Crpl::audit_proc {name params body} {
#a	set body "Crpl::IncrArray Crpl::audit_count [list $name] ;;; $body"
	uplevel 1 [list proc $name $params $body]
}

#
######################################################
#
#  unknown


rename ::unknown ::Crpl::previous_unknown

defmacro ::unknown {c args} {

	global ::Crpl::unknown_count
	if {[catch { incr ::Crpl::unknown_count($c) } ]} {
		set ::Crpl::unknown_count($c)  1
		set ::errorInfo {}
	}

	global ::Crpl::metaCrpl 
	set handler ::Crpl::previous_unknown
	foreach {p u} [set ::Crpl::metaCrpl] {
		if [regexp $p $c] { set handler $u ; break }
	}

	uplevel 1 [ list $handler $c ] $args 
}

#
######################################################
######################################################
######################################################

proc ::Crpl::Imp_unk_equals {c args} {

	# right now, either :varname or /varname means global -- which is better?

	set v [split $c =]
	set n [llength $v]
	set cmd [lindex $v end]

	# say evalute the r-value
	if { $cmd == "" } {  #optimize empty command
		set b "set rval \[join \$args \] ;"
	} else {
		set b "set rval \[uplevel 1 [list [list $cmd]] \$args \] ;"
	}

	for {set i 0} {$i < $n - 1} {incr i} {
		set w [lindex $v $i]
		
		if {$w == ""} continue

		if {$n == 2} { # simple single var
			append b "set x \$rval ;"
		} elseif {$i == $n - 2} {
			append b "set x \[lrange \$rval $i end] ;"
		} else {
			append b "set x \[lindex \$rval $i] ;"
		}
		
		if {[string match {[/:]*} $w]} {
			set level #0
			regsub {^[/:]} $w {} w
		} else {
			set level 1
		}

		switch -glob -- $w {
			"*+" {set op incr; regsub {.$} $w {} w}
			"*|" {set op append; regsub {.$} $w {} w}
			"*^" {set op lappend; regsub {.$} $w {} w}
			default {set op set}
		}

		if {[regexp {^([^()]*)[(](.*)[)]$} $w 0 1 2]} {
			append b "upvar $level [list [list $1]] a ;"
			append b "$op [list a($2)] \$x ;"
		} else {
			append b "upvar $level [list [list $w]] a ;"
			append b "$op a \$x ;"
		}
	}

	#say return the rvalue
	append b "return \$rval"

	# define the proc
	uplevel #0 [list Crpl::audit_proc $c args $b ]

	# and call the proc, for current evaluation
	uplevel 1 [list $c] $args
}

proc ::Crpl::Eval_parens_expr c {
	regsub -all {(::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?} $c {$&} c
	regsub -all {@([$](::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {[llength \1]} c
	regsub -all {#([$](::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {[string length \1]} c
	regsub -all {'[$]((::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {"\1"} c

	uplevel 1 [list expr $c]
}
proc ::Crpl::Translate_parens_expr c {
	regsub -all {(::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?} $c {$&} c
	regsub -all {@([$](::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {[llength \1]} c
	regsub -all {#([$](::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {[string length \1]} c
	regsub -all {'[$]((::)?[A-Za-z_]([:A-Za-z0-9_]*[A-Za-z0-9_])?)} $c {"\1"} c
	return $c
}


defmacro ::Crpl::Imp_unk_parens {c args} {

	# macro cauz we want `(x<0) break'

	set x [::Crpl::Translate_parens_expr $c]

	defmacro ::$c args [subst { 
	    if {\[llength \$args]} {
		set cond \[ uplevel 1 [list expr [list $x] ] ]
		if { \$cond=="" || \$cond==0 } {
		   set z {}
		} else {
		   set z \[uplevel 1 \$args]
		}
	    } else {
	    	set z \[uplevel 1 [list expr [list $x]]]
	    }
	   set z
	}]

	uplevel 1 [list $c] $args
}


proc ::Crpl::Imp_unk_tilde {c args} {
	# not a macro!  explicitly does the catch

	set f [string range $c 1 end]

	append body " set z {} ; "
	append body " catch { set z \[ uplevel 1 [list [list $f]] \$args ] } ; "
	append body " return \$z "
	Crpl::audit_proc ::$c args $body

	uplevel 1 [list $c] $args
}
proc ::Crpl::Imp_unk_hat {c args} {
	# not a macro!  explicitly does the return -code return

	set f [string range $c 1 end]

	Crpl::audit_proc ::$c args " return -code return \[ uplevel 1 [list [list $f]] \$args ] "
	uplevel 1 [list $c] $args
}
proc ::Crpl::verbosely_eval {args} {
	log "<<<< $args"
	set v [catch {uplevel 1 $args} x]
	set ec $::errorCode
	set ei $::errorInfo
	switch -- $v {
	0	{
		  log ">>>> $x"
		  return $x
		}
	1	{ 
		  log ">>ERROR>> $x"
		  return -code error -errorinfo $ei -errorcode $ec $x
		}
	2	{
		  log ">>return>> $x"
		  return -code return $x
		}
	3	{
		  log ">>break>>"
		  return -code break
		}
	4	{
		  log ">>continue>>"
		  return -code continue
		}
	default	{
		  log ">>CODE($v)>>"
		  return -code $v
		}
	}
}
defmacro ::Crpl::Imp_unk_bquot {c args} {
	set f [string range $c 1 end]

	defmacro ::$c args " uplevel 1 ::Crpl::verbosely_eval [list [list $f]] \$args "

	uplevel 1 [list $c] $args
}
proc ::Crpl::Imp_unk_squot {c args} {
	set f [string range $c 1 end]

	proc ::$c args " ::Crpl::log \"==== \[ uplevel 1 [list [list $f]] \$args ]\" "

	uplevel 1 [list ::$c] $args
}
proc ::Crpl::Imp_unk_bang {c args} {
	set f [string range $c 1 end]

	Crpl::audit_proc ::$c args " set z \[uplevel 1 [list [list $f]] \$args ] ; expr { \$z==\"\" || \$z==0 } "

	uplevel 1 [list ::$c] $args
}
proc ::Crpl::Imp_unk_assert {c args} {

	if { ![string match @@* $c] && [info exists Crpl::skip_asserts] } {
		proc ::$c args {}
	} else {
		proc ::$c args " ::Crpl::do_assert [list $c] \$args"
		uplevel 1 [list ::$c] $args
	}
}
proc Crpl::do_assert {name cmd} {
	set z [uplevel 2 $cmd]
	if !$z {
	    if {[string match @@* $name]} {
		error "CHECK FAILED: [human [expr {$name=="@@" ? $cmd : $name}]]"
	    } else {
		error "ASSERTION FAILED: [human [expr {$name=="@" ? $cmd : $name}]]"
	    }
	}
}
proc ::Crpl::Imp_unk_apos {c args} {
	# not a macro
	set cmd [split $c ']

	Crpl::audit_proc ::$c args " uplevel 1 [list $cmd] \$args  "
	uplevel 1 [list $c] $args
}
proc ::Crpl::Imp_unk_comma {c args} {
	# not a macro
	regexp {^(.*)[,](.*)} $c 0 m f

	append body " set t \[ uplevel 1 [list [list $f]] \$args ] ; "
	append body " return \[ uplevel 1 [list [list $m]] \[list \$t] ] "

	Crpl::audit_proc ::$c args $body
	uplevel 1 [list $c] $args
}


######################################################
######################################################
######################################################

proc kc {z args} {set z} ;# kc == "K Combinator", returns first arg 

interp alias {} eq {} string equal
interp alias {} ne {} string compare
proc lt { a b } { expr { [ string compare $a $b ] < 0 } }	
proc le { a b } { expr { [ string compare $a $b ] <= 0 } }	
proc gt { a b } { expr { [ string compare $a $b ] > 0 } }	
proc ge { a b } { expr { [ string compare $a $b ] >= 0 } }	

interp alias {} eq- {} string equal -nocase
interp alias {} ne- {} string compare -nocase
proc lt- { a b } { expr { [ string compare -nocase $a $b ] < 0 } }	
proc le- { a b } { expr { [ string compare -nocase $a $b ] <= 0 } }	
proc gt- { a b } { expr { [ string compare -nocase $a $b ] > 0 } }	
proc ge- { a b } { expr { [ string compare -nocase $a $b ] >= 0 } }	

interp alias {} sm {} string match
interp alias {} sc {} string compare
interp alias {} sm- {} string match -nocase
interp alias {} sc- {} string compare -nocase

interp alias {} s2u {} string toupper
interp alias {} s2l {} string tolower
interp alias {} st {} string trim
interp alias {} stl {} string trimleft
interp alias {} str {} string trimright

interp alias {} sl {} string length
interp alias {} si {} string index
interp alias {} sr {} string range
interp alias {} sa {} append

interp alias {} l {} list
interp alias {} ll {} llength
interp alias {} li {} lindex
interp alias {} lr {} lrange
interp alias {} la {} lappend

interp alias {} ie {} info exists

proc 0 l {lindex $l 0}
proc 1 l {lindex $l 1}
proc 2 l {lindex $l 2}
proc 3 l {lindex $l 3}
proc 4 l {lindex $l 4}
proc 5 l {lindex $l 5}
proc 6 l {lindex $l 6}
proc 7 l {lindex $l 7}
proc 8 l {lindex $l 8}
proc 9 l {lindex $l 9}

proc 1- l {lrange $l 1 end}
proc 2- l {lrange $l 2 end}
proc 3- l {lrange $l 3 end}
proc 4- l {lrange $l 4 end}
proc 5- l {lrange $l 5 end}
proc 6- l {lrange $l 6 end}
proc 7- l {lrange $l 7 end}
proc 8- l {lrange $l 8 end}
proc 9- l {lrange $l 9 end}

proc -1 l {lindex $l end}
proc -2 l {lindex $l end-1}
proc -3 l {lindex $l end-2}
proc -4 l {lindex $l end-3}
proc -5 l {lindex $l end-4}
proc -6 l {lindex $l end-5}
proc -7 l {lindex $l end-6}
proc -8 l {lindex $l end-7}
proc -9 l {lindex $l end-8}

# def {name args body} - defines proc "name" with arguments "args"
#	(the last argument is the body of the procedure)
#
# Beginning an argument with certain characters causes special behavior:
#	^ promotes to the variable to the caller's scope
#	/ refers to the global variable by that name


proc def {name args} {
	set vars {}
	set pre {}

	set n [llength $args]
	set n_2 [expr {$n-2}]

	foreach x [lrange $args 0 $n_2] {
		switch -glob -- $x {
			^* {
				v=string range $x 1 end
				lappend vars __name__$v
				append pre " upvar 1 \$__name__$v $v ;;"
			}

			{[/:]*} {
				v=string range $x 1 end
				append pre "[list global $v];;"
			}

			* {
				lappend vars $x
			}
		}
	}

	set body [lindex $args end]

	############# add common globals
	# optimize -- only add globals if they seem to appear plainly in the body
	set z {}
	foreach x [set ::Crpl::common] {
		if [string match "*$x*" $body] {lappend z $x}
	}

	if [llength $z] {
		set pre "[concat global $z];;; $pre"
	}
	Crpl::audit_proc $name $vars ";;;$pre;;;$body"
}



#####################################################
#
#   single-letter var-contents commands.
#	Variable is always passed by name, as first arg.
#	Three types of variables:
#	  -- scalar: operates on entire contents.
#	  -- array: operates on contents at one slot
#	  -- property array: array contents is a key-value
#			list, and we operate on value at one key.
#
#	(q (query) and d (delete) actually operate one level back,
#		and do not work on scalar variables.)
#
#   They all try to fix nonexistant var/index/prop, treating
#	nonexistant as empty (or 0 for incr), rather than
#	throwing errors.
#
#   Summary of commands:
#	n: llength
#	r: read
#	w: write
#	a: append
#	p: lappend (push to end)
#	g: grab (pop from end)
#	i: incr (does not default to 1) (treats nonexistant as 0)
#	s: shift (pop from front)
#	q: query (list array or property names)
#	d: delete (all array or property entries)
#

proc ::Crpl::lower-global-upper-local-proc {name args body} {
	uplevel 1 [list proc $name $args $body]
	regsub -all {upvar #0} $body {upvar 1} body
	uplevel 1 [list proc [string toupper $name] $args $body]
}

# n -- llength of var contents
#	n scalVar
#	n arrVar index
#	n arrVar index prop
proc n a { 
  upvar #0 $a v
    llength [~set v]
}

proc nn {a k} { 
  upvar #0 $a v
    set i $k; llength [~set v($i)]
}

proc nnn {a k p} { 
  upvar #0 $a v
    set i $k; set j $p; array set w [~set v($i)]; llength [~set w($j)] 
}


# r -- read var contents
#	r scalVar
#	r arrVar index
#	r arrVar index prop
proc r a { 
  upvar #0 $a v
    ~set v
}

proc rr {a k} { 
  upvar #0 $a v
    ~set v($k)
}

proc rrr {a k p} { 
  upvar #0 $a v
    array set w [~set v($k)]; ~set w($p) 
}


# w -- write var contents
#	w scalVar value
#	w arrVar index value
#	w arrVar index prop value
proc w {a x} { 
  upvar #0 $a v
    set v $x
}

proc ww {a k x} { 
  upvar #0 $a v
    set v($k) $x
}

proc www {a k p x} { 
  upvar #0 $a v
    array set w [~set v($k)]; set w($p) $x; set v($k) [array get w]
}



# a -- append var contents
#	a scalVar value
#	a arrVar index value
#	a arrVar index prop value
proc a {a x} { 
  upvar #0 $a v
    append v $x
}

proc aa {a k x} { 
  upvar #0 $a v
    append v($k) $x
}

proc aaa {a k p x} { 
  upvar #0 $a v
    array set w [~set v($k)]; append w($p) $x; set v($k) [array get w]
}



# p -- lappend var contents ("p"ush)
#	p scalVar value
#	p arrVar index value
#	p arrVar index prop value
proc p {a x} { 
  upvar #0 $a v
    lappend v $x
}

proc pp {a k x} { 
  upvar #0 $a v
    lappend v($k) $x
}

proc ppp {a k p x} { 
  upvar #0 $a v
    array set w [~set v($k)]; lappend w($p) $x; set v($k) [array get w]
}


##	# i -- incr var contents
##	#	i scalVar value
##	#	i arrVar index value
##	#	i arrVar index prop value
proc i {a x} { 
  upvar #0 $a v
    if {[info exists v]} {incr v $x} else {set v $x}
}

proc ii {a k x} { 
  upvar #0 $a v
    if {[info exists v($k)]} {incr v($k) $x} else {set v($k) $x}
}

proc iii {a k p x} { 
  upvar #0 $a v
    array set w [~set v($k)]; if {[info exists w($p)]} {incr w($p) $x} else {set w($p) $x}; set v($k) [array get w]
}


proc g a { 
  upvar #0 $a v
	if {![info exists v]} return
    kc [lindex $v end] [set v [lrange $v 0 end-1]]
}

proc gg {a k} { 
  upvar #0 $a v
	if {![info exists v($k)]} return
    kc [lindex $v($k) end] [set v($k) [lrange $v($k) 0 end-1]]
}

proc s a { 
  upvar #0 $a v
	if {![info exists v]} return
    kc [lindex $v 0] [set v [lrange $v 1 end]]
}

proc ss {a k} { 
  upvar #0 $a v
	if {![info exists v($k)]} return
    kc [lindex $v($k) 0] [set v($k) [lrange $v($k) 1 end]]
}


proc qq {a} {
  upvar #0 $a v
	if {![array exists v]} return
	array names v
}

proc d {a} {
  upvar #0 $a v
	catch {unset v}
	set v(0) 0
	unset v(0)
}
proc dd {a k} {
  upvar #0 $a v
	catch {unset v($k)}
}

##	# g -- grab (pop) var contents, from end -- opposite of p
##	#	i scalVar 
##	#	i arrVar index 
##	#	i arrVar index prop 
##	proc g args { 
##	  upvar #0 [0 $args] v
##	  switch [llength $args] { 
##	    1 {n=llength $v; z=lindex $v end; v=lrange $v 0 [(n-2)]; return $z}
##	    2 {set i [lindex $args 1]; n=llength $v($i); z=lindex $v($i) end; set v($i) [lrange $v($i) 0 [(n-2)]]; return $z}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; 
##	                  n=llength $w($j); 
##	                  z=lindex $w($j) end; 
##	                  set w($j) [lrange $w($j) 0 [(n-2)]]; 
##	                  set v($i) [array get w]; 
##	                  return $z
##	      }
##	    default {error "bad#args"}
##	  }
##	}
##	
##	
##	# s -- shift var contents, from front
##	#	s scalVar 
##	#	s arrVar index 
##	#	s arrVar index prop 
##	proc s args { 
##	  upvar #0 [0 $args] v
##	  switch [llength $args] { 
##	    1 {n=llength $v; z=lindex $v 0; v=lrange $v 1 end; return $z}
##	    2 {set i [lindex $args 1]; n=llength $v($i); z=lindex $v($i) 0; set v($i) [lrange $v($i) 1 end]; return $z}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; 
##	                  n=llength $w($j); 
##	                  z=lindex $w($j) 0
##	                  set w($j) [lrange $w($j) 1 end]
##	                  set v($i) [array get w]; 
##	                  return $z
##	      }
##	    default {error "bad#args"}
##	  }
##	}
##	
##	# q -- (query) "array names" or property names
##	#	q arrVar 
##	#	q arrVar index 
##	proc q args {
##	  upvar #0 [0 $args] v
##	  switch [llength $args] { 
##	    1  {~array names v}
##	    2 {set i [lindex $args 1]; array set w [~set v($i)]; ~array names w}
##	    default {error "bad#args"}
##	  }
##	}
##	
##	# d -- delete var contents
##	#	i arrVar    -- make empty array, deleting any previous contents
##	#	i arrVar index       -- delete this array entry
##	#	i arrVar index prop  -- delete property
##	proc d args {
##	  upvar #0 [0 $args] v
##	  switch [llength $args] { 
##	    1 {
##	         catch {unset v}
##	         set v(0) 0
##	         unset v(0)
##	      }
##	    2 {set i [lindex $args 1]; ~unset v($i)}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; set w($j) ""; set v($i) [array get w]}
##	    default {error "bad#args"}
##	  }
##	}
##	


###########
# n -- llength of var contents
#	n scalVar
#	n arrVar index
#	n arrVar index prop
proc N a { 
  upvar 1 $a v
    llength [~set v]
}

proc NN {a k} { 
  upvar 1 $a v
    set i $k; llength [~set v($i)]
}

proc NNN {a k p} { 
  upvar 1 $a v
    set i $k; set j $p; array set w [~set v($i)]; llength [~set w($j)] 
}


# r -- read var contents
#	r scalVar
#	r arrVar index
#	r arrVar index prop
proc R a { 
  upvar 1 $a v
    ~set v
}

proc RR {a k} { 
  upvar 1 $a v
    ~set v($k)
}

proc RRR {a k p} { 
  upvar 1 $a v
    array set w [~set v($k)]; ~set w($p) 
}


# w -- write var contents
#	w scalVar value
#	w arrVar index value
#	w arrVar index prop value
proc W {a x} { 
  upvar 1 $a v
    set v $x
}

proc WW {a k x} { 
  upvar 1 $a v
    set v($k) $x
}

proc WWW {a k p x} { 
  upvar 1 $a v
    array set w [~set v($k)]; set w($p) $x; set v($k) [array get w]
}



# a -- append var contents
#	a scalVar value
#	a arrVar index value
#	a arrVar index prop value
proc A {a x} { 
  upvar 1 $a v
    append v $x
}

proc AA {a k x} { 
  upvar 1 $a v
    append v($k) $x
}

proc AAA {a k p x} { 
  upvar 1 $a v
    array set w [~set v($k)]; append w($p) $x; set v($k) [array get w]
}



# p -- lappend var contents ("p"ush)
#	p scalVar value
#	p arrVar index value
#	p arrVar index prop value
proc P {a x} { 
  upvar 1 $a v
    lappend v $x
}

proc PP {a k x} { 
  upvar 1 $a v
    lappend v($k) $x
}

proc PPP {a k p x} { 
  upvar 1 $a v
    array set w [~set v($k)]; lappend w($p) $x; set v($k) [array get w]
}


##	# i -- incr var contents
##	#	i scalVar value
##	#	i arrVar index value
##	#	i arrVar index prop value
proc I {a x} { 
  upvar 1 $a v
    if {[info exists v]} {incr v $x} else {set v $x}
}

proc II {a k x} { 
  upvar 1 $a v
    if {[info exists v($k)]} {incr v($k) $x} else {set v($k) $x}
}

proc III {a k p x} { 
  upvar 1 $a v
    array set w [~set v($k)]; if {[info exists w($p)]} {incr w($p) $x} else {set w($p) $x}; set v($k) [array get w]
}


proc G a { 
  upvar 1 $a v
	if {![info exists v]} return
    kc [lindex $v end] [set v [lrange $v 0 end-1]]
}

proc GG {a k} { 
  upvar 1 $a v
	if {![info exists v($k)]} return
    kc [lindex $v($k) end] [set v($k) [lrange $v($k) 0 end-1]]
}

proc S a { 
  upvar 1 $a v
	if {![info exists v]} return
    kc [lindex $v 0] [set v [lrange $v 1 end]]
}

proc SS {a k} { 
  upvar 1 $a v
	if {![info exists v($k)]} return
    kc [lindex $v($k) 0] [set v($k) [lrange $v($k) 1 end]]
}


proc QQ {a} {
  upvar 1 $a v
	if {![array exists v]} return
	array names v
}

proc D {a} {
  upvar 1 $a v
	catch {unset v}
	set v(0) 0
	unset v(0)
}
proc DD {a k} {
  upvar 1 $a v
	catch {unset v($k)}
}


##	# i -- incr var contents
##	#	i scalVar value
##	#	i arrVar index value
##	#	i arrVar index prop value
##	proc I args { 
##	  upvar 1 [0 $args] v
##	  switch [llength $args] { 
##	    2 {if {![info exists v]} {set v 0} ; incr v [1 $args]}
##	    3 {set i [lindex $args 1]; if {![info exists v($i)]} {set v($i) 0} ; incr v($i) [2 $args]}
##	    4 {set i [lindex $args 1]; set j [lindex $args 2]; k=3 $args; array set w [~set v($i)]; if {![info exists w($j)]} {set w($j) 0}; incr w($j) $k; set v($i) [array get w]}
##	    default {error "bad#args"}
##	  }
##	}
##	
##	# g -- grab (pop) var contents, from end -- opposite of p
##	#	i scalVar 
##	#	i arrVar index 
##	#	i arrVar index prop 
##	proc G args { 
##	  upvar 1 [0 $args] v
##	  switch [llength $args] { 
##	    1 {n=llength $v; z=lindex $v end; v=lrange $v 0 [(n-2)]; return $z}
##	    2 {set i [lindex $args 1]; n=llength $v($i); z=lindex $v($i) end; set v($i) [lrange $v($i) 0 [(n-2)]]; return $z}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; 
##	                  n=llength $w($j); 
##	                  z=lindex $w($j) end; 
##	                  set w($j) [lrange $w($j) 0 [(n-2)]]; 
##	                  set v($i) [array get w]; 
##	                  return $z
##	      }
##	    default {error "bad#args"}
##	  }
##	}
##	
##	
##	# s -- shift var contents, from front
##	#	s scalVar 
##	#	s arrVar index 
##	#	s arrVar index prop 
##	proc S args { 
##	  upvar 1 [0 $args] v
##	  switch [llength $args] { 
##	    1 {n=llength $v; z=lindex $v 0; v=lrange $v 1 end; return $z}
##	    2 {set i [lindex $args 1]; n=llength $v($i); z=lindex $v($i) 0; set v($i) [lrange $v($i) 1 end]; return $z}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; 
##	                  n=llength $w($j); 
##	                  z=lindex $w($j) 0
##	                  set w($j) [lrange $w($j) 1 end]
##	                  set v($i) [array get w]; 
##	                  return $z
##	      }
##	    default {error "bad#args"}
##	  }
##	}
##	
##	# q -- (query) "array names" or property names
##	#	q arrVar 
##	#	q arrVar index 
##	proc Q args {
##	  upvar 1 [0 $args] v
##	  switch [llength $args] { 
##	    1  {~array names v}
##	    2 {set i [lindex $args 1]; array set w [~set v($i)]; ~array names w}
##	    default {error "bad#args"}
##	  }
##	}
##	
##	# d -- delete var contents
##	#	i arrVar    -- make empty array, deleting any previous contents
##	#	i arrVar index       -- delete this array entry
##	#	i arrVar index prop  -- delete property
##	proc D args {
##	  upvar 1 [0 $args] v
##	  switch [llength $args] { 
##	    1 {
##	         catch {unset v}
##	         set v(0) 0
##	         unset v(0)
##	      }
##	    2 {set i [lindex $args 1]; ~unset v($i)}
##	    3 {set i [lindex $args 1]; set j [lindex $args 2]; array set w [~set v($i)]; set w($j) ""; set v($i) [array get w]}
##	    default {error "bad#args"}
##	  }
##	}


} ;# END $Header: /home/strick/github/spake-0/RCS/crpl4.tcl,v 1.1 2012/11/05 04:13:08 strick Exp $
