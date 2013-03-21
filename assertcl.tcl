############################################################
# Program: assertcl.tcl
# Author: Cris A. Fugate
# Written: November 23, 2009
# 
# Assert extenion for Tcl based on work of the same name
# done by Jon Cook.
############################################################
# Variables
#
# action - fail action
# aline - line from assertlines
# allvars - all accessible variables
# anames - sorted list of array names
# args - assertcl args (all, proc list, or var list)
# assertcount - always test count used in the proc name.
# assertlines - new lines containing returnAssert (replacing assure)
# assertvars - list of enabled variables
# command - assertcl command (enable or disable)
# elema - list element
# expr - return expression
# exp - test expression
# flag - default error flag
# i - loop variable
# items - list of variable names
# line - pbody line
# lines - pbody split by new line
# lista - first input list
# listb - second input list
# listx - first working list
# listy - second working list
# lst - list to test
# ndx - string index
# ndx2 - string index
# newlines - pbody lines missing assure statements
# op - assertcl option (all, proc, or var)
# p - procedure
# pargs - procedure arguments
# pbody - procedure body
# pbody1 - left part of procedure body
# pbody2 - right part of procedure body
# plines - final pbody lines
# pname - procedure name
# result - result of test evaluation
# test - assertion test
# v - loop variable
# vars - initial procedure variables
# vname - array of variable names
############################################################
# Procedures
#
# always - setup variable trace and test procedures
# assert - handle point assertion
# assertcl - switch between enabled and disabled procedures
# assume - handle entry assertion for disabled procedures
# assure - handle return assertion for disabled procedures
# compress - sort and remove duplicates from a list (a set)
# difference - get difference between two sets
# enable_proc - create enabled and disabled procedures
# lall - universal list test
# lexists - existential list test
# procAssert - handle entry assertion for enabled procedures
# process_assure - process assure for enabled procedures
# rall - universal array test
# returnAssert - handle return assertion for enabled procedures
# rexists - existential array test
############################################################
# Directions
#
# assert <expr> [failure-action] [-default_also]
# If <expr> fails then throw an error unless [fail-action] is
# specified. [-default_also] throws an error after the execution
# of [fail-action].
#
# assume <expr> [failure-action] [-default_also]
# Functions like assert in an enabled procedure except that it
# has access to the initial state of input variables as *_in.
#
# assure <expr> [failure-action] [-default_also]
# Functions like assume except that it has access to the return
# value as return_val in addition to the *_in variables.
#
# always <varlist> <expr> [failure-action] [-default_also]
# Attach each enabled var in <varlist> to a test procedure which 
# functions like an assert when those variables are set.
#
# assertcl <enable> <all> [proc|var] 
# If <all> then enable all <proc> or <var> as specified or all 
# proc and var if not specified.
#
# assertcl <enable> <proc|var> <proclist|varlist>
# If <proc> then enable <proclist>. If <var> then enable <varlist>.
#
# lall <items> <list> <expr>
# Given variables (applied in order) listed in <items> evaluate 
# <expr> over each element of <list>. It returns 1 if all evaluate
# to true, 0 otherwise. Ex. lall {x y} $list {$x < $y} tests that 
# the list is ordered in increasing order.
#
# lexists <items> <list> <expr>
# Given variables (applied in order) listed in <items> evaluate
# <expr> over each element of <list>. It returns 1 if any evaluate
# to true, 0 otherwise.
#
# rall <items> <array> <expr> [lsort flags]
# Given variables (applied in order) listed in <items> evaluate
# <expr> over each element in array named <array>. The <lsort flags>
# will determine the order of the array names if specified. It
# returns 1 if all evaluate to true, 0 otherwise.
# Ex. rall {x y} arr {$arr($x) < $arr($y)} {-ascii -increasing}
# tests that the elements of sorted array names are in increasing
# order.
# 
# rexists <items> <list> <expr> [lsort flags]
# Functions like rall except that it returns 1 is any test evaluation
# is true, 0 otherwise.
############################################################

# initialize variables
set assertcount 0
set assertvars {}
 
# order list and remove duplicates
proc compress {lista} {
    upvar $lista listx
    set listx [lsort $listx]
    set listy [lindex $listx 0]
    set elema $listy
    foreach i $listx {
        if {$elema != $i} {
            lappend listy $i
        }
    set elema $i
    }
    set listx $listy
}

# get difference between compressed lists
proc difference {lista listb} {
    set listx {}
    compress lista
    compress listb
    foreach i $lista {
	if {[lsearch $listb $i] == -1} {
	    lappend listx $i
	}
    }
    return $listx
}

# assert is not enabled or disabled
proc assert {test {action {}} {flag ""}} {
    if {[catch {
	set result [uplevel "expr $test"]
    }]} {
	# expression error
	return -code error "assert expression error - $test"
    }
    if {$result == 0} {
	if {[string equal $action ""] || [string equal $action "-default_also"]} {
	    # throw general error
	    return -code error "assert error"
	} else {
	    if {[catch {
		uplevel "$action"
	    }]} {
		# action error
		return -code error "assert failure_action error - $action"
	    }
	}
    }
    if {![string equal $flag ""]} { 
	if {[string equal $flag "-default_also"]} {
	    # throw general error
	    return -code error "assert error"
	} else {
	    # flag error
	    return -code error "assert option error - $flag"
	}
    }
}

# disabled assume
proc assume {test {action {}} {flag ""}} {
}

# create enabled and disabled procs
proc enable_proc {pname} {
    set pargs [info args $pname]
    set pbody [info body $pname]
    rename $pname "$pname\_orig"
    
    # get vars
    set vars $pargs
    set ndx 0
    while {$ndx >= 0} {
	set ndx [string first "global" $pbody $ndx]
	if {$ndx >= 0} {
	    set ndx2 [string first "\n" $pbody $ndx]
	    append vars [string range $pbody [expr $ndx + 6] [expr $ndx2 - 1]]
	    incr ndx 6
	}
    }

    # look for last global
    set ndx [string last "global" $pbody]
    # find insertion point
    if {$ndx >= 0} {
        set ndx [string first "\n" $pbody $ndx]
        set pbody1 [string range $pbody 0 $ndx]
        set pbody2 [string range $pbody [expr $ndx + 1] end]
    } else {
        set pbody1 ""
        set pbody2 $pbody
    }
    # insert in variables
    foreach v $vars {
        append pbody1 "set $v\_in \$$v\n"
    }
    set pbody "$pbody1$pbody2"

    # replace all assume with assert
    set ndx 0
    while {$ndx >= 0} {
	set ndx [string first "assume" $pbody $ndx]
	if {$ndx >= 0} {
	    set pbody [string replace $pbody $ndx [expr $ndx + 5] "procAssert"]
	}
    }
    # process all assure
    set pbody [process_assure $pbody]
    # create procedure
    proc $pname $pargs $pbody
}


# enabled assume
proc procAssert {test {action {}} {flag ""}} {
    if {[catch {
	set result [uplevel "expr $test"]
    }]} {
	# expression error
	return -code error "assume expression error - $test"
    }
    if {$result == 0} {
	if {[string equal $action ""] || [string equal $action "-default_also"]} {
	    # throw general error
	    return -code error "assume error"
	    
	} else {
	    if {[catch {
		uplevel "$action"
	    }]} {
		# action error
		return -code error "assume failure_action error - $action"
	    }
	}
    }
    if {![string equal $flag ""]} { 
	if {[string equal $flag "-default_also"]} {
	    # throw general error
	    return -code error "assume error"
	} else {
	    # flag error
	    return -code error "assume option error - $flag"
	}
    }
}




# disabled assure
proc assure {test {action {}} {flag ""}} {
}

# process assure
proc process_assure {pbody} {
    # remove assure
    set lines [split $pbody \n]
    foreach line $lines {
	set ndx [string first "assure" $line]
	if {$ndx >= 0} {
	    # replace assure with returnAssert?
	    set line [string replace $line $ndx [expr $ndx + 5] "returnAssert"]
	    # store in assertlines
	    lappend assertlines $line
	} else {
	    lappend newlines $line
	}
    }
    
    # replace return
    foreach line $newlines {
	set line [string trimleft $line]
	set ndx [string first "return" $line]
	if {$ndx == 0} {
	    # insert result_val
	    set expr [string range $line 6 end] 
	    lappend plines "set return_val $expr"
	    foreach aline $assertlines {
		lappend plines $aline
	    }
	    lappend plines $line
	} else {
	    lappend plines $line
	}
    }
    set pbody [join $plines \n]
    return $pbody
}

# enabled assure
proc returnAssert {test {action {}} {flag ""}} {
    if {[catch {
	set result [uplevel "expr $test"]
    }]} {
	# expression error
	return -code error "assure expression error - $test"
    }
    if {$result == 0} {
	if {[string equal $action ""]} {
	    # throw general error
	    return -code error "assure error"
	} else {
	    if {[catch {
		uplevel "$action"
	    }]} {
		# action error
		return -code error "assure failure_action error - $action"
	    }
	}
    }
    if {![string equal $flag ""]} {
	if {[string equal $flag "-default_also"]} {
	    # throw general error
	    return -code error "assure error"
	} else {
	    # flag error
	    return -code error "assure option error - $flag"
	}
    }
}

# create variable trace and test procedure
proc always {varlist test {action {}} {flag ""}} {
    global assertcount

    incr assertcount
    set pname "always_test$assertcount"
    set pargs "vname ndx op"
    set pbody "global assertvars;set test {$test}; set action {$action}; set flag \"$flag\"; set enabled 1;if {\[lsearch -exact \$assertvars \$vname] == -1} {set enabled 0};if {\$enabled == 1} {set result \[uplevel \"expr \$test\"];if {\$result == 0} {if {\[string equal \$action \"\"]} {error \"always error\"} else {uplevel \$action};if {\[string equal \$flag \"-default_also\"]} {error \"always error\"}}}" 
    proc $pname $pargs $pbody
    foreach v $varlist {
	uplevel "trace variable $v w $pname"
    }
}


# enable/disable procedures and variables
proc assertcl {command op args} {
    global assertvars

    # check command
    set allvars [uplevel "info vars"]
    if {![string equal $command "enable"] && ![string equal $command "disable"]} {
	error "assertcl command error - $command"
    }
    # check option
    if {![string equal $op "all"] && ![string equal $op "proc"] && ![string equal $op "var"]} {
	error "assertcl option error - $op"
    }

    if {[string equal $command "disable"]} {
	if {[string equal $op "all"]} {
	    # disable all proc and var
	    if {[string equal $args ""]} {
		# disable all proc
		foreach p [info procs] {
		    if {![string equal [info procs "$p\_orig"] ""]} {
			rename $p "$p\_new"
			rename "$p\_orig" $p
		    }
		}
		set assertvars {}
	    } 
	    # disable all proc
	    if {[string equal $args "proc"]} {
		# disable all proc
		foreach p [info procs] {
		    if {![string equal [info procs "$p\_orig"] ""]} {
			rename $p "$p\_new"
			rename "$p\_orig" $p
		    }
		}
	    }
	    # disable all var
	    if {[string equal $args "var"]} {
		set assertvars {}
	    }
	}
	# disable specified proc
	if {[string equal $op "proc"]} {
	    foreach p $args {
		if {![string equal [info procs "$p\_orig"] ""]} {
		    rename $p "$p\_new"
		    rename "$p\_orig" $p
		}
	    }
	}
	# disable specified var
	if {[string equal $op "var"]} {
	    set assertvars [difference $assertvars $args]
	}
    }
    if {[string equal $command "enable"]} {
	if {[string equal $op "all"]} {
	    # enable all proc and var
	    if {[string equal $args ""]} {
		# enable all proc
		set procs [info procs]
		foreach p $procs {
		    if {![string equal [info procs "$p\_new"] ""]} {
			rename $p "$p\_orig"
			rename "$p\_new" $p
		    } else {
			enable_proc $p
		    }
		}
		# enable all variable
		set assertvars $allvars
	    } 
	    # enable all proc
	    if {[string equal $args "proc"]} {
		# enable all proc
		set procs [info procs]
		foreach p $procs {
		    if {![string equal [info procs "$p\_new"] ""]} {
			rename $p "$p\_orig"
			rename "$p\_new" $p
		    } else {
			enable_proc $p
		    }
		}
	    }
	    # enable all var
	    if {[string equal $args "var"]} {
		set assertvars $allvars
	    }
	}
	# enable specified proc
	if {[string equal $op "proc"]} {
	    foreach p $args {
		if {![string equal [info procs "$p\_new"] ""]} {
		    rename $p "$p\_orig"
		    rename "$p\_new" $p
		} else {
		    enable_proc $p
		}
	    }
	}
	# enable specified var
	if {[string equal $op "var"]} {
	    lappend assertvars $args
	}
    }
}

# lall <items> <list> <expr>
proc lall {items lst exp} {
    set result 1
    # set var names from items
    for {set i 0} {$i < [llength $items]} {incr i} {
	set vname($i) [lindex $items $i]
    }
    # setup variables
    for {set i 0} {$i <= [expr [llength $lst] - [llength $items]]} {incr i} {
	for {set j 0} {$j < [llength $items]} {incr j} {
	    set $vname($j) [lindex $lst [expr $i + $j]]
	}
	# apply test
	set result [expr $result && $exp]
    }
    return $result
}

# lexists <items> <list> <expr>
proc lexists {items lst exp} {
    set result 0
    # set var names from items
    for {set i 0} {$i < [llength $items]} {incr i} {
	set vname($i) [lindex $items $i]
    }
    # setup variables
    for {set i 0} {$i <= [expr [llength $lst] - [llength $items]]} {incr i} {
	for {set j 0} {$j < [llength $items]} {incr j} {
	    set $vname($j) [lindex $lst [expr $i + $j]]
	}
	# apply test
	set result [expr $result || $exp]
    }
    return $result
}

# rall <items> <array> <expr> <flags>
proc rall {items ary exp {flags ""}} {
    set result 1
    # sort array names
    if {![string equal $flags ""]} {
	set anames [lsort [lindex $flags 0] [lindex $flags 1] [array names $ary]]
    }
    # set var names from items
    for {set i 0} {$i < [llength $items]} {incr i} {
	set vname($i) [lindex $items $i]
    }
    # setup variables
    for {set i 0} {$i <= [expr [llength $anames] - [llength $items]]} {incr i} {
	for {set j 0} {$j < [llength $items]} {incr j} {
	    set $vname($j) [lindex $anames [expr $i + $j]]
	}
	# apply test
	set result [expr $result && $exp]
    }
    return $result
}

# rexists <items> <array> <expr> <flags>
proc rexists {items ary exp {flags ""}} {
    set result 0
    # sort array names
    if {![string equal $flags ""]} {
    	set anames [lsort [lindex $flags 0] [lindex $flags 1] [array names $ary]]
    }
    # set var names from items
    for {set i 0} {$i < [llength $items]} {incr i} {
	set vname($i) [lindex $items $i]
    }
    # setup variables
    for {set i 0} {$i <= [expr [llength $anames - [llength $items]]} {incr i} {
	for {set j 0} {$j < [llength $items]} {incr j} {
	    set $vname($j) [lindex $anames [expr $i + $j]]
	}
	# apply test
	set result [expr $result || $exp]
    }
    return $result
}

