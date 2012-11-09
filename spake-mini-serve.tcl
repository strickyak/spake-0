# Simple Sample httpd/1.0 server in 250 lines of Tcl
# Stephen Uhler / Brent Welch (c) 1996 Sun Microsystems
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# This is a working sample httpd server written entirely in TCL with the
# CGI and imagemap capability removed.  It has been tested on the Mac, PC
# and Unix.  It is intended as sample of how to write internet servers in
# Tcl. This sample server was derived from a full-featured httpd server,
# also written entirely in Tcl.
# Comments or questions welcome (stephen.uhler@sun.com)

# Httpd is a global array containing the global server state
#  root:	the root of the document directory
#  port:	The port this server is serving
#  listen:	the main listening socket id
#  accepts:	a count of accepted connections so far

# HTTP/1.0 error codes (the ones we use)

source crpl4.tcl

proc HANDLE/foo {w} {
  return "<html>Foo Handler. <br>Hello. &amp;</html>"
}

proc HANDLE/cls p {
  set z "<ul>\n"
  global M
  foreach k [array names M] {
    switch -glob $k {
      cls/* {
        set kk [split $k /]
        set c [1 $kk]
	append z "<li> <a href=\"/cls/$c\">$c</a>\n"
      }
    }
  }
  append z "</ul>\n"
  set z
}

array set HttpdErrors {
    204 {No Content}
    400 {Bad Request}
    404 {Not Found}
    503 {Service Unavailable}
    504 {Service Temporarily Unavailable}
    }

array set Httpd {
    bufsize	32768
    sockblock	0
}

# Start the server by listening for connections on the desired port.

proc Httpd_Server {root {port 80} {default index.html}} {
    global Httpd

    array set Httpd [list root $root default $default]
    if {![info exists Httpd(port)]} {
	set Httpd(port) $port
	set Httpd(listen) [socket -server HttpdAccept $port]
	set Httpd(accepts) 0
    }
    return $Httpd(port)
}

# Accept a new connection from the server and set up a handler
# to read the request from the client.

proc HttpdAccept {newsock ipaddr port} {
    global Httpd
    upvar #0 Httpd$newsock data

    incr Httpd(accepts)
    fconfigure $newsock -blocking $Httpd(sockblock) \
	-buffersize $Httpd(bufsize) \
	-translation {auto crlf}
    Httpd_Log $newsock Connect $ipaddr $port
    set data(ipaddr) $ipaddr
    fileevent $newsock readable [list HttpdRead $newsock]
}

# read data from a client request

proc HttpdRead { sock } {
    upvar #0 Httpd$sock data

    if {[info exists data(state)] && $data(state) == "query"} {
      set line [read $sock]
      set readCount [string length $sock]
    } else {
      set readCount [gets $sock line]
    }
    puts stderr "LINE($readCount)$line"
    if {![info exists data(state)]} {
	if [regexp {(POST|GET) ([^?]+)\??([^ ]*) HTTP/1} \
		$line x data(proto) data(url) data(query)] {
	    set data(state) mime
	    Httpd_Log $sock Query $line
	} else {
	    puts stderr "Httpd_Log $sock Error bad first line:$line"
	    HttpdError $sock 400
	    Httpd_Log $sock Error "bad first line:$line"
	    HttpdSockDone $sock
	}
	return
    }

    # string compare $readCount 0 maps -1 to -1, 0 to 0, and > 0 to 1

    set state [string compare $readCount 0],$data(state),$data(proto)
    puts stderr "STATE $state ($readCount)"
    switch -- $state {
	0,mime,GET	-
	0,query,POST	{ HttpdRespond $sock }
	//-1,query,POST	{ 
	                   HttpdRespond $sock }
	0,mime,POST	{ set data(state) query }
	1,mime,POST	-
	1,mime,GET	{
	    if [regexp {([^:]+):[ 	]*(.*)}  $line dummy key value] {
		set data(mime,[string tolower $key]) $value
	    }
	}
	1,query,POST	{
	    set data(query) $line
	    HttpdRespond $sock
	}
	default {
	    if [eof $sock] {
		Httpd_Log $sock Error "unexpected eof on <$data(url)> request"
	    } else {
		Httpd_Log $sock Error "unhandled state <$state> fetching <$data(url)>"
	    }
	    HttpdError $sock 404
	    HttpdSockDone $sock
	}
    }
}

# Close a socket.
# We'll use this to implement keep-alives some day.

proc HttpdSockDone { sock } {
    upvar #0 Httpd$sock data
    unset data
    close $sock
}

# Respond to the query.

proc ShowQuery sock {
  global Httpd
  upvar #0 Httpd$sock data

  puts stderr =====================
  parray Httpd
  puts stderr =====================
  parray data
  puts stderr =====================

  puts $sock "HTTP/1.0 200 OKAY"
  puts $sock "Content-Type: text/html"
  puts $sock ""
  # fconfigure $sock -translation binary -blocking $Httpd(sockblock)
  flush $sock

  puts $sock "dont Panic <br>"

  puts $sock "<dl>"
  foreach {k v} [array get data] {
    puts $sock "<dt>$k <dd>$v"
  }
  puts $sock "</dl>"
  flush $sock
  close $sock

}

proc HttpdRespond { sock } {
    global Httpd
    upvar #0 Httpd$sock data

    puts stderr "data(url) -> $data(url)"

    if [catch {

	    if {$data(url) == "/"} {
	      return [ShowQuery $sock]
	    }

	    #if {$data(url) == "/favicon.ico"} {
	    #  HttpdSockDone  $sock
	    #  return
	    #}

	    set cmd [lindex [split $data(url) /] 1]
	    puts DU---$data(url)
	    puts CMD---$cmd
	    HANDLE/$cmd $sock

    } what] {
	puts $sock "HTTP/1.0 200 OKAY"
	puts $sock "Content-Type: text/html"
	puts $sock ""
	puts $sock "<t1>ERROR: [Html $what]"
	puts $sock "<pre>"
	puts $sock "[Html $::errorInfo]"
	puts $sock "</pre>"
    } else {
	puts $sock "HTTP/1.0 200 OKAY"
	puts $sock "Content-Type: text/html"
	puts $sock ""
	puts $sock "$what"
    }
    flush $sock
    HttpdSockDone  $sock
    return
    -------------------------------------------

    set mypath [HttpdUrl2File $Httpd(root) $data(url)]
    puts stderr "mypath -> $mypath"

    if {[string length $mypath] == 0} {
	HttpdError $sock 400
	Httpd_Log $sock Error "$data(url) invalid path"
	HttpdSockDone $sock
	return
    }

    if {![catch {open $mypath} in]} {
	puts $sock "HTTP/1.0 200 Data follows"
	puts $sock "Date: [HttpdDate [clock clicks]]"
	puts $sock "Last-Modified: [HttpdDate [file mtime $mypath]]"
	puts $sock "Content-Type: [HttpdContentType $mypath]"
	puts $sock "Content-Length: [file size $mypath]"
	puts $sock ""
	fconfigure $sock -translation binary -blocking $Httpd(sockblock)
	fconfigure $in -translation binary -blocking 1
	flush $sock
#	copychannel $in $sock $Httpd(bufsize)
	copychannel $in $sock
	HttpdSockDone $sock
#	fileevent $sock writable [list HttpdCopy $in $sock $mypath]
    } else {
	HttpdError $sock 404
	Httpd_Log $sock Error "$data(url) $in"
	HttpdSockDone $sock
    }
}
proc HttpdCopy {in sock mypath} {
    global Httpd
    copychannel $in $sock $Httpd(bufsize)
    if [eof $in] {
	close $in
	HttpdSockDone $sock
	Httpd_Log $sock Done "$mypath"
    }
}

# convert the file suffix into a mime type
# add your own types as needed

array set HttpdMimeType {
    {}		text/plain
    .txt	text/plain
    .htm	text/html
    .html	text/html
    .gif	image/gif
    .jpg	image/jpeg
    .xbm	image/x-xbitmap
}

proc Html s {
  regsub -all {[&]} $s {\&amp;} s
  regsub -all {[""]} $s {\&quot;} s
  regsub -all {[<]} $s {\&lt;} s
  regsub -all {[>]} $s {\&gt;} s
  return $s
}

proc HttpdContentType {path} {
    global HttpdMimeType

    set type text/plain
    catch {set type $HttpdMimeType([file extension $path])}
    return $type
}

# Generic error response.

set HttpdErrorFormat {
    <title>Error: %1$s</title>
    Got the error: <b>%2$s</b><br>
    while trying to obtain <b>%3$s</b>
}

proc HttpdError {sock code} {
    upvar #0 Httpd$sock data
    global HttpdErrors HttpdErrorFormat

    append data(url) ""
    set message [format $HttpdErrorFormat $code $HttpdErrors($code)  $data(url)]
    puts $sock "HTTP/1.0 $code $HttpdErrors($code)"
    puts $sock "Date: [HttpdDate [clock clicks]]"
    puts $sock "Content-Length: [string length $message]"
    puts $sock ""
    puts $sock $message

    flush $sock
    error "HttpdErrorHttpdErrorHttpdErrorHttpdError"
}

# Generate a date string in HTTP format.

proc HttpdDate {clicks} {
    return [clock format $clicks -format {%a, %d %b %Y %T %Z}]
}

# Log an Httpd transaction.
# This should be replaced as needed.

proc Httpd_Log {sock reason args} {
    global httpdLog httpClicks
    if {[info exists httpdLog]} {
	if ![info exists httpClicks] {
	    set last 0
	} else {
	    set last $httpClicks
	}
	set httpClicks [clock clicks]
	puts $httpdLog "[clock format [clock seconds]] ([expr $httpClicks - $last])\t$sock\t$reason\t[join $args { }]"
    }
}

# Convert a url into a pathname.
# This is probably not right.

proc HttpdUrl2File {root url} {
    global HttpdUrlCache Httpd

    if {![info exists HttpdUrlCache($url)]} {
    	lappend pathlist $root
    	set level 0
	foreach part  [split $url /] {
	    set part [HttpdCgiMap $part]
	    if [regexp {[:/]} $part] {
		return [set HttpdUrlCache($url) ""]
	    }
	    switch -- $part {
		.  { }
		.. {incr level -1}
		default {incr level}
	    }
	    if {$level <= 0} {
		return [set HttpdUrlCache($url) ""]
	    } 
	    lappend pathlist $part
	}
    	set file [eval file join $pathlist]
	if {[file isdirectory $file]} {
	    set file [file join $file $Httpd(default)]
	}
    	set HttpdUrlCache($url) $file
    }
    return $HttpdUrlCache($url)
}

# Decode url-encoded strings.

proc HttpdCgiMap {data} {
    regsub -all {([][$\\])} $data {\\\1} data
    regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
    return [subst $data]
}

proc bgerror {msg} {
    global errorInfo
    puts stderr "bgerror: $msg\n$errorInfo"
}
proc copychannel {in out {size 4096}} {
  fcopy $in $out -size $size
}

if 1 {
array set M [read [open m.txt]]
parray M

Httpd_Server $env(HOME)/public_html [lindex $argv 0] index.html
puts stderr "Starting Tcl httpd server on [info hostname] port 8080"
vwait forever		;# start the Tcl event loop
}
