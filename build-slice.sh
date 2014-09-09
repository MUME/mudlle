# arguments: <version> <slice> <slices>

set -e

v=$1
shift
./mudlle $* <<-EOF
	silent = true
	garbage_collect(1024*1024)
	load("${v}.mud")
	load("pxslow.mud")
EOF
