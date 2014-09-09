set -e

# Two iterations using type inference seems to infer most
# (user-facing) return types correctly.
for v in xc icxc icxc; do
    ./mudlle <<-EOF
	garbage_collect(1024*1024)
	load("${v}.mud")
	load("pxslow.mud")
EOF
done
