# The five-region topology of the post: four regions that own rows, one warehouse,
# row-filtered publications, failover-enabled subscriptions through every node of the
# publishing group. Source lib-regions.sh first.

P() { echo "$1" | tr - _; }                   # us-east -> us_east
pq() { rpq "$(region_primary "$1")" "$2"; }   # run sql on a region's primary

topo_schema() { local r; for r in "$@"; do pq "$r" "$(cat sql/c-01-schema.sql)" >/dev/null; done; }

# topo_publications <region>...: the publications of those regions, and with the pseudo-region
# ap-south-to-ap-southeast the one ap-south serves to ap-southeast.
# pub_to_eu_west must stay row-filtered to the regions its publisher feeds: left
# unfiltered, the warehouse's own mirror, relayed back down to this region, would ride
# straight back out to the warehouse the moment it lands, an echo loop.
topo_publications() {
  local r
  for r in "$@"; do
    case $r in
      us-east)
        pq us-east      "create publication pub_to_eu_west for table events where (region = 'us-east' or region = 'us-west')"
        pq us-east      "create publication pub_to_us_west for table events where (region <> 'us-west')" ;;
      us-west)
        pq us-west      "create publication pub_to_us_east for table events where (region = 'us-west')" ;;
      ap-south)
        pq ap-south     "create publication pub_to_eu_west for table events where (region = 'ap-south' or region = 'ap-southeast')" ;;
      ap-south-to-ap-southeast)   # the link to the fifth region, created apart so a region can join later
        pq ap-south     "create publication pub_to_ap_southeast for table events where (region <> 'ap-southeast')" ;;
      ap-southeast)
        pq ap-southeast "create publication pub_to_ap_south for table events where (region = 'ap-southeast')" ;;
      eu-west)
        pq eu-west      "create publication pub_to_us_east for table events where (region <> 'us-east' and region <> 'us-west')"
        pq eu-west      "create publication pub_to_ap_south for table events where (region <> 'ap-south' and region <> 'ap-southeast')" ;;
    esac
  done
}

# sub <subscriber> <publisher> <publication> [copy_data]: a subscription is named by both
# endpoints, because the replication slot it creates on the publisher carries its name
# and must be unique there
sub() {
  pq "$1" "create subscription sub_$(P $1)_from_$(P $2) connection '$(rconninfo $2)' publication $3 with (failover = true, copy_data = ${4:-true})" >/dev/null
}

# topo_subscriptions: the eight subscriptions of the five regions
topo_subscriptions() {
  sub us-west      us-east      pub_to_us_west
  sub us-east      us-west      pub_to_us_east
  sub ap-southeast ap-south     pub_to_ap_southeast
  sub ap-south     ap-southeast pub_to_ap_south
  sub eu-west      us-east      pub_to_eu_west
  sub us-east      eu-west      pub_to_us_east
  sub eu-west      ap-south     pub_to_eu_west
  sub ap-south     eu-west      pub_to_ap_south
}

# topo_wait_sync <region>...: every table of every subscription of those regions is ready
topo_wait_sync() {
  local i r n pending
  for i in $(seq 1 120); do
    pending=0
    for r in "$@"; do
      n=$(pq "$r" "select count(*) from pg_subscription_rel where srsubstate <> 'r'")
      pending=$((pending + ${n:-1}))
    done
    [ "$pending" = 0 ] && return 0; sleep 1
  done
  return 1
}
