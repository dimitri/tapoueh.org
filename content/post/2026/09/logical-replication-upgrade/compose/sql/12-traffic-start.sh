# @service: traffic
# Step 12: start the "application" against the OLD server. It keeps running
# (one transaction per loop turn, all timestamps logged) until step 24.
docker_bg() { setsid nohup "$@" >/dev/null 2>&1 < /dev/null & }
docker_bg /work/traffic.sh old
sleep 5
echo "target: $(cat /tmp/target)"
echo "transactions logged in 5s: $(wc -l < /tmp/traffic.log)"
echo "distinct statuses: $(awk '{print $3, $4}' /tmp/traffic.log | sort | uniq -c | tr '\n' ';')"
tail -3 /tmp/traffic.log | cut -d' ' -f2-
