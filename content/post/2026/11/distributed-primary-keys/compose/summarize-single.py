#!/usr/bin/env python3
"""Median over the runs of each variant, from results/30-single-<variant>-run<n>.out."""
import glob, re, statistics, collections, sys
rows = collections.defaultdict(list)
# optional 2nd argument: result prefix and variants, e.g. "31-seqam" for the sequence access method build
PFX = sys.argv[2] if len(sys.argv) > 2 else "30-single"
for f in sorted(glob.glob(sys.argv[1] + "/" + PFX + "-*-run*.out")):
    v = re.search(PFX + r"-(.+)-run\d+\.out", f).group(1)
    phase, d = 1, {}
    for line in open(f):
        if line.startswith("== phase 2"): phase = 2; continue
        m = re.match(r"tps = ([\d.]+)", line)
        if m: d[f"tps{phase}"] = float(m.group(1))
        m = re.match(r"table_bytes \| (\d+)", line)
        if m: d["table"] = int(m.group(1))
        m = re.match(r"index_(\S+) \| (\d+)", line)
        if m and phase == 1: d.setdefault("idx", {})[m.group(1)] = int(m.group(2))
        m = re.match(r"wal_bytes \| (\d+) \| wal_fpi \| (\d+)", line)
        if m: d[f"wal{phase}"] = int(m.group(1)); d[f"fpi{phase}"] = int(m.group(2))
        m = re.match(r"idx_blks_hit\+read (\S+) \| (\d+) \| (\d+)", line)
        if m and m.group(1).endswith("pkey") or (m and m.group(1) == "t_pkey"): d[f"pkread{phase}"] = int(m.group(3))
        m = re.match(r"io_relation reads \| (\d+) \| writes \| (\d+)", line)
        if m: d["ioreads"] = int(m.group(1)); d["iowrites"] = int(m.group(2))
    rows[v].append(d)
def med(v, k): return statistics.median(r[k] for r in rows[v] if k in r)
print("variant | table MB | pk MB | companion MB | rows/s | WAL B/row | after checkpoint: WAL B/row | FPI | pk idx blocks read (500k)")
for v in ["seqstep","seqstep+","composite","uuidv4","uuidv4+","uuidv7","uuidv7+","snowflake","snowflake+","snowone","snowam","regionam","snowone1","snowam1"]:
    if v not in rows: continue
    pk = statistics.median(sum(x for k,x in r["idx"].items() if k.endswith("pkey")) for r in rows[v])
    co = statistics.median(sum(x for k,x in r["idx"].items() if not k.endswith("pkey")) for r in rows[v])
    print(f"{v} | {med(v,'table')/1e6:.0f} | {pk/1e6:.0f} | {co/1e6:.0f} | {med(v,'tps1')*100:,.0f} | {med(v,'wal1')/5e6:.0f} | {med(v,'wal2')/5e5:.0f} | {med(v,'fpi2'):,.0f} | {med(v,'pkread2'):,.0f}  (runs: {len(rows[v])}; tps phase1 spread {min(r['tps1'] for r in rows[v])*100:,.0f}-{max(r['tps1'] for r in rows[v])*100:,.0f})")
