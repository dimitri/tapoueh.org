#!/usr/bin/env python3
"""Regenerate the pg_auto_failover region services of docker-compose.yml.

Every region is one pg_auto_failover group of its own: a monitor and three
Postgres nodes (<region>-a, -b, -c). Run it after changing REGIONS.
"""
import re, sys

REGIONS = {                       # region -> compose profiles that use it
    "us-east":       ["a", "c", "f", "g"],
    "us-west":       ["c", "f", "g"],
    "ap-south":      ["c"],
    "ap-southeast":  ["c"],
    "eu-west":       ["c"],
}

def region(r, profiles):
    prof = "[" + ", ".join(profiles) + "]"
    out = []
    mon = f"{r}-monitor"
    build = ""
    if r == "us-east":
        build = """    build:
      context: .
      dockerfile: Dockerfile.pgautofailover
      args: { PGVERSION: "17" }
"""
    out.append(f"""  {mon}:
    <<: *pgaf
    profiles: {prof}
{build}    hostname: {mon}
    command: ["create", "monitor", "--pgdata", "/data/pgdata", "--auth", "trust",
              "--ssl-self-signed", "--hostname", "{mon}", "--run"]
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -h 127.0.0.1 -U postgres"]
      interval: 1s
      timeout: 3s
      retries: 90
""")
    for n in "abc":
        node = f"{r}-{n}"
        dep = f"""    depends_on:
      {mon}: {{ condition: service_healthy }}
""" if n == "a" else f"""    depends_on:
      {r}-a: {{ condition: service_healthy }}
"""
        hc = """    healthcheck:
      test: ["CMD-SHELL", "pg_isready -h 127.0.0.1 -U postgres -d app"]
      interval: 1s
      timeout: 3s
      retries: 120
""" if n == "a" else ""
        out.append(f"""  {node}:
    <<: *pgaf
    profiles: {prof}
    hostname: {node}
{dep}    command: ["create", "postgres", "--pgdata", "/data/pgdata", "--auth", "trust",
              "--ssl-self-signed", "--pg-hba-lan", "--hostname", "{node}", "--name", "{node}",
              "--dbname", "app",
              "--monitor", "postgres://autoctl_node@{mon}/pg_auto_failover?sslmode=require",
              "--run"]
{hc}""")
    return "\n".join(out)

block = """x-pgaf: &pgaf
  image: lrdist-pgaf:17
  environment:
    PGDATA: /data/pgdata
  tmpfs: ["/data:uid=999,gid=999,mode=0755"]

"""
svc = "  # ---------------- regions: one pg_auto_failover group each (profiles a, c, f, g) ----------------\n"
for r, p in REGIONS.items():
    svc += region(r, p) + "\n"
svc += """  # the application and the CDC consumer: psql, pg_recvlogical, perl
  client:
    profiles: [a, f, g]
    image: lrdist-pgaf:17
    entrypoint: ["sleep", "infinity"]
    user: root
    volumes:
      - ./traffic:/traffic:ro
      - ./results:/results

  # profile g: pgcopydb carrying a one-way link between two regions, DDL included
  pgcopydb-g:
    profiles: [g]
    build:
      context: .
      dockerfile: Dockerfile.pgcopydb
      args: { PGVERSION: "17", PGCOPYDB_PATCH: pgcopydb-failover-slot.patch }
    image: lrdist-pgcopydb:17
    entrypoint: ["sleep", "infinity"]
    volumes:
      - ./results:/results

"""

src = open("docker-compose.yml").read()
head = src[:src.index("services:\n") + len("services:\n")]
head = re.sub(r"x-pgaf:.*?\n\n", "", head, flags=re.S)
head = head.replace("services:\n", block + "services:\n")
keep = open("docker-compose.de.part").read()   # profiles d and e, maintained by hand
volumes = "volumes:\n  d-source-data:\n  d-target-data:\n"
open("docker-compose.yml", "w").write(head + svc + keep + volumes)
