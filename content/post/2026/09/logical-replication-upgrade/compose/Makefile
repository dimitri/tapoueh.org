# Zero-downtime major upgrade via logical replication - reproducible demo.
# Compose project: lrupg, host ports 5730-5739.
export COMPOSE_PROJECT_NAME := lrupg
DC := docker compose

.PHONY: all up run down clean distclean a b c d b-up b-run c-up c-run d-up d-run verify

# part A: 16 -> 18 with logical replication, rollback path
up:
	COMPOSE_PROFILES=a $(DC) up -d --wait
run:
	./run.sh 10 29
a: up run

# part B: pg_upgrade 17 -> 18 preserving subscriptions and slots
b-up:
	COMPOSE_PROFILES=b $(DC) up -d --build --wait
b-run:
	./run.sh 30 39
b: b-up b-run

# part C: PostgreSQL 19 beta4 features
c-up:
	COMPOSE_PROFILES=c $(DC) up -d --wait
c-run:
	./run.sh 40 47
c: c-up c-run

# part D: the cutover behind pgbouncer, zero client reconfiguration
d-up:
	COMPOSE_PROFILES=d $(DC) up -d --wait
d-run:
	./run.sh 50 59
d: d-up d-run

all: clean a b c d

down:
	COMPOSE_PROFILES=a,b,c,d $(DC) down --remove-orphans
clean:
	COMPOSE_PROFILES=a,b,c,d $(DC) down -v --remove-orphans

distclean: clean
	rm -f results/*.out

# run everything twice from clean and diff the normalised outputs
verify:
	./verify.sh
