# Zero-downtime major upgrade via logical replication - reproducible demo.
# Compose project: lrupg, host ports 5730-5739.
export COMPOSE_PROJECT_NAME := lrupg
DC := docker compose

.PHONY: all up run down clean distclean a b c b-up b-run c-up c-run verify

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

# part C: PostgreSQL 19 beta3 features
c-up:
	COMPOSE_PROFILES=c $(DC) up -d --wait
c-run:
	./run.sh 40 59
c: c-up c-run

all: clean a b c

down:
	COMPOSE_PROFILES=a,b,c $(DC) down --remove-orphans
clean:
	COMPOSE_PROFILES=a,b,c $(DC) down -v --remove-orphans

distclean: clean
	rm -f results/*.out

# run everything twice from clean and diff the normalised outputs
verify:
	./verify.sh
