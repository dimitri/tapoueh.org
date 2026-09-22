# Compose project name is `lrhub` (set in both compose files).
# Never use --remove-orphans: both files share the project.
export COMPOSE_IGNORE_ORPHANS := true
CORE := docker compose -f docker-compose.yml
PGL  := docker compose -f docker-compose.pglogical.yml

.PHONY: up run down clean pglogical-up pglogical-run pglogical-crash

up:
	$(CORE) up -d --wait

run:
	./run.sh core

down:
	$(CORE) --profile worker4 down
	$(PGL) down

clean:
	$(CORE) --profile worker4 down -v
	$(PGL) down -v

pglogical-up:
	$(PGL) up -d --build --wait

pglogical-run:
	./run.sh pglogical

# Optional: reproduces the pglogical crash on a partitioned target (takes ~1-2 minutes, see README)
pglogical-crash:
	./run.sh pglogical-crash
