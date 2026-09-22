# Consolidation demo. Compose project: lrcons, host ports 5720-5724.
COMPOSE = docker compose -p lrcons -f docker-compose.yml

.PHONY: up run down clean
up:
	$(COMPOSE) up -d --wait

run:
	./run.sh

down:
	$(COMPOSE) --profile standby down

clean:
	$(COMPOSE) --profile standby down -v --remove-orphans
	rm -f results/*.out
