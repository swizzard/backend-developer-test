#! /usr/bin/env bash

echo "running db migrations"
/root/.local/bin/migrate init "host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD"
/root/.local/bin/migrate migrate "host=$DB_HOST port=$DB_PORT dbname=$DB_NAME user=$DB_USER password=$DB_PASSWORD" /home/migrations
