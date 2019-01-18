CREATE SCHEMA IF NOT EXISTS api;


CREATE TABLE IF NOT EXISTS api.locations (
  id SERIAL PRIMARY KEY,
  locationname VARCHAR(256),
  lat REAL,
  lon REAL
);

CREATE TABLE IF NOT EXISTS api.users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(256) UNIQUE,
  token VARCHAR(256) UNIQUE,
  username VARCHAR(256),
  age INT,
  location_id INT REFERENCES api.locations(id),
  canhost BOOL);

CREATE TABLE IF NOT EXISTS api.gametypes (
  id SERIAL PRIMARY KEY,
  gametypename VARCHAR(256) UNIQUE
);

CREATE TABLE IF NOT EXISTS api.gametypenumplayers (
  id SERIAL PRIMARY KEY,
  numplayers INT,
  gametype_id INT REFERENCES api.gametypes(id)
);

CREATE TABLE IF NOT EXISTS api.userinterests (
  id SERIAL PRIMARY KEY,
  user_id INT REFERENCES api.users(id),
  gametype_id INT REFERENCES api.gametypes(id)
);

CREATE TABLE IF NOT EXISTS api.gameinstances (
  id SERIAL PRIMARY KEY,
  gametype_id INT REFERENCES api.gametypes(id),
  user_id INT REFERENCES api.users(id),
  gamename VARCHAR(256) UNIQUE,
  active BOOL
);

CREATE TABLE IF NOT EXISTS api.gameplayers (
  id SERIAL PRIMARY KEY,
  gameinstance_id INT REFERENCES api.gameinstances(id),
  user_id INT REFERENCES api.users(id)
);

CREATE TABLE IF NOT EXISTS api.requests (
  id SERIAL PRIMARY KEY,
  inviter_id INT REFERENCES api.users(id),
  invitee_id INT REFERENCES api.users(id),
  invgame_id INT REFERENCES api.gameinstances(id),
  invstate VARCHAR(8)
);

DO LANGUAGE plpgsql
  $do$
  BEGIN
    IF NOT EXISTS (
      SELECT
      FROM pg_catalog.pg_roles
      WHERE rolname = 'public_role') THEN
      CREATE ROLE public_role nologin;
      GRANT USAGE ON SCHEMA api TO public_role;
    END IF;
    IF NOT EXISTS (
      SELECT
      FROM pg_catalog.pg_roles
      WHERE rolname = 'app_role') THEN
      CREATE ROLE app_role noinherit login password 'password';
      GRANT USAGE ON SCHEMA api TO app_role;
      GRANT ALL ON ALL TABLES IN SCHEMA api TO app_role;
      GRANT ALL ON ALL SEQUENCES IN SCHEMA api TO app_role;
      GRANT public_role TO app_role;
    END IF;
  END
  $do$;
