INSERT INTO api.gametypes (gametypename) VALUES ('Chess') ON CONFLICT DO NOTHING;
INSERT INTO api.gametypes (gametypename) VALUES ('Settlers of Catan') ON CONFLICT DO NOTHING;
INSERT INTO api.gametypes (gametypename) VALUES ('Risk') ON CONFLICT DO NOTHING;

INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (2, (SELECT id FROM api.gametypes WHERE gametypename = 'Chess')) ON CONFLICT DO NOTHING;
INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (3, (SELECT id FROM api.gametypes WHERE gametypename = 'Settlers of Catan')) ON CONFLICT DO NOTHING;
INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (4, (SELECT id FROM api.gametypes WHERE gametypename = 'Settlers of Catan')) ON CONFLICT DO NOTHING;
INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (3, (SELECT id FROM api.gametypes WHERE gametypename = 'Risk')) ON CONFLICT DO NOTHING;
INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (4, (SELECT id FROM api.gametypes WHERE gametypename = 'Risk')) ON CONFLICT DO NOTHING;
INSERT INTO api.gametypenumplayers (numplayers, gametype_id)
  VALUES (5, (SELECT id FROM api.gametypes WHERE gametypename = 'Risk')) ON CONFLICT DO NOTHING;
