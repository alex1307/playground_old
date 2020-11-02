CREATE SCHEMA PLAYGROUND;
CREATE SCHEMA INFRASTRUCTURE;

CREATE USER playground WITH ENCRYPTED PASSWORD 'pw4play';
CREATE USER developer WITH ENCRYPTED PASSWORD 'pw4dev';

GRANT USAGE ON SCHEMA PLAYGROUND TO developer, playground;
GRANT USAGE ON SCHEMA INFRASTRUCTURE TO developer;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA PLAYGROUND TO playground, developer;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA INFRASTRUCTURE TO developer;

GRANT ALL ON ALL TABLES IN SCHEMA PLAYGROUND TO playground, developer;
GRANT ALL ON ALL TABLES IN SCHEMA INFRASTRUCTURE TO developer;

ALTER USER developer WITH SUPERUSER;