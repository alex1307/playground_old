use admin
db.createUser(
        {
            user: 'reservation',
            pwd: 'pw4reservation',
            roles: [
                {
                    role: 'readWrite',
                    db: 'reservation'
                }
            ]
        }
);


db.createUser(
  {
    user: "devadmin",
    pwd: "devadmin",
    roles: [ { role: "root", db: "admin" } ]
  }
)