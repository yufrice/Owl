var newUsers = [
    {
          user: 'Owl',
          pwd: 'Owl',
          roles: [
              {
                  role: 'readWrite',
                  db: 'Owl'
              }
          ]
      }
  ];
  
  var currentUsers = db.getUsers();
  if (currentUsers.length === newUsers.length) {
      quit();
  }
  db.dropAllUsers();
  
  for (var i = 0, length = newUsers.length; i < length; ++i) {
      db.createUser(newUsers[i]);
  }