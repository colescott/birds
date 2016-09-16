define({ "api": [
  {
    "type": "post",
    "url": "/auth/login",
    "title": "Login",
    "name": "Login",
    "group": "Auth",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "email",
            "description": "<p>Users email.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "password",
            "description": "<p>Users password.</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.token",
            "description": "<p>Auth token to use on subsequent requests</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.user",
            "description": "<p>User object</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.id",
            "description": "<p>Users id</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.email",
            "description": "<p>Users email</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.firstname",
            "description": "<p>Users firstname</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.lastname",
            "description": "<p>Users lastname</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.user.teamnumber",
            "description": "<p>Users teamnumber</p>"
          },
          {
            "group": "Success 200",
            "type": "Boolean",
            "optional": false,
            "field": "data.user.isAdmin",
            "description": "<p>If users is an admin of team</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"token\": \"correcthorsebatterystaple\",\n    \"user\": {\n      \"id\": \"FAKEIDORISIT\",\n      \"email\": \"cardinalbirdsdev@gmail.com\",\n      \"firstname\": \"CardinalBIRDS\",\n      \"lastname\": \"Dev Team\",\n      \"teamnumber\": 4159,\n      \"isAdmin\": true\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Auth"
  },
  {
    "type": "post",
    "url": "/auth/logout",
    "title": "Logout",
    "name": "Logout",
    "group": "Auth",
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.message",
            "description": "<p>Message</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"message\": \"Logged out successfully\"\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Auth"
  },
  {
    "type": "post",
    "url": "/teams",
    "title": "Create new team",
    "name": "Create_new_team",
    "group": "Teams",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "name",
            "description": "<p>Teams name.</p>"
          },
          {
            "group": "Parameter",
            "type": "Number",
            "optional": false,
            "field": "teamnumber",
            "description": "<p>Teams number.</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.team",
            "description": "<p>Team object</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.team.name",
            "description": "<p>Team name</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.team.teamnumber",
            "description": "<p>Team number</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.team.password",
            "description": "<p>Team password, 6 char long</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"team\": {\n      \"name\": \"CardinalBotics\",\n      \"teamnumber\": 4159,\n      \"password\": \"Iluvme\"\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Teams"
  },
  {
    "type": "get",
    "url": "/teams/:num",
    "title": "Get team by number",
    "name": "Get_team_by_number",
    "group": "Teams",
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.team",
            "description": "<p>Array of teams</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.team.name",
            "description": "<p>Team name</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.team.teamnumber",
            "description": "<p>Team number</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"team\": {\n      \"name\": \"CardinalBotics\",\n      \"teamnumber\": 4159\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Teams"
  },
  {
    "type": "get",
    "url": "/teams",
    "title": "Get list of teams",
    "name": "Get_teams",
    "group": "Teams",
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object[]",
            "optional": false,
            "field": "data.teams",
            "description": "<p>Array of teams</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.teams.name",
            "description": "<p>Team name</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.teams.teamnumber",
            "description": "<p>Team number</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"teams\": [{\n      \"name\": \"CardinalBotics\",\n      \"teamnumber\": 4159\n    },\n    {\n      \"name\": \"FireHawk Robotics\",\n      \"teamnumber\": 6000\n    }]\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Teams"
  },
  {
    "type": "put",
    "url": "/teams/:num/:action",
    "title": "Perform action on team",
    "name": "Perform_action_on_team",
    "group": "Teams",
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "authorization",
            "description": "<p>Authorization token with format &quot;Bearer {token}&quot;</p>"
          }
        ]
      }
    },
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "Object",
            "optional": true,
            "field": "user",
            "description": "<p>User when :action = &quot;addadmin&quot; or &quot;removeadmin&quot;</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.message",
            "description": "<p>Message</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"message\": \"Successfully removed admin\"\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Teams"
  },
  {
    "type": "get",
    "url": "/users/:id",
    "title": "Get user by id",
    "name": "Get_user",
    "group": "Users",
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.user",
            "description": "<p>User object</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.id",
            "description": "<p>Users id</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.email",
            "description": "<p>Users email</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.firstname",
            "description": "<p>Users firstname</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.lastname",
            "description": "<p>Users lastname</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.user.teamnumber",
            "description": "<p>Users teamnumber</p>"
          },
          {
            "group": "Success 200",
            "type": "Boolean",
            "optional": false,
            "field": "data.user.isAdmin",
            "description": "<p>If user is an admin of team NOTE: only returns this if logged in as user trying to get</p>"
          },
          {
            "group": "Success 200",
            "type": "Object[]",
            "optional": true,
            "field": "data.user.progress",
            "description": "<p>User progress NOTE: only returns this if logged in as user trying to get</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.progress.id",
            "description": "<p>Id of lesson</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.progress.state",
            "description": "<p>Progress of lesson</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"user\": {\n      \"id\": \"ILUVULESSTHAN3\",\n      \"email\": \"cardinalbirdsdev@gmail.com\",\n      \"firstname\": \"CardinalBIRDS\",\n      \"lastname\": \"Dev Team\",\n      \"teamnumber\": 4159,\n      \"isAdmin\": true,\n      \"progress\": [\n         {\n           \"id\": \"thisisalessonid\",\n           \"state\": \"complete\"\n         }\n       ]\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Users"
  },
  {
    "type": "get",
    "url": "/users",
    "title": "Get list of users",
    "name": "Get_users",
    "group": "Users",
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object[]",
            "optional": false,
            "field": "data.users",
            "description": "<p>Array of users</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.users.id",
            "description": "<p>Users id</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.users.email",
            "description": "<p>Users email</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.users.firstname",
            "description": "<p>Users firstname</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.users.lastname",
            "description": "<p>Users lastname</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.users.teamnumber",
            "description": "<p>Users teamnumber</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"users\": [{\n      \"id\": \"ILUVULESSTHAN3\",\n      \"email\": \"cardinalbirdsdev@gmail.com\",\n      \"firstname\": \"CardinalBIRDS\",\n      \"lastname\": \"Dev Team\",\n      \"teamnumber\": 4159\n    },\n    {\n      \"id\": \"THISISAFAKEID\",\n      \"email\": \"admin@team4159.org\",\n      \"firstname\": \"Admin\",\n      \"lastname\": \"Account\",\n      \"teamnumber\": 4159\n    }]\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Users"
  },
  {
    "type": "put",
    "url": "/users/:id/:action",
    "title": "Perform action on user",
    "name": "Perform_action_on_user",
    "group": "Users",
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "authorization",
            "description": "<p>Authorization token with format &quot;Bearer {token}&quot;</p>"
          }
        ]
      }
    },
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "id",
            "description": "<p>Id for lesson when :action = &quot;setprogress&quot;</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "state",
            "description": "<p>State for lesson when :action = &quot;setprogress&quot;</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.message",
            "description": "<p>Message</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"message\": \"Successfully deleted user\"\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Users"
  },
  {
    "type": "post",
    "url": "/users",
    "title": "Register user",
    "name": "Register",
    "group": "Users",
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "email",
            "description": "<p>Users email.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "password",
            "description": "<p>Users password.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "firstname",
            "description": "<p>Users first name.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": false,
            "field": "lastname",
            "description": "<p>Users last name.</p>"
          },
          {
            "group": "Parameter",
            "type": "Number",
            "optional": false,
            "field": "teamnumber",
            "description": "<p>Users team number.</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.user",
            "description": "<p>User object</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.id",
            "description": "<p>Users id</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.email",
            "description": "<p>Users email</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.firstname",
            "description": "<p>Users firstname</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.lastname",
            "description": "<p>Users lastname</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.user.teamnumber",
            "description": "<p>Users teamnumber</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"user\": {\n      \"id\": \"ILUVULESSTHAN3\",\n      \"email\": \"cardinalbirdsdev@gmail.com\",\n      \"firstname\": \"CardinalBIRDS\",\n      \"lastname\": \"Dev Team\",\n      \"teamnumber\": 4159\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Users"
  },
  {
    "type": "put",
    "url": "/users/:id",
    "title": "Set user values",
    "name": "Set_user_values",
    "group": "Users",
    "header": {
      "fields": {
        "Header": [
          {
            "group": "Header",
            "type": "String",
            "optional": false,
            "field": "authorization",
            "description": "<p>Authorization token with format &quot;Bearer {token}&quot;</p>"
          }
        ]
      }
    },
    "parameter": {
      "fields": {
        "Parameter": [
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "email",
            "description": "<p>Users new email.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "password",
            "description": "<p>Users new password.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "firstname",
            "description": "<p>Users new first name.</p>"
          },
          {
            "group": "Parameter",
            "type": "String",
            "optional": true,
            "field": "lastname",
            "description": "<p>Users new last name.</p>"
          },
          {
            "group": "Parameter",
            "type": "Number",
            "optional": true,
            "field": "teamnumber",
            "description": "<p>Users new team number.</p>"
          }
        ]
      }
    },
    "success": {
      "fields": {
        "Success 200": [
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data",
            "description": "<p>Data object containing info</p>"
          },
          {
            "group": "Success 200",
            "type": "Object",
            "optional": false,
            "field": "data.user",
            "description": "<p>User object</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.id",
            "description": "<p>Users id</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.email",
            "description": "<p>Users email</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.firstname",
            "description": "<p>Users firstname</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.lastname",
            "description": "<p>Users lastname</p>"
          },
          {
            "group": "Success 200",
            "type": "Number",
            "optional": false,
            "field": "data.user.teamnumber",
            "description": "<p>Users teamnumber</p>"
          },
          {
            "group": "Success 200",
            "type": "Object[]",
            "optional": false,
            "field": "data.user.progress",
            "description": "<p>User progress</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.progress.id",
            "description": "<p>Id of lesson</p>"
          },
          {
            "group": "Success 200",
            "type": "String",
            "optional": false,
            "field": "data.user.progress.state",
            "description": "<p>Progress of lesson</p>"
          }
        ]
      },
      "examples": [
        {
          "title": "Success-Response:",
          "content": "HTTP/1.1 200 OK\n{\n  \"data\": {\n    \"user\": {\n      \"id\": \"ILUVULESSTHAN3\",\n      \"email\": \"cardinalbirdsdev@gmail.com\",\n      \"firstname\": \"CardinalBIRDS\",\n      \"lastname\": \"Dev Team\",\n      \"teamnumber\": 4159,\n      \"progress\": [\n         {\n           \"id\": \"thisisalessonid\",\n           \"state\": \"complete\"\n         }\n       ]\n    }\n  }\n}",
          "type": "json"
        }
      ]
    },
    "version": "0.0.0",
    "filename": "birds/api/v1/index.js",
    "groupTitle": "Users"
  }
] });
