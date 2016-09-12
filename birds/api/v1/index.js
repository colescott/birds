const express = require('express');
const mongoose = require('mongoose');
const passport = require('passport');
const LocalStrategy = require('passport-local').Strategy;
const jwt = require('jsonwebtoken');
const ejwt = require('express-jwt');
const url  = require('url');
const router = express.Router();

const User = require('./models/user');

const jwtSecret = process.env.JWT_SECRET;

// Paths not requiring auth. Path is string or regex
const unprotectedPaths = [
    {path: '/auth/login', method: 'POST'},
    {path: '/users', method: 'POST'},
    {path: '/users', method: 'GET'},
    {path: /\/users\/\w+/igm, method: 'GET'}
]

const filter = (req) => {
    var found = false;
    unprotectedPaths.forEach( (obj) => {
        if(req.method == obj.method)
        {
            if(typeof obj.path === 'string')
            {
                if(req.url == obj.path)
                    found = true;
            } else {
                if(obj.path.test(req.url) !== null)
                    found = true;
            }
        }
    });
    return found;
}

router.use(ejwt({secret: jwtSecret, userProperty: 'tokenPayload', getToken: function fromHeaderOrQuerystring (req) {
    if (req.get('tokenPayload')) {
        return JSON.parse(req.get('tokenPayload')).token;
    } else if (req.query && req.query.token) {
        return req.query.token;
    }
        return null;
    }
}).unless(filter));

router.use(function(req, res, next) {
    if(filter(req))
        return next();
    if (req.tokenPayload) {
        getUserById(req.tokenPayload.id, (err, user) => {
            req.user = user;
            if (req.user) {
                return next();
            } else {
                return unauthorized(res);
            }
        });
    } else {
        return noSession(res);
    }
});

// Error handler middleware
router.use(function(err, req, res, next) {
    console.error(err);
    if(err.code == "credentials_required")
        return noSession(res);
    return res.status(err.status || 500).send(err);
});

passport.use(new LocalStrategy((username, password, done) => {
    User.authenticate()(username, password, (err, user, passErr) => {
        if(err)
            return done(err);
        if(passErr)
            return done(null, false, passErr);
        if(user)
        {
            done(null, user);
        }
    });
}));

// use static serialize and deserialize of model for passport session support
passport.serializeUser(User.serializeUser());
passport.deserializeUser(User.deserializeUser());

router.get("/ping", (req, res) => {
    return res.send("Pong v1!");
});

router.post("/users", (req, res) => {
    if(!req.body.email)
    {
        res.send(error("Email value required."));
    }
    var usr = new User({
        email: req.body.email,
        firstname: req.body.firstname,
        lastname: req.body.lastname,
        teamnumber: req.body.teamnumber,
    });
    User.register(usr, req.body.password, (err, thisModel, passwordErr) => {
        if(err)
            return res.send(err);
        User.findById(thisModel._id, (err, user) =>
        {
            if(err)
                return res.send(err);
            var val = {};
            val.user = sterilizeUser(user);
            return res.send(data(val));
        });
    });
});

router.get("/users", (req, res) => {
    User.find({},function(err, users){
        const usrs = users.map( user => (sterilizeUser(user)));
        var val = {};
        val.users = usrs;
        return res.send(data(val));
    });
});

router.get("/users/:id", (req, res) => {
    User.findById(req.params.id, (err, user) =>
    {
        if(err)
            return res.send(err);
        var val = {};
        val.user = sterilizeUser(user);
        return res.send(data(val));
    });
});

router.put("/users/:id", (req, res) => {
    if(!req.isAuthenticated())
    {
        return noSession(res);
    }
    if(req.user.id != req.params.id)
    {
        return unauthorized(res);
    }

    const changes = {};
    if(req.body.email)
        changes.email = req.body.email;
    if(req.body.firstname)
        changes.firstname = req.body.firstname;
    if(req.body.lastname)
        changes.lastname = req.body.lastname;
    if(req.body.teamnumber)
        changes.teamnumber = req.body.teamnumber;

    if(req.body.password)
    {
        User.findById(req.params.id, (err, user) =>
        {
            if(err)
            {
                return res.send(err);
            }
            user.setPassword(req.body.password, (err, thisModel, passwordErr) => {
                if(err)
                {
                    return res.send(err);
                }
                if(passwordErr)
                {
                    return res.send(passwordErr);
                }
            });
        });
    }

    const options = {};
    options.new = true;

    if(changes != {})
        User.findByIdAndUpdate(req.params.id, changes, options, (err, user) => {
            if(err)
                return res.send(err);
            var val = {};
            val.user = sterilizeUser(user);
            return res.send(data(val));
        });
    else
        User.findById(req.params.id, (err, user) => {
            if(err)
                return res.send(err);
            return res.send(data({user: sterilizeUser(user)}));
        });
});

router.put("/users/:id/:action", (req, res) => {
    if(!req.isAuthenticated())
    {
        return noSession(res);
    }
    if(req.user.id != req.params.id)
    {
        return unauthorized(res);
    }
    User.findById(req.params.id, (err, user) => {
        if(err)
            return res.send(err);

        switch(req.params.action)
        {
        case "delete":
            User.findByIdAndRemove(req.params.id, (err) => {
                if(err)
                    return res.send(err);
                return res.send(data({message: "successfully deleted user."}));
            });
        }

    });
});

router.post('/auth/login', function(req, res, next) {
    passport.authenticate('local', function(err, user, info) {
        if (err) return next(err);
        if (!user) {
            return unauthorized(res);
        } else {
            var val = {};
            val.token = jwt.sign({id: user.id}, jwtSecret, { expiresIn: 18000 /* 60 * 60 * 5 */ });
            val.user = sterilizeUser(user);
            return res.send(data(val));
        }
    })(req, res, next);
});

router.post('/auth/logout', (req, res) => {
    if(!req.isAuthenticated())
    {
        return noSession(res);
    }

    req.logout();
    req.session.destroy(function() {
        res.clearCookie('connect.sid');
        return res.send(data({message: "Logged out successfully"}));
    });
});

const getUserById = (id, cb) => {
    User.findById(id, (err, user) =>
    {
        if(err)
            return cb(er, null);
        return cb(null, sterilizeUser(user));
    });
};

const sterilizeUser = (user) => {
    return {
        id: user.id,
        email: user.email,
        firstname: user.firstname,
        lastname: user.lastname,
        teamnumber: user.teamnumber
    };
};

const noSession = (res) => {
    res.status(401).send(error("You need to be logged in to do this."))
};

const unauthorized = (res) => {
    res.status(401).send(error("Unauthorized"))
};

const error = (message) => {
    return {error: {message: message}};
};

const data = (data) => {
    return {data: data}
};

module.exports = router;
