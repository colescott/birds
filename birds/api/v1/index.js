const express = require('express');
const mongoose = require('mongoose');
const passport = require('passport');
const LocalStrategy = require('passport-local').Strategy;
const jwt = require('jsonwebtoken');
const expressJwt = require('express-jwt');
const url  = require('url');
const router = express.Router();

const User = require('./models/user');

const jwtSecret = process.env.JWT_SECRET;

mongoose.Promise = global.Promise;

router.use(passport.initialize());

const ejwt = expressJwt({secret: jwtSecret});

const authenticate = (req, res, next) => {
    ejwt(req, res, (err) => {
        if(err && err.code && err.code == 'invalid_token')
            return invalidToken(res);
        if(err && err.code && err.code == 'credentials_required')
            return noSession(res);
        next();
    });
};

passport.use(new LocalStrategy({
        usernameField: 'email'
    }, (username, password, done) => {
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

router.get("/ping", (req, res) => {
    return res.send("Pong v1!");
});

router.post("/users", (req, res) => {
    if(!req.body.email)
    {
        return res.send(error("Email value required."));
    }
    var usr = new User({
        email: req.body.email,
        firstname: req.body.firstname,
        lastname: req.body.lastname,
        teamnumber: req.body.teamnumber,
        progress: []
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

router.get("/users/:id", authenticate, (req, res) => {
    User.findById(req.params.id, (err, user) =>
    {
        if(err)
            return res.send(err);
        var val = {};
        val.user = sterilizeUser(user);
        if(req.params.id == req.user.id)
            val.user.progress = user.progress;
        return res.send(data(val));
    });
});

router.put("/users/:id", authenticate, (req, res) => {
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

//Options for mongoose
const options = {};
options.new = true;

router.put("/users/:id/:action", authenticate, (req, res) => {
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
            break;
        case "setprogress":
            if(!req.body.id)
                return res.status(400).send(error("Id not set!"));
            if(!req.body.state)
                return res.status(400).send(error("State not set!"));

            var found = false;

            user.progress.forEach( (obj) => {
                if(found)
                    return;
                if(obj.id == req.body.id)
                {
                    found = true;

                    User.update({'progress.id': req.body.id}, {'$set': {
                        'progress.$.state': req.body.state
                    }}, options, (err, user) => {
                        if(err)
                            return res.send(error(err));
                        return res.send(data({message: "successfully set progress"}));
                    });
                }
            });
            if(found)
                return;

            User.findByIdAndUpdate(req.user.id, {$push: {"progress": {id: req.body.id, state: req.body.state}}}, options, (err, user) => {
                if(err)
                    return res.send(error(err));
                return res.send(data({message: "successfully set progress"}));
            });
            break;
        case "resetprogress":
            User.findByIdAndUpdate(req.user.id, {progress: []}, options, (err, user) => {
                if(err)
                    return res.send(error(err));
                return res.send(data({message: "successfully reset progress"}));
            });
            break;
        default:
            return res.status(400).send(error("Action not found"));
        }

    });
});

router.post('/auth/login', function(req, res, next) {
    passport.authenticate('local', {
        session: false
    }, function(err, user, info) {
        if (err) return next(err);
        if (!user) {
            return unauthorized(res);
        } else {
            var val = {};
            val.token = jwt.sign({id: user.id}, jwtSecret, { expiresIn: 2 * 60 * 60 });
            val.user = sterilizeUser(user);
            return res.send(data(val));
        }
    })(req, res, next);
});

router.post('/auth/logout', authenticate, (req, res) => {
    req.logout();
    return res.send(data({message: "Logged out successfully"}));
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

const invalidToken = (res) => {
    res.status(401).send(error("Invalid token."))
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
