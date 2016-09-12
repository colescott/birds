const express = require("express");
const mongoose = require('mongoose');
const passport = require('passport');
const session = require('express-session');
const LocalStrategy = require('passport-local').Strategy;
const router = express.Router();

var User = require('./models/user');

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
        error("Email value required.");
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
            return res.send({
                id: user.id,
                email: user.email,
                firstname: user.firstname,
                lastname: user.lastname,
                teamnumber: user.teamnumber
            });
        });
    });
});

router.get("/users", (req, res) => {
    User.find({},function(err, users){
        const usrs = users.map( user => ({
            id: user.id,
            email: user.email,
            firstname: user.firstname,
            lastname: user.lastname,
            teamnumber: user.teamnumber
        }));
        return res.send({data: usrs});
    });
});

router.get("/users/:id", (req, res) => {
    User.findById(req.params.id, (err, user) =>
    {
        if(err)
            return res.send(err);
        return res.send({
            id: user.id,
            email: user.email,
            firstname: user.firstname,
            lastname: user.lastname,
            teamnumber: user.teamnumber
        });
    });
});

router.put("/users/:id", (req, res) => {
    if(!req.isAuthenticated())
    {
        return res.send(error("You need to be logged in to do this."));
    }
    if(req.user.id != req.params.id)
    {
        return res.send(error("You do not have the required permisions to do this."));
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
            return res.send({
                id: user.id,
                email: user.email,
                firstname: user.firstname,
                lastname: user.lastname,
                teamnumber: user.teamnumber
            });
        });
    else
        User.findById(req.params.id, (err, user) => {
            if(err)
                return res.send(err);
            return res.send({
                id: user.id,
                email: user.email,
                firstname: user.firstname,
                lastname: user.lastname,
                teamnumber: user.teamnumber
            });
        });
});

router.put("/users/:id/:action", (req, res) => {
    if(!req.isAuthenticated())
    {
        return res.send(error("You need to be logged in to do this."));
    }
    if(req.user.id != req.params.id)
    {
        return res.send(error("You do not have the required permisions to do this."));
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
                return res.send({data: {message: "successfully deleted user."}});
            });
        }

    });
});

router.post('/auth/login', passport.authenticate('local'), (req, res) => {
    return res.send({data: {
        id: req.user.id,
        email: req.user.email,
        firstname: req.user.firstname,
        lastname: req.user.lastname,
        teamnumber: req.user.teamnumber
    }});
});

router.post('/auth/logout', (req, res) => {
    req.logout();
    req.session.destroy(function() {
        res.clearCookie('connect.sid');
        return res.send({data: {message: "Logged out successfully"}});
    });
});

const error = (message) => {
    return {error: {message: message}};
};

module.exports = router;
