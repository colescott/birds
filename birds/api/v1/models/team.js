const mongoose = require("mongoose");
const Schema = mongoose.Schema;

const Team = new Schema({
    name: String,
    teamnumber: Number,
    password: String,
    users: [
        {
            id: String,
            isAdmin: Boolean,
            isModerator: Boolean,
            moderates: [
                {
                    branch: String
                }
            ]
        }
    ]
});

Team.query.byNumber = function(teamnumber) {
    return this.find({ teamnumber: teamnumber });
};

// cb is (err, bool)
Team.statics.containsUser = function(teamnumber, user) {
    return new Promise((resolve, reject) => {
        this.findOne().byNumber(teamnumber).exec((err, data) => {
            if (err) return reject(err);
            if (data.length <= 0) return reject("That team does not exist.");
            if (!data.users) return resolve(false);
            data.users.forEach(usr => {
                if (usr.id == user.id) return resolve(true);
            });
            return resolve(false);
        });
    });
};

Team.statics.userIsAdmin = function(teamnumber, user) {
    return new Promise((resolve, reject) => {
        this.findOne().byNumber(teamnumber).exec((err, data) => {
            if (err) return reject(err);
            if (data.length <= 0) return reject("That team does not exist.");
            let done = false;
            data[0].users.forEach(usr => {
                if (usr.id == user.id) {
                    done = true;
                    resolve(usr.isAdmin);
                }
            });
            if (!done) return resolve(false);
        });
    });
};

Team.statics.numberOfAdmins = function(teamnumber) {
    return new Promise((resolve, reject) => {
        this.findOne().byNumber(teamnumber).exec((err, data) => {
            if (err) return reject(err);
            if (data.length <= 0) return reject("That team does not exist.");
            let adminNum = 0;
            data[0].users.forEach(usr => {
                if (usr.isAdmin) adminNum++;
            });
            return resolve(adminNum);
        });
    });
};

// cb is (err)
Team.statics.setAdmin = function(teamnumber, user, isAdmin) {
    return new Promise((resolve, reject) => {
        this.findOne().byNumber(teamnumber).update({ "users.id": user.id }, {
            $set: {
                "users.$.isAdmin": isAdmin
            }
        }, err => {
            if (err) return reject(err);
            return resolve();
        });
    });
};

Team.statics.exists = function(teamnumber) {
    return new Promise((resolve, reject) => {
        this.findOne().byNumber(teamnumber).exec((err, data) => {
            if (err) return reject(err);
            if (data.length <= 0) return resolve(false);
            return resolve(true);
        });
    });
};

Team.statics.addUser = function(teamnumber, user) {
    return new Promise(async (resolve, reject) => {
        const exists = await this.containsUser(teamnumber, user);
        if (exists) return resolve();
        this.findOne({ teamnumber: teamnumber }).update(
            { $push: { users: { id: user.id, isAdmin: false } } },
            err => {
                if (err) return reject(err);
                return resolve();
            }
        );
    });
};

module.exports = mongoose.model("Team", Team);
