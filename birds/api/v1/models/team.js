const mongoose = require('mongoose');
const Schema = mongoose.Schema;

const Team = new Schema({
    name: String,
    teamnumber: Number,
    users: [{
        id: String,
        isAdmin: Boolean,
        isModerator: Boolean,
        moderates: [{
            branch: String
        }]
    }]
});

module.exports = mongoose.model('Team', Team);
