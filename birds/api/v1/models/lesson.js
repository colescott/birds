const mongoose = require('mongoose');
const Schema = mongoose.Schema;

const Lesson = new Schema({
    title: String,
    gDriveId: String,
    branch: String,
    prerequisites: [{
        id: String,
    }]
});

module.exports = mongoose.model('Lesson', Lesson);
