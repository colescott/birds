var exports = module.exports = {};

const util = require("./util.js");
const Lesson = require("./models/lesson");
const User = require("./models/user");

const aws = require("aws-sdk");

const s3bucket = new aws.S3({ params: { Bucket: process.env.AWS_BUCKET } });

s3bucket.createBucket();

const uploadLessonData = (lesson, data, cb) => {
    let params = { Key: `lessons/${lesson.id}`, Body: data };
    s3bucket.upload(params, (err) => cb(err));
};

const getLessonData = (lesson, cb) => {
    let params = { Key: `lessons/${lesson.id}` };
    s3bucket.getObject(params, function(err, data) {
        if (err)
            cb(err);
        else
            cb(null, data.Body.toString());
    });
};

const getLesson = (req, res) => {
    if (!util.validId(req.params.id)) {
        return util.error(res, "That lesson does not exist!", 400);
    }

    Lesson.findById(req.params.id, (err, lesson) => {
        if (err)
            return util.error(res, err);
        if (!lesson)
            return util.error(res, "That lesson does not exist!", 400);

        getLessonData({ id: lesson.id }, (err, data) => {
            if (err)
                return util.error(res, "There was a error when loading this lesson. Retry in a bit.");
            else
                return util.data(res, util.sterilizeLessonWithData(lesson, data));
        });
    });
};

const createLesson = (req, res) => {
    User.findById(req.user.id, (err, user) => {

        if (!user.permissions.editLessons)
            return util.error(res, "You do not have permissions.editLessons permission.", 401);

        if (!req.body.title)
            return util.error(res, "title required.", 400);
        if (!req.body.branch)
            return util.error(res, "branch required", 400);

        Lesson.findOne({ title: req.body.title, branch: req.body.branch }, (err, data) => {
            if (err)
                return util.error(res, err);
            if (data)
                return util.error(res, "That lesson already exists!", 400);

            let lesson = new Lesson({
                title: req.body.title,
                branch: req.body.branch,
                prerequisites: req.body.prerequisites || []
            });
            lesson.save((err, lessonModel) => {
                if (err)
                    return util.error(res, err);

                let lesson = util.sterilizeLesson(lessonModel);

                uploadLessonData({ id: lesson.id }, req.body.data || "This is a default lesson", (err) => {
                    if (err)
                        return util.error(res, err);
                    else
                        return util.data(res, lesson);
                });
            });
        });
    });
};

const setLessonData = (req, res) => {
    User.findById(req.user.id, (err, user) => {

        if (!user.permissions.editLessons)
            return util.error(res, "You do not have permissions.editLessons permission.", 401);

        Lesson.findOne({ _id: req.params.id }, (err, lesson) => {
            if (err)
                return util.error(res, err);
            if (!lesson)
                return util.error(res, "That lesson does not exist!", 400);

            const set = _.pick(req.body, ["title", "branch", "prerequisites"]);

            Lesson.findOneAndUpdate({ _id: req.params.id }, { $set: set }, (err) => {
                if (err)
                    return util.error(res, err);
            });

            if (req.body.data)
                uploadLessonData({ id: req.params.id }, req.body.data, (err) => {
                    if (err)
                        return util.error(res, err);
                    else
                        return util.data(res, util.sterilizeLesson(lesson));
                });
        });
    });
};

const getLessons = (req, res) => {
    Lesson.find({}, (err, lessons) => {
        if (err)
            return util.error(res, err);
        if (!lessons)
            return util.error(res, "There are no lessons in the database.");
        return util.data(res, lessons.map((lesson) => util.sterilizeLesson(lesson)));
    });
};

exports.getLesson = getLesson;
exports.createLesson = createLesson;
exports.getLessons = getLessons;
exports.setLessonData = setLessonData;
