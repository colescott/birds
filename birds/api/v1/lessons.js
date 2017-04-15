const express = require("express");
const router = express.Router();

const util = require("./util.js");
const { authenticate, errorWrapper } = require("./middleware.js");
const Lesson = require("./models/lesson");
const User = require("./models/user");

const _ = require("lodash");

const aws = require("aws-sdk");
const s3params = { Bucket: process.env.AWS_BUCKET };
const s3bucket = new aws.S3({ params: s3params });

const uploadLessonData = async (lesson, data) => {
    return new Promise( (resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`,
            Body: data
        };
        s3bucket.upload(params, (err) => {
            if (err)
                reject(err);
            else
                resolve();
        });
    });
};

const getLessonData = async (lesson) => {
    return new Promise( (resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`
        };
        s3bucket.getObject(params, function(err, data) {
            if (err)
                reject(err);
            else
                resolve(data.Body.toString());
        });
    });
};

/**
 * @api {post} /lessons Create lesson
 * @apiName Create lesson
 * @apiGroup Lessons
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} title Lesson title
 * @apiParam {String} branch Lesson branch
 * @apiParam {String} [data] Lesson data
 * @apiParam {String} [data.prerequisites] Lesson prerequisites
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.id Lesson id
 * @apiSuccess {String} data.title Lesson title
 * @apiSuccess {String} data.branch Lesson branch
 * @apiSuccess {String} data.prerequisites Lesson prerequisites
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "id": "4159<3u",
 *         "title": "CAD Basics",
 *         "branch": "design",
 *         "prerequisites": []
 *       }
 *     }
 *
 */
router.post("/", authenticate, errorWrapper(async (req, res) => {
    const user = await User.findById(req.user.id);

    if (!user.permissions.editLessons)
        return util.error(res, "You do not have permissions.editLessons permission.", 401);

    // TODO: require all stuff

    const data = Lesson.findOne({ title: req.body.title, branch: req.body.branch });

    if (data)
        return util.error(res, "That lesson already exists!", 400);

    const lesson = new Lesson({
        title: req.body.title,
        branch: req.body.branch,
        prerequisites: req.body.prerequisites || []
    });
    lessonModel = await lesson.save();

    const sterilizedLesson = util.sterilizeLesson(lessonModel);

    try {
        await uploadLessonData({ id: sterilizedLesson.id }, req.body.data || "This is a default lesson");
    } catch (e) {
        await Lesson.findByIdAndRemove(sterilizedLesson.id);
        return util.error(res, err);
    }

    return util.data(res, sterilizedLesson);
}));

/**
 * @api {get} /lessons/:id Get lesson by id
 * @apiName Get lesson by id
 * @apiGroup Lessons
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.id Lesson id
 * @apiSuccess {String} data.title Lesson title
 * @apiSuccess {String} data.branch Lesson branch
 * @apiSuccess {String} data.prerequisites Lesson prerequisites
 * @apiSuccess {String} data.data Lesson data
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "id": "4159<3u",
 *         "title": "CAD Basics",
 *         "branch": "design",
 *         "prerequisites": [],
 *         "data": "This is a default lesson"
 *       }
 *     }
 *
 */
router.get("/:id", errorWrapper(async (req, res) => {
    if (!util.validId(req.params.id)) {
        return util.error(res, "That lesson does not exist!", 400);
    }

    const lesson = await Lesson.findById(req.params.id);

    if (!lesson)
        return util.error(res, "That lesson does not exist!", 400);

    const data = await getLessonData({ id: lesson.id });

    return util.data(res, util.sterilizeLessonWithData(lesson, data));
}));

/**
 * @api {put} /lessons/:id Update lesson
 * @apiName Upload lesson data
 * @apiGroup Lessons
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} [title] Lesson title
 * @apiParam {String} [branch] Lesson branch
 * @apiParam {String} [data] Lesson data
 * @apiParam {String} [prerequisites] Lesson prerequisites
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.id Lesson id
 * @apiSuccess {String} data.title Lesson title
 * @apiSuccess {String} data.branch Lesson branch
 * @apiSuccess {String} data.prerequisites Lesson prerequisites
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "id": "4159<3u",
 *         "title": "CAD Basics",
 *         "branch": "design",
 *         "prerequisites": []
 *       }
 *     }
 *
 */
router.put("/:id", authenticate, errorWrapper(async (req, res) => {
    const user = await User.findById(req.user.id);

    if (!user.permissions.editLessons)
        return util.error(res, "You do not have permissions.editLessons permission.", 401);

    const lesson = await Lesson.findOne({ _id: req.params.id });

    if (!lesson)
        return util.error(res, "That lesson does not exist!", 400);

    let set = _.pick(req.body, ["title", "branch", "prerequisites"]);

    const lessonUpdated = await Lesson.findOneAndUpdate({ _id: req.params.id }, { $set: set });

    return util.data(res, lessonUpdated);
}));

/**
 * @api {get} /lessons Get all lessons
 * @apiName Get all lessons
 * @apiGroup Lessons
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.id Lesson id
 * @apiSuccess {String} data.title Lesson title
 * @apiSuccess {String} data.branch Lesson branch
 * @apiSuccess {String} data.prerequisites Lesson prerequisites
 * @apiSuccess {String} data.data Lesson data
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": [
 *         {
 *           "id": "i<3u",
 *           "title": "CAD22",
 *           "branch": "design",
 *           "prerequisites": []
 *         },
 *         {
 *           "id": "4159<3u",
 *           "title": "CAD Basics",
 *           "branch": "design",
 *           "prerequisites": []
 *         }
 *       ]
 *     }
 *
 */
router.get("/", errorWrapper(async (req, res) => {
    const lessons = await Lesson.find({});

    if (!lessons)
        return util.error(res, "There are no lessons in the database.");

    return util.data(res, lessons.map((lesson) => util.sterilizeLesson(lesson)));
}));

module.exports = router;
