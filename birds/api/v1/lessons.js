const express = require("express");
const router = express.Router();

const util = require("./util.js");
const { error } = require("./util.js");
const {
    authenticate,
    errorWrapper,
    validator,
    permissions
} = require("./middleware.js");
const Lesson = require("./models/lesson");
const User = require("./models/user");

const _ = require("lodash");

const aws = require("aws-sdk");
const s3params = { Bucket: process.env.AWS_BUCKET };
const s3bucket = new aws.S3({ params: s3params });
const { uploadLessonData, getLessonData } = require("./stores/lessons.js");

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
 * @apiSuccessExample {json} Success-Response
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
router.post(
    "/",
    authenticate,
    validator(["title", "branch", "prerequisites", "data"]),
    permissions(["editLessons"]),
    errorWrapper(async (req, res) => {
        const user = req.user;

        const data = await Lesson.findOne({
            title: req.body.title,
            branch: req.body.branch
        });

        if (data) {
            console.warn(data);
            return res
                .status(400)
                .send(error(400, "That lesson already exists"));
        }

        const lesson = new Lesson({
            title: req.body.title,
            branch: req.body.branch,
            prerequisites: req.body.prerequisites || []
        });
        const lessonModel = await lesson.save();

        const sterilizedLesson = util.sterilizeLesson(lessonModel);

        try {
            await uploadLessonData({ id: sterilizedLesson.id }, req.body.data);
        } catch (e) {
            await Lesson.findByIdAndRemove(sterilizedLesson.id);
            return res.status(500).send(error(500, "Unable to create lesson"));
        }

        return res.status(200).send({ lesson: sterilizedLesson });
    })
);

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
router.get(
    "/:id",
    authenticate,
    errorWrapper(async (req, res) => {
        // Mongoose strongly dislikes invalid id
        if (!util.validId(req.params.id))
            return res.status(400).send(error(400, "Invalid ID"));

        const lesson = await Lesson.findById(req.params.id);

        if (!lesson)
            return res
                .status(404)
                .send(error(404, "That lesson does not exist."));

        try {
            const data = await getLessonData({ id: lesson.id });
            return res
                .status(200)
                .send({ lesson: util.sterilizeLessonWithData(lesson, data) });
        } catch (e) {
            return res.status(500).send({
                code: 500,
                error: "Internal Server Error",
                message: "Unable to load lesson data"
            });
        }
    })
);

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
router.patch(
    "/:id",
    authenticate,
    permissions(["editLessons"]),
    errorWrapper(async (req, res) => {
        // Mongoose strongly dislikes invalid ids
        if (!util.validId(req.params.id))
            return res.status(400).send(error(400, "Invalid ID"));

        const user = req.user;
        const id = req.params.id;
        const lesson = await Lesson.findOne({ _id: req.params.id });

        if (!lesson)
            return res
                .status(400)
                .send(error(400, "That lesson does not exist!"));

        // Update S3
        const data = req.body.data;
        if (data) {
            try {
                await uploadLessonData({ id }, data);
            } catch (e) {
                return res.status(500).send({
                    code: 500,
                    error: "Internal Server Error",
                    message: "Unable to update lesson"
                });
            }
        }

        let set = _.pick(req.body, ["title", "branch", "prerequisites"]);
        Object.assign(lesson, set);
        await lesson.save();

        if (data) {
            return res
                .status(200)
                .send({ lesson: util.sterilizeLessonWithData(lesson, data) });
        }

        return res.status(200).send({ lesson: util.sterilizeLesson(lesson) });
    })
);

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
router.get(
    "/",
    authenticate,
    errorWrapper(async (req, res) => {
        const lessons = await Lesson.find({});

        return res.status(200).send({
            lessons: lessons.map(lesson => util.sterilizeLesson(lesson))
        });
        return res.send({
            lessons: lessons.map(lesson => util.sterilizeLesson(lesson))
        });
    })
);

module.exports = router;
