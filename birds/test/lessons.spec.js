const request = require("supertest");
const testUtil = require("./util.js");
const User = require("../api/v1/models/user");
const mongoose = require("mongoose");
const jwt = require("jsonwebtoken");
const _ = require("lodash");

jest.mock("../api/v1/stores/lessons.js");
const lessonStore = require("../api/v1/stores/lessons.js");

describe("Lessons", () => {
    let app;
    let db;
    let user;
    let token;

    const testUser = {
        email: "test@e.mail",
        firstname: "first",
        lastname: "last",
        progress: []
    }
    const testLesson = {
        title: "TITLE",
        branch: "tree",
        prerequisites: [{
            id: "speling" 
        }],
        data: "?"
    }
    beforeAll(() => {
        process.env.JWT_SECRET = "TEST";
        process.env.NODE_ENV = "TEST";
        process.env.MONGODB_URI = "mongodb://localhost/test-lessons";
        app = require("../app")
        db = mongoose.connection.db;
        const lessonsRouter = require("../api/v1/lessons");
        app.use("/api/v1/lessons", lessonsRouter);
    });
    beforeEach(async () => {
        process.env.JWT_SECRET = "TEST";

        // add a new user
        const newUser = new User(testUser);
        user = await testUtil.addUser(newUser, "testpass");

        // generate a token
        const signedUser = jwt.sign(user, "TEST");
        token = `Bearer ${signedUser}`; 

    });
    afterEach(async () => {
        // nuke the db
        await testUtil.clearDB(db);
    });
    afterAll(async () => {
        await mongoose.disconnect();
    });

    describe("Get Lessons", () => {
        it("Should list all lessons", async () => {
            await request(app)
                .get("/api/v1/lessons")
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        lessons: []
                    });
                });
        });
    });
    describe("Add Lesson", () => {
        it("Should allow adding lessons", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send(testLesson)
                .expect(200)
                .expect(res => {
                    expect(res.body.lesson).toBeDefined();
                    expect(_.omit(res.body.lesson, ["id"]))
                        .toEqual(_.omit(testLesson, ["data"]))
                })
                // I hate this
                .then(data => data.res.body.lesson.id);

            expect(lessonStore.uploadLessonData).toBeCalledWith({ id }, testLesson.data);
        });
        it("Should recover from an aws failure", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            // This currently works, but should it?
            lessonStore.uploadLessonData
                .mockImplementationOnce(() => Promise.reject("LOL"));

            await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send(testLesson)
                .expect(500)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 500,
                        error: "Internal Server Error",
                        message: "Unable to create lesson"
                    });
                });

            expect(lessonStore.uploadLessonData).toBeCalled();

            // Make sure the lesson was removed from the database
            await request(app)
                .get("/api/v1/lessons")
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        lessons: []
                    });
                });
        });
        it("Should require authencticaion", async () => {
            await request(app)
                .post("/api/v1/lessons")
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should require the correct paramaters", async () => {
            await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                })
        });
        it("Should require correct permissions", async () => {
            await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send({
                    title: "Great Unit Tests",
                    branch: "Thing you need to know",
                    prerequisites: "Great Bugs",
                    data: "As if anyone knows"
                })
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "You do not have the required permissions"
                    });
                });
        });
    });
    describe("Get Lessons", () => {
        it("Should require auth", async () => {
            await request(app)
                .get("/api/v1/lessons/1")
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
    });
});