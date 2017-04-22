const request = require("supertest");
const testUtil = require("./util.js");
const User = require("../api/v1/models/user");
const mongoose = require("mongoose");
const jwt = require("jsonwebtoken");
const _ = require("lodash");

const express = require("express");
const app = express();

const middleware = require("../middleware");
const lessonsRouter = require("../api/v1/lessons");

jest.mock("../api/v1/stores/lessons.js");
const lessonStore = require("../api/v1/stores/lessons.js");

describe("Lessons", () => {
    let db;
    let user;
    let token;

    const testUser = {
        email: "test@e.mail",
        firstname: "first",
        lastname: "last",
        progress: []
    };
    const testLesson = {
        title: "TITLE",
        branch: "tree",
        prerequisites: [
            {
                id: "speling"
            }
        ],
        data: "?"
    };
    beforeAll(async () => {
        mongoose.Promise = Promise;
        await mongoose.connect("mongodb://localhost/test-lessons");
        app.set("JWT_SECRET", "TEST");
        app.use(middleware("TEST"));
        app.use("/api/v1/lessons", lessonsRouter);
        db = mongoose.connection.db;
    });
    beforeEach(async () => {
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
                    expect(_.omit(res.body.lesson, ["id"])).toEqual(_.omit(testLesson, ["data"]));
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
            lessonStore.uploadLessonData.mockImplementationOnce(() => Promise.reject("LOL"));

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
            await request(app).post("/api/v1/lessons").expect(401).expect(res => {
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
                });
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
    describe("Get Lesson", () => {
        it("Should allow getting lessons", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .send(testLesson)
                .set("authorization", token)
                .expect(200)
                .then(data => data.res.body.lesson.id);

            lessonStore.getLessonData.mockReturnValueOnce(testLesson.data);

            await request(app)
                .get(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.lesson, ["id"])).toEqual(testLesson);
                });

            expect(lessonStore.getLessonData).toBeCalledWith({ id });
        });
        it("Should recover from an aws failure", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .send(testLesson)
                .set("authorization", token)
                .expect(200)
                .then(data => data.res.body.lesson.id);

            lessonStore.getLessonData.mockImplementationOnce(() => Promise.reject("LOL"));

            await request(app)
                .get(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .expect(500)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 500,
                        error: "Internal Server Error",
                        message: "Unable to load lesson data"
                    });
                });
        });
        it("Should require auth", async () => {
            await request(app).get("/api/v1/lessons/1").expect(401).expect(res => {
                expect(res.body).toEqual({
                    code: 401,
                    error: "Unauthorized",
                    message: "No authorization token was found"
                });
            });
        });
        it("Should 400 on an invalid id", async () => {
            await request(app)
                .get("/api/v1/lessons/420")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Invalid ID"
                    });
                });
        });
        it("Should 404 on a non-existant lesson", async () => {
            await request(app)
                .get("/api/v1/lessons/41224d776a326fb40f000001")
                .set("authorization", token)
                .expect(404)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 404,
                        error: "Not Found",
                        message: "That lesson does not exist."
                    });
                });
        });
    });
    describe("Patch Lesson", () => {
        it("Should allow updates", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send(testLesson)
                .expect(200)
                .then(data => data.res.body.lesson.id);

            await request(app)
                .patch(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .send({
                    branch: "Oak Tree"
                })
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.lesson, ["id"])).toEqual(
                        Object.assign({}, testLesson, {
                            branch: "Oak Tree",
                            data: undefined
                        })
                    );
                });

            // Make sure the lesson was actually updated
            await request(app)
                .get(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.lesson, "id")).toEqual(
                        Object.assign({}, testLesson, {
                            branch: "Oak Tree",
                            data: undefined
                        })
                    );
                });
        });
        it("Should allow updating the data", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send(testLesson)
                .expect(200)
                .then(data => data.res.body.lesson.id);

            await request(app)
                .patch(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .send({
                    data: "DATA"
                })
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.lesson, ["id"])).toEqual(
                        Object.assign({}, testLesson, { data: "DATA" })
                    );
                });

            expect(lessonStore.uploadLessonData).toBeCalledWith({ id }, "DATA");
        });
        it("Should survive aws errors", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            const id = await request(app)
                .post("/api/v1/lessons")
                .set("authorization", token)
                .send(testLesson)
                .expect(200)
                .then(data => data.res.body.lesson.id);

            lessonStore.uploadLessonData.mockImplementationOnce(() => Promise.reject("LOL"));

            await request(app)
                .patch(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .send({
                    data: "DATA",
                    branch: "Oak"
                })
                .expect(500)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 500,
                        error: "Internal Server Error",
                        message: "Unable to update lesson"
                    });
                });

            // Updates should be atomic
            await request(app)
                .get(`/api/v1/lessons/${id}`)
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.lesson, "id")).toEqual(_.omit(testLesson, "data"));
                });

            expect(lessonStore.uploadLessonData).toBeCalledWith({ id }, "DATA");
        });
        it("Should require authentication", async () => {
            await request(app).patch("/api/v1/lessons/1").expect(401).expect(res => {
                expect(res.body).toEqual({
                    code: 401,
                    error: "Unauthorized",
                    message: "No authorization token was found"
                });
            });
        });
        it("Should require the correct permissions", async () => {
            await request(app)
                .patch("/api/v1/lessons/1")
                .set("authorization", token)
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "You do not have the required permissions"
                    });
                });
        });
        it("Should 400 on an invalid id", async () => {
            user.permissions = {
                editLessons: true
            };
            await user.save();

            await request(app)
                .patch("/api/v1/lessons/420")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Invalid ID"
                    });
                });
        });
    });
});
