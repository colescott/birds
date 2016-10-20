import { assert } from "chai";
import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import auth, { authWrapper, login, logout, register, getUser } from "./auth";
import api from "../../api";

import * as c from "../constants.js";
import * as s from "../selectors.js";
import * as a from "../actions.js";

describe("AUTH SAGA", () => {
    it("should wait for LOGIN, LOGOUT, or REGISTER actions", () => {
        const saga = auth();
        assert.deepEqual(
            saga.next().value,
            take([
                c.AUTH_LOGOUT,
                c.AUTH_LOGIN,
                c.AUTH_REGISTER
            ])
        );
    });
    it("should put an error action on throw", () => {
        const saga = auth();
        try {
            saga.next();
            saga.next(new Error("error"));
        } catch (e) {
            assert.deepEqual(
                e,
                put(a.authFailure("error"))
            );
        }
    });
    describe("LOGIN", () => {
        describe("Has joined team", () => {
            const saga = auth();
            const loginData = {
                token: "test",
                user: {
                    email: "test",
                    password: "pass",
                    teamnumber: 1337
                }
            };
            saga.next();
            it("should call the login saga", () => {
                const { value: out } = saga.next(a.loginAuth(loginData.user));
                assert.deepEqual(
                    out,
                    call(authWrapper, login, loginData.user.email, loginData.user.password)
                );
            });
            it("should update the user data", () => {
                const { value: out } = saga.next(loginData);
                assert.deepEqual(
                    out,
                    put(a.setUser({
                        token: loginData.token,
                        ...loginData.user
                    }))
                );
            });
            it("should call auth login reset", () => {
                const { value: out } = saga.next();
                assert.deepEqual(
                    out,
                    put(a.resetLoginForm())
                );
            });
            it("should redirect to the home page if teamnumber is set", () => {
                const { value: out } = saga.next();
                assert.deepEqual(
                    out,
                    put(push("/"))
                );
            });
        });
        describe("Has not joined team", () => {
            const saga = auth();
            const loginData = {
                token: "test",
                user: {
                    email: "test",
                    password: "pass",
                }
            };
            saga.next();
            it("should call the login saga", () => {
                const { value: out } = saga.next(a.loginAuth(loginData.user));
                assert.deepEqual(
                    out,
                    call(authWrapper, login, loginData.user.email, loginData.user.password)
                );
            });
            it("should update the user data", () => {
                const { value: out } = saga.next(loginData);
                assert.deepEqual(
                    out,
                    put(a.setUser({
                        token: loginData.token,
                        ...loginData.user
                    }))
                );
            });
            it("should call auth login reset", () => {
                const { value: out } = saga.next();
                assert.deepEqual(
                    out,
                    put(a.resetLoginForm())
                );
            });
            it("should redirect to the home page if teamnumber is not set", () => {
                const { value: out } = saga.next();
                assert.deepEqual(
                    out,
                    put(push("/selectTeam"))
                );
            });
        });
    });
    describe("LOGOUT", () => {
        const saga = auth();
        saga.next();
        it("should clear the user reducer", () => {
            const { value: out } = saga.next(a.logoutAuth());
            assert.deepEqual(
                out,
                call(logout)
            );
        });
        it("should redirect back to the home page", () => {
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/"))
            );
        });
    });
    describe("REGISTER", () => {
        const saga = auth();
        saga.next();
        const userData = { email: "email", pass: "pass", firstname: "fname", lastname: "lname" };
        const token = "meh";
        it("should select the register form", () => {
            const { value: out } = saga.next(a.registerAuth());
            assert.deepEqual(
                out,
                select(s.getRegisterForm)
            );
        });
        it("should register", () => {
            const { value: out } = saga.next(userData);
            assert.deepEqual(
                out,
                call(authWrapper, register, userData)
            );
        });
        it("should reset the register form", () => {
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(a.resetRegisterForm())
            );
        });
        it("should login", () => {
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                call(authWrapper, login, userData.email, userData.password)
            );
        });
        it("it should update the user data", () => {
            const { value: out } = saga.next({ user: userData, token: "meh" });
            assert.deepEqual(
                out,
                put(a.setUser({
                    token,
                    ...userData,
                }))
            );
        });
        it("it should go to the select team page", () => {
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/selectTeam"))
            );
        });
    });
});

describe("LOGIN SAGA", () => {
    const email = "test";
    const pass = "pass";
    it("should call the api login function", () => {
        const saga = login(email, pass);
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            call(api.auth.login, email, pass)
        );
    });
    it("should throw if it gets an error", () => {
        const saga = login(email, pass);
        saga.next();
        try {
            saga.next({
                error: {
                    message: "error"
                }
            });
        } catch (e) {
            assert.deepEqual(
                e,
                new Error(e)
            );
        }
    });
    it("should return the user", () => {
        const saga = login(email, pass);
        const testData = {
            data: {
                user: {
                    email,
                    pass
                }
            }
        };
        saga.next();
        const { value: out, done } = saga.next(testData);
        assert.ok(done);
        assert.deepEqual(out, testData.data);
    });
});

describe("LOGOUT SAGA", () => {
    it("should reset the auth reducer", () => {
        const saga = logout();
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            put(a.resetUser())
        );
    });
});

describe("REGISTER SAGA", () => {
    const user = {
        email: "test",
        password: "pass",
        firstname: "James",
        lastname: "Bond"
    };
    it("should call the api register", () => {
        const saga = register(user);
        const { value: out } = saga.next({
            data: {
                user
            }
        });
        assert.deepEqual(
            out,
            call(api.auth.register, user)
        );
    });
    it("should throw if it gets an error", () => {
        const saga = register(user);
        saga.next();
        try {
            saga.next({
                error: {
                    message: "error"
                }
            });
        } catch (e) {
            assert.deepEqual(
                e,
                new Error(e)
            );
        }
    });
    it("should return the user object", () => {
        const saga = register(user);
        saga.next();
        const { value: out, done } = saga.next({
            data: {
                user
            }
        });
        assert.ok(done);
        assert.deepEqual(
            out,
            user
        );
    });
});

describe("GET_USER SAGA", () => {
    const id = 1337;
    const token = "mayo";
    it("should call api get user", () => {
        const saga = getUser(id, token);
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            call(api.auth.getUser, id, token)
        );
    });
    it("should return the user", () => {
        const saga = getUser(id, token);
        saga.next();
        const { value: out, done } = saga.next({
            data: {
                user: {
                    id, token
                }
            }
        });
        assert.ok(done);
        assert.deepEqual(
            out,
            { id, token }
        );
    });
    it("should throw on an error", () => {
        const saga = getUser(id, token);
        saga.next();
        try {
            saga.next({
                error: {
                    message: "error"
                }
            });
        } catch (e) {
            assert.deepEqual(
                e,
                Error("errro")
            );
        }
    });
});
