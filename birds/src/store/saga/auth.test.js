import { assert } from "chai";
import { take, call, put } from "redux-saga/effects";
import api from "../../api";

import auth from "./auth";
import * as c from "../constants.js";
import * as a from "../actions.js";

describe("AUTH SAGA", () => {
    it("should return a function", () => {
        assert.typeOf(auth, "Function");
    });
    it("should wait for LOGIN, LOGOUT, or REGISTER actions", () => {
        const saga = auth();
        assert.deepEqual(
            saga.next().value,
            take([
                c.LOGOUT_AUTH,
                c.LOGOUT_AUTH
            ])
        );
    })
    describe("LOGIN", () => {
        it("should call the login api", () => {
            const saga = auth();
            saga.next();
            const apiCall = saga.next({
                type: c.LOGIN_AUTH,
                payload: {
                    username: "test",
                    password: "test"
                }
            });
            assert.deepEqual(
                apiCall,
                call(api.auth.login, "test", "password")
            );
        });
        it("should update the auth reducer on success", () => {
            const data = {
                username: "test",
                password: "test",
                token: "aaa.bbb.ccc"
            };
            const saga = auth();
            saga.next();
            saga.next({
                type: c.LOGIN_AUTH,
                payload: {
                    username: "test",
                    password: "test"
                }
            });
            const action = saga.next({ data });
            assert.deepEqual(
                action,
                put(a.setAuth(data))
            );
        })
        it("should dispatch an error action on failure", () => {
            const error = {
                message: "Something went wrong",
            };
            const saga = auth();
            saga.next();
            saga.next({
                type: c.LOGIN_AUTH,
                payload: {
                    username: "test",
                    password: "test"
                }
            });
            const action = saga.next({ error });
            assert.deepEqual(
                action,
                put(a.setAuth(new Error(error.message)))
            );
        });
    });
    describe("LOGOUT", () => {
        it("should call an auth reducer reset", () => {
            const saga = auth();
            saga.next();
            const action = saga.next({ type: c.LOGOUT_AUTH });
            assert.deepEqual(
                action,
                put(a.resetAuth())
            );
        });
    });
    describe("REGISTER", () => {
        it("it should call the regester api")
        it("it should dispatch a login action on success")
        it("it should dispatch an error action on failure")
    });
    // TODO: check that all trigger acions eventually return to the ready state
    it("should go back to listening for events", () => {

    })
});
