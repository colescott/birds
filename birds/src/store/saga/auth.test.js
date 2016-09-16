import { assert } from "chai";
import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import auth from "./auth";
import { login, logout, register } from "./user";

import * as c from "../constants.js";
import * as s from "../selectors.js";

describe("AUTH SAGA", () => {
    it("should wait for LOGIN, LOGOUT, or REGISTER actions", () => {
        const saga = auth();
        assert.deepEqual(
            saga.next().value,
            take([
                c.LOGOUT_AUTH,
                c.LOGIN_AUTH,
                c.REGISTER_AUTH
            ])
        );
    });
    describe("LOGIN", () => {
        it("should call the login saga", () => {
            const saga = auth();
            const userData = { email: "test", password: "pass" };
            saga.next();
            const { value: out } = saga.next({ type: c.LOGIN_AUTH, payload: userData });
            assert.deepEqual(
                out,
                call(login, userData.email, userData.password)
            );
        });
        it("should redirect to the register success page", () => {
            const userData = { email: "test", password: "pass" };
            const saga = auth();
            saga.next();
            saga.next({ type: c.LOGIN_AUTH, payload: userData });
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/registerSuccess"))
            );
        });
    });
    describe("LOGOUT", () => {
        it("should clear the user reducer", () => {
            const saga = auth();
            saga.next();
            const { value: out } = saga.next({ type: c.LOGOUT_AUTH });
            assert.deepEqual(
                out,
                call(logout)
            );
        });
        it("should redirect back to the home page", () => {
            const saga = auth();
            saga.next();
            saga.next({ type: c.LOGOUT_AUTH });
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/"))
            );
        });
    });
    describe("REGISTER", () => {
        it("should select the register form", () => {
            const saga = auth();
            saga.next();
            const { value: out } = saga.next({ type: c.REGISTER_AUTH });
            assert.deepEqual(
                out,
                select(s.getRegisterForm)
            );
        });
        it("should register", () => {
            const saga = auth();
            const userData = { email: "email", pass: "pass", firstname: "fname", lastname: "lname" };
            saga.next();
            saga.next({ type: c.REGISTER_AUTH });
            const { value: out } = saga.next(userData);
            assert.deepEqual(
                out,
                call(register, userData)
            );
        });
        it("should login", () => {
            const saga = auth();
            const userData = { email: "email", pass: "pass", firstname: "fname", lastname: "lname" };
            saga.next();
            saga.next({ type: c.REGISTER_AUTH });
            saga.next(userData);
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                call(login, userData.email, userData.password)
            );
        });
        it("it should redirect back to the front page", () => {
            const saga = auth();
            const userData = { email: "email", pass: "pass", firstname: "fname", lastname: "lname" };
            saga.next();
            saga.next({ type: c.REGISTER_AUTH });
            saga.next(userData);
            saga.next();
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/"))
            );
        });
    });
});
