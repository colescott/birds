// @flow

import type { User } from "./remoteTypes";

declare type LOGIN_START = {
    type: "LOGIN_START"
};

declare type LOGIN_ERROR = {
    type: "LOGIN_ERROR",
    payload: string
};

declare type LOGIN_SUCCESS = {
    type: "LOGIN_SUCCESS",
    payload: {
        user: User,
        token: string
    }
};

declare type REGISTER_START = {
    type: "REGISTER_START"
};

declare type REGISTER_SUCCESS = {
    type: "REGISTER_SUCCESS",
    payload: User
};

declare type REGISTER_ERROR = {
    type: "REGISTER_ERROR",
    payload: string
};

declare type LOGOUT = {
    type: "LOGOUT"
};

declare type Action =
    | LOGIN_START
    | LOGIN_ERROR
    | LOGIN_SUCCESS
    | REGISTER_START
    | REGISTER_SUCCESS
    | REGISTER_ERROR
    | LOGOUT;
