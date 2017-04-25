// @flow

import { Validator } from "jsonschema";

const v = new Validator();

export const userSchema = {
    id: "user",
    type: "object",
    properties: {
        id: { type: "number" },
        email: { type: "string" },
        lastname: { type: "string" }
    }
};

v.addSchema(userSchema, "/user");

export const errorSchema = {
    id: "error",
    type: "object",
    properties: {
        code: { type: "number" },
        error: { type: "string" }
    }
};

v.addSchema(errorSchema, "error");

export const loginSchema = {
    id: "login",
    type: "object",
    properties: {
        token: { type: "string" },
        user: { $ref: "user " }
    }
};

export default v;
