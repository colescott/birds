// @flow

import { userSchema, loginSchema } from "./validators.js";
import { validate, checkOkay } from "./util.js";

export const register = (data: RegisterData): Promise<User> =>
    fetch("http://localhost:8000/api/v1/users", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(data)
    })
        .then(res => {
            return res.json().then(data => {
                return {
                    data,
                    res
                };
            });
        })
        .then(checkOkay)
        .then(res => res.user)
        .then(validate(userSchema));

export const login = (
    email: string,
    password: string
): Promise<{ user: User, token: string }> =>
    fetch("http://localhost:8000/api/v1/auth/login", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify({
            email,
            password
        })
    })
        .then(res => {
            return res.json().then(data => {
                return {
                    data,
                    res
                };
            });
        })
        .then(checkOkay)
        .then(validate(loginSchema));
