// @flow

import Validator, * as v from "./validators.js";

export const checkOkay = ({ res, data }: { res: Response, data: Object }) => {
    if (res.ok) {
        return data;
    } else {
        if (Validator.validate(data, v.errorSchema)) {
            throw data.message || "Something went wrong";
        } else {
            return Promise.reject(
                new Error("Error does not match error schema")
            );
        }
    }
};

export const validate = (schema: Object) => (data: Object) => {
    if (Validator.validate(data, schema)) {
        return data;
    } else {
        return Promise.reject(new Error("Response does not match schema"));
    }
};
