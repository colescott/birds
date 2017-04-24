// @flow

import { Component } from "react";

import mapValues from "lodash.mapvalues";
import pickBy from "lodash.pickby";
import head from "lodash.head";

type validator = (value: string, form: { [map: string]: string }) => ?string;

export default class Form extends Component<*, *, *> {
    state: {
        form: { [map: string]: string },
        validation: { [map: string]: string },
        submitted: boolean,
        visited: { [map: string]: string }
    };
    validation: { [map: string]: validator };
    constructor() {
        super();
        this.validation = {};
        this.state = {
            form: {},
            validation: {},
            visited: {},
            submitted: false
        };
    }
    componentWillMount() {
        // This is to prepopulate the validaton errors with the validator of the subclass
        this.setState(state => ({
            ...state,
            validation: this.validate(state.form)
        }));
    }
    validate(form: { [map: string]: string }) {
        const validation = this.validation;

        const values = pickBy(
            mapValues(validation, (fun, key) => {
                return (fun || (() => false))(form[key], form);
            }),
            x => Boolean(x)
        );
        return values;
    }
    updateForm = (key: string) => {
        return (value: string) => {
            const form = {
                ...this.state.form,
                [key]: value
            };
            this.setState(state => ({
                ...state,
                form,
                validation: this.validate(form)
            }));
        };
    };
    formBlur = (key: string) => {
        return () => {
            const visited = {
                ...this.state.visited,
                [key]: true
            };
            this.setState(state => ({
                ...state,
                visited
            }));
        };
    };
    displayError = (key: string): ?string => {
        const { visited, validation, submitted } = this.state;
        if (submitted || (visited[key] && validation[key]))
            return validation[key];
    };
    submitForm = (f: void => void) => {
        if (this.formValid()) {
            f();
        } else {
            this.setState(state => ({
                ...state,
                submitted: true
            }));
        }
    };
    formValid() {
        return !Boolean(head(Object.keys(this.state.validation)));
    }
    disableSubmit = () => {
        return !this.state.valid && this.state.submitted;
    };
}
