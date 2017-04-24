// @flow

import React from "react";
import { connect } from "react-redux";
import { push } from "react-router-redux";

import Form from "../util/Form";
import TextInput from "../components/TextInput";
import Alert from "../components/Alert";

import * as a from "../../store/actions";
import * as s from "../../store/selectors";

import "./login.css";

class Login extends Form {
    props: {
        login: () => void,
        data: Object,
        status: Object
    };
    static validation = {
        email(email = "") {
            if (!email) return "You must enter an email";
        },
        password(password = "") {
            if (!password) return "You must enter a password";
        }
    };
    constructor() {
        super();
        this.validation = Login.validation;
    }
    render() {
        const { displayError, updateForm, formBlur, submitForm } = this;
        const { form } = this.state;
        const { login } = this.props;
        const { loading, error } = this.props.status;

        const disableButton = this.disableSubmit() || loading;

        const submit = () =>
            submitForm(() => {
                login(form.email, form.password);
            });

        return (
            <div id="login-page">
                <div id="login-form">
                    <h1> Login </h1>
                    <Alert type="danger" text={error} />
                    <TextInput
                        placeHolder="Email"
                        value={form["email"]}
                        onChange={updateForm("email")}
                        onBlur={formBlur("email")}
                        error={displayError("email")}
                    />
                    <TextInput
                        placeHolder="Password"
                        type="password"
                        value={form["password"]}
                        onChange={updateForm("password")}
                        onBlur={formBlur("password")}
                        error={displayError("password")}
                    />
                    <button
                        className="btn btn-primary"
                        disabled={disableButton}
                        onClick={submit}
                    >
                        {" "}
                        Login{" "}
                    </button>
                </div>
            </div>
        );
    }
}

const login = (email: string, password: string) => async function(
    dispatch: Dispatch
) {
    try {
        await dispatch(a.login(email, password));
        dispatch(push("/"));
    } catch (e) {
        console.error(e);
        dispatch(a.loginError(e));
    }
};

const mapStateToProps = (state: State) => ({
    data: s.getAuthData(state),
    status: s.getAuthStatus(state)
});

export default connect(mapStateToProps, { login })(Login);
