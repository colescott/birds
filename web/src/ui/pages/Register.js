// @flow

import React from "react";
import PropTypes from "prop-types";
import { connect } from "react-redux";
import { push } from "react-router-redux";

import pick from "lodash.pick";

import Form from "../util/Form";
import TextInput from "../components/TextInput";
import Alert from "../components/Alert";

import * as a from "../../store/actions";
import * as s from "../../store/selectors";

import "./register.css";

class Register extends Form {
    props: {
        register: () => void,
        status: Object
    };
    static propTypes = {
        register: PropTypes.func
    };
    static validation = {
        firstname(firstname = "") {
            if (!firstname) return "You must enter a first name";
        },
        lastname(lastname = "") {
            if (!lastname) return "You must enter a last name";
        },
        email(email = "") {
            if (!email.match("@")) return "You must enter a valid email";
        },
        password(password = "") {
            if (!password) return "You must enter a password";
        },
        password2(verification = "", { password = "" }) {
            if (!verification) return "You must verify your password";
            if (verification !== password) return "Passwords must match";
        }
    };
    constructor() {
        super();
        this.validation = Register.validation;
    }
    render() {
        const { displayError, updateForm, formBlur, submitForm } = this;
        const { form } = this.state;
        const { register } = this.props;
        const { loading, error } = this.props.status;

        const disableButton = this.disableSubmit() || loading;

        const submit = () =>
            submitForm(() => {
                register(
                    pick(form, ["email", "firstname", "lastname", "password"])
                );
            });

        return (
            <div id="register-page">
                <div id="register-form">
                    <h1> Register </h1>
                    <Alert type="danger" text={error} />
                    <TextInput
                        className="register-input"
                        placeHolder="First Name"
                        value={form["firstname"]}
                        onChange={updateForm("firstname")}
                        error={displayError("firstname")}
                        onBlur={formBlur("firstname")}
                    />
                    <TextInput
                        className="register-input"
                        placeHolder="Last Name"
                        value={form["lastname"]}
                        onChange={updateForm("lastname")}
                        error={displayError("lastname")}
                        onBlur={formBlur("lastname")}
                    />
                    <TextInput
                        className="register-input"
                        placeHolder="Email"
                        value={form["email"]}
                        onChange={updateForm("email")}
                        error={displayError("email")}
                        onBlur={formBlur("email")}
                    />
                    <TextInput
                        className="register-input"
                        placeHolder="Password"
                        type="password"
                        value={form["password"]}
                        onChange={updateForm("password")}
                        error={displayError("password")}
                        onBlur={formBlur("password")}
                    />
                    <TextInput
                        className="register-input"
                        placeHolder="Password Verification"
                        type="password"
                        value={form["password2"]}
                        onChange={updateForm("password2")}
                        error={displayError("password2")}
                        onBlur={formBlur("password2")}
                    />
                    <button
                        className="btn btn-primary"
                        onClick={submit}
                        disabled={disableButton}
                    >
                        Register
                    </button>
                </div>
            </div>
        );
    }
}

const register = (data: RegisterData) => async (dispatch: Dispatch) => {
    try {
        await dispatch(a.register(data));
        await dispatch(a.login(data.email, data.password));
        dispatch(push("/"));
    } catch (e) {
        console.error(e);
        dispatch(a.registerError(e.message));
    }
};

const mapStateToProps = (state: State) => ({
    data: s.getAuthData(state),
    status: s.getAuthStatus(state)
});

export default connect(mapStateToProps, { register })(Register);
