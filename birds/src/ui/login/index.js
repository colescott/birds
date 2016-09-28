import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

import Form from "../components/form";

import { isValidEmail } from "../../util";

const Login = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            <Card>
                <CardHeader
                    title="Login"
                />
                    <CardText>
                        <Form
                            handleUpdate={props.handleUpdate()}
                            handleSubmit={props.login(props.form)}
                            items={[
                                "email",
                                "password"
                            ]}
                            values={props.form}
                            labels={{
                                email: "Email",
                                password: "Password"
                            }}
                            types={{
                                password: "password"
                            }}
                            validation={{
                                email: isValidEmail
                            }}
                        />
                    </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = (state) => ({
    form: s.getLoginForm(state)
});

const mapDispatchToProps = (dispatch) => ({
  handleUpdate: () => (obj) => dispatch(a.setLoginForm({ ...obj })),
  login: (data) => () => dispatch(a.loginAuth(data))
});

export default connect(mapStateToProps, mapDispatchToProps)(Login);
