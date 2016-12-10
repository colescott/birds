import React from "react";
import { connect } from "react-redux";
import OmniForm from "react-omniform/src";

//import * as s from "../../store/selectors.js";
import * as a from "../../store/actions.js";

import { Card, CardHeader, CardText } from "material-ui/Card";
import { FormItem, ButtonItem } from "../components/form.js";

import withState from "recompose/withState";

const addJoinForm = withState("joinForm", "updateJoinForm", {});
const addCreateForm = withState("createForm", "updateCreateForm", {});

const selectTeam = (props) => {
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
                    title="Select Team"
                />
                <CardText>
                    <h2> Join a Team </h2>
                    <OmniForm
                        items={{
                            number: "Team Number",
                            pass: "Team Password"
                        }}
                        types={{
                            number: "number",
                            password: "password"
                        }}
                        values={props.joinForm}
                        updateValue={props.updateJoinForm}
                        FormItem={FormItem}
                        ButtonItem={ButtonItem}
                        submitForm={props.joinTeam(props.joinForm)}
                    />
                    <h2> Create a Team </h2>
                    <OmniForm
                        items={{
                            name: "Team Name",
                            number: "Team Number"
                        }}
                        types={{
                            number: "number"
                        }}
                        values={props.createForm}
                        updateValue={props.updateCreateForm}
                        FormItem={FormItem}
                        ButtonItem={ButtonItem}
                        submitForm={props.createTeam(props.createForm)}
                    />
                </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = (state, { joinForm, createForm }) => ({
    joinForm,
    createForm
});

const mapDispatchToProps = (dispatch, { updateJoinForm, updateCreateForm }) => ({
    updateJoinForm: (key, value) => updateJoinForm(form => ({
        ...form,
        [ key ]: value
    })),
    updateCreateForm: (key, value) => updateCreateForm(form => ({
        ...form,
        [ key ]: value
    })),
    createTeam: ({ name, number }) => () => dispatch(a.createTeam(name, number)),
    joinTeam: ({ number, pass }) => () => dispatch(a.joinTeam(number, pass))
});

export default addCreateForm(
    addJoinForm(
        connect(mapStateToProps, mapDispatchToProps)(selectTeam)
    )
);
