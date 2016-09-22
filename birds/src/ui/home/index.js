import React, { Component } from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

const adminPage = (user) => {
    return (
        <div>
            <p> { `You are an admin of team ${user.teamnumber}`} </p>
            {
                user.teamPass
                ? <p> { `Your teams passsord is ${user.teamPass}`} </p>
                : null
            }
        </div>
    );
};

const memberPage = (user) => {
    return (
        <div>
            <p> {`You are a member of team ${user.teamnumber}`} </p>
        </div>
    );
};

const Home = (props) => {
    if (props.user.id) {
        return (
            <div>
                <h1> {`Welcome, ${props.user.firstname}`} </h1>
                <p>
                    {
                        props.user.isAdmin
                        ? adminPage(props.user)
                        : memberPage(props.user)
                    }
                </p>
                <p>Welcome to birds!</p>
                <p>The content is currently under construction. Check back in a few days to get started!</p>
            </div>
        );
    } else {
        return (
            <div>
                <h1> Welcome to Birds </h1>
                <p>Birds is the Beginning and Intermediate Role Development System devloped by Team 4159 CardinalBotics.</p>
                <p>This website will guide you through learning about everything robotics. Sign in to start!</p>
            </div>
        );
    }
};

const mapStateToProps = (state) => ({
    user: s.getUser(state)
});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Home);
