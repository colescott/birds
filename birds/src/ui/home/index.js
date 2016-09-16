import React, { Component } from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

const Home = (props) => {
    if (props.user.teamnumber) {
        return (
            <div>
                <h1> {`Welcome, ${props.user.firstname}`} </h1>
                <p>
                    {
                        props.user.isAdmin
                        ? `You are an admin on team ${props.user.teamnumber}`
                        : `You are a member of team ${props.user.teamnumber}`
                    }
                </p>
            </div>
        );
    } else {
        return (
            <div>
                <h1> Welcome to Birds </h1>
                Get learned
            </div>
        );
    }
};

const mapStateToProps = (state) => ({
    user: s.getAuth(state)
});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Home);
