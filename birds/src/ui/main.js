import React from "react";
import { connect } from "react-redux";

import * as s from "../store/selectors.js";

import NavBar from "./NavBar";

const Main = ({ children, auth }) => {
    return (
        <div className="wrapper">
            <div className="main">
                <NavBar
                    title={"Birds"}
                    links={[
                        { name: "Home", to: "/" },
                        { name: "Register", to: "/register" }
                    ]}
                    user={ auth.firstname || "Not Logged In"}
                />
                {
                    children
                }
            </div>
        </div>
  );
};

const mapStateToProps = (state) => ({
    auth: s.getAuth(state)
});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Main);
