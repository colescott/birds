import React from "react";
import { connect } from "react-redux";

import * as s from "../store/selectors.js";

import NavBar from "./navBar";

const Main = ({ children, auth }) => {
    return (
        <div className="wrapper">
            <div className="main">
                <NavBar
                    title={"Birds"}
                    links={[
                        { text: "Home", to: "/" },
                        ...(
                            auth.token
                            ? [{ text: "Logout", to: "/logout" }]
                            : [
                                { text: "Register", to: "/register" },
                                { text: "Login", to: "/login" },
                            ]
                        )
                    ]}
                    status={ auth.firstname || "Not Logged In"}
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
