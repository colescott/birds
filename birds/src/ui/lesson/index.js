import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardTitle, CardText } from "material-ui/Card";

import ReactMarkdown from "react-markdown";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    flex: {
        flex: 1,
    }
};

const lessonProgressColors = {
    complete: "green",
    unstarted: "red",
    inprogress: "yellow",
    missingPrereqs: "darkred"
};

const lessonProgressNames = {
    complete: "Finished",
    unstarted: "Not started",
    inprogress: "In progress",
    missingPrereqs: "Missing prerequisites"
};

const Lesson = (props) => {
    // TODO: move this to map state to props
    const progress =  props.lessonProgress[ props.lesson.id ] || "unstarted";

    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardTitle
                        title={props.lesson.title || "Loading..."}
                        subtitle={props.lesson.title ? lessonProgressNames[ progress ] : ""}
                        subtitleColor={lessonProgressColors[ progress ]}
                    />
                    <Card zDepth={1} style={styles.flex}>
                        <CardText>
                            <ReactMarkdown source={props.lesson.data || ""} />
                        </CardText>
                    </Card>
                </Card>
            </div>
        </div>
    );
};

const mapStateToProps = (state) => ({
    lesson: s.getLesson(state),
    list: s.getLessonList(state),
    get lessonProgress() {
        var progressArr = s.getUser(state).progress;
        var progressObj = {};
        for (const item in progressArr) {
            progressObj[ progressArr[ item ].id ] = progressArr[ item ].state;
        }
        Object.values(this.list).forEach((lesson) => {
            for (const prereq in lesson.prerequisites) {
                if (progressObj[ prereq.id ] != "complete") {
                    progressObj[ lesson.id ] = "missingPrereqs";
                    return;
                }
            }
        });
        return progressObj;
    },
});

const mapDispatchToProps = (dispatch) => ({
  loadLesson: (id) => () => dispatch(a.getLesson(id)),
});

export default connect(mapStateToProps, mapDispatchToProps)(Lesson);
