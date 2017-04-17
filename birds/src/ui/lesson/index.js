import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardTitle, CardHeader, CardText } from "material-ui/Card";

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

const listPrerequisites = (props) => {
    if (!props.lesson.prerequisites || props.lesson.prerequisites.length <= 0)
        return <CardText>
            Looks like there's no prerequisites! Woohoo!
        </CardText>;

    if (!props.list)
        return <CardText>
            Loading...
        </CardText>;

    return props.lesson.prerequisites.map(function(lesson, i) {
        return <div key={i}>
            <Link to={`/lesson/${lesson.id}`}>
                <CardText>
                    <div style={{ lineHeight: 0 }}>
                        {props.list[ lesson.id ].title}
                        <div style={{ textAlign: "right", color: lessonProgressColors[ props.lessonProgress[ lesson.id ] ] }}>
                            {lessonProgressNames[ props.lessonProgress[ lesson.id ] ]}
                        </div>
                    </div>
                </CardText>
            </Link>
        </div>;
    });
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
            {props.error && (
                <div style={styles.div}>
                    <Card>
                        <CardHeader
                            title="Error"
                        />
                        <CardText>
                            <p
                                style={{
                                    color: "red"
                                }}
                            >
                                { props.error || ""}
                            </p>
                        </CardText>
                    </Card>
                    <br/>
                </div>
            )}
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardTitle
                        title={props.lesson.title || "Loading..."}
                        subtitle={props.lesson.title ? lessonProgressNames[ progress ] : ""}
                        subtitleColor={lessonProgressColors[ progress ]}
                        actAsExpander={true}
                        showExpandableButton={true}
                    />
                    <CardText
                        expandable={true}
                        >
                        { props.lesson.title && listPrerequisites(props) }
                    </CardText>
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
    error: s.getLessonStatus(state).error,
    lesson: s.getLesson(state),
    list: s.getLessonList(state),
    get lessonProgress() {
        var progressArr = s.getUser(state).progress;
        var progressObj = {};
        for (const item in progressArr) {
            progressObj[ progressArr[ item ].id ] = progressArr[ item ].state;
        }
        const prereqs = this.lesson.prerequisites;
        for (const prereq in prereqs) {
            if (!progressObj[ prereqs[ prereq ].id ]) {
                progressObj[ prereqs[ prereq ].id ] = "unstarted";
            }
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
